##Function that performs the entire workflow for the one step calibration process with the tnorm distribution
#' @param ensemble_name The name of the ensemble csv file
#' @param control_name The name of the control csv file
#' @param obs_name The name of the observation csv file
#' @param power_name The name of the wind power csv file
#' @param file_path The file path for the csv files
#' @param weather_params The weather parameters to be considered
#' @param horizons THe forecast horizons to be considered
#' @param normalise_params The parameters that should be normalised
#' @param scaleRequired Boolean to indicate whether the data should be scaled or not
#' @param scale_params The paramters that should be scaled
#' @param safe_params The parameters that should not be touched
#' @param structure_cutoff_time THe end time when generating the initial ensemble data structure
#' @param train_time The time used to divide the training data set
#' @param test_start_time The time used to select that start of the test data set
#' @param test_end_time The time used to select the end of the test data set
#' @param regression_parameters The parameters used for the regression models
#' @param linearModel A boolean to indicate whether a linear model should be used or not
#' @param linearOnlyPositives A boolean to indicate whether the linear model should be restricted to positive coefficients
#' @param linearIncludeIntercept A boolean to indicate whether the linear model should include an intercept
#' @param neuralModel A boolean to indicate whether a neural network should be used or not
#' @param neuralThreshold The threshold used for training the neural network
#' @param neuralConfig The layer configuration of the neural network
#' @param emosDaysToTrain The number of days used to train the EMOS models.
#' @param isBenchmark A boolean to indicate whether a benchmark dataset is being used or not
#' @param benchMaxCap The maximum capacity of the wind power generation for benchmark datasets
#' @return The list of important results - CRPS, plots etc.
#' @export

workflow_one_bma <- function(ensemble_name,
                                 control_name,
                                 obs_name,
                                 power_name,
                                 file_path,
                                 weather_params,
                                 horizons,
                                 normalise_params,
                                 scaleRequired = TRUE,
                                 scale_params,
                                 safe_params,
                                 structure_cutoff_time,
                                 train_time,
                                 test_start_time,
                                 test_end_time,
                                 regression_parameters,
                                 linearModel = TRUE,
                                 linearOnlyPositive = FALSE,
                                 linearIncludeIntercept = TRUE,
                                 neuralModel = FALSE,
                                 neuralThreshold = 0.01,
                                 neuralConfig = c(7,10),
                                 emosDaysToTrain = 30,
                                 isBenchmark = FALSE,
                                 benchMaxCap = NULL,
                                 distrib,
                                 RFModel = FALSE,
                                 predict_data) {
  
  if(linearModel & neuralModel & RFModel) {
    stop("Currently not possible to implement a workflow for multiple models at the same time!")
  }
  
  rank_limits <- predict_data[["rank_limits"]]
  raw_ens_prediction <- predict_data[["raw_ens_prediction"]]
  emos_prediction <- predict_data[["emos_prediction"]]
  weather_raw_ranked_hist <- predict_data[["weather_raw_ranked_hist"]]
  weather_pit <- predict_data[["weather_pit"]]
  power_emos_verification_rank <- predict_data[["power_emos_verification_rank"]]
  raw_emos_eval <- predict_data[["raw_emos_eval"]]
  rank_hist <- predict_data[["rank_hist"]]
  power_emos_limits <- predict_data[["power_emos_limits"]]
  
  
  source("~/Work/Gits/ensemble_post_processing/calibration/bma_power_ensembles.R")
  
  emos_parameters <- bma_power_ensembles(power_ensembles = raw_ens_prediction, 
                                          horizons = horizons,
                                          daysToTrain = emosDaysToTrain)
  
  distribution_parameters <- bma_power_distribution(bma_params = emos_parameters, 
                                                     power_ensembles = raw_ens_prediction, 
                                                     horizons = horizons,
                                                     daysToTrain = emosDaysToTrain)
  
  pit_hist <- create_pit_histogram(power_distribution_list = distribution_parameters, 
                                   limits = rank_limits, 
                                   horizons = horizons,
                                   distrib = "gamma")
  
  
  cal_ens_prediction <- bma_power_forecast(bma_params = emos_parameters, 
                                            power_ensembles = raw_ens_prediction, 
                                            horizons = horizons,
                                            daysToTrain = emosDaysToTrain)
  
  source("~/Work/Gits/ensemble_post_processing/evaluation/calculate_crps.R")
  
  
  raw_eval <- calculate_mean_crps(pred_list = raw_ens_prediction,
                                  horizons = horizons)
  
  cal_eval <- calculate_mean_crps(pred_list = cal_ens_prediction,
                                  horizons = horizons)
  
  l <- length(raw_eval$CRPS)
  
  result_df <- as.data.frame(matrix(0, nrow = 2, ncol = l))
  
  result_df[1,] <- raw_eval$CRPS
  result_df[2,] <- cal_eval$CRPS
  
  rownames(result_df) <- c("RAW", "CALIBRATED")
  colnames(result_df) <- horizons
  
  if(isBenchmark) {
    result_df[,'0'] <- NULL
  }
  
  full_raw_eval <- calculate_full_crps(pred_list = raw_ens_prediction,
                                       horizons = horizons)
  
  full_cal_eval <- calculate_full_crps(pred_list = cal_ens_prediction,
                                       horizons = horizons)
  
  full_crps <- list(full_raw_eval, full_cal_eval)
  
  all_results <- list()
  all_results[["CRPS"]] <- result_df
  all_results[["Power_Full_CRPS"]] <- full_crps
  all_results[["Power_Rank_Histogram"]] <- rank_hist
  all_results[["Power_PIT_Histrogram"]] <- pit_hist
  all_results[["Prediction_Raw"]] <- raw_ens_prediction
  all_results[["Prediction_cal"]] <- cal_ens_prediction
  
  return(all_results)
  
}