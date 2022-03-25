##Function that performs the entire workflow for the weather only and two step calibration process with tnormal for power distributions
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
#' @param norm_l The list of weather parameters to be fitted with the normal distribution
#' @param tnorm_l The list of parameters to be fitted with the truncated normal distribution
#' @param gamma_l The list of parameters to be fitted with the gamma distribution
#' @return A list of important results - CRPS, Plots etc.
#' @export

workflow_combined_tnormal <- function(ensemble_name,
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
                                      norm_l,
                                      tnorm_l,
                                      gamma_l,
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
  
  source("~/Work/Gits/ensemble_post_processing/calibration/emos_power_ensembles.R")
  
  power_emos_emos <- emos_power_ensembles(power_ensembles = emos_prediction,
                                          horizons = horizons,
                                          daysToTrain = emosDaysToTrain)
  message("EMOS Power Calibration Complete")
  
  
  power_emos_emos_dist <- emos_power_distribution(emos_params = power_emos_emos,
                                                  power_ensembles = emos_prediction,
                                                  horizons = horizons,
                                                  daysToTrain = emosDaysToTrain)
  
  
  power_emos_cal_pit <- create_pit_histogram(power_distribution_list = power_emos_emos_dist,
                                             limits = power_emos_limits,
                                             horizons = horizons,
                                             distrib = "truncnormal")
  
  
  cal_emos_prediction <- emos_power_forecast(emos_params = power_emos_emos,
                                             power_ensembles = emos_prediction,
                                             horizons = horizons,
                                             daysToTrain = emosDaysToTrain)
  
  
  source("~/Work/Gits/ensemble_post_processing/evaluation/calculate_crps.R")
  
  cal_emos_eval <- calculate_mean_crps(pred_list = cal_emos_prediction,
                                       horizons = horizons)
  

  
  final_res <- data.frame(matrix(0, nrow = 2, ncol = length(raw_emos_eval$CRPS)))
  colnames(final_res) <- raw_emos_eval$horizon
  rownames(final_res) <- c("One-W", "Two-WP")
  final_res[1,] <- raw_emos_eval$CRPS
  final_res[2,] <- cal_emos_eval$CRPS
  
  full_weather_eval <- calculate_full_crps(pred_list = emos_prediction,
                                           horizons = horizons)
  
  full_power_eval <- calculate_full_crps(pred_list = cal_emos_prediction,
                                         horizons = horizons)
  
  full_crps <- list(full_weather_eval, full_power_eval)
  
  
  all_results <- list()
  all_results[["Weather_Ranked_Hist"]] <- weather_raw_ranked_hist
  all_results[["Weather_Pit"]] <- weather_pit
  all_results[["Power_Uncal_Rank_Hist"]] <- power_emos_verification_rank
  all_results[["Power_Cal_Pit"]] <- power_emos_cal_pit
  all_results[["CRPS"]] <- final_res
  all_results[["Full_CRPS"]] <- full_crps
  
  return(all_results)
}