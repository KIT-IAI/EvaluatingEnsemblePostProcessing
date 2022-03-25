##Function that trains models
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
#' @param standardLinear A boolean to indicate if the standard or alternative linear model should be used
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

workflow_train <- function(ensemble_name,
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
                             standardLinear = TRUE,
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
                             RFModel = FALSE) {
  
  if(linearModel & neuralModel & RFModel) {
    stop("Currently not possible to implement a workflow for multiple models at the same time!")
  }
  
  source("~/Work/Gits/ensemble_post_processing/pre_processing/create_correct_structure.R")
  
  structured_data <- create_correct_structure(ens_name =  ensemble_name,
                                              control_name =  control_name,
                                              obs_name =  obs_name,
                                              file_path = file_path,
                                              filter_time = structure_cutoff_time,
                                              w_parameters = weather_params)
  
  
  if(scaleRequired) {
    source("~/Work/Gits/ensemble_post_processing/pre_processing/scale_structured_data.R")

    
    structured_data <- scale_structured_data(ensemble_list = structured_data, 
                                             weather_parameters = weather_params,
                                             normalise_l = normalise_params,
                                             scale_l = scale_params)
    
  }
  
  source("~/Work/Gits/ensemble_post_processing/forecasting/prepare_training_data.R")
  
  train_data <- prepare_training_data(wp_name = power_name,
                                      obs_name = obs_name,
                                      file_path = file_path,
                                      train_time = train_time,
                                      weather_params = weather_params)
  
  
  if(scaleRequired) {
    source("~/Work/Gits/ensemble_post_processing/forecasting/scale_params_power.R")
    
    power_scale_saved <- save_power_scale_params(train_data)
    
    source("~/Work/Gits/ensemble_post_processing/forecasting/scale_training_data.R")
    
    train_data <- scale_training_data(training_data = train_data,
                                      scale_parameters = scale_params,
                                      take_l = safe_params,
                                      normalise_l = normalise_params,
                                      scale_l = scale_params)
    
    
  }
  
  if(linearModel) {
    if(standardLinear) {
      source("~/Work/Gits/ensemble_post_processing/forecasting/train_linear_model.R")
      
      linear_models <- train_linear_models(training_data = train_data,
                                           onlyPositive =  linearOnlyPositive,
                                           includeIntercept = linearIncludeIntercept,
                                           horizons = horizons,
                                           reg_params = regression_parameters)
      models = linear_models
    }if(!standardLinear){
      source("~/Work/Gits/ensemble_post_processing/forecasting/train_alternative_linear_model.R")
      
      linear_models <- train_linear_models(training_data = train_data,
                                           onlyPositive =  linearOnlyPositive,
                                           includeIntercept = linearIncludeIntercept,
                                           horizons = horizons,
                                           reg_params = regression_parameters)
      models = linear_models
    }
   
    
    
  } else if(neuralModel) {
    
    source("~/Work/Gits/ensemble_post_processing/forecasting/train_neural_model.R")
    
    neural_models <- train_neural_models(training_data = train_data,
                                         horizons = horizons,
                                         reg_params = regression_parameters,
                                         hidden_l = neuralConfig,
                                         threshold_l = neuralThreshold)
    models = neural_models
    
  } else if(RFModel) {
    
    source("~/Work/Gits/ensemble_post_processing/forecasting/train_rf_model.R")
    
    rf_models <- train_rf_models(training_data = train_data, 
                                 horizons = horizons, 
                                 reg_params = regression_parameters)
    models = rf_models
    
  }
  
  return(models)
}