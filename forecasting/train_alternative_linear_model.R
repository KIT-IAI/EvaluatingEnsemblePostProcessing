##Function that trains the linear prediction models
#' @param training_data The dataframe containing the training data for the regression
#' @param onlyPositive Boolean indicating whether only positive regression coefficients should be allowed
#' @param includeIntercept Boolean indicating whether the intercept should be included or not. NOTE: It
#'                         is currently impossible to include the intercept when training a strictly positive linear model
#' @param horizons The forecast horizons for which models should be developed
#' @param reg_params The regression parameters to be used
#' @return A list of the trained linear models.
#' @export


train_linear_models <- function(training_data, onlyPositive, includeIntercept, horizons, reg_params) {
  require(dplyr)
  require(nnls)
  
  #Correct mistake in observation data
  if(training_data[length(training_data$time), "horizon"] == 0) {
    training_data <- training_data[-length(training_data$time),]
  }
  
  lin.m <- list()
  print("FANCY LINEAR MODELS STARTING")
  for (h in horizons) {
    hr <- paste("h", h, sep = "")
    
    message(paste("Currently Training for a Horizon of",h,"hours"))
    
    
    
    #Extract full data frame to use for origins
    origins <- training_data %>% filter(is_origin == 1)
    
    #Filter horizon
    train_data <- training_data %>% filter(horizon == h)
    
    #For Swedish Data correct size
    if(unique(origins$horizon == 24)) {
    origins <- origins[-length(origins$time),]
    train_data <- train_data[-1,]
    }
    
    train_data[,"origins"] <- origins[,"wind_power"]
    
    #Train the models
    if(!onlyPositive) { 
      #Allowing negative regression coefficients
      
      if(includeIntercept) { 
        #Including intercept in model
        h_reg_params <- c("speed","speed2","speed3","b4","b5","b6","b7")
        info <- paste("wind_power ~", paste("", h_reg_params, collapse = "+"))
        message(paste("Linear Model trained as: ", info, sep = ""))
        linear_formula <- as.formula(paste("wind_power ~", paste("", h_reg_params, collapse = "+")))
        linear_model <- lm(linear_formula, data = train_data)
        
      } else if(!includeIntercept) { 
        #No intercept in model
        print("THIS IS WRONG")
      }
    } else if(onlyPositive) { 
      #Allowing only positive regression coefficients
      
      print("THIS IS ALSO WRONG")
    }
    lin.m[[hr]] <- linear_model
  }
  return(lin.m)
}