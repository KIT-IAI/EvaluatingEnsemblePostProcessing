##BMA method to calibrate the raw weather ensembles
#' @param original_ensembles A list containing the raw weather ensembles
#' @param weather_params A vector containing the weather params that need to be calibrated
#' @param norm_l A vector containing the parameters that need to be calibrated with the normal distribution
#' @param tnorm_l A vector containing the parameters that need to be calibrated with the truncated normal distribution
#' @param horizons A vector of the forecast horizons that need to be calibrated
#' @param daysToTrain number of days used to train
#' @param startDate Optional parameter to determine when the training will start (if not from the begining of the dataset)
#' @return A list of a list of dataframes containing the EMOS parameters for each weather variable and each forecast horizon
#' @export

bma_weather_variables <- function(original_ensembles, weather_params, norm_l, gamma_l, horizons, daysToTrain = 30, startDate = NULL) {
  require(dplyr)
  require(scoringRules)
  require(ensembleBMA)
  
  param.list <- list()
  
  for (w in weather_params) {
    
    param.list[[w]] <- list()
    
    #Select model type
    if(w %in% norm_l) {
      model_type <- "normal"
    } else if(w %in% gamma_l) {
      model_type <- "gamma"
    }
    
    #Select only one weather variable
    ens <- original_ensembles[[w]]
    
    #save ensembles
    ens.t <- ens
    
    for (h in horizons) {
      hr <- paste("h",h, sep = "")
      
      full_df <- ens.t %>% filter(horizon == h)
      
      if(!is.null(startDate)) {
        df_to_use <- full_df %>% filter(time >= startDate)
      } else {
        df_to_use <- full_df
      }
      
      
      #Create ensemble member names
      ensMemberNames <- 0
      for (i in 1:51) {
        n <- paste("ens", i, sep = "")
        ensMemberNames[i] <- n
      }
      
      only.ensembles <- df_to_use[,ensMemberNames]
      ensMeans <- rowMeans(only.ensembles)
      ensVars <- apply(only.ensembles, MARGIN = 1, FUN = var)
      only.obs <- df_to_use$obs
      times <- df_to_use$time
      train_times <- format(times, "%Y%m%d%H")
      
      train_df <- data.frame(train_times,only.ensembles,only.obs)
      colnames(train_df) <- c("time", ensMemberNames, "obs")
      
      
      exchange = c(ens1=1,ens2=1,ens3=1,ens4=1,ens5=1,ens6=1,
                   ems7=1,ens8=1,ens9=1,ens10=1,ens11=1,ens12=1,
                   ens13=1,ens14=1,ens15=1,ens16=1,ens17=1,ens18=1,
                   ems19=1,ens20=1,ens21=1,ens22=1,ens23=1,ens24=1,
                   ens25=1,ens26=1,ens27=1,ens28=1,ens29=1,ens30=1,
                   ems31=1,ens32=1,ens33=1,ens34=1,ens35=1,ens36=1,
                   ens37=1,ens38=1,ens39=1,ens40=1,ens41=1,ens42=1,
                   ems43=1,ens44=1,ens45=1,ens46=1,ens47=1,ens48=1,
                   ens49=1,ens50=1,ens51=1)
      
      #Data frame for training
      train_ens_data <- ensembleData(forecasts = train_df[,ensMemberNames],
                                    dates = train_df[,"time"],
                                    observations = train_df[,"obs"],
                                    forecastHour = h,
                                    exchangeable = exchange,
                                    initializationTime = "00")
      
      
      #Training index
      p_start <- daysToTrain + 1
      p_end <- length(only.obs)
      
      #Data frame to save results
      result_times <- times[p_start:(p_end)]
      result_times_lookup <- train_times[p_start:(p_end)]
      
      
      if(model_type == "normal") {
        
        
        result_df <- as.data.frame(matrix(0, ncol = 4, nrow = length(result_times)))
        r_names <- c("m1","m2","sd")
        c_names <- c("time",r_names)
        colnames(result_df) <- c_names
        result_df$time <- result_times

        trained_bma <- ensembleBMAnormal(ensembleData = train_ens_data,
                                         trainingDays = daysToTrain,
                                         control = controlBMAnormal(maxIter = 100000, tol = 1.e-4))
        
        
        result_df[,"m1"] <- as.numeric(trained_bma$biasCoefs[1,1,result_times_lookup])
        result_df[,"m2"] <- as.numeric(trained_bma$biasCoefs[2,1,result_times_lookup])
        result_df[,"sd"] <- as.numeric(trained_bma$sd[result_times_lookup])
        
      }
      else if(model_type == "gamma") {
        result_df <- as.data.frame(matrix(0, ncol = 5, nrow = length(result_times)))
        r_names <- c("a1","a2","b1","b2")
        c_names <- c("time",r_names)
        colnames(result_df) <- c_names
        result_df$time <- result_times
        
        
        trained_bma <- ensembleBMAgamma(ensembleData = train_ens_data,
                                        trainingDays = daysToTrain,
                                        control =  controlBMAgamma(maxIter = 100000, tol = 1.e-4, startupSpeed = 0))
        
        result_df[,"a1"] <- as.numeric(trained_bma$biasCoefs[1,result_times_lookup])
        result_df[,"a2"] <- as.numeric(trained_bma$biasCoefs[2,result_times_lookup])
        result_df[,"b1"] <- as.numeric(trained_bma$varCoefs[1,result_times_lookup])
        result_df[,"b2"] <- as.numeric(trained_bma$varCoefs[2,result_times_lookup])
      }
      
      
      param.list[[w]][[hr]] <- result_df
      
    }
  }
  return(param.list)
}


##Function that generates a calibrated ensemble of predictions based on the emos_prediction parameters
#' @param emos_params The EMOS parameters calculated before
#' @param weather_ensembles A list containing raw weather ensembles
#' @param weather_params A vector containing the weather params that need to be calibrated
#' @param norm_l A vector containing the parameters that need to be calibrated with the normal distribution
#' @param tnorm_l A vector containing the parameters that need to be calibrated with the truncated normal distribution
#' @param horizons A vector of the forecast horizons to be used
#' @param daysToTrain number of days used to train
#' @param startDate Optional parameter to determine when the training will start (if not from the begining of the dataset)
#' @return A list of dataframes containing the calibrated forecasts for each forecast horizon
#' @export

bma_weather_forecast <- function(bma_params, weather_ensembles, weather_params, norm_l, gamma_l, horizons, daysToTrain = 30, startDate = NULL) {
  require(crch)
  require(scoringRules)
  #Create ensemble member names
  ensMemberNames <- 0
  for (i in 1:51) {
    n <- paste("ens", i, sep = "")
    ensMemberNames[i] <- n
  }
  
  pred.list <- list()
  
  for (w in weather_params) {
    
    pred.list[[w]] <- list()
    ens <- weather_ensembles[[w]]
    ens.t <- ens
    
    #Select model type
    if(w %in% norm_l) {
      model_type <- "normal"
    } else if(w %in% gamma_l) {
      model_type <- "gamma"
    }
    
    
    for (h in horizons) {
      
      hr <- paste("h",h,sep="")
      
      #Ensemble prediction for means and variance
      pred_df <- ens.t %>% filter(horizon == h)
      bma_df <- bma_params[[w]][[hr]]
      
      if(!is.null(startDate)) {
        use_df <- pred_df %>% filter(time >= startDate)
        bma_df <- bma_df %>% filter(time >= startDate)
      } else {
        use_df <- pred_df[-(1:daysToTrain),]
        bma_df <- bma_df
      }
      
      if(length(use_df$time) != length(bma_df$time)) {
        stop("Lengths are inconsistent, check the startDate and try again")
      }
      
      #Save times
      only.times <- use_df$time
      
      only.ensembles <- use_df[,ensMemberNames]
      ensMeans <- rowMeans(only.ensembles)
      ensVars <- apply(only.ensembles, MARGIN = 1, FUN = var)
      only.obs <- use_df$obs
      
      
      if(model_type == "normal") {
        
        m1 <- bma_df[,"m1"]
        m2 <- bma_df[,"m2"]
        sd <- bma_df[,"sd"]
        
        mean_matrix <- cbind(1,ensMeans)
        
        mp_matrix <- rbind(m1,m2)
        
        mu <- NULL
        sigma <- NULL
        
        for (i in 1:length(m1)) {
          mu[i] <- mean_matrix[i,] %*% mp_matrix[,i]
          sigma[i] <- sd[i]
        }
        
        f_params <- data.frame(time = only.times, obs = only.obs, mu = mu, sigma = sigma)
        
        
      } else if(model_type == "gamma"){ 
        
        a1 <- bma_df[,"a1"]
        a2 <- bma_df[,"a2"]
        b1 <- bma_df[,"b1"]
        b2 <- bma_df[,"b2"]
        
        mean_matrix <- cbind(1,ensMeans)
        var_matrix <- cbind(1,ensMeans)
        
        mp_matrix <- rbind(a1,a2)
        vp_matrix <- rbind(b1,b2)
        
        
        shape <- NULL
        scale <- NULL
        
        for (i in 1:length(a1)) {
          mean <- mean_matrix[i,] %*% mp_matrix[,i]
          variance <- var_matrix[i,] %*% vp_matrix[,i]
          shape[i] <- mean^2 / variance
          scale[i] <- variance / mean
        }
        
        f_params <- data.frame(time = only.times, obs = only.obs, shape = shape, scale = scale)
      }
      
      res_df <- as.data.frame(matrix(0, ncol = 53, nrow = length(only.times)))
      res_names <- c("time", "obs", ensMemberNames)
      colnames(res_df) <- res_names
      res_df$time <- only.times
      res_df$obs <- only.obs
      
      if(w %in% norm_l) {
        for (i in 1:length(res_df$time)) {
          res_df[i,ensMemberNames] <- rnorm(51, mean = f_params$mu[i], sd = f_params$sigma[i])
        }
      } else if(w %in% gamma_l){ 
        for (i in 1:length(res_df$time)) {
          res_df[i,ensMemberNames] <- rgamma(51, scale = f_params$scale[i], shape = f_params$shape[i])
        }
      }
      pred.list[[w]][[hr]] <- res_df
    }
  }
  return(pred.list)
}

##Function that returns the distribution parameters for weather ensembles
#' @param bma_params The EMOS parameters calculated before
#' @param weather_ensembles A list containing raw weather ensembles
#' @param weather_params A vector containing the weather params that need to be calibrated
#' @param norm_l A vector containing the parameters that need to be calibrated with the normal distribution
#' @param tnorm_l A vector containing the parameters that need to be calibrated with the truncated normal distribution
#' @param horizons A vector of the forecast horizons to be used
#' @param daysToTrain number of days used to train
#' @param startDate Optional parameter to determine when the training will start (if not from the begining of the dataset)
#' @return A list of dataframes containing the distribution parameters for each forecast horizon
#' @export

bma_weather_dist_parameters <- function(bma_params, weather_ensembles, weather_params, norm_l, gamma_l, horizons, daysToTrain = 30, startDate = NULL) {
  require(crch)
  #Create ensemble member names
  ensMemberNames <- 0
  for (i in 1:51) {
    n <- paste("ens", i, sep = "")
    ensMemberNames[i] <- n
  }
  
  param.list <- list()
  
  for (w in weather_params) {
    
    param.list[[w]] <- list()
    ens <- weather_ensembles[[w]]
    ens.t <- ens
    
    #Select model type
    if(w %in% norm_l) {
      model_type <- "normal"
    } else if(w %in% gamma_l) {
      model_type <- "gamma"
    }
    
    
    for (h in horizons) {
      
      hr <- paste("h",h,sep="")
      
      #Ensemble prediction for means and variance
      pred_df <- ens.t %>% filter(horizon == h)
      bma_df <- bma_params[[w]][[hr]]
      
      if(!is.null(startDate)) {
        use_df <- pred_df %>% filter(time >= startDate)
        bma_df <- bma_df %>% filter(time >= startDate)
      } else {
        use_df <- pred_df[-(1:daysToTrain),]
        bma_df <- bma_df
      }
      
      if(length(use_df$time) != length(bma_df$time)) {
        stop("Lengths are inconsistent, check the startDate and try again")
      }
      
      #Save times
      only.times <- use_df$time
      
      only.ensembles <- use_df[,ensMemberNames]
      ensMeans <- rowMeans(only.ensembles)
      ensVars <- apply(only.ensembles, MARGIN = 1, FUN = var)
      only.obs <- use_df$obs
      
      
      if(model_type == "normal") {
        
        m1 <- bma_df[,"m1"]
        m2 <- bma_df[,"m2"]
        sd <- bma_df[,"sd"]
        
        mean_matrix <- cbind(1,ensMeans)
        
        mp_matrix <- rbind(m1,m2)
        
        mu <- NULL
        sigma <- NULL
        
        for (i in 1:length(m1)) {
          mu[i] <- mean_matrix[i,] %*% mp_matrix[,i]
          sigma[i] <- sd[i]
        }
        
        f_params <- data.frame(time = only.times, obs = only.obs, mu = mu, sigma = sigma)
        
        
      } else if(model_type == "gamma"){ 
        
        a1 <- bma_df[,"a1"]
        a2 <- bma_df[,"a2"]
        b1 <- bma_df[,"b1"]
        b2 <- bma_df[,"b2"]
        
        mean_matrix <- cbind(1,ensMeans)
        var_matrix <- cbind(1,ensMeans)
        
        mp_matrix <- rbind(a1,a2)
        vp_matrix <- rbind(b1,b2)
        

        shape <- NULL
        scale <- NULL
        
        for (i in 1:length(a1)) {
          mean <- mean_matrix[i,] %*% mp_matrix[,i]
          variance <- var_matrix[i,] %*% vp_matrix[,i]
          shape[i] <- mean^2 / variance
          scale[i] <- variance / mean
        }
        
        f_params <- data.frame(time = only.times, obs = only.obs, shape = shape, scale = scale)
        
        }
     
      
      param.list[[w]][[hr]] <- f_params
    }
  }
  return(param.list)
}