library(dplyr)
library(tidyr)
library(caret)
library(randomForest)
library(doMC)
library(twitteR)


MODEL_DIR <- "models"
MODEL_LC_DIR <- file.path(MODEL_DIR,"learning_curve") 
LOG_DIR <- "log"
PROF_DIR <- "profiling"
PARAM_DIR <- "param"

TW_USER <- "Tenolios"

init_train <- function() {
  if(!dir.exists(PROF_DIR)) {
    dir.create(PROF_DIR)
  }
  
  if(!dir.exists(MODEL_DIR)) {
    dir.create(MODEL_DIR)
  }
  
  if(!dir.exists(MODEL_LC_DIR)) {
    dir.create(MODEL_LC_DIR)
  }
  
  if(!dir.exists(LOG_DIR)) {
    dir.create(LOG_DIR)
  }
  
  if(!dir.exists(PARAM_DIR)) {
    dir.create(PARAM_DIR)
  }
  
  nb_cores <- detectCores(all.tests = FALSE, logical = TRUE)
  registerDoMC(cores = nb_cores)
}


# Twitter API auth
tw_auth <- function() {
  #write.csv2(twitter_api_param, file.path(PARAM_DIR, "twitter_api_param.csv"), row.names = FALSE)
  
  tw_auth_param <- read.csv2(file.path(PARAM_DIR, "twitter_api_param.csv"), stringsAsFactors = FALSE)
  setup_twitter_oauth(tw_auth_param$api_key,tw_auth_param$api_secret
                      , tw_auth_param$access_token, tw_auth_param$access_token_secret )
}

tw_send <- function(txt) {
  # Auth everytime just in case
  tw_auth()
  dmSend(txt, TW_USER)
}

tw_send_cv <- function(fit, model_name = "") {
  msg <- paste("Model :", model_name," - Min local CV :", min(fit$results$logLoss))
  tw_send(msg)
}

# Used to stop the instance after training a model
shutdown_computer <- function() {
  # user needs to be in the sudoers file like this
  # username ALL = NOPASSWD: /sbin/shutdown
  # use "sudo visudo" to edit file
  tw_send("Shutting down instance")
  system('sudo shutdown -h now', wait = FALSE)
}




# TODO : useless function, replace with "select(df, one_of(features))"
get_feature_subset <- function(data) {
  out_data <- data %>%
    select(interest_level
           , bathrooms
           , bedrooms
           , latitude
           , longitude
           #, price
           , log_price
           , log_price_per_bedroom_p1
           , log_price_per_bathroom_p1
           #, log_price_per_room_p2
           , nb_features
           #, logp1_nb_features
           #, created_mday    # may be useful but expensive to train
           , created_wday
           #, created_hour
           , nb_photos
           #, logp1_nb_photos
           , description_nchar
    )
  
  out_data
}


train_rf_0 <- function(train_data, k, cv_repeat, model_name, save_model_rds = TRUE, profile = FALSE, send_notif = FALSE) {
  start_ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  log_file = file.path(LOG_DIR, paste0("train_model_", model_name, "_"
                                       , start_ts
                                       , ".log"))
  
  # Normalization parameters
  #pre_process_ctrl <- preProcess(train_data, method = c("center", "scale"))
  
  fit_control <- trainControl(method = "repeatedcv", number = k, repeats = cv_repeat
                              , classProbs = TRUE
                              , summaryFunction = mnLogLoss)
  
  
  # best practices RF paremeters (from "applied predictive modeling")
  # mtry around sqrt(# of predictors) for classification, around 1/3 of # of predictors for regression
  # => Start with 5 values evenly spaced between 2 and number of predictors
  # Start with 1k trees and increase if plateau not reached
  
  if(profile) {
    prof_fname <- file.path(prof_dir, paste0(model_name, ".prof"))
    Rprof(filename = prof_fname, append = FALSE, memory.profiling = FALSE, gc.profiling = FALSE)
  }
  
  log_msg <- paste("Training start for model", model_name, ":", Sys.time(),"\n")
  cat(log_msg, file = log_file, append = TRUE)
  
  set.seed(1234)
  rf_fit_0 <- train(interest_level ~ .
                    , data = train_data
                    , method = "parRF"
                    # , preProcess = pre_process_ctrl
                    , trControl = fit_control
                    #, mtry = TO TUNE
                    , ntree = 1000
                    , importance = TRUE
                    #, proximity = TRUE
                    , verbose = TRUE)
  if(profile) {
    Rprof(NULL)
  }
  log_msg <- paste0(print(rf_fit_0), "\n")
  cat(log_msg, file = log_file, append = TRUE)
  
  log_msg <- paste("Training end for model", model_name, ":", Sys.time(),"\n")
  cat(log_msg, file = log_file, append = TRUE)
  
  # Save to file
  if(save_model_rds) {
    saveRDS(rf_fit_0, file.path(MODEL_DIR, paste0(model_name, "_", start_ts, ".rds")))
  }
  
  # Send local CV score via Twitter (mail problem on AWS instance)
  if(send_notif) {
    tw_send_cv(rf_fit_0, model_name)
  }
  
  rf_fit_0
}


# From Kaggle
MultiLogLoss <- function(act, pred){
  eps <- 1e-15
  pred <- pmin(pmax(pred, eps), 1 - eps)
  sum(act * log(pred) + (1 - act) * log(1 - pred)) * -1/NROW(act)
}
