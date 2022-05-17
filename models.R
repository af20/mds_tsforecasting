
# ------------------------------------------------------------------------
# S-NAIVE & WINDOWS -------------------------------------------------------

get_Models__Naive_ = function(do_CEP=TRUE) {
  t_start = Sys.time()
  
  # do_CEP = do_calibrate_evaluate_plot
  # df = training(splits)

  # NAIVE
  model_fit_naive = naive_reg() %>%
    set_engine("naive") %>%
    fit(value ~ date, df_hd_train)

  # SNAIVE
  model_fit_snaive = naive_reg() %>%
    set_engine("snaive") %>%
    fit(value ~ date, df_hd_train)



  M = list(
          "model_group"= "Naive",
          "M_naive" = model_fit_naive,
          "M_snaive" = model_fit_snaive
          )

  if(do_CEP == TRUE) {
    My_calibrate_evaluate_append_CSV(
      M$M_naive,
      M$M_snaive,
      model_group=M$model_group,
      t_start_model_group=t_start
    )
  }

  return(M)
}





get_Models__ARIMA_Seas_Regr = function(do_CEP=TRUE) {
  t_start = Sys.time()
  
  # ..............................................................
  # ..............................................................
  # Seasonal Regression with ARIMA Errors and External Regressors

  # Auto-SARIMA
  model_fit_auto_sarima = arima_reg() %>%
    set_engine("auto_arima") %>%
    fit(value ~ date, df_hd_train)

  M = list(
    "model_group"= "Arima_Seas",
    "auto_sarima" = model_fit_auto_sarima
  )

  if(do_CEP == TRUE) {
    # * Calibration, Evaluation & Plotting ------------------------------------
    My_calibrate_evaluate_append_CSV(
      M$auto_sarima,
      model_group=M$model_group,
      t_start_model_group=t_start
    )
  }
  M$t_delta_s = as.numeric(round(difftime(Sys.time(), t_start, units = 'secs'), 2))
  
  return(M)

}




get_Models__EXP_smoothing__TBATS = function(do_CEP=TRUE) {
  t_start = Sys.time()
  
  # EXPONENTIAL SMOOTHING (ETS) ---------------------------------------------
  # Error, Trend & Seasonality (Holt-Winters Seasonal)
  # - Automatic forecasting method based on Exponential Smoothing
  # - Single Seasonality
  # - Cannot use XREGs (purely univariate)

  # Auto-ETS
  model_fit_auto_ets = exp_smoothing() %>%
    set_engine("ets") %>%
    fit(value ~ date, data = df_hd_train)

  # ThetaF
  model_fit_theta = exp_smoothing() %>%
    set_engine("theta") %>%
    fit(value ~ date, data = df_hd_train)



  # TBATS -------------------------------------------------------------------
  # Exponential Smoothing with Box-Cox transformation, ARMA errors, Trend and Seasonality
  #     - Multiple Seasonality Model
  #     - Extension of ETS for complex seasonality
  #     - Automatic
  #     - Does not support XREGS
  #     - Computationally low (often)


  # Auto-TBATS
  model_fit_auto_tbats = seasonal_reg() %>%
    set_engine("tbats") %>%
    fit(value ~ date, df_hd_train)


  M = list(
    "model_group"= "EXP_smoothing__TBATS",
    "ets_auto" = model_fit_auto_ets,
    "theta" = model_fit_theta,
    "tbats_auto" = model_fit_auto_tbats
  )

  if(do_CEP == TRUE) {
    # * Calibration, Evaluation & Plotting ------------------------------------
    My_calibrate_evaluate_append_CSV(
      M$ets_auto,
      M$theta,
      M$tbats_auto,
      model_group=M$model_group,
      t_start_model_group=t_start
    )
  }
  M$t_delta_s = as.numeric(round(difftime(Sys.time(), t_start, units = 'secs'), 2))
  
  return(M)
}





get_Models__SLTM = function(do_CEP=TRUE) {
  t_start = Sys.time()
  
  # STLM --------------------------------------------------------------------
  # Seasonal & Trend Decomposition using LOESS Models


  # - Uses seasonal decomposition to model trend & seasonality separately
  #   - Trend modeled with ARIMA or ETS
  #   - Seasonality modeled with Seasonal Naive (SNAIVE)
  #   - Can handle multiple seasonality
  #   - ARIMA version accepts XREGS, ETS does not


  # STLM with ETS
  model_fit_stlm_ets = seasonal_reg() %>%
    set_engine("stlm_ets") %>%
    fit(value ~ date, data = df_hd_train)



  # Auto-STLM with ARIMA (simply STL with ARIMA on the ts frequency seasonlity)
  model_fit_auto_stlm_arima = seasonal_reg() %>%
    set_engine("stlm_arima") %>%
    fit(value ~ date, data = df_hd_train)


  M = list(
    "model_group"= "SLTM",
    "stlm_ets" = model_fit_stlm_ets,
    "stlm_arima_auto" = model_fit_auto_stlm_arima
  )

  if(do_CEP == TRUE) {
    # * Calibration, Evaluation & Plotting ------------------------------------
    My_calibrate_evaluate_append_CSV(
      M$stlm_ets,
      M$stlm_arima_auto,
      model_group=M$model_group,
      t_start_model_group=t_start
    )
  }
  M$t_delta_s = as.numeric(round(difftime(Sys.time(), t_start, units = 'secs'), 2))
  
  return(M)
}






get_Models__Prophet = function(do_CEP=TRUE) {
  t_start = Sys.time()
  
  # Facebook's Prophet Algorithm
  #   Can handle daily, weekly and yearly seasonality
  #   Automatic
  #   Can use smoothing trend
  #   Accepts external regressors (can be used to include other seasonalities)


  # * Engines ---------------------------------------------------------------

  # Auto-PROPHET
  model_fit_auto_prophet = prophet_reg() %>%
    set_engine("prophet") %>%
    fit(value ~ date, data = df_hd_train)

  M = list(
    "model_group"= "Prophet",
    "prophet_auto" = model_fit_auto_prophet
  )

  if(do_CEP == TRUE) {
    # * Calibration, Evaluation & Plotting ------------------------------------
    My_calibrate_evaluate_append_CSV(
      M$prophet_auto,
      model_group=M$model_group,
      t_start_model_group=t_start
    )
  }
  M$t_delta_s = as.numeric(round(difftime(Sys.time(), t_start, units = 'secs'), 2))
  
  return(M)
}






get_Models_H20 = function() {
  t_start = Sys.time()
  
  h2o.init()
  #Recipes$rcp_H20 %>% prep() %>% juice() %>% glimpse()

  model_spec_h2o <- automl_reg(mode = 'regression') %>%
    set_engine(
      engine = "h2o",
      max_runtime_secs = 30,
      max_runtime_secs_per_model = 30,
      project_name = "project_tsf_course",
      max_models = 10,
      nfolds = 10,
      sort_metric = "rmse",
      verbosity = NULL,
      seed = 123
    )
      #exclude_algos = c("DeepLearning"), # remove deeplearning for computation time

  #?automl_reg
  # stacking ---- Model Stacking is a way to improve model predictions by combining the outputs of multiple models and running them through another machine learning model called a meta-learner.

  wrkfl_fit_h2o <- workflow() %>%
    add_model(model_spec_h2o) %>%
    add_recipe(Recipes$rcp_H20) %>%
    fit(df_hd_train)

  #wrkfl_fit_h2o %>% automl_leaderboard() %>% head(20)
  M = list("CT" = CT, "model_group"= "H2O")
  CT = My_calibrate_evaluate_append_CSV(wrkfl_fit_h2o, model_group=M$model_group, t_start_model_group=t_start)
  
  M$t_delta_s = as.numeric(round(difftime(Sys.time(), t_start, units = 'secs'), 2))
  
  return(M)


  # gbm_name_1 <- "GBM_10_AutoML_2_20220516_182251"# "GBM_2_AutoML_1_20220219_112345"
  # ensemble_name <- "StackedEnsemble_AllModels_5_AutoML_1_20220516_181949" # "XGBoost_3_AutoML_1_20220219_112345"
  # gbm_name_2 <- "GBM_5_AutoML_1_20220516_181949" # "StackedEnsemble_AllModels_5_AutoML_1_20220219_112345"
  #
  # # change default selected models
  # wrkfl_fit_h20_gbm <- wrkfl_fit_h2o %>%
  #   automl_update_model(gbm_name_1)
  # wrkfl_fit_h20_xgb <- wrkfl_fit_h2o %>%
  #   automl_update_model(ensemble_name)
  # wrkfl_fit_h20_stack <- wrkfl_fit_h2o %>%
  #   automl_update_model(gbm_name_2)
  #
  #
  #
  # # * Calibration, Evaluation & Plotting ------------------------------------
  #
  #
  # AF_calibrate_evaluate_plot(wrkfl_fit_h2o)
  #
  # AF_calibrate_evaluate_plot(
  #   wrkfl_fit_h20_gbm,
  #   wrkfl_fit_h20_xgb,
  #   wrkfl_fit_h20_stack
  # )
  # return wrkfl_fit_h2o

}






get_H20_ensemble <- function() {
  t_start = Sys.time()
  
  # 1. Generate a 2-model ensemble (GBM + RF)

  # Train & Cross-validate a GBM
  my_gbm <- h2o.gbm(x = date,
                    y = value,
                    training_frame = df_hd_train,
                    distribution = "bernoulli",
                    ntrees = 10,
                    max_depth = 3,
                    min_rows = 2,
                    learn_rate = 0.2,
                    nfolds = nfolds,
                    keep_cross_validation_predictions = TRUE,
                    seed = 1)
  perf_gbm_test <- h2o.performance(my_gbm, newdata = test)

  # Train & Cross-validate a RF
  my_rf <- h2o.randomForest(x = x,
                            y = y,
                            training_frame = train,
                            ntrees = 50,
                            nfolds = nfolds,
                            keep_cross_validation_predictions = TRUE,
                            seed = 1)

  # Train a stacked ensemble using the GBM and RF above
  ensemble <- h2o.stackedEnsemble(x = x,
                                  y = y,
                                  training_frame = train,
                                  base_models = list(my_gbm, my_rf))

  # Eval ensemble performance on a test set
  perf <- h2o.performance(ensemble, newdata = df_hd_test)
}
