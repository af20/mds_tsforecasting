
# ------------------------------------------------------------------------
# S-NAIVE & WINDOWS -------------------------------------------------------

get_Models__Naive__Windows_ = function(do_CEP=TRUE) {
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


  # WINDOW - MEAN
  model_fit_mean = window_reg(
    window_size = 7
  ) %>%
    set_engine(
      "window_function",
      window_function = mean,
      na.rm = TRUE
    ) %>%
    fit(value ~ date, df_hd_train)

  # WINDOW - WEIGHTED MEAN
  model_fit_wmean = window_reg(
    window_size = 7
  ) %>%
    set_engine(
      "window_function",
      window_function = ~ sum(tail(.x, 3) * c(0.1, 0.3, 0.6))
    ) %>%
    fit(value ~ date, df_hd_train)

  # WINDOW - MEDIAN
  model_fit_median = window_reg(
    window_size = 7
  ) %>%
    set_engine(
      "window_function",
      window_function = median,
      na.rm = TRUE
    ) %>%
    fit(value ~ date, df_hd_train)



  M = list(
          "M_naive" = model_fit_naive,
          "M_snaive" = model_fit_snaive,
          "M_window_mean" = model_fit_mean,
          "M_window_weighted_mean" = model_fit_wmean,
          "M_window_median" = model_fit_median
        )

  if(do_CEP == TRUE) {
    My_calibrate_evaluate_plot(
      M$M_naive,
      M$M_snaive,
      M$M_window_mean,
      M$M_window_weighted_mean,
      M$M_window_median
    )
  }

  return(M)
}





get_Models__ARIMA_Seas_Regr = function(do_CEP=TRUE) {

  # ..............................................................
  # ..............................................................
  # Seasonal Regression with ARIMA Errors and External Regressors
  # SARIMA
  model_fit_arima = arima_reg(
    non_seasonal_ar = 1,
    non_seasonal_differences = 1,
    non_seasonal_ma = 1,
    seasonal_period = 7,
    seasonal_ar = 1,
    seasonal_differences = 1,
    seasonal_ma = 1
  ) %>%
    set_engine("arima") %>%
    fit(value ~ date, df_hd_train)

  # Auto-SARIMA
  model_fit_auto_sarima = arima_reg() %>%
    set_engine("auto_arima") %>%
    fit(value ~ date, df_hd_train)

  # Auto-SARIMA with XREG
  model_spec_auto_sarima_xregs = arima_reg() %>%
    set_engine("auto_arima")


  # * Workflows -------------------------------------------------------------

  # Auto-SARIMA with XREG
  wrkfl_fit_auto_sarima_xregs = workflow() %>%
    add_recipe(Recipes$rcp_spec_fourier) %>%
    add_model(model_spec_auto_sarima_xregs) %>%
    fit(df_hd_train)

  M = list(
    "arima" = model_fit_arima,
    "auto_sarima" = model_fit_auto_sarima,
    "auto_sarima_xregs" = wrkfl_fit_auto_sarima_xregs
  )

  if(do_CEP == TRUE) {
    # * Calibration, Evaluation & Plotting ------------------------------------
    My_calibrate_evaluate_plot(
      M$arima,
      M$auto_sarima,
      M$auto_sarima_xregs
    )
  }

  return(M)

}




get_Models__EXP_smoothing__TBATS = function(do_CEP=TRUE) {

  # EXPONENTIAL SMOOTHING (ETS) ---------------------------------------------
  # Error, Trend & Seasonality (Holt-Winters Seasonal)
  # - Automatic forecasting method based on Exponential Smoothing
  # - Single Seasonality
  # - Cannot use XREGs (purely univariate)

  # ETS Additive
  model_fit_ets = exp_smoothing(
    error = "additive",
    trend = "additive",
    season = "additive"
  ) %>%
    set_engine("ets") %>%
    fit(value ~ date, data = df_hd_train)

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

  # TBATS with 3 Seasonalities
  model_fit_tbats = seasonal_reg(
    seasonal_period_1 = 7,
    seasonal_period_2 = 30,
    seasonal_period_3 = 365
  ) %>%
    set_engine("tbats") %>%
    fit(value ~ date, df_hd_train)

  # Auto-TBATS
  model_fit_auto_tbats = seasonal_reg() %>%
    set_engine("tbats") %>%
    fit(value ~ date, df_hd_train)


  M = list(
    "ets" = model_fit_ets,
    "ets_auto" = model_fit_auto_ets,
    "theta" = model_fit_theta,
    "tbats" = model_fit_tbats,
    "tbats_auto" = model_fit_auto_tbats
  )

  if(do_CEP == TRUE) {
    # * Calibration, Evaluation & Plotting ------------------------------------
    My_calibrate_evaluate_plot(
      M$ets,
      M$ets_auto,
      M$theta,
      M$tbats,
      M$tbats_auto
    )
  }

  return(M)
}





get_Models__SLTM = function(do_CEP=TRUE) {

  # STLM --------------------------------------------------------------------
  # Seasonal & Trend Decomposition using LOESS Models


  # - Uses seasonal decomposition to model trend & seasonality separately
  #   - Trend modeled with ARIMA or ETS
  #   - Seasonality modeled with Seasonal Naive (SNAIVE)
  #   - Can handle multiple seasonality
  #   - ARIMA version accepts XREGS, ETS does not


  # STLM with ETS
  model_fit_stlm_ets = seasonal_reg(
    seasonal_period_1 = 7,
    seasonal_period_2 = 30,
    seasonal_period_3 = 364 / 2
  ) %>%
    set_engine("stlm_ets") %>%
    fit(value ~ date, data = df_hd_train)

  # STLM with ARIMA
  model_fit_stlm_arima = seasonal_reg(
    seasonal_period_1 = 7,
    seasonal_period_2 = 30,
    seasonal_period_3 = 364 / 2
  ) %>%
    set_engine("stlm_arima") %>%
    fit(value ~ date, data = df_hd_train)

  # STLM with ARIMA + XREGS
  model_fit_stlm_arima_xregs = seasonal_reg(
    seasonal_period_1 = 7,
    seasonal_period_2 = 30,
    seasonal_period_3 = 364 / 2
  ) %>%
    set_engine("stlm_arima") %>%
    fit(value ~ date, data = df_hd_train)

  # Auto-STLM with ARIMA (simply STL with ARIMA on the ts frequency seasonlity)
  model_fit_auto_stlm_arima = seasonal_reg() %>%
    set_engine("stlm_arima") %>%
    fit(value ~ date, data = df_hd_train)

  # Auto-STLM with ARIMA + XREGS
  model_fit_auto_stlm_arima_xregs = seasonal_reg() %>%
    set_engine("stlm_arima") %>%
    fit(value ~ date, data = df_hd_train)


  M = list(
    "ets" = model_fit_stlm_ets,
    "arima" = model_fit_stlm_arima,
    "arima_xregs" = model_fit_stlm_arima_xregs,
    "arima_auto" = model_fit_auto_stlm_arima,
    "arima_xregs_auto" = model_fit_auto_stlm_arima_xregs
  )

  if(do_CEP == TRUE) {
    # * Calibration, Evaluation & Plotting ------------------------------------
    My_calibrate_evaluate_plot(
      M$ets,
      M$arima,
      M$arima_xregs,
      M$arima_auto,
      M$arima_xregs_auto
    )
  }

  return(M)
}






get_Models__Prophet = function(do_CEP=TRUE) {

  # Facebook's Prophet Algorithm
  #   Can handle daily, weekly and yearly seasonality
  #   Automatic
  #   Can use smoothing trend
  #   Accepts external regressors (can be used to include other seasonalities)


  # * Engines ---------------------------------------------------------------

  # PROPHET
  model_fit_prophet = prophet_reg(
    changepoint_num = 10,
    changepoint_range = 0.9,
    seasonality_weekly = TRUE,
    seasonality_yearly = TRUE
  ) %>%
    set_engine("prophet") %>%
    fit(value ~ date, data = df_hd_train)


  # Auto-PROPHET
  model_fit_auto_prophet = prophet_reg() %>%
    set_engine("prophet") %>%
    fit(value ~ date, data = df_hd_train)

  # PROPHET with XREGs
  model_fit_prophet_xregs = prophet_reg(
    seasonality_weekly = TRUE,
    seasonality_yearly = TRUE
  ) %>%
    set_engine("prophet") %>%
    fit(value ~ date, data = df_hd_train)


  M = list(
    "prophet" = model_fit_prophet,
    "prophet_auto" = model_fit_auto_prophet,
    "prophet_xregs" = model_fit_prophet_xregs
  )

  if(do_CEP == TRUE) {
    # * Calibration, Evaluation & Plotting ------------------------------------
    My_calibrate_evaluate_plot(
      M$prophet,
      M$prophet_auto,
      M$prophet_xregs
    )
  }

  return(M)
}

