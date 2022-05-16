
# ----- Recipes ---------------------------------------------------------------
get_Recipes = function(df) {
  artifacts_list <- read_rds("artifacts/feature_engineering_artifacts_list.rds")
  rcp_spec = artifacts_list$recipes$rcp_spec
  rcp_spec_spline = artifacts_list$recipes$rcp_spec_spline
  rcp_spec_lag = artifacts_list$recipes$rcp_spec_lag
  rcp_spec_fourier = recipe(value ~ date, data = df) %>%
                      step_fourier(date, period = c(7, 14, 30, 90), K = 1)

  # * Recipes ---------------------------------------------------------------

  rcp_H20 <- recipe(value ~ ., data = df_hd_train) %>%
    step_timeseries_signature(date) %>%
    step_rm(matches("(id)|(type)|(period)"))
  #   step_normalize(matches("(value))) %>%
  
  
  my_list = list( "rcp_spec" = rcp_spec,
                  "rcp_spec_spline" = rcp_spec_spline,
                  "rcp_spec_lag" = rcp_spec_lag,
                  "rcp_spec_fourier" = rcp_spec_fourier,
                  'rcp_H20' = rcp_H20
  )
  
  return(my_list)

}