
# Function to calibrate models, evaluate their accuracy and plot results
My_calibrate_evaluate_plot <- function(..., updated_desc = NULL) {

  calibration_tbl = modeltime_table(...)

  calibration_tbl <- calibration_tbl %>%
    modeltime_calibrate(df_hd_test, quiet=FALSE)

  print(calibration_tbl %>% modeltime_accuracy())

  print(
    calibration_tbl %>%
      modeltime_forecast(actual_data = df_hd, new_data = df_hd_test) %>%
      plot_modeltime_forecast(.conf_interval_show = FALSE)
  )

  return(invisible(calibration_tbl))

}
