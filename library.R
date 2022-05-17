
# Function to calibrate models, evaluate their accuracy and plot results
My_calibrate_evaluate_plot <- function(..., updated_desc = NULL) {

  calibration_tbl = modeltime_table(...)

  calibration_tbl <- calibration_tbl %>%
     modeltime_calibrate(df_hd_test, quiet=FALSE)
  MAC = calibration_tbl %>% modeltime_accuracy()
  
  print(MAC)

  print(
    calibration_tbl %>%
      modeltime_forecast(actual_data = df_hd, new_data = df_hd_test) %>%
      plot_modeltime_forecast(.conf_interval_show = FALSE)
  )

  return(invisible(calibration_tbl))

}




My_calibrate_evaluate_append_CSV <- function(..., model_group=NULL, t_start_model_group=NULL) {

  calibration_tbl = modeltime_table(...)
  # model_group = 'H2O' # calibration_tbl = M_H20$CT

  calibration_tbl <- calibration_tbl %>%
       modeltime_calibrate(df_hd_test, quiet=FALSE)
  MAC = calibration_tbl %>% modeltime_accuracy()
  
  
  for (i in seq_len(nrow(MAC))) { # 1 riga = 1 modello
    x = MAC[i,]
    model_name = x$.model_desc
    smape = x$smape
    mase = x$mase
    rmsse = x$rmse / sd(df_hd_test$value)
    
    smape = round(smape, digits = N_Digits_Results)
    mase = round(mase, digits = N_Digits_Results)
    rmsse = round(rmsse, digits = N_Digits_Results)
    # https://www.marinedatascience.co/blog/2019/01/07/normalizing-the-rmse/
    
    df1 = data.frame(
      id_serie = c(id_serie, id_serie, id_serie), 
      model_name = c(model_name, model_name, model_name),
      model_group = c(model_group,model_group,model_group),
      metric = c('smape', 'mase', 'rmsse'),
      value = c(smape, mase, rmsse)
    )
    DF = rbind(DF, df1)
  }


  if(file.exists(Results_Csv_path) == TRUE) {
    csv_columns_results = F}  else csv_columns_results = T
  
  write.table( DF,  
               file=Results_Csv_path, 
               append = T, 
               sep=';', 
               row.names=F,
               col.names=csv_columns_results 
  )

  
  # Appendo il DF dei tempi nel CSV (a livello di Model Group)
  t_delta_s = as.numeric(round(difftime(Sys.time(), t_start_model_group, units = 'secs'), 2))
  
  DF_Time = data.frame(id_serie=c(id_serie), 'model_group'=c(model_group), 'seconds'=c(t_delta_s))
  
  if(file.exists(Results_Times_Csv_path) == TRUE) {
    csv_columns_times = F}  else csv_columns_times = T
  
  write.table( DF_Time,  
               file=Results_Times_Csv_path, 
               append = T, 
               sep=';', 
               row.names=F,
               col.names=csv_columns_times 
              )

}
