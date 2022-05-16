# Packages ----------------------------------------------------------------
source("R/packages.R")

source("R/utils.R")
source("AF_models.R")
source("AF_Recipes.R")
source("AF_utils.R")


# ----- Data
DATA = read_rds("data/hackathon_dataset.rds") # $info  e  $data
artifacts_list <- read_rds("artifacts/feature_engineering_artifacts_list.rds")

# INPUT --
IN_SAMPLE_PERCENTAGE = 70
my_ID = 30

# Find my DF
v_id = unique(DATA$info$id)
id_plot = v_id[my_ID] #  SS
df_hd = DATA$data[ which(DATA$data$id==id_plot),]
df_hd = df_hd[with(df_hd, order(date)),]


#... PLOT id series ....
#df_hd %>%
#  plot_time_series(date, value, .smooth = FALSE)

# SPLIT in TRAIN e TEST
idx_IN_SAMPLE = as.integer(nrow(df_hd) * IN_SAMPLE_PERCENTAGE/100)
df_hd_train = df_hd[c(1: idx_IN_SAMPLE),]
df_hd_test = df_hd[c(idx_IN_SAMPLE+1:nrow(df_hd)),]



# ........... MODELS ............

Recipes = get_Recipes(df_hd_train)

M_NW = get_Models__Naive__Windows_()
M_ASR = get_Models__ARIMA_Seas_Regr()
M_EXP = get_Models__EXP_smoothing__TBATS()
M_SLTM = get_Models__SLTM()
M_PROPHET = get_Models__Prophet()


last_error()



#
# # PROPHET
# model_fit_prophet = prophet_reg(
#   changepoint_num = 10,
#   changepoint_range = 0.9,
#   seasonality_weekly = TRUE,
#   seasonality_yearly = TRUE
# ) %>%
#   set_engine("prophet") %>%
#   fit(value ~ date, data = df_hd_train)
#
#
# # Auto-PROPHET
# model_fit_auto_prophet = prophet_reg() %>%
#   set_engine("prophet") %>%
#   fit(value ~ date, data = df_hd_train)
#
# # PROPHET with XREGs
# model_fit_prophet_xregs = prophet_reg(
#   seasonality_weekly = TRUE,
#   seasonality_yearly = TRUE
# ) %>%
#   set_engine("prophet") %>%
#   fit(value ~ date, data = df_hd_train)
#
# calibration_tbl <- modeltime_table(model_fit_prophet, model_fit_auto_prophet, model_fit_prophet_xregs)
#
#
# calibration_tbl <- calibration_tbl %>%
#   modeltime_calibrate(df_hd_test, quiet=FALSE)
# last_error()
#
# print(calibration_tbl %>% modeltime_accuracy())
#
# print(
#   calibration_tbl %>%
#     modeltime_forecast(actual_data = df_hd, new_data = df_hd_test) %>%
#     plot_modeltime_forecast(.conf_interval_show = FALSE)
# )








# model_fit_naive = naive_reg() %>%
#   set_engine("naive") %>%
#   fit(value ~ date, df_hd_train)
#
# model_fit_auto_sarima = arima_reg() %>%
#   set_engine("auto_arima") %>%
#   fit(value ~ date, df_hd_train)
#
# calibration_tbl <- modeltime_table(model_fit_naive, model_fit_auto_sarima)
# calibration_tbl <- calibration_tbl %>%
#   modeltime_calibrate(df_hd_test)
#
# print(calibration_tbl %>% modeltime_accuracy())
#
#
#
# print(
#   calibration_tbl %>%
#     modeltime_forecast(actual_data = df_hd, new_data = df_hd_test) %>%
#     plot_modeltime_forecast(.conf_interval_show = FALSE)
# )


