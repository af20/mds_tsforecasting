# Packages ----------------------------------------------------------------
source("R/utils.R")
source("R/packages.R")

source("models.R")
source("recipes.R")
source("library.R")


# ----- Data
DATA = read_rds("data/hackathon_dataset.rds") # $info  e  $data
artifacts_list <- read_rds("artifacts/feature_engineering_artifacts_list.rds")

# Initial Variables --

v_id = unique(DATA$info$id)
Results_Csv_path = 'data/Results.csv'
Results_Times_Csv_path =  'data/Results_Times.csv'
N_Digits_Results = 6
csv_columns_results = c('id_serie', 'model_group', 'model_name', 'metric', 'value')
csv_columns_times = c('id_serie', 'model_group', 'seconds')
v_cols_df = c('date', 'value')

# Leggo nel CSV quali modelli ho già calcolato
v_id_inserted = c() # length(v_id_inserted)
if(file.exists(Results_Csv_path) == TRUE) {
  Results_CSV_df = read.csv(file = Results_Csv_path, sep=';')
  v_id_inserted = unique(Results_CSV_df$id_serie)
  Results_CSV_df$id_serie
  cat('v_id_inserted:', v_id_inserted)
}


counter = 0
T_START = Sys.time()

for (id_serie in v_id) {
  counter = counter +1
  DF = data.frame(id_serie=NA, model_name=NA, metric=NA, value=NA)[0,]
  
  if (id_serie %in% v_id_inserted) 
    next
  t_start = Sys.time()
  t_delta_START = round(difftime(Sys.time(), T_START, units = 'mins'), 2)
  
  cat(counter, '/', length(v_id), '     id_serie:', id_serie, '     t_delta_START (mins):', t_delta_START)

  df_hd = DATA$data[ which(DATA$data$id==id_serie),]
  df_hd = df_hd[with(df_hd, order(date)),]
  period = df_hd$period[1]

  # SPLIT in TRAIN e TEST
  df_hd_train = df_hd[which(df_hd$type=='train'),][v_cols_df]
  df_hd_test = df_hd[which(df_hd$type=='test'),][v_cols_df]
  
  # ........... MODELS ............
  Recipes = get_Recipes(df_hd_train)
  
  M_NW = get_Models__Naive_()
  M_ASR = get_Models__ARIMA_Seas_Regr()
  M_EXP = get_Models__EXP_smoothing__TBATS()
  M_SLTM = get_Models__SLTM()
  M_PROPHET = get_Models__Prophet()
  #M_H20 = get_Models_H20()
}


# In particular, you have to report:
#   - list of forecasting methods used
# - accuracy results on test set for each time series and each method using RMSSE, MASE and sMAPE
# - best accuracy results on test set for each time series using RMSSE, MASE and sMAPE
# - average accuracy result on test set (Average RMSSE, MASE and sMAPE)
# - total computation time required to make the computations with system information
# - total time spent on developing the project
