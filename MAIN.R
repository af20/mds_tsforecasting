# Packages ----------------------------------------------------------------
source("R/utils.R")
source("R/packages.R")

source("models.R")
source("recipes.R")
source("library.R")


# ----- Data
DATA = read_rds("data/hackathon_dataset.rds") # $info  e  $data
artifacts_list <- read_rds("artifacts/feature_engineering_artifacts_list.rds")

# INPUT --
my_ID = 30

# Find my DF
v_id = unique(DATA$info$id)
id_serie = id_plot = v_id[my_ID] #  SS
df_hd = DATA$data[ which(DATA$data$id==id_plot),]
df_hd = df_hd[with(df_hd, order(date)),]


#... PLOT id series ....
#df_hd %>%
#  plot_time_series(date, value, .smooth = FALSE)

# SPLIT in TRAIN e TEST
df_hd_train = df_hd[which(df_hd$type=='train'),]
df_hd_test = df_hd[which(df_hd$type=='test'),]
period = df_hd_train$period[1]
v_cols = c('date', 'value')
df_hd_train = df_hd_train[v_cols]
df_hd_test = df_hd_test[v_cols]



# ........... MODELS ............
source("models.R")

Recipes = get_Recipes(df_hd_train)

M_NW = get_Models__Naive__Windows_()
M_ASR = get_Models__ARIMA_Seas_Regr()
M_EXP = get_Models__EXP_smoothing__TBATS()
M_SLTM = get_Models__SLTM()
M_PROPHET = get_Models__Prophet()
M_H20 = get_Models_H20()

v_Results = c(M_ASR, M_EXP, M_SLTM, M_PROPHET, M_H20)

# ALTRI mode di automl_reg ?
# Cosa aggiungere a automl_reg? => Ensamble
# Formula RMSSE


# print(M_NW$MAC)
# x = M_NW$MAC
# length(x$mape)
last_error()

