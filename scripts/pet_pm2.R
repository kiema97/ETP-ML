# Étapes de définition du modèle
## Charger les bibliothèques nécessaires
rm(list = ls())
library(tidymodels)
library(parsnip)

## 2. Définir et enregistrer le modèle, les modes et les arguments
# Définir un nouveau modèle appelé 'pet_pm'
set_new_model("pet_pm")

# Définir le mode du modèle comme 'regression'
set_model_mode(model = "pet_pm", mode = "regression")

# Définir le moteur du modèle comme 'PET.PM2'
set_model_engine(
  "pet_pm", 
  mode = "regression", 
  eng = "PET.PM2"
)

# Définir les dépendances du modèle
set_dependency("pet_pm", eng = "PET.PM2")

# Montrer les informations du modèle
show_model_info("pet_pm")

## 3. Configurer les arguments du modèle

# Configurer les arguments du modèle
set_model_arg(
  model = "pet_pm",
  eng = "PET.PM2",
  parsnip = "cd",
  original = "cd",
  func = list(fun = "PET.PM2"),
  has_submodel = FALSE
)

set_model_arg(
  model = "pet_pm",
  eng = "PET.PM2",
  parsnip = "lat",
  original = "lat",
  func = list(fun = "PET.PM2"),
  has_submodel = FALSE
)

set_model_arg(
  model = "pet_pm",
  eng = "PET.PM2",
  parsnip = "z",
  original = "z",
  func = list(fun = "PET.PM2"),
  has_submodel = FALSE
)

set_model_arg(
  model = "pet_pm",
  eng = "PET.PM2",
  parsnip = "albedo",
  original = "albedo",
  func = list(fun = "PET.PM2"),
  has_submodel = FALSE
)

set_model_arg(
  model = "pet_pm",
  eng = "PET.PM2",
  parsnip = "rs_rso_min",
  original = "rs_rso_min",
  func = list(fun = "PET.PM2"),
  has_submodel = FALSE
)

set_model_arg(
  model = "pet_pm",
  eng = "PET.PM2",
  parsnip = "rs_rso_max",
  original = "rs_rso_max",
  func = list(fun = "PET.PM2"),
  has_submodel = FALSE
)

set_model_arg(
  model = "pet_pm",
  eng = "PET.PM2",
  parsnip = "checkTdiff",
  original = "checkTdiff",
  func = list(fun = "PET.PM2"),
  has_submodel = FALSE
)

show_model_info("pet_pm")


## 4. Créer la fonction du modèle

pet_pm <- function(mode = "regression", cd = NULL, lat = NULL, z = NULL, albedo = 0.23, rs_rso_min = 0.33, rs_rso_max = 1, checkTdiff = FALSE) {
  # Vérifier le mode
  if (mode  != "regression") {
    rlang::abort("`mode` should be 'regression'")
  }
  
  # Capturer les arguments en quosures
  args <- list(cd = rlang::enquo(cd), lat = rlang::enquo(lat), z = rlang::enquo(z), albedo = rlang::enquo(albedo), rs_rso_min = rlang::enquo(rs_rso_min), rs_rso_max = rlang::enquo(rs_rso_max), checkTdiff = rlang::enquo(checkTdiff))
  
  # Sauvegarder des slots vides pour les parties futures de la spécification
  new_model_spec(
    "pet_pm",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = NULL
  )
}


## 5. Ajouter un module de fitting

set_fit(
  model = "pet_pm",
  eng = "PET.PM2",
  mode = "regression",
  value = list(
    interface = "formula",
    protect = c("formula", "data"),
    func = c(fun = "PET.PM2"),
    defaults = list()
  )
)
show_model_info("pet_pm")

## 6. Configurer l'encodage des prédicteurs
set_encoding(
  model = "pet_pm",
  eng = "PET.PM2",
  mode = "regression",
  options = list(
    predictor_indicators = "none",
    compute_intercept = TRUE,
    remove_intercept = FALSE,
    allow_sparse_x = FALSE
  )
)

## 7. Ajouter des modules de prédiction
predict_pm <- function(model,data){
  model <- model
  lat <- unique(data$lat)
  z <- unique(data$z)
  prediction <- PET.PM(data,lat,z)
  prediction2 <- data.frame(.pred=prediction$PET)
  return(prediction2)
}
numeric_info <- list(
  pre = NULL,
  post = NULL,
  func = c(fun = "predict_pm"),
  args = list(
    object = quote(object$fit),
    newdata = quote(new_data),
    type = "numeric"
  )
)

set_pred(
  model = "pet_pm",
  eng = "PET.PM2",
  mode = "regression",
  type = "numeric",
  value = numeric_info
)

show_model_info("pet_pm")


##============================================================================##
setwd("G:/PROJET/Article/ET_ML")
# Charger les données
clim_data <- read.csv(file = "data/stations_clim_data.csv") %>%
  mutate(Date=lubridate::as_date(Date))

# Définir la latitude et l'altitude
lat <- 35.6895 # exemple de latitude (Tokyo)
z <- 44 # exemple d'altitude (Tokyo)
clim_data$lat <- lat
clim_data$z <- z
# Définir un modèle 'pet_pm' en utilisant 'parsnip'
pet_model <- pet_pm(lat = lat, z = z) %>%
  set_engine("PET.PM2")

# Ajuster le modèle
fit_pet <- pet_model %>%
  fit(et0 ~ tx + tn + rh + rs + ws, data = clim_data)

# Faire des prédictions avec le modèle ajusté
predictions <- predict_pm(model=fit_pet,data=clim_data)

# Afficher les prédictions
print(predictions)


##=============================================================================#

Sys.setenv(TZ="UTC")
#setwd("D:/Recherche/Article_ET0_ML/")


library(kernlab)
library(DALEX)
library(baguette)
library(shapper)
library(readxl)
library(lubridate)
library(shapviz)
# library(reticulate)
# use_python("C:/Users/ousmane.yonaba/AppData/Local/Programs/Python/Python311/")
# shapper::install_shap()

# Example dataset
#data(iris)
#data(ames, package = "modeldata")

predict_pm2 <- function(model,data,lat,z){
  model <- model
  data$Date <-seq.Date(from =as.Date("2000-01-01") ,
                              to = as.Date("2000-12-31"),by = "d") 

  prediction <- PET.PM(data,lat=lat,z=z)
  prediction2 <- data.frame(.pred=prediction$PET)
  return(prediction2)
}
lnames <- c(  "PM" = "Penman Monteith")

bf.stations <- read.csv(file = paste0("data/bf_stations2.csv"),header = T)
station_names <- bf.stations[order(bf.stations$Latitude, decreasing=T),]$shName

read_station <- function(fpath, station) {
  #fpath <- "Data/cli_data_bf.xlsx"
  #station <- "bobo"
  vars <- excel_sheets(fpath)
  df <- data.frame(Date = seq(ymd("1988-1-1"), ymd("2017-12-31"), by = "day"))
  df$stations <- station
  for (var in vars) {
    #var <- "tx"
    data <- read_xlsx(fpath, sheet = var)[,station]
    df <- cbind(df, data)
  }
  colnames(df)[3:8] <- vars
  return (df)
}

read_data <- function(fpath, stations) {
  vars <- excel_sheets(fpath)
  df <- data.frame(matrix(nrow=0, ncol=length(vars)+2))
  colnames(df) <- c("Date","stations",vars)
  for (station in stations) {
    #print(paste0("rr ",station))
    df <- rbind(df, read_station(fpath, station))
  }
  return (df)
}

data <- read_data("data/cli_data_bf2.xlsx", station_names)
data$j <- yday(data$Date)
data <- data[,-1]
data <- aggregate(.~j+stations, data, mean)

vars <- tail(colnames(data),6)
ml.models <- names(lnames)
set.seed(123)

cp <- 0
sv.plist <- list()

for (model in ml.models) {
  #model <- "SVM"
  print(model)
  rds <- dir(pattern = "*rand_forest_model")
  
  cp <- cp + 1
  pshapl <- list()
  
  for (station in unique(data$stations)) {
    lat <- bf.stations[bf.stations$shName==station,"Latitude"]
    z <- bf.stations[bf.stations$shName==station,"Elevation"]
    #station <- "ouaga"
    print(paste0("Model: ",model, " / Shapviz: ",station))
    model.rds <-fit_pet# rds[grep(station,rds)]
    #model.rds <- readRDS(model.rds)
    sdata <- data[data$stations == station, vars]
    #sdata$Date <- Date <-seq.Date(from =as.Date("2000-01-01") ,
                                  #to = as.Date("2000-12-31"),by = "d") 
    #sdata$lat <- lat
    #sdata$z <- z
    pshap <- permshap(model.rds, X = sdata[,c(1:5)], 
                      bg_X = sdata[,c(1:5)], 
                      pred_fun = predict_pm2, lat=lat,z=z,verbose = T)
    
    pshapl[[station]] <- shapviz(pshap)
    
  }
  mshap <- mshapviz(pshapl)
  saveRDS(mshap, file = paste0("outputs","/",model,"_mshapviz2.rds"))
}


