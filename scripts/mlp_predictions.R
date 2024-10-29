rm(list = ls())
setwd("G:/PROJET/Article/ET_ML")

# Chargement des données
ml_data <- read.csv(file = "data/stations_clim_data.csv")

# Division des données en ensembles d'entraînement et de test
set.seed(123)
data_split <- initial_split(ml_data, prop = 0.8,strata="stations")
train_data <- training(data_split)
test_data <- testing(data_split)

mdl_filname <- dir("outputs/MLP",pattern = ".rds")

mdls <- list()
j <- 0
stations <- unique(train_data$stations)
for(station in stations){
  j <- j+1
  mdls[[station]] <- readRDS(file.path("outputs/MLP",mdl_filname[j]))
  
}

mdl_predictions <- ml_data%>%
  dplyr::filter(stations=="bobo")%>%
  select(Date)
mdl_metrics <- rbind()
for (station in names(mdls)) {
  mdl <- mdls[[station]]
  test_data <- ml_data%>%
    dplyr::filter(stations==station)
  # Prédictions et évaluation sur le test set
  mdl_prediction <- predict(mdl, test_data) %>%
    bind_cols(test_data)
  
  mld_prediction2 <- mdl_prediction%>%
    dplyr::select(Date,.pred)
  colnames(mld_prediction2)[2] <- station
  
  
  mld_metric <- mdl_prediction %>%
    metrics(truth = et0, estimate = .pred)%>%
    mutate(stations=station)
  
  mdl_metrics <- rbind(mdl_metrics,mld_metric)
  
  mdl_predictions <- mdl_predictions%>%
    left_join(mld_prediction2,by="Date")
  
}

write.table(x =mdl_metrics ,file ="outputs/MLP/mlp_metrics.csv" ,
            append =FALSE ,quote = FALSE,sep = ",",row.names =FALSE)

