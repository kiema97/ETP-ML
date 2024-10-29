rm(list = ls())
setwd("G:/PROJET/Article/ET_ML")

# Chargement des données
ml_data <- read.csv(file = "data/stations_clim_data.csv")

# Division des données en ensembles d'entraînement et de test
set.seed(123)
data_split <- initial_split(ml_data, prop = 0.8,strata="stations")
train_data <- training(data_split)
test_data <- testing(data_split)

mdl_filname <- dir("outputs/GBoost",pattern = ".rds")

mdls <- list()
j <- 0
stations <- unique(train_data$stations)
for(station in stations){
  j <- j+1
  mdls[[station]] <- readRDS(file.path("outputs/GBoost",mdl_filname[j]))
  
}

gboost_predictions <- ml_data%>%
  dplyr::filter(stations=="bobo")%>%
  select(Date)
gboost_metrics <- rbind()
for (station in names(mdls)) {
  mdl <- mdls[[station]]
  test_data <- ml_data%>%
    dplyr::filter(stations==station)
  # Prédictions et évaluation sur le test set
  gboost_prediction <- predict(mdl, test_data) %>%
    bind_cols(test_data)
  
  gboost_prediction2 <- gboost_prediction%>%
    dplyr::select(Date,.pred)
  colnames(gboost_prediction2)[2] <- station
  
  
  gboost_metric <- gboost_prediction %>%
    metrics(truth = et0, estimate = .pred)%>%
    mutate(stations=station)
  
  gboost_metrics <- rbind(gboost_metrics,gboost_metric)
  
  gboost_predictions <- gboost_predictions%>%
    left_join(gboost_prediction2,by="Date")
  
}

write.table(x =gboost_predictions ,file ="outputs/GBoost/gboost_predictions.csv" ,
            append =FALSE ,quote = FALSE,sep = ",",row.names =FALSE)

