rm(list = ls())
setwd("G:/PROJET/Article/ET_ML")

# Chargement des données
ml_data <- read.csv(file = "data/stations_clim_data.csv")

# Division des données en ensembles d'entraînement et de test
set.seed(123)
data_split <- initial_split(ml_data, prop = 0.8,strata="stations")
train_data <- training(data_split)
test_data <- testing(data_split)

mdl_filname <- dir("outputs/KNN",pattern = ".rds")

mdls <- list()
j <- 0
stations <- unique(train_data$stations)
for(station in stations){
  j <- j+1
  mdls[[station]] <- readRDS(file.path("outputs/KNN",mdl_filname[j]))
  
}

knn_predictions <-  ml_data%>%
  dplyr::filter(stations=="bobo")%>%
  select(Date)
knn_metrics <- rbind()
for (station in names(mdls)) {
  mdl <- mdls[[station]]
  test_data <- ml_data%>%
    dplyr::filter(stations==station)
  # Prédictions et évaluation sur le test set
  knn_prediction <- predict(mdl, test_data) %>%
    bind_cols(test_data)
  
  knn_prediction2 <- knn_prediction%>%
    dplyr::select(Date,.pred)
  colnames(knn_prediction2)[2] <- station
  
  
  knn_metric <- knn_prediction %>%
    metrics(truth = et0, estimate = .pred)%>%
    mutate(stations=station)
  
  knn_metrics <- rbind(knn_metrics,knn_metric)
  
  knn_predictions <- knn_predictions%>%
    left_join(knn_prediction2,by="Date")
  
}

write.table(x =knn_predictions ,file ="outputs/KNN/knn_predictions.csv" ,
            append =FALSE ,quote = FALSE,sep = ",",row.names =FALSE)

