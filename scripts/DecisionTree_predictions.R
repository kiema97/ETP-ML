rm(list = ls())
setwd("G:/PROJET/Article/ET_ML")

# Chargement des données
ml_data <- read.csv(file = "data/stations_clim_data.csv")

# Division des données en ensembles d'entraînement et de test
set.seed(123)
data_split <- initial_split(ml_data, prop = 0.8,strata="stations")
train_data <- training(data_split)
test_data <- testing(data_split)

mdl_filname <- dir("outputs/DecisionTree",pattern = ".rds")

mdls <- list()
j <- 0
stations <- unique(train_data$stations)
for(station in stations){
  j <- j+1
  mdls[[station]] <- readRDS(file.path("outputs/DecisionTree",mdl_filname[j]))
  
}


dt_predictions <- ml_data%>%
  dplyr::filter(stations=="bobo")%>%
  select(Date)

dt_metrics <- rbind()
for (station in names(mdls)) {
  mdl <- mdls[[station]]
  test_data <- ml_data%>%
    dplyr::filter(stations==station)
  # Prédictions et évaluation sur le test set
  dt_prediction <- predict(mdl, test_data) %>%
    bind_cols(test_data)
  
  dt_prediction2 <- dt_prediction%>%
    dplyr::select(Date,.pred)
  colnames(dt_prediction2)[2] <- station
  
  
  dt_metric <- dt_prediction %>%
    metrics(truth = et0, estimate = .pred)%>%
    mutate(stations=station)
  
  dt_metrics <- rbind(dt_metrics,dt_metric)
  
  dt_predictions <- dt_predictions%>%
    left_join(dt_prediction2,by="Date")
  
}

write.table(x =dt_metrics ,file ="outputs/DecisionTree/decisiontree_metrics.csv" ,
            append =FALSE ,quote = FALSE,sep = ",",row.names =FALSE)

