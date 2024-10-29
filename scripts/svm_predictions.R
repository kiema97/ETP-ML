setwd("G:/PROJET/Article/ET_ML")

# Chargement des données
ml_data <- read.csv(file = "data/stations_clim_data.csv")

# Division des données en ensembles d'entraînement et de test
set.seed(123)
data_split <- initial_split(ml_data, prop = 0.8,strata="stations")
train_data <- training(data_split)
test_data <- testing(data_split)


svm_rbf_model_bobo <- readRDS("outputs/SVM/svm_rbf_model_bobo.rds")
svm_rbf_model_boromo <- readRDS("outputs/SVM/svm_rbf_model_boromo.rds")
svm_rbf_model_dedougou <- readRDS("outputs/SVM/svm_rbf_model_dedougou.rds")
svm_rbf_model_dori <- readRDS("outputs/SVM/svm_rbf_model_dori.rds")
svm_rbf_model_fada <- readRDS("outputs/SVM/svm_rbf_model_fada.rds")
svm_rbf_model_gaoua <- readRDS("outputs/SVM/svm_rbf_model_gaoua.rds")
svm_rbf_model_ouaga <- readRDS("outputs/SVM/svm_rbf_model_ouaga.rds")
svm_rbf_model_ouahigouya <- readRDS("outputs/SVM/svm_rbf_model_ouahigouya.rds")
svm_rbf_model_po <- readRDS("outputs/SVM/svm_rbf_model_po.rds")

svm_models <- list(bobo=svm_rbf_model_bobo,boromo=svm_rbf_model_boromo,dedougou=svm_rbf_model_dedougou,
                   dori=svm_rbf_model_dori,fada=svm_rbf_model_fada,gaoua=svm_rbf_model_gaoua,
                   ouaga=svm_rbf_model_ouaga,ouahigouya=svm_rbf_model_ouahigouya,po=svm_rbf_model_po)

svm_rbf_predictions <- ml_data%>%
  select(Date)
svm_rbf_metrics <- rbind()
for (station in names(svm_models)) {
  mdl <- svm_models[[i]]
  test_data <- ml_data%>%
    dplyr::filter(stations==station)
  # Prédictions et évaluation sur le test set
  svm_rbf_prediction <- predict(mdl, test_data) %>%
    bind_cols(test_data)
  
  svm_rbf_prediction2 <- svm_rbf_prediction%>%
    dplyr::select(Date,.pred)
  colnames(svm_rbf_prediction2)[2] <- station
  
  
  svm_rbf_metric <- svm_rbf_prediction %>%
    metrics(truth = et0, estimate = .pred)%>%
    mutate(stations=station)
  
  svm_rbf_metrics <- rbind(svm_rbf_metrics,svm_rbf_metric)
  
  svm_rbf_predictions <- svm_rbf_predictions%>%
    left_join(svm_rbf_prediction2,by="Date")
  
  
  
  
}

write.table(x =svm_rbf_metrics ,file ="outputs/SVM/svm_rbf_metrics.csv" ,
            append =FALSE ,quote = FALSE,sep = ",",row.names =FALSE)

