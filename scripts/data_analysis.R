# Netoyage
rm(list = ls())

# Charger les bibliothèques nécessaires
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

# Work space
setwd("G:/PROJET/Article/ET_ML")

# Définir le chemin du fichier
file_path <- "data/cli_data_bf.xlsx"

# Charger les données de chaque feuille
tx_data <- read_excel(file_path, sheet = "tx")%>%
  gather(key = "stations",value ="tx" ,-Date)

tn_data <- read_excel(file_path, sheet = "tn")%>%
  gather(key = "stations",value ="tn" ,-Date)

rh_data <- read_excel(file_path, sheet = "rh")%>%
  gather(key = "stations",value ="rh" ,-Date)

rs_data <- read_excel(file_path, sheet = "rs")%>%
  gather(key = "stations",value ="rs" ,-Date)

ws_data <- read_excel(file_path, sheet = "ws")%>%
  gather(key = "stations",value ="ws" ,-Date)

et0_data <- read_excel(file_path, sheet = "et0")%>%
  gather(key = "stations",value ="et0" ,-Date)


# Fusionner toutes les données en une seule base

merged_data <- tx_data %>%
  inner_join(tn_data, by = c("Date","stations")) %>%
  inner_join(rh_data, by = c("Date","stations")) %>%
  inner_join(rs_data , by = c("Date","stations")) %>%
  inner_join(ws_data , by = c("Date","stations")) %>%
  inner_join(et0_data , by = c("Date","stations"))
write.table(x = merged_data,file = "data/stations_clim_data.csv",
            append = FALSE,quote = FALSE,sep = ",",row.names = FALSE)
merged_data_aaaa <- merged_data%>%
  mutate(YYYY=year(Date))%>%
  group_by(YYYY,stations)%>%
  summarize(tx=round(mean(tx,na.rm = TRUE),2),
            tn=round(mean(tn,na.rm = TRUE),2),
            rh=round(mean(rs,na.rm = TRUE),2),
            rs=round(mean(rh,na.rm = TRUE),2),
            ws=round(mean(ws,na.rm = TRUE),2),
            et0=round(sum(et0,na.rm = TRUE),2))


# Statistiques descriptives
summary_stats <- merged_data_aaaa %>% 
  select(-YYYY) %>% 
  summarise(across(everything(), list(mean = ~mean(.), sd = ~sd(.), min = ~min(.), max = ~max(.), median = ~median(.))))

print(summary_stats)

# Visualisation
data_aaaa_long <- merged_data_aaaa%>%
  gather(key = "variables",value = "values",-YYYY,-stations)

# Température maximale
p1 <- ggplot(tx_data, aes(x = Date)) +
  geom_line(aes(y = bobo, color = "bobo")) +
  geom_line(aes(y = boromo, color = "boromo")) +
  labs(title = "Température maximale", y = "Température (°C)")

# Température minimale
p2 <- ggplot(tn_data, aes(x = Date)) +
  geom_line(aes(y = bobo, color = "bobo")) +
  geom_line(aes(y = boromo, color = "boromo")) +
  labs(title = "Température minimale", y = "Température (°C)")

# Humidité relative
p3 <- ggplot(rh_data, aes(x = Date)) +
  geom_line(aes(y = bobo, color = "bobo")) +
  geom_line(aes(y = boromo, color = "boromo")) +
  labs(title = "Humidité relative", y = "Humidité (%)")

# Rayonnement solaire
p4 <- ggplot(rs_data, aes(x = Date)) +
  geom_line(aes(y = bobo, color = "bobo")) +
  geom_line(aes(y = boromo, color = "boromo")) +
  labs(title = "Rayonnement solaire", y = "Rayonnement (MJ/m²)")

# Vitesse du vent
p5 <- ggplot(ws_data, aes(x = Date)) +
  geom_line(aes(y = bobo, color = "bobo")) +
  geom_line(aes(y = boromo, color = "boromo")) +
  labs(title = "Vitesse du vent", y = "Vitesse (m/s)")

# Évapotranspiration
p6 <- ggplot(et0_data, aes(x = Date)) +
  geom_line(aes(y = bobo, color = "bobo")) +
  geom_line(aes(y = boromo, color = "boromo")) +
  labs(title = "Évapotranspiration", y = "ET0 (mm)")

# Afficher les graphiques
grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)
