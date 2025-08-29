############# MEMBRE DU PROJET ####################
#   DOSSOU Borice Fiacre
#   ZINA Salhi
#   KEZA Fleurine

########### VISUALISATION DES DONNEES DE  ventes #########################

############## LIBRARY ####################
library(readxl) # importation des fichiers excel
library(dplyr)#traitement des données
library(ggplot2) # Visualisation statique
library(plotly) # pour la visualisation
library(stringr)# Chaines de caractères
library(lubridate) # Dates
library(forcats) # traite les facteur
library(tidyr)
library(shiny)
library(forecast)# modèle prévision  
library(rsconnect)
library(shinythemes)
library(TSstudio)# utile pour les données de serie chronologique
library(prophet)# modèle de prévision Prophet Facebook
library(shiny)
library(DT) # faire de intération avec la table
library(forecast)# modèle prévision 
library(reshape2)
############ IMPORTATION ###################

Datav<-read_excel("./data/Sell.xlsx")

Datav<-Datav |>
  rename(Mois=Famille,Etuve=ETUVE,Cd=CD,Brasse=BRASSE,Fromage='FROMAGE FRAIS'
         ,Yaourt='YAOURT A BOIRE',Compote=COMPOTE,Jus_Au_lait='JUS AU LAIT',lait=LAIT) |> 
  mutate(annee = year(date)) |> 
  arrange(date) |>
  mutate( Moisv=month(date)) |> 
  mutate(trimestre =
           ifelse(Moisv %in% c(01, 02, 03), "1er",
                  ifelse(Moisv %in% c(04, 05, 06), "2ème",
                         ifelse(Moisv %in% c(07, 08, 09), "3ème", "4ème"))))




