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
fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    # Commande CSS  pour gérer les paramères de positionnement du logo
    tags$style(HTML('
      .us, .ub {
        width: 130px; /* Modifier la taille du logo */
        height: auto; /* Pour maintenir les proportions */
        position: absolute; /* Position absolue pour le placer */
        top: 10px; /* Position du haut en pixels */
      }
      .us {
        left: 30x; /* Position du logo de gauche */
      }
      .ub {
        right: 30px; /* Position du logo de droite */
      }
    ')),
  ),
  # Commande CSS  pour gérer les paramères  de la bande d'annonce
  tags$style(HTML('
    .bande-annonce {
      position: fixed;
      bottom: 0;
      width: 60%;
      background-color: green;
      color: white;
      padding: 2px;
      animation: defile 15s linear infinite;
    }
    
    @keyframes defile {
      from { transform: translateX(100%); }
      to { transform: translateX(-100%); }
    }
  ')),
  # sortir selon les paramètres pour les logos
  tags$div(
    tags$img(src = "logsoum.jpg", class = "us"),
    tags$img(src = "ubss.png", class = "ub")) ,
  # sortir selon les paramètres pour de la bande d'annonce           
  div(class = "bande-annonce", "Cette application est créé 
      par Borice DOSSOU étudiant en M1 DSMS à l'UBS Vannes (2023-2024)."),
  
  # Titre de l'application
  tags$head(
    tags$style(HTML("h1,h3{text-align: center;}"))
  ),
  h1("MONLÈ",style="font-family: 'Jura'; color: green; fond-size: 80px;"),
  h3("Application R shiny permettant de visualiser et de faire la prévision des ventes
  sur les douze (12) prochains mois."),
  
  # Choix de visualization
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "type_vis", 
        "Choix Visualisation",
        choices=c("Données","Prévision")
      ),
      #Choix des produits
      selectInput(
        "prod", 
        "Produit : ",
        choices = c( 'Etuve','Cd','Brasse','Fromage','Yaourt','Compote',
                     'Jus_Au_lait','lait',"tous les produits"),
        selected = "Drame" # Liste des genres
      ),
      #Condition d'affichage des paramètres du choix Prévision
      conditionalPanel(
        condition="input.type_vis=='Prévision'",
        checkboxInput("decompose", label="Decompose ETS", value = TRUE),
        selectInput(
          "forcecastmodel",
          label='Forecasting Model',
          choices= c("ETS"="auto", "Holt-Winters"="hw",
                     "TBATS"="tbats", "Auto ARIMA"= "autoarima",
                     "Facebook Prophet"="fbpro")
          
        ),
        # Le Button reactif 
        actionButton("reactif", "Prévision des Ventes")
      ),
      #Condition d'affichage des paramètres du choix Données
      conditionalPanel(
        condition="input.type_vis=='Données'",
        #Choix de l'année 
        selectInput(
          "an", 
          "Année : ",
          choices = c(unique(Datav$annee),"Au cours des 5 ans"),
          selected = "Drame" # Liste des genres
        ),
        # Choix du trimestre
        selectInput(
          "Tri", 
          "Trimestre : ",
          choices = c(unique(Datav$trimestre),"Au cours des 4 trimestres"),
          selected = "Drame" # Liste des genres
        ),
        #Choix du Button reactif 
        actionButton("declencheur", "Valider")
        
      ),
      
      width = 2
    ),
    
    
    # Affichage du graphique choisi
    mainPanel(
      conditionalPanel(
        condition="input.type_vis=='Données'",
        tabsetPanel(
          tabPanel("Graphique", plotlyOutput("Plot"),
                   p("Bienvenue dans l'application de visualisation et de prévision des ventes de l'entripse MONLÈ.
                 Pour utiliser cette application,
           manipuler les widgets sur le côté pour effectuer les choix selon vos préférences ! Pour télécharger une image de haute qualité du
           tracé  ou des données prédire que vous avez créé, vous pouvez également le télécharger avec le bouton 'Save to csv'.
           Pour voir les données de la prévison des six (12) prochain mois , utilisez l'onglet Table Prévision selon les différents modèle.")),
          #Affichage Histograme
          tabPanel("Histogramme", plotlyOutput("hist")),
          # Affichage des données visualise
          tabPanel("Ventes", DTOutput("vent")),
          #Onglet de visualisation des statistique des données
          tabPanel("Statistiques", verbatimTextOutput("stat"))
        )),
      #Onglet la visualisation de la saisonnalié
      conditionalPanel(
        condition="input.type_vis=='Prévision'",
        tabsetPanel(
          tabPanel("Saisonnalité",plotlyOutput("actual_ts")),
          #onglet d'affichage du graphique de la prévision
          tabPanel("Graphe Prévision", plotlyOutput("autoplotforecast")),
          #Onglet d'affichage de la  table des données prédictes
          tabPanel("Table Prévision",DTOutput ("forecastdata")),
          p("Voici les colonnes qui correspondent aux valeurs des prévisions:"),
          tags$li(tags$b("yhat"), " - C'est la colonne des valeurs prédictes par
          le modèle Prophet Facebook"),
          tags$li(tags$b("Point Forecast"), "  - C'est la colonne des valeurs prédictes par
          les autres modèles"),
        ))
    )
  )
)