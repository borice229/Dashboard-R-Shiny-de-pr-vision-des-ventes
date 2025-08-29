############# MEMBRE DU PROJET ####################
#   DOSSOU Borice Fiacre
#   ZINA Salhi
#   KEZA Fleurine

############# LIBRARIES ###################
rsconnect::setAccountInfo(name='df-borice'
                          , token='1E68ACDE755FD657F02FBFE306186743',
                          secret='jZimdXl9Z/Uqzn1VcvT5I6DVqyfxIIrKu6bSOQIL')

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
library(rsconnect)
library(reshape2)

# Define server logic required to draw a graphique
function(input, output, session) {
  
  # Fonction réactive générant la base des données
  donne<- eventReactive(input$declencheur, { 
    if (input$prod == 'tous les produits' & input$an== 'Au cours des 5 ans' &
        input$Tri=='Au cours des 4 trimestres'){
      donne<-  Datav |>
        rename(Sell=ventes)
    }
    else if (input$an=='Au cours des 5 ans' & input$Tri=='Au cours des 4 trimestres'
    ) {
      donne <-  Datav |> 
        select(input$prod, date, annee, trimestre) |> 
        rename(Sell=input$prod)
    }
    
    else if (input$an=='Au cours des 5 ans' & input$prod == 'tous les produits'){
      donne<- Datav |> 
        filter(trimestre==input$Tri) |> 
        rename(Sell=ventes)
    }
    else if (input$Tri=='Au cours des 4 trimestres' & input$prod == 'tous les produits'){
      donne<- Datav |> 
        filter(annee==input$an) |> 
        rename(Sell=ventes)
    }
    
    else if (input$an=='Au cours des 5 ans'){
      donne<- Datav |> 
        select(input$prod,date,Mois,annee,trimestre) |> 
        filter(trimestre==input$Tri) |> 
        rename(Sell=input$prod)
    }
    
    else if (input$Tri=='Au cours des 4 trimestres'){
      donne<- Datav |> 
        select(input$prod,date,Mois,annee,trimestre) |> 
        filter(annee==input$an) |> 
        rename(Sell=input$prod)
    }
    
    else if (input$prod == 'tous les produits'){
      donne<- Datav |>
        select(ventes,date,Mois,annee,trimestre) |> 
        filter(annee==input$an) |> 
        filter(trimestre==input$Tri) |> 
        rename(Sell=ventes)
    }
    
    else {
      donne<- Datav |>
        select(input$prod,date,Mois,annee,trimestre) |> 
        filter(annee==input$an) |> 
        filter(trimestre==input$Tri) |> 
        rename(Sell=input$prod)
    }
  })
  
  # Fonction réactive générant la base des données permettant la prévision
  donnepre<- eventReactive(input$reactif , { 
    if (input$prod == 'tous les produits'){
      donnepre<-  Datav |>
        rename(Produit=ventes) |> 
        select(date,Produit)
    }
    else {
      donnepre<- Datav |>
        select(date,input$prod) |> 
        rename(Produit=input$prod)
    }
  })
  
  
  # Fonction réactive générant la base des données des histogramme
  hisbas<- eventReactive(input$declencheur, { 
    if (input$prod == 'tous les produits' & input$an== 'Au cours des 5 ans' &
        input$Tri=='Au cours des 4 trimestres'){
      hisbas<-Datav |>
        group_by(annee,trimestre) |> 
        summarise(e_Etuve=sum(Etuve),e_Cd=sum(Cd),e_Brasse=sum(Brasse),e_Fromage=sum(Fromage),
                  e_Yaourt=sum(Yaourt), e_Compote=sum(Compote),e_Jus_Au_lait=sum (Jus_Au_lait),
                  e_lait=sum(lait),.groups = 'keep') |> 
        mutate(tota=rowSums(across(starts_with("e"))),
               Etuve = e_Etuve / tota * 100,
               Cd = e_Cd / tota* 100,
               Brasse = e_Brasse / tota* 100,
               Fromage = e_Fromage / tota* 100,
               Yaourt = e_Yaourt / tota* 100,
               Compote = e_Compote / tota * 100,
               Jus_Au_lait = e_Jus_Au_lait / tota* 100,
               lait = e_lait / tota* 100) |>   
        mutate(across(where(is.numeric), ~round(., 2))) |> 
        group_by(annee) |> 
        select(annee,e_Etuve,e_Cd,e_Brasse,e_Fromage,e_Yaourt,e_Compote,e_Jus_Au_lait,e_lait) |> 
        summarise(e_Etuve=sum(e_Etuve),e_Cd=sum(e_Cd),e_Brasse=sum(e_Brasse),e_Fromage=sum(e_Fromage),
                  e_Yaourt=sum(e_Yaourt), e_Compote=sum(e_Compote),e_Jus_Au_lait=sum (e_Jus_Au_lait),
                  e_lait=sum(e_lait)) |> 
        mutate(tota=rowSums(across(starts_with("e"))),
               Etuve = e_Etuve / tota * 100,
               Cd = e_Cd / tota* 100,
               Brasse = e_Brasse / tota* 100,
               Fromage = e_Fromage / tota* 100,
               Yaourt = e_Yaourt / tota* 100,
               Compote = e_Compote / tota * 100,
               Jus_Au_lait = e_Jus_Au_lait / tota* 100,
               lait = e_lait / tota* 100) |>
        select(annee,Etuve,Cd,Brasse,Fromage,Yaourt,Compote,Jus_Au_lait,lait) |> 
        melt(id.vars = "annee", variable.name = "Produit", value.name = "Pourcentage") 
      
    }
    else if (input$an=='Au cours des 5 ans' & input$Tri=='Au cours des 4 trimestres'
    ) {
      hisbas<-Datav |>
        group_by(annee,trimestre) |> 
        summarise(e_Etuve=sum(Etuve),e_Cd=sum(Cd),e_Brasse=sum(Brasse),e_Fromage=sum(Fromage),
                  e_Yaourt=sum(Yaourt), e_Compote=sum(Compote),e_Jus_Au_lait=sum (Jus_Au_lait),
                  e_lait=sum(lait),.groups = 'keep') |> 
        mutate(tota=rowSums(across(starts_with("e"))),
               Etuve = e_Etuve / tota * 100,
               Cd = e_Cd / tota* 100,
               Brasse = e_Brasse / tota* 100,
               Fromage = e_Fromage / tota* 100,
               Yaourt = e_Yaourt / tota* 100,
               Compote = e_Compote / tota * 100,
               Jus_Au_lait = e_Jus_Au_lait / tota* 100,
               lait = e_lait / tota* 100) |>   
        mutate(across(where(is.numeric), ~round(., 2))) |>
        select(annee, trimestre,input$prod) |>
        rename(Pourcentage=input$prod,Produit=trimestre)
    }
    else if (input$an=='Au cours des 5 ans' & input$prod == 'tous les produits'){
      hisbas<-Datav |>
        group_by(annee,trimestre) |> 
        summarise(e_Etuve=sum(Etuve),e_Cd=sum(Cd),e_Brasse=sum(Brasse),e_Fromage=sum(Fromage),
                  e_Yaourt=sum(Yaourt), e_Compote=sum(Compote),e_Jus_Au_lait=sum (Jus_Au_lait),
                  e_lait=sum(lait),.groups = 'keep') |> 
        mutate(tota=rowSums(across(starts_with("e"))),
               Etuve = e_Etuve / tota * 100,
               Cd = e_Cd / tota* 100,
               Brasse = e_Brasse / tota* 100,
               Fromage = e_Fromage / tota* 100,
               Yaourt = e_Yaourt / tota* 100,
               Compote = e_Compote / tota * 100,
               Jus_Au_lait = e_Jus_Au_lait / tota* 100,
               lait = e_lait / tota* 100) |>   
        mutate(across(where(is.numeric), ~round(., 2))) |>
        select(annee,trimestre,Etuve,Cd,Brasse,Fromage,Yaourt,Compote,Jus_Au_lait,lait) |> 
        filter(trimestre==input$Tri ) |> 
        select(annee,Etuve,Cd,Brasse,Fromage,Yaourt,Compote,Jus_Au_lait,lait) |>
        melt(id.vars = "annee", variable.name = "Produit", value.name = "Pourcentage")
    }
    else if (input$Tri=='Au cours des 4 trimestres' & input$prod == 'tous les produits'){
      hisbas<-Datav |>
        group_by(annee,trimestre) |> 
        summarise(e_Etuve=sum(Etuve),e_Cd=sum(Cd),e_Brasse=sum(Brasse),e_Fromage=sum(Fromage),
                  e_Yaourt=sum(Yaourt), e_Compote=sum(Compote),e_Jus_Au_lait=sum (Jus_Au_lait),
                  e_lait=sum(lait),.groups = 'keep') |> 
        mutate(tota=rowSums(across(starts_with("e"))),
               Etuve = e_Etuve / tota * 100,
               Cd = e_Cd / tota* 100,
               Brasse = e_Brasse / tota* 100,
               Fromage = e_Fromage / tota* 100,
               Yaourt = e_Yaourt / tota* 100,
               Compote = e_Compote / tota * 100,
               Jus_Au_lait = e_Jus_Au_lait / tota* 100,
               lait = e_lait / tota* 100) |>   
        mutate(across(where(is.numeric), ~round(., 2))) |>
        filter(annee==input$an) |>
        data.frame() |> 
        select(trimestre,Etuve,Cd,Brasse,Fromage,Yaourt,Compote,Jus_Au_lait,lait) |>
        melt(id.vars = "trimestre", variable.name = "Produit", value.name = "Pourcentage") |> 
        rename(annee=trimestre)
    }
    
    else if (input$an=='Au cours des 5 ans'){
      hisbas<-Datav |>
        group_by(annee,trimestre) |> 
        summarise(e_Etuve=sum(Etuve),e_Cd=sum(Cd),e_Brasse=sum(Brasse),e_Fromage=sum(Fromage),
                  e_Yaourt=sum(Yaourt), e_Compote=sum(Compote),e_Jus_Au_lait=sum (Jus_Au_lait),
                  e_lait=sum(lait),.groups = 'keep') |> 
        mutate(tota=rowSums(across(starts_with("e"))),
               Etuve = e_Etuve / tota * 100,
               Cd = e_Cd / tota* 100,
               Brasse = e_Brasse / tota* 100,
               Fromage = e_Fromage / tota* 100,
               Yaourt = e_Yaourt / tota* 100,
               Compote = e_Compote / tota * 100,
               Jus_Au_lait = e_Jus_Au_lait / tota* 100,
               lait = e_lait / tota* 100) |>   
        mutate(across(where(is.numeric), ~round(., 2))) |>
        select(annee,trimestre,input$prod) |> 
        rename(Pourcentage=input$prod,Produit=trimestre) |> 
        filter(Produit==input$Tri)
    }
    
    else if (input$Tri=='Au cours des 4 trimestres'){
      hisbas<-Datav |>
        group_by(annee,trimestre) |> 
        summarise(e_Etuve=sum(Etuve),e_Cd=sum(Cd),e_Brasse=sum(Brasse),e_Fromage=sum(Fromage),
                  e_Yaourt=sum(Yaourt), e_Compote=sum(Compote),e_Jus_Au_lait=sum (Jus_Au_lait),
                  e_lait=sum(lait),.groups = 'keep') |> 
        mutate(tota=rowSums(across(starts_with("e"))),
               Etuve = e_Etuve / tota * 100,
               Cd = e_Cd / tota* 100,
               Brasse = e_Brasse / tota* 100,
               Fromage = e_Fromage / tota* 100,
               Yaourt = e_Yaourt / tota* 100,
               Compote = e_Compote / tota * 100,
               Jus_Au_lait = e_Jus_Au_lait / tota* 100,
               lait = e_lait / tota* 100) |>   
        mutate(across(where(is.numeric), ~round(., 2))) |>
        filter(annee==input$an) |>
        data.frame() |> 
        select(trimestre,input$prod) |>
        mutate(Produit=trimestre) |> 
        rename(annee=trimestre,Pourcentage=input$prod)
    }
    
    else if (input$prod == 'tous les produits'){
      hisbas<-Datav |>
        group_by(annee,trimestre) |> 
        summarise(e_Etuve=sum(Etuve),e_Cd=sum(Cd),e_Brasse=sum(Brasse),e_Fromage=sum(Fromage),
                  e_Yaourt=sum(Yaourt), e_Compote=sum(Compote),e_Jus_Au_lait=sum (Jus_Au_lait),
                  e_lait=sum(lait),.groups = 'keep') |> 
        mutate(tota=rowSums(across(starts_with("e"))),
               Etuve = e_Etuve / tota * 100,
               Cd = e_Cd / tota* 100,
               Brasse = e_Brasse / tota* 100,
               Fromage = e_Fromage / tota* 100,
               Yaourt = e_Yaourt / tota* 100,
               Compote = e_Compote / tota * 100,
               Jus_Au_lait = e_Jus_Au_lait / tota* 100,
               lait = e_lait / tota* 100) |>   
        mutate(across(where(is.numeric), ~round(., 2))) |>
        filter(annee==input$an) |>
        data.frame() |> 
        select(trimestre,Etuve,Cd,Brasse,Fromage,Yaourt,Compote,Jus_Au_lait,lait) |>
        melt(id.vars = "trimestre", variable.name = "Produit", value.name = "Pourcentage") |> 
        rename(annee=trimestre) |> 
        filter(annee==input$Tri) |>
        mutate(annee=Produit)
    }
    else {
      hisbas<-Datav |>
        group_by(annee,trimestre) |> 
        summarise(e_Etuve=sum(Etuve),e_Cd=sum(Cd),e_Brasse=sum(Brasse),e_Fromage=sum(Fromage),
                  e_Yaourt=sum(Yaourt), e_Compote=sum(Compote),e_Jus_Au_lait=sum (Jus_Au_lait),
                  e_lait=sum(lait),.groups = 'keep') |> 
        mutate(tota=rowSums(across(starts_with("e"))),
               Etuve = e_Etuve / tota * 100,
               Cd = e_Cd / tota* 100,
               Brasse = e_Brasse / tota* 100,
               Fromage = e_Fromage / tota* 100,
               Yaourt = e_Yaourt / tota* 100,
               Compote = e_Compote / tota * 100,
               Jus_Au_lait = e_Jus_Au_lait / tota* 100,
               lait = e_lait / tota* 100) |>   
        mutate(across(where(is.numeric), ~round(., 2))) |>
        filter(annee==input$an) |>
        data.frame() |> 
        select(trimestre,input$prod) |>
        mutate(Produit=trimestre) |> 
        rename(Pourcentage=input$prod,annee=trimestre)
    }
  })
  
  
  # Dataframe pour la prévision
  dataforcas<-reactive(Datav |>
                         rename("tous les produits"=ventes) |> 
                         select(input$prod,date) |> 
                         rename(Produit=input$prod))
  
  
  # Création d'un objet ts (Série temporelle du produit selectionné)
  ts_data<-reactive({
    ts(
      data=dataforcas()$Produit,
      start=c(year(min(dataforcas()$date)),month(min(dataforcas()$date))),
      frequency = 12
    )
  })
  
  #Dataframe pour la prévision avec Facebook prophet
  prophet_df<-reactive({
    donnepre() |> 
      rename(ds=date,y=Produit) 
  })
  
  #Model Prophet
  md1<-reactive ({
    prophet(prophet_df())
  })
  
  #Model Auto Arima
  md2<- reactive({
    forecast(auto.arima(ts_data()))
  })
  
  #Model TBATS
  md3<- reactive({
    forecast(tbats(ts_data()))
  })
  
  #Model ETS
  md4<- reactive({
    forecast(ts_data())
  })
  #Model Holt-winters
  md5<-reactive({
    forecast(HoltWinters(ts_data()))
  })
  
  # Fonction réactive générant de la courbe
  pros <- eventReactive (input$reactif, { 
    if(input$forcecastmodel=="fbpro"){
      plot(md1(),
           predict(
             md1(),
             make_future_dataframe(
               md1(),
               periods = 12, freq= "month"
             )
           )
      )
    }else if(input$forcecastmodel=="autoarima"){
      autoplot(md2())
    }else if (input$forcecastmodel=="tbats"){
      autoplot(md3()) 
    }else if (input$forcecastmodel=="auto"){
      autoplot(md4()) 
    }else if (input$forcecastmodel=="hw"){
      autoplot(md5()) 
    }
  })
  
  # Fonction réactive générant de la base des prévisions
  pros1 <- eventReactive (input$reactif, { 
    if(input$forcecastmodel=="fbpro"){
      predict(
        md1(),
        make_future_dataframe(
          md1(),
          periods = 12, freq= "month"
        )
      )
    }else if(input$forcecastmodel=="autoarima"){
      as.data.frame(forecast(md2(),h=12))
    }else if (input$forcecastmodel=="tbats"){
      as.data.frame(forecast(md3(),h=12)) 
    }else if (input$forcecastmodel=="auto"){
      as.data.frame(forecast(md4(),h=12))
    }else if (input$forcecastmodel=="hw"){
      as.data.frame(forecast(md5(),h=12))
    }
  })
  
  
  # Construire le graphique  suivant l'évolution des ventes  
  output$Plot <- renderPlotly({ 
    donne() |>
      plot_ly(x= ~ date, y=~ Sell, type = 'scatter', mode= 'lines') |>
      layout(title=paste("Evolution des ventes (tonnes) de :",input$prod,
                         "au cours de", input$an))
  })
  # Construire l'histogramme  des ventes  
  output$hist <- renderPlotly({ 
    hisbas() |>
      plot_ly(x = ~annee, y = ~Pourcentage, color = ~Produit, type = 'bar') |> 
      # Personnaliser l'aspect du graphique
      layout(title = paste("Pourcentage des ventes (tonnes) de :",input$prod,
                           "au cours de", input$an),
             xaxis = list(title = 'Temps'),
             yaxis = list(title = 'Pourcentage de vente'),
             barmode = 'stack')
  })
  
  # Afficher les données des ventes en fonction des choix  
  output$vent <- renderDT(
    donne(),
    extensions = 'Buttons',
    filter = 'top',
    options = list(
      dom="lftiprB",    
      buttons = c('copy', 'csv', 'excel', 'pdf','print')
    )
  )
  
  # Affichage des statistiques des données sorties selon 
  #les choix de l'utilisateur
  output$stat<- renderPrint ({
    summary(donne())
  })
  
  #Affichage de la série temporelle
  output$actual_ts<- renderPlotly({
    if(input$decompose){
      ts_decompose(ts_data())
    }else{
      ts_plot(ts_data(), title=input$prod)
    }
  })
  
  #Afficher les prévision dans un graphique
  output$autoplotforecast<-renderPlotly({
    pros()
  })
  
  #Afficher les résultats de la prévision dans une table
  output$forecastdata<- renderDT(
    tail(pros1(),12),
    extensions = 'Buttons',
    filter = 'top',
    options = list(
      dom="lftiprB",    
      buttons = c('copy', 'csv', 'excel', 'pdf','print')
    )   
  )
  
}
