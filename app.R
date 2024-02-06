# Chargement des differentes packages
#install.packages("shinyWidgets")
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)
library(shinymanager)
library(sqldf)
library(RPostgreSQL)
library(shinyWidgets)
library(waiter)
library(shinythemes)
library(shinycssloaders)
library(shinyjs)
#library(readxl)
#production <- read_excel("T:/Générale des Assurances/Tableau de bord/Projet dashbord GA/AppGA/data/production.xlsx")
#View(production)

#il ya deux packages shinyautr et shinymanager qui permet de securiser on peux aller
#au de la ca pour attribuer des droits d'administrateurs et utilisateurs 
#creation du compte 

credentials <- data.frame(
  user = c("shiny", "shinymanager"),
  password = c("azerty", "12345"), 
  start = c("2021-09-28"), 
  expire = c(NA, "2021-12-31"),
  admin = c(FALSE, TRUE),
  comment = "Simple and secure authentification mechanism 
  for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)#fin 



production <- read_excel("data/production.xlsx")
#etat <- read_excel("data/etat.xlsx")

option_1= unique(production$INTERMEDIAIRE)

# inserer le logo de la GA

tableau= tags$a(href="www",
                tags$img(src="logo.jpg",height= '40', width='60'),
                "TABLEAU DE BORD") #inserer une image dans le header

header=dashboardHeader(title = tableau,titleWidth = 500) #fin de declaration de header
sidebar=dashboardSidebar(
  sidebarMenu(
    menuItem("RESUME",tabName = "resume" ,icon = icon("dashboard")  )#,
    
   # menuItem("TABLEAU DE BORD",tabName = "tableau",icon = icon("thumbs-up"))
  ) #fin de sidebar qui contient les 2 ecrans
)
body= dashboardBody(
  tabItems(
    #debut du premier ecran 
    tabItem(
      tabName = "resume",
      fluidRow(
        box(
          width = 3,
          status = "primary",
          solidHeader = TRUE,
          title = "INTERMEDIAIRE",
          selectInput(inputId = "inter",label ="",
                      choices = option_1)
        ),#fin de box 
       

        box(
          width = 3,
          status = "primary",
          solidHeader = TRUE,
          title = "DATE D'EMISSION",
          selectizeInput(inputId = "annee",label ="",
                      choices = NULL,options = list(dateFormat = "dd/mm/yyyy"))
          #unique( production$DATEEFFE<-as.Date(production$DATEEFFE,format="%Y/%m/%d"))
        ),#fin de box
        
   
        box( width = 3,
             status = "warning",
             solidHeader = TRUE,
             title = "REDUCTION",
             selectInput(inputId = "reduction",label ="",
                         choices = c(0,10,15,20), selected = 0)

        ),
        useShinyjs(),
        box( width = 3,
             status = "warning",
             solidHeader = TRUE,
             title = "DETECTER LES ANOMOLIES",
             actionButton(inputId = "bouton1", label = "Lancer"),
              
        ),

      ),
      
      fluidRow(
        valueBox(
          value = textOutput("nombre"),
          subtitle = "Nombres des emissions",
          #icon = icon("CFA"),
          color = "teal",
          width = 2
        ),#fin de valuebox
        
   
        valueBox(
          value = textOutput("somme"),
          subtitle = "Montant Total des emissions",
          #icon = icon("list-alt"),
          color = "blue",
          width = 3
        ),#fin de valuebox
      
      
      valueBox(
        value = textOutput("nombre1"),
        subtitle = "Nombres des emissions défaillants",
        #icon = icon("CFA"),
        color = "teal",
        width = 3,
        uiOutput("spinner")
      ),#fin de valuebox
      
      valueBox(
        value = textOutput("somme1"),
        subtitle = "Montant des emissions défaillants",
        #icon = icon("list-alt"),
        color = "green",
        width = 3
      ),#fin de valuebox

        
      
      box(
        title = "Tableau des emissions",
        footer = "en CFA",
        status = "info",
        #icon = icon("table"),
        solidHeader = TRUE,
        width = 12,
        # Utilisez withSpinner pour ajouter un spinner à cet output
        dataTableOutput("result_table")
        
      ) # fin de box
      
      )#fin du fluidRow
      
    
        
  )
  )
)#fin de dashbordbody
ui <- dashboardPage(header,sidebar,body, skin="yellow")# fin de declaration de ui

ui <- secure_app(ui,choose_language=TRUE) # metre tout le code ui dans la serrure

server <- function(input, output, session) ({
  #la fonction d'utilisateur
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  #fin de la fonction d'utilisateur
  
  #############Premier tabitem##############################################################################
  # Mettre à jour les options du deuxième selectInput en fonction de la sélection du premier selectInput
  observeEvent(input$inter, {
    options_2 <- unique(as.Date(production$DATEEFFE,format="%d/%m/%Y")[production$INTERMEDIAIRE == input$inter])
    updateSelectInput(session, "annee", choices = options_2)
  })
 
  
  

 base<- reactive({
  filter (production[ production$INTERMEDIAIRE == input$inter & as.Date(production$DATEEFFE,format="%d/%m/%Y") == input$annee , ]) %>%
     summarize(
       count = n(),
       somme= sum(primenet),
       moyenne= mean(primenet)
     )
 })
  

 #Afficher la sortie finale sous forme de tableau en fonction des sélections des selectInputs
  

  
  output$nombre <- renderText({
    nb=base()
    nombre<- sum(nb$count)
    formatted_nombre <- formatC(nombre, format = "d", big.mark = " ")
    paste0(formatted_nombre)
  })# nombre d'enregistrement
  
  output$somme <- renderText({
    nb <- base()
    somme <- sum(nb$somme)
    formatted_somme <- formatC(somme, format = "d", big.mark = " ")
    paste0(formatted_somme)
  })
  
  # Réaction au bouton de recherche
  base1 <- reactive({
    if (!is.null(input$inter) && !is.null(input$annee)) {
      prod <- production[production$INTERMEDIAIRE == input$inter & as.Date(production$DATEEFFE, format = "%d/%m/%Y") == input$annee, ]
      prod[, c("Code intermediaire","Numéro Police","DATEEFFE","DATEECHA","Catégorie", "Statut", "Zone", "puissance", "primenet")] # keep only the relevant columns
    }
  })
  
  etat <- reactive({
    # Code pour obtenir la base etat
    # ...
    etat <- read_excel("data/etat.xlsx")
    
  })


  # Comparer les données des deux bases
  observeEvent(input$bouton1, {
    
    
    showModal(modalDialog(
      title = "Traitement en cours",
      "Veuillez patienter...",
      easyClose = FALSE,
      footer = NULL))
  
    # Simuler un long traitement
    Sys.sleep(3) # Remplacer par le code de traitement réel
    #rechercheEnCours(FALSE)
    removeModal()
    
    # Vérifier que les données sont disponibles
    req(etat())
    req(base1())
    etat <- reactive({
      # Code pour obtenir la base etat
      # ...
      etat <- read_excel("data/etat.xlsx")
      if (input$reduction > 0) {
        etat$primenet <- etat$primenet * (1 - (as.numeric(input$reduction))/100)
      }
      etat
    })
   
    # Initialiser la liste des résultats
    result_list <- list()

   reduction1= 0.2
   reduction2= 0.3
   reduction3= 0.4
   reduction4= 0.7
   reduction5= 0.85
   reduction6= 1
    # Comparer les données de chaque ligne de la base à la base témoin
    for (i in 1:nrow(base1())) {
      for (j in 1:nrow(etat())) {
        if (base1()[i, "Catégorie"] == etat()[j, "Catégorie"] &
            base1()[i, "Statut"] == etat()[j, "Statut"] &
            base1()[i, "Zone"] == etat()[j, "Zone"] &
            base1()[i, "puissance"] == etat()[j, "puissance"]) {
          
          if(as.Date(base1()$DATEECHA, format = "%d/%m/%Y")[i] - as.Date(base1()$DATEEFFE, format = "%d/%m/%Y")[i] <= 30){
            
            prime_etat_reduite <- etat()[j, "primenet"] * reduction1
            
            if( base1()[i, "primenet"]>0){
              
                if( base1()[i, "primenet"] < prime_etat_reduite){
              
                  
                    result_list <- append(result_list, list(cbind(base1()[i,], prime_etat_reduite,(prime_etat_reduite - base1()[i,9]))))
                    colnames(result_list[[length(result_list)]])[10] <- "prime_DA"
                    colnames(result_list[[length(result_list)]])[11] <- "ecart"
                    }# fin du si
          }
          }
          if((as.Date(base1()$DATEECHA, format = "%d/%m/%Y")[i] - as.Date(base1()$DATEEFFE, format = "%d/%m/%Y")[i]) > 30 & (as.Date(base1()$DATEECHA, format = "%d/%m/%Y")[i] - as.Date(base1()$DATEEFFE, format = "%d/%m/%Y")[i])<=60){
           
            
             prime_etat_reduite <- etat()[j, "primenet"] * reduction2
            
             if( base1()[i, "primenet"]>0){
               
                    if( base1()[i, "primenet"] < prime_etat_reduite ){
              
                   #result_list <- append(result_list, list(base1()[i,]))
                    result_list <- append(result_list, list(cbind(base1()[i,], prime_etat_reduite,(prime_etat_reduite - base1()[i,9]))))
                     colnames(result_list[[length(result_list)]])[10] <- "prime_DA"
                     colnames(result_list[[length(result_list)]])[11] <- "ecart"

                     }# fin du si
          }
          }
          
          if((as.Date(base1()$DATEECHA, format = "%d/%m/%Y")[i] - as.Date(base1()$DATEEFFE, format = "%d/%m/%Y")[i]) > 60 & (as.Date(base1()$DATEECHA, format = "%d/%m/%Y")[i] - as.Date(base1()$DATEEFFE, format = "%d/%m/%Y")[i]) <= 90){
            
            prime_etat_reduite <- etat()[j, "primenet"] * reduction3
            
            if( base1()[i, "primenet"]>0){
              
                      if( base1()[i, "primenet"] < prime_etat_reduite ){
                      #result_list <- append(result_list, list(base1()[i,]))
                       result_list <- append(result_list, list(cbind(base1()[i,], prime_etat_reduite,(prime_etat_reduite - base1()[i,9]))))
                       colnames(result_list[[length(result_list)]])[10] <- "prime_DA"
                       colnames(result_list[[length(result_list)]])[11] <- "ecart"

                       } # fin
          }
          }
          
          if((as.Date(base1()$DATEECHA, format = "%d/%m/%Y")[i] - as.Date(base1()$DATEEFFE, format = "%d/%m/%Y")[i]) > 90 & (as.Date(base1()$DATEECHA, format = "%d/%m/%Y")[i] - as.Date(base1()$DATEEFFE, format = "%d/%m/%Y")[i])<=180){
            
            prime_etat_reduite <- etat()[j, "primenet"] * reduction4
           
             if( base1()[i, "primenet"]>0){
                      if( base1()[i, "primenet"] < prime_etat_reduite ){
                      #result_list <- append(result_list, list(base1()[i,]))
                      result_list <- append(result_list, list(cbind(base1()[i,], prime_etat_reduite,(prime_etat_reduite - base1()[i,9]))))
                      colnames(result_list[[length(result_list)]])[10] <- "prime_DA"
                       colnames(result_list[[length(result_list)]])[11] <- "ecart"

                      }#fin de si
          }
          }
          
          
          if((as.Date(base1()$DATEECHA, format = "%d/%m/%Y")[i] - as.Date(base1()$DATEEFFE, format = "%d/%m/%Y")[i]) > 180 & (as.Date(base1()$DATEECHA, format = "%d/%m/%Y")[i] - as.Date(base1()$DATEEFFE, format = "%d/%m/%Y")[i])<=270){
            
            prime_etat_reduite <- etat()[j, "primenet"] * reduction5
            
            if( base1()[i, "primenet"]>0){
              
                    if( base1()[i, "primenet"] < prime_etat_reduite ){
                      result_list <- append(result_list, list(cbind(base1()[i,], prime_etat_reduite, (prime_etat_reduite - base1()[i,9]))))
                      colnames(result_list[[length(result_list)]])[10] <- "prime_DA"
                      colnames(result_list[[length(result_list)]])[11] <- "ecart"
        
                    }# fin de si
          }
          }
          
          
          if(as.Date(base1()$DATEECHA, format = "%d/%m/%Y")[i] - as.Date(base1()$DATEEFFE, format = "%d/%m/%Y")[i]>270){
            
            prime_etat_reduite <- etat()[j, "primenet"] * reduction6
            #print(prime_etat_reduite)
            
            if( base1()[i, "primenet"]>0){
                        if( base1()[i, "primenet"] < prime_etat_reduite ){
                          
                          result_list <- append(result_list, list(cbind(base1()[i,], prime_etat_reduite, (prime_etat_reduite - base1()[i,9]))))
                          colnames(result_list[[length(result_list)]])[10] <- "prime_DA"
                          colnames(result_list[[length(result_list)]])[11] <- "ecart"
            
                        }# fin de si
          }
          }
          
        }
        
        # else {
        #   
        #   showModal(modalDialog("La prime est supérieure ou égale à la prime de l'arrêté DA"))
        #   return()
        #   result_list
        # }
          
          }
          
        }
      
      ##################icic
    
   #return(result_list)
   
    # Afficher le tableau des résultats si la liste n'est pas vide
    if (length(result_list) > 0) {
      output$result_table <- DT::renderDataTable({
        DT::datatable(
          do.call(rbind, result_list ),
          extensions = 'Buttons',
          options = list(
            dom = 'Bfrtip',
            lengthChange = FALSE,
            buttons = list(

              list(
                extend = 'excel',
                text = 'Excel',
                exportOptions = list(
                  modifier = list(
                    page = 'all'
                  )
                )
              ),
              'print',
              'copy',
              'csvHtml5',
              'excelHtml5',
              'pdfHtml5',
              'selectAll',
              'selectNone'
            )
          )
        )
      })
      
      # Calculer les statistiques
      bd <- reactive({
        df <- do.call(rbind, result_list)
        df[1:9] %>% 
          summarize(
            count = n(),
            somme = sum(primenet),
            moyenne = mean(primenet)
          )
      })
      
      # Afficher le nombre d'enregistrements
      output$nombre1 <- renderText({
        nb <- bd()
        nombre1<- sum(nb$count)
                formatted_nombre1 <- formatC(nombre1, format = "d", big.mark = " ")
                paste0(formatted_nombre1)
              })  # nombre d'enregistrement
              
              output$somme1 <- renderText({
                nb <- bd()
                somme1 <- sum(nb$somme)
                formatted_somme1 <- formatC(somme1, format = "d", big.mark = " ")
                paste0(formatted_somme1)
              })  # somme des primenet 
            }
  })
  
  
  
  ##########du premier tabitem#######################################################""
  
 
 
})# fin du server
# Exécution de l'application
shinyApp(ui , server )

