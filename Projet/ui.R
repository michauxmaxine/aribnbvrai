#choix des variables pour l'UI

source("readdata.R")

shinyUI(navbarPage(theme = "style.css",
                   title = "ACObnb",
                   fluid = TRUE, 
                   collapsible = TRUE,
                   
                   #1 HOME
                   tabPanel("Home", includeHTML("home.html")
                   ),
                   
                   #2 Locataire
                   tabPanel("Vous Ãªtes locataire",
                          h1("Vous etes a la recherche d'un logement a New-York ?",style = "font-family: 'Bookman, URW '; font-size : 20pt; color: #FF3399 ; text-align: center"),
                         
                          textOutput("bronx"),
                          textOutput("brooklyn"),
                          textOutput("manhattan"), 
                          textOutput("queens"),
                          textOutput("staten"),
                            
                          fluidRow(
                              column(width = 4,
                                     selectizeInput(inputId = "quartier",
                                                    label = "Selectionnez le quartier",
                                                    choices = rbnb$neighbourhood_group)
                              ),
                              column(width = 4,
                                     selectizeInput(inputId = "logement",
                                                    label = "Selectionnez le type de logement",
                                                    choices = rbnb$room_type)
                              ),
                              column(width = 4,
                                     sliderInput(inputId = "price",
                                                 label = "Selectionnez le prix desire pour une nuit entre : ",
                                                 min(rbnb$price),
                                                 max(rbnb$price),
                                                 value = c(min, max),
                                                 step = 10
                                     )
                              )
                            ), 
                            fluidRow(
                              column(width = 6,
                                     leafletOutput("map", height = 1000)
                              ),
                              column(width = 6,
                                     plotOutput("plot"))
                            )
                            
                   ),
                   
                   #3 Propiétaire
                   tabPanel("Vous etes proprietaire",
                            radioButtons(
                              inputId = "arr",
                              label = "Arrondissement du bien",
                              choices = c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten.Island"),
                              inline=TRUE
                            ),
                            ####---select quartier-----
                            uiOutput('quartier1'),
                            ###----
                            numericInput(
                              inputId = "night",
                              "Nombre minimum de nuits",
                              1,
                              min = 1,
                              300,
                              1),
                            numericInput(
                              inputId = "appart",
                              "Avez-vous d'autres appartement sur ACObnb ?",
                              0,
                              min = 0,
                              300,
                              1
                            ),
                            radioButtons(
                              inputId = "room",
                              label = "Type de chambre",
                              choices = c("Entire.home.apt", "Private.room", "Shared.room"),
                              selected = NULL,
                              inline = 1
                            ),
                            actionButton(inputId = "run","Lancer mon estimation", icon("check")),
                            
                            h3(textOutput("selected_var"))
                            
                   ),
                   
                   #4 Notre entreprise
                   tabPanel("Nous",
                            includeHTML("nous.html"),
                            includeHTML("projet.html")
                            
                   )
                   
)
)

