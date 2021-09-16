
library(shiny)
library(bslib)
options(encoding = "UTF-8")
options(shiny.maxRequestSize = 500*1024^2)

source("uiCode.R")
source("lib.R")
# Define UI for dataset viewer application
fluidPage(theme = "styles.css", 
          #theme = bs_theme(version = 4),
          
          # Application title.
          titlePanel("R+Medic"),
          br(),
          
          
          fluidRow(
            column(1),
            column(10, 
                    tabsetPanel(
                    tabPanel(title = "Base de Datos", 
                        icon = icon("user-md"), 
                        value = 1,
                        br(),
                    fluidRow(
                      column(4, OpcionesDeCarga),
                      column(8, 
                             h3(textOutput("TextBase_Alert")), 
                             htmlOutput("TextBase_InfoDataSet"), br(),
                             h3(textOutput("TextBase_Intro")),
                             dataTableOutput('BASE_SALIDA'))
                      )
              ), 
              tabPanel(title = "Control", 
                       icon = icon("user-md"), 
                       value = 2, 
              ), 
              tabPanel(title = "Tablas", 
                       icon = icon("user-md"), 
                       value = 3, 
              ), 
              tabPanel(title = "Graficos", 
                       icon = icon("user-md"), 
                       value = 4, 
              ),
              tabPanel(title = "Pruebas de Hipotesis", 
                       icon = icon("user-md"), 
                       value = 5, 
              ),
              tabPanel(title = "Sobrevida", 
                       icon = icon("user-md"), 
                       value = 6, 
              )
              
            )
            ),
            column(1)
          )
)