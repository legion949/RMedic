
# run_with_themer()
library(shiny)
library(bslib)
options(encoding = "UTF-8")
options(shiny.maxRequestSize = 500*1024^2)

source("uiCode.R")
source("lib.R")
# Define UI for dataset viewer application
fluidPage( theme = "styles.css", 
  #         theme = bs_theme(version = 4, bootswatch = "minty",
  #                          fg = "rgb(28, 5, 5)", primary = "#0025FF", 
  #                          secondary = "#42E1C2", success = "#2FB8E3", info = "#A9B817", 
  #                          `enable-shadows` = TRUE, bg = "rgb(255, 255, 255)"),
  #         h1("Colored Tabs"),
  #         tags$style(HTML("
  #    .tabbable > .nav > li > a                  {background-color: orange;  color:black}
  # #   .tabbable > .nav > li > a[data-value='t1'] {background-color: red;   color:white}
  # #   .tabbable > .nav > li > a[data-value='t2'] {background-color: blue;  color:white}
  # #   .tabbable > .nav > li > a[data-value='t3'] {background-color: green; color:white}
  #    .tabbable > .nav > li[class=active]    > a  {background-color: white; color:black}
  #  ")),
  #         tabsetPanel(
  #           tabPanel("t0",h2("normal tab")),
  #           tabPanel("t1",h2("red tab")),
  #           tabPanel("t2",h2("blue tab")), 
  #           tabPanel("t3",h2("green tab")),
  #           tabPanel("t4",h2("normal tab")),
  #           tabPanel("t5",h2("normal tab"))
  #         ),
  #         # Application title.
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