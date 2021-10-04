
# run_with_themer()

options(encoding = "UTF-8")
options(shiny.maxRequestSize = 500*1024^2)

source("uiCode.R")
source("lib.R")
# Define UI for dataset viewer application
fluidPage (
  useShinyjs(),
  theme = "styles.css", 
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
  bsButton("showpanel", "Show/hide sidebar", type = "toggle", value = TRUE),
  fluidRow(
    column(1),
    column(11,
           actionButton("toggleSidebar", "Mostrar/Quitar Carga de Datos",
                        icon("bars"), 
                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    )
  ),
  br(),
 
  br()
    ,

 
 textOutput("prueba1"),
 textOutput("prueba2"),
 
      sidebarLayout(
        div(id = "MySidebar",sidebarPanel(id = "Sidebar", OpcionesDeCarga)),
        mainPanel( id ="Main",       
            #  fluidRow(
                column(1,
                   actionButton("MiniButton", "",
                                icon("bars"), 
                                width = "75px",
                                style ="color: #fff; background-color: #337ab7; 
                                border-color: #2e6da4; height:65px;
                                font-size:200%")),
                column(10, 
                  uiOutput("RMedicSoft"), br(),
                  tableOutput("BasePlaneta")
                   ),
            column(1)
          )
          
), fluid = T
 # )
)


