
source("uiCode.R")

fluidPage(
  useShinyjs(),
  theme = "styles.css", 
  titlePanel("R+Medic"),
  
  
  
  br(), br(),
  fluidRow(
           bsButton("showpanel", "Ocultar/Mostrar Carga de Datos", type = "toggle", value = TRUE,
                    icon("bars"), style = "primary", size = "large"
           )
  ),
  br(), br(),
  
  sidebarLayout(
    div(id = "MySidebar",sidebarPanel(id = "Sidebar", SideBarBaseUI("tablas05"))),
  mainPanel(id = "Main",

    BatallaNavalUI("tablas01"),
    MiniBaseUI("tablas02"),
    Tablas1Q_UI("tablas03"),
    Tablas1C_UI("tablas04"),
    br(), br()
    
  )
) # End MainPanel ------------------------------------------
)
