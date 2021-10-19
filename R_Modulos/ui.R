
source("uiCode.R")

fluidPage(
  useShinyjs(),
  theme = "styles.css", 
  titlePanel("R+Medic"),
  
  
  
  br(), br(),
  fluidRow(
    column(1),
    column(4,bsButton("showpanel", "Ocultar/Mostrar Carga de Datos", type = "toggle", value = TRUE,
                    icon("bars"), style = "primary", size = "large"
           )
    )
  ),
  br(), br(),
  
  sidebarLayout(
    div(id = "MySidebar",
        
        sidebarPanel(id = "Sidebar", 
                                      SideBarBaseUI("base01"))),
  mainPanel(id = "Main",
  
            
            uiOutput("RMedicSoft"),
            
    #        MiTexto01_UI("tablas05"),
  #  MiBase01_UI("tablas05"), # tableOutput("BaseSalida"),
  #          MiBase01_UI("tablas05"),
  #  BatallaNavalUI("tablas01"),
  #  MiniBaseUI("tablas02"),
   # Tablas1Q_UI("tablas03"),
  #  Tablas1C_UI("tablas04"),
    br(), br()
    
  )
) # End MainPanel ------------------------------------------
)
