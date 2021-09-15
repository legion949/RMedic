
# Librerias y Funcion EVA
source("../lib/libraries.R")




# Define server logic required to summarize and view the 
# selected ejemplo_file
function(input, output, session) {
  
  # Server del sidebarPanel
  source("script/003serverALL.R", local = T)$value
 
 
  
  
  
  
  
  
  # Objeto Salida ui para el MainPanel
  output$uiMainPanel <- renderUI({
    conditionalPanel("1 == 1",
                     
                     # Si es Ejemplo   
                     h3("Esta es una prueba del SideBarPanel"),
                     verbatimTextOutput("valuesMainPanel")   
                     
    )
  })
  
   
}

