source(file="mini/004_lib.R")


# Comienza el Shiny
shinyUI(fluidPage(
  
  # Titulo de la aplicación (sale arriba a la izqueirda... arriba del menu)
  titlePanel("Distribución Binomial"),
  
  # Lenguaje Matematico
  # withMathJax(),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(

      uiOutput("gmenu1")
     
        ),
    
    
    
    # Opciones de eleccion de grafico
    mainPanel(
      
      uiOutput("gmenu2")
      
    )
    
    
    
  )
))
################### FIN FIN FIN FIN FIN #####################