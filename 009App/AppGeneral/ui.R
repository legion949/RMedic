#nuevo <- "/home/david/ARN/PROYECTOS/AULA VIRTUAL/ESTADISTICA/pagmod/mod2/content/002_Distribucion_de_Probabilidades/001_Binomial"

#setwd(nuevo)

# Comienza el Shiny
shinyUI(fluidPage(
  div(
    id = "form",
#  useShinyjs(),
withMathJax(),
  # Titulo de la aplicación (sale arriba a la izqueirda... arriba del menu)
  titlePanel("Aula Virtual"),
 
  # Lenguaje Matematico
  # withMathJax(),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      # uiOutput("file_aula"),     
      # uiOutput("gmenu1")
      source(file = "script/002_sidebarPanel.R", local=T)$value
    ),
    
    
    
    # Opciones de eleccion de grafico
    mainPanel(
      
      source(file = "script/003_mainPanel.R", local=T)$value
      
    )
    
    
    
  )
)))
################### FIN FIN FIN FIN FIN #####################