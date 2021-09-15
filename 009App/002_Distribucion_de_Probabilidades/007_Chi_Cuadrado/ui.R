library(shiny)
# titlePanel("Distribución Chi Cuadrado: probabilidad entre dos valores Chi"),

#nuevo <- "/home/david/ARN/PROYECTOS/AULA VIRTUAL/ESTADISTICA/pagmod/mod2/content/002_Distribucion_de_Probabilidades/001_Binomial"

#setwd(nuevo)

source(file="mini/004_lib.R")


# Comienza el Shiny
shinyUI(fluidPage(
  
  # Titulo de la aplicación (sale arriba a la izqueirda... arriba del menu)
  titlePanel("Distribución Chi Cuadrado: probabilidad entre dos valores Chi"),
  
  # Lenguaje Matematico
  # withMathJax(),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      withMathJax(),
      uiOutput("gmenu1")
      
    ),
    
    
    
    # Opciones de eleccion de grafico
    mainPanel(
      
      uiOutput("gmenu2")
      
    )
    
    
    
  )
))
################### FIN FIN FIN FIN FIN #####################
