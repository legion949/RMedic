# OK a todo...

# t <- proc.time()
# Librerias y Funcion EVA
source("../lib/libraries.R")
library("DT")
# options(defaultPackages = c(listado_inicial, listado_agregado))
# proc.time() - t

shinyServer(function(input, output, session)  {
  
  output$Manual_Rmedic = downloadHandler(
    filename = 'Manual e Iniciación - Rmedic.pdf',
    content ='Manual e Iniciación - Rmedic.pdf',
    contentType = 'application/pdf'
  )

  # 1) Home
  source("script/001svHome.R", local = TRUE)$value

  
  # 2) Rmedic
  source("script/001svRmedic.R", local = TRUE)$value
  
  
  # 3) App de Educacion
  source("script/002_svApp.R", local = TRUE)$value
  

  

  
  

  }) # Fin Shiny Server


