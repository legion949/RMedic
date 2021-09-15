# Librerias y Funcion EVA
source("../lib/libraries.R")


# Define server logic required to summarize and view the 
# selected ejemplo_file
function(input, output, session) {
  
  # Server del sidebarPanel
  source("script/007serverALL.R", local = T)$value
  
}

