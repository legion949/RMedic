# Librerias y Funcion EVA
source("../lib/libraries.R")
# source("../load_functions/load_functions.R")


# Cargamos todas las otras funciones
# EVA01("../functions")

# Define server logic required to summarize and view the 
# selected ejemplo_file
function(input, output, session) {
  
  # Server del sidebarPanel
  source("script/002serverALL.R", local = T)$value
  
}

