 # Librerias y Funcion EVA
suppressWarnings(suppressMessages(source("../lib/libraries.R")))


# Define server logic required to summarize and view the 
# selected ejemplo_file
function(input, output, session) {
  
  # Server del sidebarPanel
  source("script/008serverALL.R", local = T)$value
  
}

