source(file="script/004_lib.R", local= T)


# # # Comienzo del Shiny
shinyServer(function(input, output) {


  
  source(file = "script/001_server.R", local=T)$value
  
  
  
}) 