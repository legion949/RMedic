
source(file="mini/004_lib.R", local= T)

# # # Comienzo del Shiny
shinyServer(function(input, output) {
  
  source(file="mini/001_server.R", local=T)
  
  
  output$gmenu1 <- renderUI({
    source(file="mini/002_sidebarPanel.R", local=T)$value
    
  })
  
  
  
  output$gmenu2 <- renderUI({
    source(file="mini/003_mainPanel.R", local=T)$value
    
  })
  
}) 