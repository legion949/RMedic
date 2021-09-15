cat("Inicio Aula Server", "\n")

library(shinyjs)

# source(file="mod3/script/004_lib.R", local= T)

output$file_aula <- renderUI({
  
  selectInput("file_aula", "Distribución:",
              choices = files,
              multiple=FALSE)
  
})




# observeEvent(input$file_aula,{
#   reset("form")
# })

if (1 == 1) {
observeEvent(input$file_aula,{
  
  dir_gen <- paste0(ubi, "/", input$file_aula)
  dir_script <- paste0(dir_gen, "/mini")
  
  dir_lib <- paste0(dir_script, "/004_lib.R")
  source(file=dir_lib, local= T)
  
  dir_server <- paste0(dir_script, "/001_server.R")
  source(file=dir_server, local=T)   
  
  #  reset("gmenu1")
  
  #  reset("gmenu2")
  
  
  
  
  output$gmenu1 <- renderUI({
    
    dir_sidebarPanel <- paste0(dir_script, "/002_sidebarPanel.R")
    source(file = dir_sidebarPanel, local=T)$value
    
  })
  
  
  
  output$gmenu2 <- renderUI({
    
    dir_mainPanel <- paste0(dir_script, "/003_mainPanel.R")
    source(file=dir_mainPanel, local=T)$value
    
  })
  
  
})
}

cat("Fin Aula Server", "\n")
