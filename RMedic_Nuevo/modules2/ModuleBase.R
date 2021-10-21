ModuleBaseUI <- function(id) {
  
  ns <- NS(id)
  
  tabPanel(title = "Base de Datos", 
           icon = icon("user-md"), 
           value = 1,
           br(),
           fluidRow(
             
             MiBase01_UI(ns("base01"))#,
             
           ),
           br(), br()
  )
  
  #tabs
  
  
}



ModuleBaseSERVER <-  function(input, output, session) {
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  
}