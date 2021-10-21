

ModuleControlUI <- function(id) {
  
  ns <- NS(id)
  
  
  
  
}



ModuleControlSERVER <-  function(input, output, session, base,
                                RMedic_general, status_BaseSalida,
                                zocalo_CIE) { 
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  
  UserSelection <- callModule(module = BatallaNavalSERVER, 
                              id =  "control01",
                              base = base,
                              zocalo_CIE = zocalo_CIE,
                              verbatim = FALSE)
  
  
  MiniBase <- callModule(module = MiniBaseSERVER, id =  "control02",
                                base = base,
                                batalla_naval = UserSelection$batalla_naval,
                                verbatim = FALSE)
  
  
  
 
  
  
  menuCONTROL <- reactive({
    
    # Si no hay orden de salir a la cancha... Nadie sale...
    if(is.null(RMedic_general())) return(NULL)
    if(!RMedic_general()) return(NULL)
    
    # Si no hay status de BaseSalida(), nos vamos...
    if(is.null(status_BaseSalida())) return(NULL)
    if(!status_BaseSalida()) return(NULL)
    
    
    tabs <- list()
    
    
    tabs[[1]] <-  tabPanel(
      title = "Control", 
      icon = icon("user-md"), 
      value = 2,
      fluidRow(
        column(1),
        column(10,
               h3("Menú para Control"),
               BatallaNavalUI(ns("control01")),
               MiniBaseUI(ns("control02"))
        ),
        column(1)
      )
      
    ) # End TabPanel
    
    
    
    tabs
    
  })
  
  
  #Return del Modulo
  return(menuCONTROL)
}