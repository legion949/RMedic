

ModuleHoUI <- function(id) {
  
  ns <- NS(id)
  
  
  
  
}



ModuleHoSERVER <-  function(input, output, session, base,
                            RMedic_general, status_BaseSalida,
                            zocalo_CIE = zocalo_CIE) { 
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  
  UserSelection <- callModule(module = BatallaNavalSERVER, 
                              id =  "ho01",
                              base = base,
                              zocalo_CIE = zocalo_CIE,
                              verbatim = FALSE)
  
  
  MiniBase <- callModule(module = MiniBaseSERVER, id =  "ho02",
                         base = base,
                         batalla_naval = UserSelection$batalla_naval,
                         verbatim = FALSE)
  
  
  
  callModule(module = Tablas1Q_SERVER, id =  "ho03",
             minibase = MiniBase,
             batalla_naval = UserSelection$batalla_naval,
             decimales = UserSelection$decimales)
  
  
  callModule(module = Tablas1C_SERVER, id =  "ho04",
             minibase = MiniBase,
             batalla_naval = UserSelection$batalla_naval,
             decimales = UserSelection$decimales)
  
  
  menu <- reactive({
    
    # Si no hay orden de salir a la cancha... Nadie sale...
    if(is.null(RMedic_general())) return(NULL)
    if(!RMedic_general()) return(NULL)
    
    # Si no hay status de BaseSalida(), nos vamos...
    if(is.null(status_BaseSalida())) return(NULL)
    if(!status_BaseSalida()) return(NULL)
    
    
    tabs <- list()
    
    
    tabs[[1]] <-  tabPanel(
      title = "Prueba de Hipótesis", 
      icon = icon("user-md"), 
      value = 5,
      fluidRow(
        column(1),
        column(10,
               h3("Menú para Prueba de Hipótesis"),
               BatallaNavalUI(ns("ho01")),
               MiniBaseUI(ns("ho02")),
               Tablas1Q_UI(ns("ho03")),
               Tablas1C_UI(ns("ho04"))
        ),
        column(1)
      )
      
    ) # End TabPanel
    
    
    
    tabs
    
  })
  
  
  #Return del Modulo
  return(menu)
}