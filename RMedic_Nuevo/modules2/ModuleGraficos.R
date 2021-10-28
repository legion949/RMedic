

ModuleGraficosUI <- function(id) {
  
  ns <- NS(id)
  
  
  
  
}



ModuleGraficosSERVER <-  function(input, output, session, base,
                                RMedic_general, status_BaseSalida,
                                zocalo_CIE) { 
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  
  UserSelection <- callModule(module = BatallaNavalSERVER, 
                              id =  "graficos01",
                              base = base,
                              zocalo_CIE = zocalo_CIE,
                              verbatim = FALSE)
  
  
  MiniBase <- callModule(module = MiniBaseSERVER, id =  "graficos02",
                         base = base,
                         batalla_naval = UserSelection$batalla_naval,
                         verbatim = FALSE)
  
  
  callModule(module = Graficos1Q_SERVER, id =  "graficos03",
             minibase = MiniBase,
             batalla_naval = UserSelection$batalla_naval,
             decimales = UserSelection$decimales)
  
  # TABLAS!
  callModule(module = Tablas1Q_SERVER, id =  "graficos04",
             minibase = MiniBase,
             batalla_naval = UserSelection$batalla_naval,
             decimales = UserSelection$decimales)
  
  
  callModule(module = Tablas1C_SERVER, id =  "graficos05",
             minibase = MiniBase,
             batalla_naval = UserSelection$batalla_naval,
             decimales = UserSelection$decimales)
  
  
  menuGRAFICOS <- reactive({
    
    # Si no hay orden de salir a la cancha... Nadie sale...
    if(is.null(RMedic_general())) return(NULL)
    if(!RMedic_general()) return(NULL)
    
    # Si no hay status de BaseSalida(), nos vamos...
    if(is.null(status_BaseSalida())) return(NULL)
    if(!status_BaseSalida()) return(NULL)
    
    
    tabs <- list()
    
    
    tabs[[1]] <-  tabPanel(
      title = "Gráficos", 
      icon = icon("user-md"), 
      value = 4,
      fluidRow(
        column(1),
        column(10,
               h3("Menú para Gráficos"),
               BatallaNavalUI(ns("graficos01")),
               MiniBaseUI(ns("graficos02")),
               Graficos1Q_UI(ns("graficos03")),
               Tablas1Q_UI(ns("graficos04")),
               Tablas1C_UI(ns("graficos05"))
        ),
        column(1)
      )
      
    ) # End TabPanel
    
    
    
    tabs
    
  })
  
  
  #Return del Modulo
  return(menuGRAFICOS)
}