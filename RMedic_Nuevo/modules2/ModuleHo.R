

ModuleHoUI <- function(id) {
  
  ns <- NS(id)
  
  
  
  
}



ModuleHoSERVER <-  function(input, output, session, base,
                                  RMedic_general, status_BaseSalida,
                                  zocalo_CIE) { 
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  
  UserSelection <- callModule(module = BatallaNavalSERVER2, 
                              id =  "ho01",
                              base = base,
                              zocalo_CIE = zocalo_CIE,
                              verbatim = FALSE)
  
  batalla_naval <- UserSelection$batalla_naval
  casoRMedic <- reactive({
    
    if(is.null(batalla_naval())) return(NULL)
    if(is.null(batalla_naval()[[4]])) return(NULL)
    if(length(batalla_naval()[[4]]) == 0) return(NULL)
    if(batalla_naval()[[4]] == '') return(NULL)
    casoRMedic <- batalla_naval()[[4]]
    #casoRMedic <- as.numeric(as.character(as.vector(batalla_naval()[[4]])))
    casoRMedic
    
  })
  decimales <- UserSelection$decimales
  alfa <- UserSelection$alfa
  
  # observe(cat("casoRMedic()1: ", casoRMedic(), "\n"))
  
  MiniBase <- callModule(module = MiniBaseSERVER, id =  "ho02",
                         base = base,
                         batalla_naval = UserSelection$batalla_naval,
                         verbatim = FALSE)
  
  
  
  
  
  
  
  # Caso 1: 1Q
  callModule(module = Ho1Q_SERVER, 
             id =  "ho03",
             minibase = MiniBase,
             casoRMedic = casoRMedic,
             caso = 1,
             decimales = decimales,
             alfa = alfa)
  
  
  
  
  #Caso 2 : 1C
  callModule(module = Ho1C_SERVER,
             id =  "ho04",
             minibase = MiniBase,
             casoRMedic = casoRMedic,
             caso = 2,
             decimales = decimales,
             alfa = alfa,
             batalla_naval = batalla_naval)
  # # 
  # 
  # # Caso 3: 2Q
  callModule(module = Ho2Q_SERVER, id =  "ho05",
             minibase = MiniBase,
             casoRMedic = casoRMedic,
             caso = 3,
             decimales = decimales,
             alfa = alfa,
             batalla_naval = batalla_naval)
  # 
  # 
  # 
  # 
  # Caso 4: 2C
  callModule(module = Ho2C_SERVER, id =  "ho06",
             minibase = MiniBase,
             casoRMedic = casoRMedic,
             caso = 4,
             decimales = decimales,
             alfa = alfa,
             batalla_naval = batalla_naval)
  # 
  # 
  # 
  # 
  # Caso 5: 2Q
  callModule(module = HoQC_SERVER, 
             id =  "ho07",
             minibase = MiniBase,
             casoRMedic = casoRMedic,
             caso = 5,
             decimales = decimales,
             alfa = alfa,
             batalla_naval = batalla_naval)
  # 
  
  
  ###################################################################### 
  
  # TABLAS!
  callModule(module = Tablas1Q_SERVER, id =  "ho08",
             minibase = MiniBase,
             batalla_naval = UserSelection$batalla_naval,
             decimales = decimales)
  
  
  callModule(module = Tablas1C_SERVER, id =  "ho09",
             minibase = MiniBase,
             batalla_naval = UserSelection$batalla_naval,
             decimales = decimales)
  
  
  callModule(module = Tablas2Q_SERVER, id =  "ho10",
             minibase = MiniBase,
             batalla_naval = UserSelection$batalla_naval,
             decimales = decimales)
  
  callModule(module = Tablas2C_SERVER, id =  "ho11",
             minibase = MiniBase,
             batalla_naval = UserSelection$batalla_naval,
             decimales = decimales)
  
  callModule(module = TablasQC_SERVER, id =  "ho12",
             minibase = MiniBase,
             batalla_naval = UserSelection$batalla_naval,
             decimales = decimales)
  
  menuHO <- reactive({
    
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
      value = 4,
      fluidRow(
        column(1),
        column(10,
               h3("Menú para Prueba de Hipótesis"),
               BatallaNavalUI2(ns("ho01")),
               MiniBaseUI(ns("ho02")),
               Ho1Q_UI(ns("ho03")),
               Ho1C_UI(ns("ho04")),
               Ho2Q_UI(ns("ho05")),
               Ho2C_UI(ns("ho06")),
               HoQC_UI(ns("ho07")),
               # Graficos1C_UI(ns("graficos04")),
               # Graficos2Q_UI(ns("graficos05")),
               # Graficos2C_UI(ns("graficos06")),
               # GraficosQC_UI(ns("graficos07")),
               br(), br(), br(), br(), br(),
               Tablas1Q_UI(ns("ho08")),
               Tablas1C_UI(ns("ho09")),
               Tablas2Q_UI(ns("ho10")),
               Tablas2C_UI(ns("ho11")),
               TablasQC_UI(ns("ho12"))
        ),
        column(1)
      )
      
    ) # End TabPanel
    
    
    
    tabs
    
  })
  
  
  #Return del Modulo
  return(menuHO)
}