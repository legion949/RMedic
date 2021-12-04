

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
  
 # observe(cat("casoRMedic()1: ", casoRMedic(), "\n"))
  
  MiniBase <- callModule(module = MiniBaseSERVER, id =  "graficos02",
                         base = base,
                         batalla_naval = UserSelection$batalla_naval,
                         verbatim = FALSE)
  
  

    

    
    
  # Caso 1: 1Q
  callModule(module = Graficos1Q_SERVER, id =  "graficos03",
             minibase = MiniBase,
             casoRMedic = casoRMedic,
             caso = 1,
             decimales = decimales)



  # Caso 2 : 1C
  callModule(module = Graficos1C_SERVER, id =  "graficos04",
             minibase = MiniBase,
             casoRMedic = casoRMedic,
             caso = 2,
             decimales = decimales,
             batalla_naval = batalla_naval)

  # callModule(module = Graficos2Q_SERVER, id =  "graficos05",
  #            minibase = MiniBase,
  #            batalla_naval = UserSelection$batalla_naval,
  #            decimales = UserSelection$decimales)
  # 
  # 
  # 
  
  # Caso 4: 2C
  callModule(module = Graficos2C_SERVER, id =  "graficos06",
             minibase = MiniBase,
             casoRMedic = casoRMedic,
             caso = 4,
             decimales = decimales,
             batalla_naval = batalla_naval)

  
  # Caso 5: QC
  callModule(module = GraficosQC_SERVER, id =  "graficos07",
             minibase = MiniBase,
             casoRMedic = casoRMedic,
             caso = 5,
             decimales = decimales,
             batalla_naval = batalla_naval)
  
 
  
  ###################################################################### 
  
  # TABLAS!
  callModule(module = Tablas1Q_SERVER, id =  "graficos08",
             minibase = MiniBase,
             batalla_naval = UserSelection$batalla_naval,
             decimales = decimales)
  
  
  callModule(module = Tablas1C_SERVER, id =  "graficos09",
             minibase = MiniBase,
             batalla_naval = UserSelection$batalla_naval,
             decimales = decimales)
  
 
  callModule(module = Tablas2Q_SERVER, id =  "graficos10",
             minibase = MiniBase,
             batalla_naval = UserSelection$batalla_naval,
             decimales = decimales)
  
  callModule(module = Tablas2C_SERVER, id =  "graficos11",
             minibase = MiniBase,
             batalla_naval = UserSelection$batalla_naval,
             decimales = decimales)
  
  callModule(module = TablasQC_SERVER, id =  "graficos12",
             minibase = MiniBase,
             batalla_naval = UserSelection$batalla_naval,
             decimales = decimales)
  
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
                    Graficos1C_UI(ns("graficos04")),
                    Graficos2Q_UI(ns("graficos05")),
                    Graficos2C_UI(ns("graficos06")),
                    GraficosQC_UI(ns("graficos07")),
               br(), br(), br(), br(), br(),
                    Tablas1Q_UI(ns("graficos08")),
                    Tablas1C_UI(ns("graficos09")),
                    Tablas2Q_UI(ns("graficos10")),
                    Tablas2C_UI(ns("graficos11")),
                    TablasQC_UI(ns("graficos12"))
        ),
        column(1)
      )
      
    ) # End TabPanel
    
    
    
    tabs
    
  })
  
  
  #Return del Modulo
  return(menuGRAFICOS)
}