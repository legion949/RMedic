## Segmento del UI
GraficosQC_UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("SeccionGraficosQC"))
  
}




## Segmento del server
GraficosQC_SERVER <- function(input, output, session, 
                              minibase, 
                              batalla_naval,
                              casoRMedic,
                              decimales) {
  
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  # Caso 5: QC
  casoRMedic <- reactive({
    
    if(is.null(batalla_naval())) return(NULL)
    if(is.null(batalla_naval()[[4]])) return(NULL)
    if(length(batalla_naval()[[4]]) == 0) return(NULL)
    if(batalla_naval()[[4]] == '') return(NULL)
    casoRMedic <- batalla_naval()[[4]]
    #casoRMedic <- as.numeric(as.character(as.vector(batalla_naval()[[4]])))
    casoRMedic
    
  })
  
  
  
  # DF_interna <-  reactive({RMedic_1q_tablas(minibase(), decimales())[[1]] })
  
  
  # callModule(module = Graficos1C_02_Barras_SERVER, id =  "graficos03B",
  #            minibase = minibase,
  #            batalla_naval = batalla_naval,
  #            decimales = decimales,
  #            casoRMedic = casoRMedic,
  #            DF_interna = DF_interna)
  # 
  # 
  # callModule(module = Graficos1Q_03_Tortas_SERVER, id =  "graficos03C",
  #            minibase = minibase,
  #            batalla_naval = batalla_naval,
  #            decimales = decimales,
  #            casoRMedic = casoRMedic,
  #            DF_interna = DF_interna)
  
  
  
  output$SeccionGraficosQC <- renderUI({
    
    # Especificaciones de cumplimiento
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 5) return(NULL)
    
    
    
    # Si es el caso 5, seguimos!
    
    div(
      h2("RMedic - Gráficos para 1 Variable Cuantitativa y 1 Variable Cualitativa"),
      tabsetPanel(id = ns("Graficos_qc"),
                  tabPanel(title = "RMedic Help!", value = 1),
                  tabPanel(title = "Media y Desvío Estándard", value = 2),
                  tabPanel(title = "Media y Error Estándard", value = 3),
                  tabPanel(title = "Boxplot", value = 4),
                  tabPanel(title = "Violín Plot", value = 5),
                  tabPanel(title = "Histograma", value = 6),
                  tabPanel(title = "Dispersión", value = 7),
                  tabPanel(title = "Puntos", value = 8),
                  tabPanel(title = "Distribución Bivariada", value = 9)
      )
    )
    
  })
  
  
  
  
  
  
}


