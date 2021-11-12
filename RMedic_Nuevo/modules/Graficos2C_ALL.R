## Segmento del UI
Graficos2C_UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("SeccionGraficos2C"))
  
}




## Segmento del server
Graficos2C_SERVER <- function(input, output, session, 
                              minibase, 
                              batalla_naval,
                              casoRMedic,
                              decimales) {
  
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  # Caso 4: 2C
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
  
  
  
  output$SeccionGraficos2C <- renderUI({
    
    # Especificaciones de cumplimiento
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 4) return(NULL)
    
    
    
    # Si es el caso 1, seguimos!
    
    div(
      h2("RMedic - Gráficos para 2 Variables Cuantitativas"),
      tabsetPanel(id = ns("Graficos_2c"),
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


