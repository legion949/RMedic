## Segmento del UI
Graficos2Q_UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("SeccionGraficos2Q"))
  
}




## Segmento del server
Graficos2Q_SERVER <- function(input, output, session, 
                              minibase, 
                              batalla_naval,
                              casoRMedic,
                              decimales) {
  
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  # Caso 3: 2Q
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
  
  
  
  output$SeccionGraficos2Q <- renderUI({
    
    # Especificaciones de cumplimiento
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 3) return(NULL)
    
    
    
    # Si es el caso 3, seguimos!
    
    div(
      h2("RMedic - Gráficos para w Variables Cualitativas"),
      tabsetPanel(id = ns("Graficos_2q"),
                  tabPanel(title = "RMedic Help!", value = 1),
                  tabPanel(title = "Gráfico de Barras", value = 2),
                  tabPanel(title = "Gráfico 3D", value = 3)
      )
    )
    
  })
  
  
  
  
  
  
}


