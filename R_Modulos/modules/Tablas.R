## Segmento del UI
TablasUI <- function(id) {
  ns <- NS(id)
  
  div(
  uiOutput(ns("DinamicPanelTables")),
  tableOutput(ns("Salida_tabla_1q_RMedic_01"))
  )
}




## Segmento del server
TablasSERVER <- function(input, output, session, 
                         minibase, 
                         batalla_naval,
                         decimales) {
  
  
  
  # casoRMedic <- reactive({
  #   if(is.null(batalla_naval())) return(NULL)
  #   if(is.null(batalla_naval()[[4]])) return(NULL)
  #   if(length(batalla_naval()[[4]]) == 0) return(NULL)
  #   if(batalla_naval()[[4]] == '') return(NULL)
  #   
  #   casoRMedic <- as.numeric(as.character(as.vector(batalla_naval()[[4]])))
  #   casoRMedic
  # })
  
  
  Reactive_tabla_1q_RMedic <- reactive({
    
    # if(is.null(batalla_naval())) return(NULL)
    # if(is.null(batalla_naval()[[4]])) return(NULL)
    # if(length(batalla_naval()[[4]]) == 0) return(NULL)
    # if(batalla_naval()[[4]] == '') return(NULL)
    # 
    # casoRMedic <- as.numeric(as.character(as.vector(batalla_naval()[[4]])))
    # 
    # if (casoRMedic != 1) return(NULL)
    # 
    
    salida <-  RMedic_1q_tablas(minibase(), decimales())
    salida[[1]][,2] <- as.character(salida[[1]][,2])
    salida[[1]][,3] <- as.character(salida[[1]][,3])
    
    # Return Exitoso
    return(salida)
    
    
  })  
  
  
  observe(
    output$Salida_tabla_1q_RMedic_01 <- renderTable(digits = input$decimales_tabla,
                                                    align= "c",{
                                                      
                     if(!is.null(Reactive_tabla_1q_RMedic())) {
        # Reactive_tabla_1q_RMedic()[[1]][[2]]
              Reactive_tabla_1q_RMedic()[[1]]
                                                        
                  } else return(NULL)
                                                    })
  )
  
  observe(
    output$Salida_tabla_1q_RMedic_02 <- renderTable(digits = input$decimales_tabla,
                                                    align= "c",{
                                                      
                                                      minibase()
                                                    })
  )
  
  
  
  
output$DinamicPanelTables <- renderUI({
  
  if(is.null(batalla_naval())) return(NULL)
  if(is.null(batalla_naval()[[4]])) return(NULL)
  if(length(batalla_naval()[[4]]) == 0) return(NULL)
  if(batalla_naval()[[4]] == '') return(NULL)
  
  casoRMedic <- as.numeric(as.character(as.vector(batalla_naval()[[4]])))
  
 

  if(casoRMedic == 1) {
  # # 1Q
  div(
    tabsetPanel(id = "Tablas_1q",
                tabPanel(title = "RMedic Help!", value = 1),
                tabPanel(title = "Distribución de Frecuencias", value = 2),
                tabPanel(title = "Intervalos de Confianza", value = 3)
    )
  )
   } else 
    if(casoRMedic == 2) {
  # 1C
  div(
    tabsetPanel(id = "Tablas_1c",
                tabPanel("RMedic Help!", value = 1),
                tabPanel("Medidas Resumen", value = 2),
                tabPanel("Medidas de Posición", value = 3),
                tabPanel("Medidas de Dispersión", value = 4),
                tabPanel("Distribución de Frecuencias", value = 5,
                         #uiOutput("Controlador_1c_RMedic")
                         br()
                         )
    )
  )
  
    } else 
      # 2 Q
      if(casoRMedic == 3) { 
        tabsetPanel(id= "Tablas_2q",
                    tabPanel("Clásico", value = 1),
                    tabPanel("Por filas", value = 3),
                    tabPanel("Por columnas", value = 4),
                    tabPanel("Al Total", value = 2),
                    tabPanel("Simple entrada", value = 5)
        )
        } else 
        if(casoRMedic == 4) {
          div(
            tabsetPanel(id = "Tablas_2c",
                        tabPanel("RMedic Help!", value = 1),
                        tabPanel("Medidas Resumen", value = 2),
                        tabPanel("Medidas de Posición", value = 3),
                        tabPanel("Medidas de Dispersión", value = 4),
                        tabPanel("Distribución de Frecuencias", value = 5,
                                 #uiOutput("Controlador_1c_RMedic")
                                 br()
                        )
            )
          )
        } else 
          if(casoRMedic == 5) {
            div(
              tabsetPanel(id = "Tablas_qc",
                          tabPanel("RMedic Help!", value = 1),
                          tabPanel("Medidas Resumen Particionadas", value = 2),
                          tabPanel("Medidas de Posición Particionadas", value = 3),
                          tabPanel("Medidas de Dispersión Particionadas", value = 4),
                          tabPanel("Distribución de Frecuencias Particionadas", value = 5,
                                   #uiOutput("Controlador_1c_RMedic")
                                   br()
                          )
              )
            )
          }
})


  
  
  
  
}


