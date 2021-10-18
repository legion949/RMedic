## Segmento del UI
Tablas1Q_UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("SeccionTablas1Q"))
  # div(
  # uiOutput(ns("DinamicPanelTables")),
  # tableOutput(ns("Salida_tabla_1q_RMedic_01")),
  # tableOutput(ns("Salida_tabla_1q_RMedic_02")),
  # tableOutput(ns("Salida_tabla_1q_RMedic_03")),
  # tableOutput(ns("Salida_tabla_1q_RMedic_04")),
  # )
 
}




## Segmento del server
Tablas1Q_SERVER <- function(input, output, session, 
                         minibase, 
                         batalla_naval,
                         decimales) {
  
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  # Caso 1: 1Q
  casoRMedic <- reactive({

    if(is.null(batalla_naval())) return(NULL)
    if(is.null(batalla_naval()[[4]])) return(NULL)
    if(length(batalla_naval()[[4]]) == 0) return(NULL)
    if(batalla_naval()[[4]] == '') return(NULL)
    casoRMedic <- batalla_naval()[[4]]
    #casoRMedic <- as.numeric(as.character(as.vector(batalla_naval()[[4]])))
    casoRMedic

  })
  
  # Todas las tablas 1Q
  Reactive_tabla_1q_RMedic <- reactive({
    
    if(is.null(casoRMedic())) return(NULL)
    
   
    
    salida <-  RMedic_1q_tablas(minibase(), decimales())
    salida[[1]][,2] <- as.character(salida[[1]][,2])
    salida[[1]][,3] <- as.character(salida[[1]][,3])
    
    # Return Exitoso
    return(salida)
    
    
  })  
  
  

    # Cantidad de tablas
   cantidad_tablas <- reactive({
     
     if(is.null(Reactive_tabla_1q_RMedic)) return(NULL)
     
      # Return Exitoso
      return(length(Reactive_tabla_1q_RMedic()))
   })
   
   # Create all renderTables!!!     
   observe(
          lapply(c(1:cantidad_tablas()), function(i) {
            
    nombre_fusion1 <- paste0('Salida_texto_1q_RMedic_', CifrasPerfectas(i))
    nombre_fusion2 <- paste0('Salida_tabla_1q_RMedic_', CifrasPerfectas(i))
    
    # El rotulo de cada tabla       
    output[[nombre_fusion1]] <- renderText({
      names(Reactive_tabla_1q_RMedic())[i]
    })
    
    # Cada tabla
    output[[nombre_fusion2]] <- renderTable(digits = decimales(), align= "c",{
      Reactive_tabla_1q_RMedic()[[i]]
    })
            
           
            
           })
   ) 
   
        
        
     
    
   output$SeccionTablas1Q <- renderUI({
     
     # Especificaciones de cumplimiento
     if(is.null(casoRMedic())) return(NULL)
     if(casoRMedic() != 1) return(NULL)
     
     # Si es el caso 1, seguimos!
     div(
       tabsetPanel(id = ns("Tablas_1q"),
                   tabPanel(title = "RMedic Help!", value = 1),
                   tabPanel(title = "Distribución de Frecuencias", value = 2,
                            h3(textOutput(ns("Salida_texto_1q_RMedic_01"))),
                            tableOutput(ns("Salida_tabla_1q_RMedic_01")),
                            br()),
                   tabPanel(title = "Intervalos de Confianza", value = 3,
                            h3(textOutput(ns("Salida_texto_1q_RMedic_02"))),
                            tableOutput(ns("Salida_tabla_1q_RMedic_02")),
                            br(),
                            h3(textOutput(ns("Salida_texto_1q_RMedic_03"))),
                            tableOutput(ns("Salida_tabla_1q_RMedic_03")),
                            br(),
                            h3(textOutput(ns("Salida_texto_1q_RMedic_04"))),
                            tableOutput(ns("Salida_tabla_1q_RMedic_04")),
                            br())
       ),
     )
   })
 
  
 
  
  
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


