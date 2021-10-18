## Segmento del UI
Tablas1C_UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("SeccionTablas1C"))
  # div(
  # uiOutput(ns("DinamicPanelTables")),
  # tableOutput(ns("Salida_tabla_1q_RMedic_01")),
  # tableOutput(ns("Salida_tabla_1q_RMedic_02")),
  # tableOutput(ns("Salida_tabla_1q_RMedic_03")),
  # tableOutput(ns("Salida_tabla_1q_RMedic_04")),
  # )
  
}




## Segmento del server
Tablas1C_SERVER <- function(input, output, session, 
                            minibase, 
                            batalla_naval,
                            decimales) {
  
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  # Caso 2: 1C
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
  Reactive_tabla_1c_RMedic <- reactive({
    
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 2) return(NULL)
    
    
    # salida <-  RMedic_1c_tablas(input_base = minibase(),
    #                             input_decimales = decimales(),
    #                             input_min = NULL,
    #                             input_max = NULL,
    #                             input_breaks = NULL,
    #                             input_side = NULL
    # )

    
    
    salida <-  RMedic_1c_tablas(input_base =  minibase(),
                              input_decimales = decimales(),
                              input_min = input$x_min,
                              input_max = input$x_max,
                              input_breaks = input$x_breaks,
                              input_side = input$x_side
                  )
                     
    # Return Exitoso
    return(salida)
    
    
  })  
  
  
  
  # Cantidad de tablas
  cantidad_tablas <- reactive({
    
    if(is.null(Reactive_tabla_1c_RMedic)) return(NULL)
    
    # Return Exitoso
    return(length(Reactive_tabla_1c_RMedic()))
  })
  
  # Create all renderTables!!!     
  observe(
    lapply(c(1:cantidad_tablas()), function(i) {
      
      nombre_fusion1 <- paste0('Salida_texto_1c_RMedic_', CifrasPerfectas(i))
      nombre_fusion2 <- paste0('Salida_tabla_1c_RMedic_', CifrasPerfectas(i))
      
      # El rotulo de cada tabla       
      output[[nombre_fusion1]] <- renderText({
        names(Reactive_tabla_1c_RMedic())[i]
      })
      
      # Cada tabla
      output[[nombre_fusion2]] <- renderTable(digits = decimales(), align= "c",{
        Reactive_tabla_1c_RMedic()[[i]]
      })
      
      
      
    })
  ) 
  
  
  # 09) Distribucion de Frecuencias
  observe( 
    output$Salida_tabla_1c_RMedic_09_especial <- renderTable(digits = decimales(), align= "c",{
      
      if(!is.null(Reactive_tabla_1c_RMedic())) {
        
    
        
        mi_tabla <-  RMedic_1c_tablas(input_base = minibase(),
                                      input_decimales = decimales(),
                                      input_min = input$ns("x_min"),
                                      input_max = input$ns("x_max"),
                                      input_breaks = input$ns("x_breaks"),
                                      input_side = input$ns("x_side")
        )[[9]]
        
        mi_tabla[,2] <- as.character(mi_tabla[,2])
        mi_tabla[,3] <- as.character(mi_tabla[,3])
        mi_tabla[,5] <- as.character(mi_tabla[,5])
        
        mi_tabla
        
      } else return(NULL)
    })
  )
  
  output$Controlador_1c_RMedic <- renderUI({
    
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 2) return(NULL)
    
    div(
      fluidRow(
        column(4,
               numericInput(
                 inputId = ns("x_min"),
                 label = "Valor mínimo: ",
                 value = min(minibase()[,1]),
                 min = NA,
                 max = min(minibase()[,1]),
                 step = 0.01,
                 width = NULL
               ),
               numericInput(
                 inputId = ns("x_max"),
                 label = "Valor máximo: ",
                 value = max(minibase()[,1]),
                 min = max(minibase()[,1]),
                 max = NA,
                 step = 0.01,
                 width = NULL
               )
        ),
        column(4,
               radioButtons(inputId = ns("x_side"), 
                            label = "Cierre del intervalo: ", choices = c("A la Derecha" = T , "A la Izquierda" = F)
               )
        ),
        column(4, 
               numericInput(
                 inputId = ns("x_breaks"),
                 label = "Cantidad de intervalos: ",
                 value = nclass.Sturges(minibase()[,1]),
                 min = 1,
                 max = NA,
                 step = 1,
                 width = NULL
               )
        )
      )
    )
    
  })
  
  
  
  # Variable criterio de inclusion
  observeEvent(input[[ns("x_min")]],{
    
    if(input[[ns("x_min")]] > min(BasePlaneta_tablas()[,1])) {
      
      updateNumericInput(session, inputId = ns("x_min"),
                         label = "Valor mínimo: ",
                         value = min(BasePlaneta_tablas()[,1]),
                         min = NA,
                         max = min(BasePlaneta_tablas()[,1]),
                         step = 0.01
      )
      
      
      
    }
  })
  
  
  # Variable criterio de inclusion
  observeEvent(input[[ns("x_max")]],{
    
    if(input[[ns("x_max")]] < max(BasePlaneta_tablas()[,1])) {
      
      updateNumericInput(session, inputId = ns("x_max"),
                         label = "Valor máximo: ",
                         value = max(BasePlaneta_tablas()[,1]),
                         min = max(BasePlaneta_tablas()[,1]),
                         max = NA,
                         step = 0.01
      )
      
      
      
    }
  })
  
  
  output$SeccionTablas1C <- renderUI({
    
    # Especificaciones de cumplimiento
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 2) return(NULL)
    
    # Si es el caso 1, seguimos!
    div(
      tabsetPanel(id = "Tablas_1c",
                  tabPanel("RMedic Help!", value = 1),
                  tabPanel("Medidas Resumen", value = 2,
                           h3(textOutput(ns("Salida_texto_1c_RMedic_01"))),
                           tableOutput(ns("Salida_tabla_1c_RMedic_01")),
                           br()),
                  tabPanel("Medidas de Posición", value = 3,
                           h3(textOutput(ns("Salida_texto_1c_RMedic_02"))),
                           tableOutput(ns("Salida_tabla_1c_RMedic_02")),
                           br(),
                           h3(textOutput(ns("Salida_texto_1c_RMedic_03"))),
                           tableOutput(ns("Salida_tabla_1c_RMedic_03")),
                           br(),
                           h3(textOutput(ns("Salida_texto_1c_RMedic_04"))),
                           tableOutput(ns("Salida_tabla_1c_RMedic_04")),
                           br(),
                           h3(textOutput(ns("Salida_texto_1c_RMedic_05"))),
                           tableOutput(ns("Salida_tabla_1c_RMedic_05")),
                           br()),
                  tabPanel("Medidas de Dispersión", value = 4,
                           h3(textOutput(ns("Salida_texto_1c_RMedic_06"))),
                           tableOutput(ns("Salida_tabla_1c_RMedic_06")),
                           br(),
                           h3(textOutput(ns("Salida_texto_1c_RMedic_07"))),
                           tableOutput(ns("Salida_tabla_1c_RMedic_07")),
                           br()),
                  tabPanel("Intervalos de Confianza", value = 5,
                           h3(textOutput(ns("Salida_texto_1c_RMedic_08"))),
                           tableOutput(ns("Salida_tabla_1c_RMedic_08")),
                           br()),
                  tabPanel("Distribución de Frecuencias", value = 6,
                           h3(textOutput(ns("Salida_texto_1c_RMedic_09"))),
                           tableOutput(ns("Salida_tabla_1c_RMedic_09")),
                           uiOutput(ns("Controlador_1c_RMedic")),
                           br(),
                           
                  )
      )
    )
  })
  
  
  
  
  
  
  
  
  
}


