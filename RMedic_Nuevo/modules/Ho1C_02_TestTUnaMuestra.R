


Ho1C_02_TestTUnaMuestra_UI <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("armado_ho"))
  
  
}






## Segmento del server
Ho1C_02_TestTUnaMuestra_SERVER <- function(input, output, session, 
                                                      minibase,
                                                      decimales,
                                                      control_ejecucion,
                                                      tablas_1c,
                                                      alfa) {
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  # Control interno 01
  control_interno01 <- reactive({
    
    if(is.null(control_ejecucion())) return(FALSE)
    else return(control_ejecucion())
  })
  
  
 
  
  ##################################################
  
  
  
  
  # # # # #
  # 1C - 01 - Test de Proporciones
  
  
  
  
  # Menu del opciones para el test de proporciones
  output$opciones_ho <- renderUI({
    
    
    div(
      # Seleccion de una categoria
      fluidRow(
        column(4,
               # Seleccion del valor bajo H0
               numericInput(inputId = ns("valor_bajo_ho"),
                            label = "Media poblacional (Valor esperado bajo hipótesis): ",
                            min = NA,  max = NA, step = 0.01, value = 0)
        ),
        column(4,
               
               # Seleccion del tipo de prueba
               radioButtons(ns("tipo_prueba_ho"), "Tipo de Prueba de Hipótesis:",
                            choices = c("Bilateral" = "two.sided",
                                        "Unilateral izquierda" = "less",
                                        "Unilateral derecha" = "greater")
               )
        )
        
        
      )
    )
    
    
    
  })
  
  # Test de Proporciones
  The_Test <- reactive({
    
    if(!control_interno01()) return(NULL)
    if(is.null(minibase())) return(NULL)
    if(is.null(input$tipo_prueba_ho)) return(NULL)
    if(is.null(input$valor_bajo_ho)) return(NULL)
    if(is.null(decimales())) return(NULL)
    if(is.null(alfa())) return(NULL)
    
    
    
    Test_1C_TestT_UnaMuestra( input_base = minibase(),
                              input_tipo_prueba = input$tipo_prueba_ho, 
                              input_media_ho = input$valor_bajo_ho,
                              input_decimales = decimales(),
                              input_alfa = alfa())
    
    
    
    
    
    
  })
  # #######################################################
  # 

  # Tabla Requisitos
  observe( output$tabla_resumen <- renderTable(rownames = FALSE, digits=decimales(), align = "c",{
    
    The_Test()$tabla_resumen
    
  }))
  
    
  # Tabla Resumen
  observe( output$tabla_requisitos <- renderTable(rownames = FALSE, digits=decimales(), align = "c",{
    
    The_Test()$tabla_requisitos
    
  }))
  
  # Frase 1: Sobre los requisitos
  observe(output$frase_requisitos <- renderUI({
    HTML(The_Test()$frase_requisitos)
  }))
  
  # Frase 2: Explicacion Estadistica
  observe(output$frase_estadistica <- renderUI({
    HTML(The_Test()$frase_estadistica)
  }))
  
  
  # Frase 3: Advertencia por redondeo
  observe(output$frase_redondeo <- renderUI({
    HTML(The_Test()$frase_redondeo)
  }))
  
  
  # Frase 4: Juego de Hipotesis
  observe(output$frase_juego_hipotesis <- renderUI({
    HTML(The_Test()$frase_juego_hipotesis)
  }))
  
  # Armado/Salida del test de Proporciones 1Q
  output$armado_ho <- renderUI({
    
    div(
      h2("Test t (una muestra)"),
      "Nota: para la utilización del test t (una muestra) la variable debe ser numérica y no debe ser 
      ordinal (cualitativa representada con números).", 
      br(),
      br(),
      h3("Elecciones del usuario"),
      uiOutput(ns("opciones_ho")),
      br(),
      # Mensaje de advertencia por redondeo
      span(htmlOutput(ns("frase_redondeo")), style="color:red"),
      br(),
      h3("Tabla de Requisitos test t (una muestra)"),
      tableOutput(ns("tabla_requisitos")),
      htmlOutput(ns("frase_requisitos")),
      br(),
      h3("Juego de Hipótesis"),
      htmlOutput(ns("frase_juego_hipotesis")),
      br(),
      h3("Tabla Resumen del test t (una muestra)"),
      tableOutput(ns("tabla_resumen")),
      br(),
      h3("Frases y conclusiones"),
      htmlOutput(ns("frase_estadistica")),
      br(), br()
    )
    
  })
  
  
  
  
  
  
  
}


