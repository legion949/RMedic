


HoQC_03_TestWilcoxonDosMuestrasIndependientes_UI <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("armado_ho"))
  
  
}






## Segmento del server
HoQC_03_TestWilcoxonDosMuestrasIndependientes_SERVER <- function(input, output, session, 
                                         minibase, 
                                         decimales,
                                         control_ejecucion,
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
  # # # # ESTO LO DEJE EN EL SCRIPT PERO ESTA INACTIVO!!!!!!
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
    # if(is.null(input$tipo_prueba_ho)) return(NULL)
    # if(is.null(input$valor_bajo_ho)) return(NULL)
    if(is.null(decimales())) return(NULL)
    if(is.null(alfa())) return(NULL)
    
    
    
    Test_QC_TestWilcoxon_DosMuestras_Independientes( input_base = minibase(),
                                        input_tipo_prueba = "two.sided", 
                                        input_mediana_ho = 0,
                                        input_decimales = decimales(),
                                        input_alfa = alfa())
    
    
    
    
    
    
  })
  # #######################################################
  # 
  
  # Tabla Resumen
  observe( output$tabla_resumen <- renderTable(rownames = FALSE, digits=decimales(), align = "c",{
    
    The_Test()$tabla_resumen
    
  }))
  
  
  
  # Frase 2: Explicacion Estadistica
  observe(output$frase_estadistica <- renderUI({
    HTML(The_Test()$frase_estadistica)
  }))
  
  
  # Frase 3: Advertencia por redondeo
  observe(output$frase_redondeo <- renderUI({
    HTML(The_Test()$frase_redondeo)
  }))
  
  
  # Frase 4: Juego de Hipotesis1
  observe(output$frase_juego_hipotesis <- renderUI({
    HTML(The_Test()$frase_juego_hipotesis)
  }))
  
  # # Frase 5: Juego de Hipotesis2
  # observe(output$frase_juego_hipotesis2 <- renderUI({
  #   HTML(The_Test()$frase_juego_hipotesis2)
  # }))
  
  # Armado/Salida del test de Proporciones 1Q
  output$armado_ho <- renderUI({
    
    div(
      h2("Test Mann-Whitney-Wilcoxon (Dos muestras independientes)"),
      br(),
      # h3("Elecciones del usuario"),
      # uiOutput(ns("opciones_ho")),
      # br(),
      # br(),
      # Mensaje de advertencia por redondeo
      span(htmlOutput(ns("frase_redondeo")), style="color:red"),
      br(),
      h3("Juego de Hipótesis"),
  #    "Forma 1 de 2:",
  #    htmlOutput(ns("frase_juego_hipotesis1")),
      br(),
  #    "Forma 2 de 2:",
      htmlOutput(ns("frase_juego_hipotesis")),
      br(),
      br(),
      h3("Tabla Resumen del test de Mann-Whitney-Wilcoxon (Dos muestras independientes)"),
      tableOutput(ns("tabla_resumen")),
      br(),
      h3("Frases y conclusiones"),
      htmlOutput(ns("frase_estadistica")),
      br(), br()
    )
    
  })
  
  
  
  
  
  
  
}


