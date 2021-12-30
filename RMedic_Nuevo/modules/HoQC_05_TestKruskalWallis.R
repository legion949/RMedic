


HoQC_05_TestKruskalWallis_UI <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("armado_ho"))
  
  
}






## Segmento del server
HoQC_05_TestKruskalWallis_SERVER <- function(input, output, session, 
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
  # 1C - 03 - Test de Normalidad (Shapiro-Wilk)
  
  
  
  
  
  # Test de Proporciones
  The_Test <- reactive({
    
    if(!control_interno01()) return(NULL)
    if(is.null(minibase())) return(NULL)
    if(is.null(decimales())) return(NULL)
    if(is.null(alfa())) return(NULL)
    
    
    
    Test_QC_TestKruskalWallis(input_base = minibase(), 
                             input_decimales = decimales(), 
                             input_alfa = alfa())
    
    
    
    
    
  })
  # #######################################################
  
  
  # Tabla Resumen
  observe( output$tabla_resumen <- renderTable(rownames = FALSE, digits=decimales(), align = "c",{
    
    The_Test()$tabla_resumen
    
  }))
  
  
  # Tabla Comparacion1
  observe( output$tabla_comparacion1 <- renderTable(rownames = FALSE, digits=decimales(), align = "c",{
    
    The_Test()$tabla_comparacion1
    
  }))
  
  
  # Tabla Comparacion2
  observe( output$tabla_comparacion2 <- renderTable(rownames = FALSE, digits=decimales(), align = "c",{
    
    The_Test()$tabla_comparacion2
    
  }))
  
 

  # Tabla Comparacion3
  observe( output$tabla_comparacion3 <- renderTable(rownames = FALSE, digits=decimales(), align = "c",{
    
    The_Test()$tabla_comparacion3
    
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
      h2("Test Kruskal-Wallis"),
      "Nota: para la utilización del test de Kruskal-Wallis la variable numérica puede ser 
      cuantitativa u ordinal representada con números y la variable categórica debe contener al 
      menos dos categorías.", 
      br(),
      br(),
      #  h3("Elecciones del usuario"),
      #  uiOutput(ns("opciones_ho")),
      br(),
      # Mensaje de advertencia por redondeo
      span(htmlOutput(ns("frase_redondeo")), style="color:red"),
      br(),
      br(),
      h3("Tabla Resumen del Test Kruskal-Wallis"),
      tableOutput(ns("tabla_resumen")),
      br(),
      br(),
      h3("Juego de Hipótesis"),
      htmlOutput(ns("frase_juego_hipotesis")),
      br(),
      br(),
      h3("Frases y conclusiones"),
      htmlOutput(ns("frase_estadistica")),
      br(),
      br(),
      h3("Tabla de comparación 1"),
      tableOutput(ns("tabla_comparacion1")),
                 "Letras iguales en 'Grupo Estadístico' indica que las categorías son 
                 estadísticamente iguales.",br(),
                 "Letras diferentes en 'Grupo Estadístico' indica que las categorías son 
                 estadísticamente diferentes.",
                 br(),
                 br(),
      h3("Tabla de comparación 2"),
      tableOutput(ns("tabla_comparacion2")),
                 br(),
                 br(),
      h3("Tabla de comparación 3"),
      tableOutput(ns("tabla_comparacion3"))
    )
    
    
  })
  
  
  
  
  
  
  
}


