


HoQC_08_TestHomogeneidadDeVarianzasBartlett_UI <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("armado_ho"))
  
  
}






## Segmento del server
HoQC_08_TestHomogeneidadDeVarianzasBartlett_SERVER <- function(input, output, session, 
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
    
    
    
    Test_QC_TestHomogeneidadDeVarianzas_Bartlett(input_base = minibase(), 
                                                 input_decimales = decimales(), 
                                                 input_alfa = alfa())
    
    
    
    
    
  })
  # #######################################################
  
  
  # Tabla Resumen
  observe( output$tabla_resumen <- renderTable(rownames = FALSE, digits=decimales(), align = "c",{
    
    The_Test()$tabla_resumen
    
  }))
  
  
  # Tabla Requisitos
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
      h2("Test de Homogeneidad de Varianzas de Bartlett"),
      "Nota: para la utilización de Homogeneidad de Varianzas de Fisher la variable numérica no debe ser 
      ordinal (cualitativa representada con números).", 
      br(),
      br(),
      #  h3("Elecciones del usuario"),
      #  uiOutput(ns("opciones_ho")),
      br(),
      # Mensaje de advertencia por redondeo
      span(htmlOutput(ns("frase_redondeo")), style="color:red"),
      br(),
      h3("Tabla de Requisitos del test de homogeneidad de varianzas de Bartlett"),
      tableOutput(ns("tabla_requisitos")),
      htmlOutput(ns("frase_requisitos")),
      br(),
      h3("Juego de Hipótesis"),
      htmlOutput(ns("frase_juego_hipotesis")),
      br(),
      h3("Tabla Resumen del test de homogeneidad de varianzas de Bartlett"),
      tableOutput(ns("tabla_resumen")),
      br(),
      h3("Frases y conclusiones"),
      htmlOutput(ns("frase_estadistica")),
      br(), br()
    )
    
  })
  
  
  
  
  
  
  
}


