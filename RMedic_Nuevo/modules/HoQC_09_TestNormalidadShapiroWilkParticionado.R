


HoQC_09_TestNormalidadShapiroWilkParticionado_UI <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("armado_ho"))
  
  
}






## Segmento del server
HoQC_09_TestNormalidadShapiroWilkParticionado_SERVER <- function(input, output, session, 
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
    
    
    
    Test_QC_TestNormalidad_ShapiroWilk_Particionado( input_base = minibase(),
                                        input_decimales = decimales(),
                                        input_alfa = alfa())
    
    
    
    
    
  })
  # #######################################################
  # 
  # Salida de tabla resumen del test de Proporciones 1Q
  observe( output$tabla_resumen <- renderTable(rownames = FALSE, 
                                               digits=decimales(), align = "c",{
                                                 
                                                 The_Test()$tabla_resumen
                                                 
                                               }))
  
  # Frase 1: Explicacion
  observe(output$frase01 <- renderUI({
    HTML(The_Test()$frase_estadistica)
  }))
  
  

  
  
  # Frase 3: Juego de Hipotesis
  observe(output$frase03 <- renderUI({
    HTML(The_Test()$frase_juego_hipotesis)
  }))
  
  # Armado/Salida del test de Proporciones 1Q
  output$armado_ho <- renderUI({
    
    div(
      h2("Test de Normalidad (Shapiro-Wilk Particionado)"),
      "Nota: para la utilización de Normalidad (Shapiro-Wilk) la variable debe ser numérica y no debe ser 
      ordinal (cualitativa representada con números).", 
      # Mensaje de advertencia por redondeo
      # br(),
      # span(htmlOutput(ns("frase02")), style="color:red"),
      br(),
      br(),
      h3("Juego de Hipótesis"),
      "Se lleva a cabo un test de normalidad sobre cada una de las categorías de la variable cualitativa.", br(),
      "Para cada categoría se generará un test de Normalidad de Shapiro-Wilk, obteniendo de cada uno un valor p y 
      una decisión.", br(),
      br(),
      htmlOutput(ns("frase03")),
      br(),
      br(),
      h3("Tabla Resumen del Test de Normalidad (Shapiro-Wilk)"),
      tableOutput(ns("tabla_resumen")),
      br(),
      # h3("Frases y conclusiones"),
      # htmlOutput(ns("frase01")),
      br(), br()
    )
    
  })
  
  
  
  
  
  
  
}


