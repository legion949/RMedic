


HoQC_04_TestAnova1Factor_UI <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("armado_ho"))
  
  
}






## Segmento del server
HoQC_04_TestAnova1Factor_SERVER <- function(input, output, session, 
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
    
    
    
    Test_QC_TestAnova1Factor(input_base = minibase(), 
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
  
  
  # Tabla normalidad residuos
  observe( output$tabla_normalidad_residuos <- renderTable(rownames = FALSE, digits=decimales(), align = "c",{
    
    The_Test()$tabla_normalidad_residuos
    
  }))
  
  
  # Tabla homogeneidad residuos
  observe( output$tabla_homogeneidad_residuos <- renderTable(rownames = FALSE, digits=decimales(), align = "c",{
    
    The_Test()$tabla_homogeneidad_residuos
    
  }))
  
  # Tabla Requisitos
  observe( output$tabla_anova <- renderTable(rownames = FALSE, digits=decimales(), align = "c",{
    
    The_Test()$tabla_anova
    
  }))
  
  
  # Tabla Tukey1
  observe( output$tabla_tukey1 <- renderTable(rownames = FALSE, digits=decimales(), align = "c",{
    
    The_Test()$tabla_tukey1
    
  }))
  
  # Tabla Tukey2
  observe( output$tabla_tukey2 <- renderTable(rownames = FALSE, digits=decimales(), align = "c",{
    
    The_Test()$tabla_tukey2
    
  }))
  
  
  # Tabla Tukey3
  observe( output$tabla_tukey3 <- renderTable(rownames = FALSE, digits=decimales(), align = "c",{
    
    The_Test()$tabla_tukey3
    
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
      h2("Test Anova a 1 Factor"),
      "Nota: para la utilización del test Anova a 1 Factor la variable numérica no debe ser 
      ordinal (cualitativa representada con números) y la variable categórica debe contener al 
      menos dos categorías.", 
      br(),
      br(),
      #  h3("Elecciones del usuario"),
      #  uiOutput(ns("opciones_ho")),
      br(),
      # Mensaje de advertencia por redondeo
      span(htmlOutput(ns("frase_redondeo")), style="color:red"),
      br(),
      h3("Tabla de Requisitos del test Anova a 1 Factor"),
      tableOutput(ns("tabla_requisitos")),
      htmlOutput(ns("frase_requisitos")),
      br(),
      br(),
      h3("Juego de Hipótesis"),
      htmlOutput(ns("frase_juego_hipotesis")),
      br(),
      tabsetPanel(
        tabPanel("Residuos",
                 h3("Tabla de Requisito de Normalidad de los residuos"),
                 tableOutput(ns("tabla_normalidad_residuos")),
                 br(),
                 h3("Tabla de Requisito de Homogeneidad de varianzas de los residuos"),
                 tableOutput(ns("tabla_homogeneidad_residuos"))),
        tabPanel("Tabla Anova 1 Factor", 
                 h3("Tabla Resumen del test Anova a 1 Factor"),
                 tableOutput(ns("tabla_resumen")),
                 br(),
                 h3("Frases y conclusiones"),
                 br(),
                 htmlOutput(ns("frase_estadistica")),
                 h3("Tabla Anova"),
                 tableOutput(ns("tabla_anova"))),
        tabPanel("Test de Tukey",
                 h3("Tabla Tukey 1"),
                 tableOutput(ns("tabla_tukey1")),
                 br(),
                 br(),
                 h3("Tabla Tukey 2"),
                 tableOutput(ns("tabla_tukey2")),
                 br(),
                 br(),
                 h3("Tabla Tukey 3"),
                 tableOutput(ns("tabla_tukey3")))
      )
    )
    
  })
  
  
  
  
  
  
  
}


