


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
  
  
  # Tabla LSD1
  observe( output$tabla_lsd1 <- renderTable(rownames = FALSE, digits=decimales(), align = "c",{
    
    The_Test()$tabla_lsd1
    
  }))
  
  
  # Tabla LSD2
  observe( output$tabla_lsd2 <- renderTable(rownames = FALSE, digits=decimales(), align = "c",{
    
    The_Test()$tabla_lsd2
    
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
  
  
  output$tabla_seleccionada <- renderUI({
    
    if(is.null(input$test_comparacion)) return(NULL)
    if(is.null(input$tipo_tabla)) return(NULL)
    
    if(input$test_comparacion == 1 && input$tipo_tabla == 1){
      div(
        h3("Test de Tukey (Lista Descendente)"),
        tableOutput(ns("tabla_tukey1")),
        "Los niveles del factor se encuentran ordenados a partir de sus valores de media, 
                 de mayor a menor.", br(),
        "Letras iguales en 'Grupo Estadístico' indica que las categorías son 
                 estadísticamente iguales.",br(),
        "Letras diferentes en 'Grupo Estadístico' indica que las categorías son 
                 estadísticamente diferentes."
      )
    } else 
      if(input$test_comparacion == 1 && input$tipo_tabla == 2){ 
        div(
          h3("Test de Tukey (Lista Matricial)"),
          tableOutput(ns("tabla_tukey3"))
        )
        } else
          if(input$test_comparacion == 2 && input$tipo_tabla == 1){
            div(
              h3("Test LSD (Lista Descendente)"),
              tableOutput(ns("tabla_lsd1")),
              "Los niveles del factor se encuentran ordenados a partir de sus valores de media, 
                 de mayor a menor.", br(),
              "Letras iguales en 'Grupo Estadístico' indica que las categorías son 
                 estadísticamente iguales.",br(),
              "Letras diferentes en 'Grupo Estadístico' indica que las categorías son 
                 estadísticamente diferentes."
            )
          } else 
            if(input$test_comparacion == 2 && input$tipo_tabla == 2){ 
              div(
                h3("Test LSD (Lista Matricial)"),
                tableOutput(ns("tabla_lsd2"))
              )
            }
    
     
     
    
    
    
    
    
  })
  
  # Armado/Salida del test de Proporciones 1Q
  output$armado_ho <- renderUI({
    
    div(
      br(),
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
      br(),
      tabsetPanel(
        tabPanel("Tabla Anova 1 Factor", 
                 br(),
                 h3("Tabla Resumen del test Anova a 1 Factor"),
                 tableOutput(ns("tabla_resumen")),
                 br(),
                 br(),
                 h3("Frases y conclusiones"),
                 htmlOutput(ns("frase_estadistica")),
                 br(),
                 br(),
                 h3("Tabla Anova"),
                 tableOutput(ns("tabla_anova"))),
        tabPanel("Test de Comparaciones",
                 br(),
                 fluidRow(
                   column(4,
                 radioButtons(inputId = ns("test_comparacion"),
                              label = "Test de comparaciones múltiples",
                              choices = c("Tukey" = 1,
                                          "LSD" = 2)
                              )
                 ),
                 column(4,
                 radioButtons(inputId = ns("tipo_tabla"),
                              label = "Tipo de tabla",
                              choices = c("Lista Descendente" = 1,
                                          "Lista Matricial" = 2)
                 )
                 )
                 ),
                 
                 tableOutput(ns("tabla_seleccionada"))
                 ),
                 
        tabPanel("Residuos",
                 h3("Tabla de Requisito de Normalidad de los residuos"),
                 tableOutput(ns("tabla_normalidad_residuos")),
                 br(),
                 h3("Tabla de Requisito de Homogeneidad de varianzas de los residuos"),
                 tableOutput(ns("tabla_homogeneidad_residuos")))
      )
    )
    
  })
  
  
  
  
  
  
  
}


