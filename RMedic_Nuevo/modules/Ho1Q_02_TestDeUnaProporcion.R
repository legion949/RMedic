


Ho1Q_02_TestDeUnaProporcion_UI <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("armado_ho"))
  
  
}






## Segmento del server
Ho1Q_02_TestDeUnaProporcion_SERVER <- function(input, output, session, 
                                            minibase,
                                            decimales,
                                            control_ejecucion,
                                            tablas_1q,
                                            alfa) {
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  # Control interno 01
  control_interno01 <- reactive({
    
    if(is.null(control_ejecucion())) return(FALSE)
    else return(control_ejecucion())
  })
  
  
  # Cantidad de categorias
  cantidad_categorias <- reactive({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    return(nrow(tablas_1q()))

  })
  
  
##################################################



  
  # # # # #
  # 1Q - 01 - Test de Proporciones
  
  
  
  
  # Menu del opciones para el test de proporciones
  output$opciones_ho <- renderUI({
    
    
    titulo_armado <- paste("Categoría de 'Éxito' de la variable '", colnames(minibase())[1], "'", sep="")
    opciones_categorias <- levels(as.factor(minibase()[,1]))
    
    div(
      # Seleccion de una categoria
      fluidRow(
        column(4,
      selectInput(inputId = ns("categoria_exito"), 
                  label = titulo_armado, 
                  choices = opciones_categorias)
      ),
  

      column(4,
      # Seleccion del valor bajo H0
      numericInput(inputId = ns("valor_bajo_ho"),
                   label = "Valor bajo Hipótesis de proporción: ",
                   min=0,  max=1, step=0.01, value=0.50)
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
    if(is.null(input$categoria_exito)) return(NULL)
    if(is.null(input$tipo_prueba_ho)) return(NULL)
    if(is.null(input$valor_bajo_ho)) return(NULL)
    if(is.null(decimales())) return(NULL)
    if(is.null(alfa())) return(NULL)
    
  
    
    Test_1Q_TestDeUnaProporcion( input_base = minibase(),
                          input_categoria_exito = input$categoria_exito,
                          input_tipo_prueba = input$tipo_prueba_ho, 
                          input_prop_ho = input$valor_bajo_ho,
                          input_decimales = decimales(),
                          input_alfa = alfa())#input$alfa_ho)

    




  })
  # #######################################################
  # 
  # Salida de tabla resumen del test de Proporciones 1Q
  observe( output$tabla_resumen <- renderTable(rownames = FALSE, digits=decimales(), align = "c",{

    The_Test()$resumen

  }))

  # Frase 1: Explicacion
  observe(output$frase01 <- renderUI({
    HTML(The_Test()$frase_estadistica)
  }))

 
  # Frase 2: Advertencia por redondeo
  observe(output$frase02 <- renderUI({
    HTML(The_Test()$frase_redondeo)
  }))
  
  
  # Frase 3: Juego de Hipotesis
  observe(output$frase03 <- renderUI({
    HTML(The_Test()$frase_juego_hipotesis)
  }))
  
  # Armado/Salida del test de Proporciones 1Q
  output$armado_ho <- renderUI({
    
    div(
      h2("Test de una proporción"),
      br(),
      h3("Elecciones del usuario"),
      uiOutput(ns("opciones_ho")),
      br(),
      # Mensaje de advertencia por redondeo
      span(htmlOutput(ns("frase02")), style="color:red"),
      br(),
      h3("Juego de Hipótesis"),
      htmlOutput(ns("frase03")),
      br(),
      br(),
      h3("Tabla Resumen del Test de una proporción"),
      tableOutput(ns("tabla_resumen")),
      br(),
      br(),
      h3("Frases y conclusiones"),
      htmlOutput(ns("frase01")),
      br(), br()
    )
    
  })
  
  
  
  
  
  
  
}


