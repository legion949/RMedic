


Ho1Q_02_TestDeProporciones_UI <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("armado_ho"))
  
  
}






## Segmento del server
Ho1Q_02_TestDeProporciones_SERVER <- function(input, output, session, 
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
      h3("Elecciones del usuario"),
      selectInput(inputId = ns("categoria_exito"), 
                  label = titulo_armado, 
                  choices = opciones_categorias),
      br(),


      # Seleccion del valor bajo H0
      numericInput(inputId = ns("valor_bajo_ho"),
                   label = "Valor bajo Hipótesis de proporción: ",
                   min=0,  max=1, step=0.01, value=0.50),
      br(),

      # Seleccion del tipo de prueba
      radioButtons(ns("tipo_prueba_ho"), "Tipo de Prueba de Hipótesis:",
                   choices = c("Bilateral" = "two.sided",
                               "Unilateral izquierda" = "less",
                               "Unilateral derecha" = "greater")
                   )
      
      
    )
    
    
    
  })
  
  # Test de Proporciones
  The_Test <- reactive({

   if(!control_interno01()) return(NULL)
    if(is.null(minibase())) return(NULL)
    if(is.null(input$categoria_exito)) return(NULL)
    if(is.null(input$tipo_prueba_ho)) return(NULL)
    if(is.null(input$tipo_prueba_ho)) return(NULL)
    if(is.null(input$valor_bajo_ho)) return(NULL)
    if(is.null(decimales())) return(NULL)
    if(is.null(alfa())) return(NULL)
    
  
    
    Test_1Q_Proporciones( input_base = minibase(),
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

    The_Test()$RESUMEN

  }))

  # Frase 1: Explicacion
  observe(output$frase01 <- renderUI({
    HTML(The_Test()$FRASE)
  }))

 

  
  # Armado/Salida del test de Proporciones 1Q
  output$armado_ho <- renderUI({
    
    div(
      uiOutput(ns("opciones_ho")),
      br(),
      h3("Test de Proporciones"), br(),
      #h3(span(uiOutput(ns("frase01_ho_1q_01")), style="color:red")),
      tableOutput(ns("tabla_resumen")),
      uiOutput(ns("frase01")),
      br(), br()
    )
    
  })
  
  
  
  
  
  
  
}


