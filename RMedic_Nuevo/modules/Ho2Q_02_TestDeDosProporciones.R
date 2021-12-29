


Ho2Q_02_TestDeDosProporciones_UI <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("armado_ho"))
  
  
}






## Segmento del server
Ho2Q_02_TestDeDosProporciones_SERVER <- function(input, output, session, 
                                        minibase,
                                        decimales,
                                        control_ejecucion,
                                        tablas_2q,
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
  
  cambio_orden_columnas <- reactive({
    
    dt_col <- colnames(minibase())[1] == input$divisor_grupos
    
    if(dt_col) el_orden <- c(1,2) else el_orden <- c(2,1)
    
    return(el_orden)
    
  })
  
  
  # Variable criterio de inclusion
  observeEvent(input$divisor_grupos,{
    
    the_var <- colnames(minibase()) == input$divisor_grupos
    
    la_otra <- colnames(minibase())[!the_var]
    
    titulo2 <- paste("Éxito - Variable 2 - '", la_otra, "'", sep="")
    opciones_categorias2 <- levels(as.factor(minibase()[,la_otra]))
    
    
    freezeReactiveValue(input, "categoria_exito")
    updateSelectInput(session,
                       inputId = "categoria_exito", 
                       label = titulo2, 
                       choices = opciones_categorias2)
    })
  

  # Menu del opciones para el test de proporciones
  output$opciones_ho <- renderUI({
    
    titulo1 <- paste("Divisor de Grupos", sep="")
    opciones_categorias1 <- colnames(minibase()) 
   

    titulo2 <- paste("Éxito - Variable 2 - '", colnames(minibase())[2], "'", sep="")
    opciones_categorias2 <- levels(as.factor(minibase()[,2]))
    
    
    
    div(
      # Seleccion de una categoria
      fluidRow(
        column(4,
               selectInput(inputId = ns("divisor_grupos"), 
                           label = titulo1, 
                           choices = opciones_categorias1
               )
        ),
        
        
        column(4,
               # Seleccion del valor bajo H0
               selectInput(inputId = ns("categoria_exito"), 
                           label = titulo2, 
                           choices = opciones_categorias2)
        ),
        column(4,
               
               # # Seleccion del tipo de prueba
               # radioButtons(ns("tipo_prueba_ho"), "Tipo de Prueba de Hipótesis:",
               #              choices = c("Bilateral" = "two.sided",
               #                          "Unilateral izquierda" = "less",
               #                          "Unilateral derecha" = "greater")
               )
        )
        
        
      
    )
    
    
    
  })
  

  # Test de Proporciones
  The_Test <- reactive({
    
    if(!control_interno01()) return(NULL)
    if(is.null(minibase())) return(NULL)
    if(is.null(input$categoria_exito)) return(NULL)
    if(is.null(decimales())) return(NULL)
    if(is.null(alfa())) return(NULL)
    if(is.null(cambio_orden_columnas())) return(NULL)
    
    
    Test_2Q_TestDeDosProporciones_Fisher(input_base = minibase()[cambio_orden_columnas()],
                                 input_categoria_exito = input$categoria_exito,
                                 input_decimales = decimales(),
                                 input_alfa = alfa())#input$alfa_ho)
    
    
    
    
    
    
  })
  # #######################################################
  # 
  # Salida de tabla resumen del test de Proporciones 1Q
  observe( output$tabla_resumen <- renderTable(rownames = FALSE, digits=decimales(), align = "c",{
    
    The_Test()$tabla_resumen
    
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
      h2("Test de Dos proporciones de Fisher"),
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
      h3("Tabla Resumen del Test de dos proporciones de Fisher"),
      tableOutput(ns("tabla_resumen")),
      br(),
      br(),
      h3("Frases y conclusiones"),
      htmlOutput(ns("frase01")),
      br(), br()
    )
    
  })
  
  
  
  
  
  
  
}


