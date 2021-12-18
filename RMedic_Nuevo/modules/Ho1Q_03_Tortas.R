


Ho1Q_03_Tortas_UI <- function(id) {
  
  ns <- NS(id)
  
  div(
     h2("Gráfico de Tortas"),
    
    fluidRow(
      column(8,
             plotOutput(ns("grafico01"))
      ),
      column(4,
             bsButton(inputId = ns("controlador01"), 
                      label = "Mostrar/Ocultar opciones gráficas",
                      icon = icon("bars"), 
                      style = "primary",
                      size = "large",
                      type = "toggle", 
                      value = TRUE
                      , 
             ),br(),br(),
             div(id = ns("James01"),
                 conditionalPanel(condition = "input.controlador01", ns = ns, 
                                  uiOutput(ns("menu_general01"))
                 )
                 )
    )
    
    )
  )
  

  
  
  
}






## Segmento del server
Ho1Q_03_Tortas_SERVER <- function(input, output, session, 
                                        minibase,
                                        decimales,
                                        control_ejecucion,
                                        tablas_1q) {
  
  # Control interno 01
  control_interno01 <- reactive({
    
    if(is.null(control_ejecucion())) return(FALSE)
    else return(control_ejecucion())
  })
  

  # NameSpaceasing for the session
  ns <- session$ns




  # Cantidad de categorias
  cantidad_categorias <- reactive({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    return(nrow(tablas_1q()))
    
  })
  
  
  # Objetos reactivos de contorl
  reseteo_logico <- reactiveVal(F)
  aplicador_logico <- reactiveVal(F)
  
  
  observeEvent(minibase(), {
    
    reseteo_logico(!reseteo_logico())
    delay(400,     aplicador_logico(!aplicador_logico()))
    # aplicador_logico(!aplicador_logico())
    
  })
  
  # # Reseteo
  observeEvent(input$reset, {
    
    
    
    # Damos la orden de setear!
    reseteo_logico(!reseteo_logico())
    
    
    # Luego de un delay de 400 milisegundos la pagina tomara los
    # valores reseteados para generarla nuevamente
    delay(400,     aplicador_logico(!aplicador_logico()))
  })
  
  
  
  
  # Variable criterio de inclusion
  observeEvent(reseteo_logico(),{
    
    
    reset("coronacion", asis = F)
   
    
   
   
    
    
  })
  
  
  # Variable criterio de inclusion
  observeEvent(reseteo_logico(),{
    

    if(is.null(cantidad_categorias())) return(NULL)
    
    cantidad <- cantidad_categorias()
    colores_internos <- rainbow(cantidad)
    label_armado <- paste0("Categoría '", as.vector(tablas_1q()[,1]), "'")
    
    lapply(1:cantidad, function(i) {
      
      nombre_input <- paste("col", i, sep="_")
   
        
        freezeReactiveValue(input, nombre_input)
        colourpicker::updateColourInput(session,
                                        inputId = nombre_input,
                                        label = label_armado[i],
                                        value = colores_internos[i])
        
        
       
 
      
    })
    
    
   # delay(400,     aplicador_logico(!aplicador_logico()))
    
  })
  
  
  
  # Variable criterio de inclusion
  observeEvent(input$controlador02,{
    
    
    aplicador_logico(!aplicador_logico())
    
    
    
    
    
    
  })

  
  # Corona para la torta
  corona <- reactive({

    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    la_corona <- list()

    la_suma <- sum(as.numeric(as.character(input$coronacion)))


    la_nada <- rep("", cantidad_categorias())
    la_categoria <- as.character(as.vector(tablas_1q()[,1]))
    la_fa <- as.character(as.vector(tablas_1q()[,2]))
    la_porcentaje <- as.character(as.vector(tablas_1q()[,6]))
    la_fusion <- as.character(as.vector(tablas_1q()[,7]))

    if(is.null(input$coronacion)) {
      la_corona[[1]] <- la_categoria

    } else
      if(length(input$coronacion) == 0) {

        la_corona[[1]] <- la_nada

      } else
        if(la_suma == 0) {

          la_corona[[1]] <- la_nada

        } else
        if(la_suma == 1) {

            la_corona[[1]] <-  paste0(la_categoria, " (", la_fa, ")")
          } else
            if(la_suma == 10) {

              la_corona[[1]] <- paste0(la_categoria, " (", la_porcentaje, ")")
            } else
              if(la_suma == 11) {

                la_corona[[1]] <- paste0(la_categoria, " (", la_fa, " ; ",la_porcentaje ,")")
              }

    return(la_corona)
  })




  # Colores seleccionados
  # Evento reactivo asociado al controlador02
  colores_seleccionados <-  eventReactive(aplicador_logico(),({

    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    cantidad <- cantidad_categorias()
    mis_colores <- rep(NA, cantidad)

    for(i in 1:cantidad){
      nombre_input <- paste("col", i, sep="_")

      if(is.null(input[[nombre_input]])) return(NULL)

      mis_colores[i] <- input[[nombre_input]]

    }

    # Return Exitoso
    return(mis_colores)

  }),ignoreNULL  = FALSE)



  # Generador de un color para cada categoria
  output$MODcolor <- renderUI({

    # Control interno 01
    if(!control_interno01()) return(NULL)




    cantidad <- c()
    cantidad <- nrow(tablas_1q())
    label_armado <- paste0("Categoría '", as.vector(tablas_1q()[,1]), "'")



     colores_internos <- rainbow(cantidad)


    lapply(1:cantidad, function(i) {

      nombre_input <- paste("col", i, sep="_")
      div(
        colourpicker::colourInput(inputId = ns(nombre_input),
                                  label = label_armado[i],
                                  value = colores_internos[i]), br()
      )

    })

  })


  # Controlador General
  output$menu_general01 <- renderUI({

    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    div(
        # Coronacion de las porciones de la torta
        checkboxGroupInput( inputId = ns("coronacion"),
                            label = h3("Agregar al gráfico de tortas..."),
                            choices = c("Frecuencias Absolutas" = 1,
                                      "Porcentaje " = 10),
                            width = "100%"
      ),
      br(),

      # Todos los colores...
      uiOutput(ns("MODcolor")),
      br(),

      # Controlador02
      bsButton(inputId = ns("controlador02"),
               label = "Aplicar cambio de color",
               icon = icon("bars"),
               type = "toggle",
               value = TRUE,
               style = "primary",
               size = "large"
      ),
      bsButton(ns("reset"), "Resetear todo", type = "toggle", value = TRUE,
               icon("bars"), style = "primary", size = "large"
      )
    )





  })


  # Grafico de tortas
  output$grafico01 <- renderPlot({

    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    # Valores a graficar...
    MyValues <- as.numeric(as.character(tablas_1q()[,2]))

    # Labels nuevos...
    MyLabel <- corona()[[1]]

    # Colores
    if(is.null(colores_seleccionados())) MyColours <- rainbow(cantidad_categorias()) else
      MyColours <- colores_seleccionados()

    # Grafico de Torta
    pie( x = MyValues, label = MyLabel, col = MyColours, radius = 1)
  })
  
}

