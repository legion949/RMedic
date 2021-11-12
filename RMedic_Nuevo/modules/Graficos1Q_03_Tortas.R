


Graficos1Q_03_Tortas_UI <- function(id) {
  
  ns <- NS(id)
  
  div(
    bsButton(inputId = ns("controlador01"), 
             label = "Mostrar/Ocultar opciones gráficas",
             icon = icon("bars"), 
             style = "primary",
             size = "large",
             type = "toggle", 
             value = TRUE
             , 
    ),br(),br(),
    h2("Gráfico de Tortas"),
    
    fluidRow(
      column(4,
             div(id = ns("James01"),
                 uiOutput(ns("menu_general01"))
                 )
    ),
    column(8,
            plotOutput(ns("grafico01"))
    )
    )
  )
  

  
  
  
}






## Segmento del server
Graficos1Q_03_Tortas_SERVER <- function(input, output, session, 
                                        minibase, 
                                        batalla_naval,
                                        decimales,
                                        casoRMedic,
                                        DF_interna) {
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  
  
  
  # Cantidad de categorias de la variable...
  cantidad_categorias <- reactive({

    
    return(nrow(DF_interna()))
    
  })
  
  # Corona para la torta
  corona <- reactive({
    
    la_corona <- list()
    
    la_suma <- sum(as.numeric(as.character(input$coronacion)))
    

    la_nada <- rep("", cantidad_categorias())
    la_categoria <- as.character(as.vector(DF_interna()[,1]))
    la_fa <- as.character(as.vector(DF_interna()[,2]))
    la_porcentaje <- as.character(as.vector(DF_interna()[,6]))
    la_fusion <- as.character(as.vector(DF_interna()[,7]))
      
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
  
  # Mostrar/Ocultar menues (James01)...
  # Evento asociado al controlador01
  observeEvent(input$controlador01, {
    
    shinyjs::toggle(id = ns("James01"), 
                    asis = T, 
                    anim = TRUE, 
                    animType = "fade")
    
  })
  
  
  # Colores seleccionados
  # Evento reactivo asociado al controlador02
  colores_seleccionados <-  eventReactive(input$controlador02,({

   
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
    
 #   if (is.null(DF_interna())) return(NULL)

    
    
    
    cantidad <- c()
    cantidad <- nrow(DF_interna())
    label_armado <- paste0("Categoría '", as.vector(DF_interna()[,1]), "'")
        
 
    
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
      )
    )
    
    
    
    
    
  })
  
  
  # Grafico de tortas
  output$grafico01 <- renderPlot({
    
    # Valores a graficar...
    MyValues <- as.numeric(as.character(DF_interna()[,2]))
    
    # Labels nuevos...
    MyLabel <- corona()[[1]]

    # Colores
    if(is.null(colores_seleccionados())) MyColours <- rainbow(cantidad_categorias()) else
      MyColours <- colores_seleccionados()
    
    # Grafico de Torta
    pie( x = MyValues, label = MyLabel, col = MyColours, radius = 1)
  })
  
}

