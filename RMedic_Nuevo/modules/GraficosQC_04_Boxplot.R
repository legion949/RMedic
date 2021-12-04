


GraficosQC_04_Boxplot_UI <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("armado_grafico"))
  
  
}






## Segmento del server
GraficosQC_04_Boxplot_SERVER <- function(input, output, session, 
                                                      minibase, 
                                                      decimales,
                                                      control_ejecucion,
                                                      tablas_qc) {
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  # Control interno 01
  control_interno01 <- reactive({
    
    if(is.null(control_ejecucion())) return(FALSE) else
      if(is.null(tablas_qc())) return(FALSE) else return(TRUE)
  })
  
  
  
  
  cantidad_categorias <- reactive({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    # Control interno 01
    if(is.null(tablas_qc())) return(NULL)
    
    cantidad_categorias <- nrow(tablas_qc()[[1]]) - 1
    cantidad_categorias
  })
  
  categorias <- reactive({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    # Control interno 01
    if(is.null(tablas_qc())) return(NULL)
    
    categorias <- tablas_qc()[[1]][c(1:cantidad_categorias()),1]
    categorias
  })
  
  
  limites <- reactive({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    # Control interno 01
    if(is.null(cantidad_categorias())) return(NULL)
    
    cantidad_categorias <- cantidad_categorias()
    seleccionados <- c(1:cantidad_categorias)
    
    # Medias
    media <- tablas_qc()[[1]][seleccionados,2]
    names(media) <- tablas_qc()[[1]][seleccionados,1]
    
    # Desvios estandard
    desvio_estandard <- as.numeric(as.character(tablas_qc()[[6]][seleccionados,4]))
    names(desvio_estandard) <- tablas_qc()[[1]][seleccionados,1]
    
    
    # Errores estandard
    error_estandard <- as.numeric(as.character(tablas_qc()[[6]][seleccionados,5]))
    names(error_estandard) <- tablas_qc()[[1]][seleccionados,1]
    
    # Limites
    lim_inf <- min(media - error_estandard)
    lim_sup <- max(media + error_estandard)
    
    salida <- c(lim_inf, lim_sup)
    names(salida) <- c("Inferior", "Superior")
    return(salida)
    
  })
  
  
  
  
  colores_usuario <- reactive({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    # Control interno 01
    if(is.null(cantidad_categorias())) return(NULL)
    
    
    cantidad <- cantidad_categorias()
    armado <- "Color..."
    
    
    
    mis_colores <- rep(NA, cantidad)
    
    
    if(length(mis_colores) == 0) return(NULL)
    
    
    for(i in 1:cantidad){
      nombre_input <- paste("col", i, sep="_")
      if(is.null(input[[nombre_input]])) return(NULL)
      
      mis_colores[i] <- input[[nombre_input]]
      
    }
    
    
    
    return(mis_colores)
  })
  
  
  
  valores_iniciales <-  reactive({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    if(is.null(minibase())) return(NULL)
    if(is.null(cantidad_categorias())) return(NULL)
    
    valores <- list(x_min = NULL,
                    x_max = NULL,
                    y_min = min(minibase()[,2]),
                    y_max = max(minibase()[,2]),
                    xlab = colnames(minibase())[1],
                    ylab = colnames(minibase())[2],
                    ayuda = F,
                    color = rep("#FF0000", cantidad_categorias())
    )
    
    
    
    
    return(valores)
  })
  
  
  #   
  # 
  reseteo_logico <- reactiveVal(F)
  aplicador_logico <- reactiveVal(F)
  
  
  observeEvent(minibase(), {
    
    reseteo_logico(!reseteo_logico())
    delay(400,     aplicador_logico(!aplicador_logico()))
    # aplicador_logico(!aplicador_logico())
    
  })
  
  
  # 
  # 
  # 
  # 
  # 
  # # 
  # # # observeEvent(input$controlador01, {
  # # #   
  # # #   shinyjs::toggle(ns("James01"), asis = T, anim = TRUE, animType = "fade")
  # # #   
  # # # })
  # # 
  # # 
  observeEvent(input$controlador02, {
    
    
    aplicador_logico(!aplicador_logico())
    
  })
  # # 
  # # 
  observeEvent(input$reset, {
    
    reseteo_logico(!reseteo_logico())
    delay(400,     aplicador_logico(!aplicador_logico()))
  })
  # 
  # 
  # 
  # 
  # 
  # Variable criterio de inclusion
  observeEvent(reseteo_logico(),{
    
    # updateRadioButtons(session,
    #                    inputId = "ayuda",
    #                    label = "Ayuda en el gráfico...",
    #                    choices = c("Sin detalle" = F,
    #                                "Agregar especificaciones" = T
    #                    ),
    #                    selected = F
    # )
    
    
    updateNumericInput(session,
                       inputId = "y_max",
                       label = "Máximo eje Y",
                       value = valores_iniciales()$y_max,
                       min = valores_iniciales()$y_max,
                       max = NA
    )
    
    
    updateNumericInput(session,
                       inputId = "y_min",
                       label = "Mínimo eje Y",
                       value = valores_iniciales()$y_min,
                       min = NA,
                       max = valores_iniciales()$y_min
    )
    
    
    
    updateTextInput(session,
                    inputId = "ylab",
                    label = "Rótulo eje Y",
                    value = valores_iniciales()$ylab
    )
    
    updateTextInput(session,
                    inputId = "xlab",
                    label = "Rótulo eje X",
                    value = valores_iniciales()$xlab
    )
    
    
    # colourpicker::updateColourInput(session,
    #                                 inputId = "col_1",
    #                                 label = "Color...",
    #                                 value =  valores_iniciales()$color[1])
    
    colores_internos <- valores_iniciales()$color
    cantidad <- length(colores_internos)
    label_armado <- paste0("Color categoria '", categorias(), "': ")
    
    
    lapply(1:cantidad, function(i) {
      
      nombre_input <- paste("col", i, sep="_")
      
      colourpicker::updateColourInput(session,
                                      inputId = nombre_input,
                                      label = label_armado[i],
                                      value = colores_internos[i])
      
    })
    
    
    #  delay(100,     aplicador_logico(!aplicador_logico()))
    
    
    
  })
  
  
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # Salida de colores
  output$MODcolor <- renderUI({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    colores_internos <- valores_iniciales()$color
    cantidad <- length(colores_internos)
    label_armado <- paste0("Color categoria '", categorias(), "': ")
    
    lapply(1:cantidad, function(i) {
      
      nombre_input <- paste("col", i, sep="_")
      div(
        colourpicker::colourInput(inputId = ns(nombre_input),
                                  label = label_armado[i],
                                  value = colores_internos[i]), br()
      )
      
    })
    
  })
  
  
  # 
  # 
  # 
  # 
  output$texto_ayudaMin_y <- renderText({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    texto <- paste0("El límite inferior del eje Y debe ser igual o menor al
    mínimo valor de 'Media - Error Estánrdard' de todas las categorías. En este caso debe ser igual o menor a ",
                    limites()[1], ".")
    
    if(!(input$y_min <= limites()[1])) return(texto) else return(NULL)
    
  })
  
  
  output$texto_ayudaMax_y <- renderText({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    texto <- paste0("El límite supeior del eje Y debe ser igual o mayor al
    máximo valor de 'Media + Error Estánrdard' de todas las categorías. En este caso debe ser igual o mayor a ",
                    limites()[2], ".")
    
    if(!(input$y_max >= limites()[2])) return(texto) else return(NULL)
    
  })
  
  
  output$menu_general01 <- renderUI({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    div(
      fluidRow(
        column(6,
               numericInput(inputId = ns("y_min"),
                            label = "Mínimo eje Y",
                            value = valores_iniciales()$y_min,
                            min = NA,
                            max = valores_iniciales()$y_min
               ),
               textOutput(ns("texto_ayudaMin_y")),
               br(),
               numericInput(inputId = ns("y_max"),
                            label = "Máximo eje Y",
                            value = valores_iniciales()$y_max,
                            min = valores_iniciales()$y_max,
                            max = NA
               ),
               textOutput(ns("texto_ayudaMax_y")),
               br(),
               textInput(inputId = ns("xlab"),
                         label = "Rótulo eje X",
                         value = valores_iniciales()$xlab
               ),
               br(),
               textInput(inputId = ns("ylab"),
                         label = "Rótulo eje Y",
                         value = valores_iniciales()$ylab
               )
               
               
        ),
        column(6,
               uiOutput(ns("MODcolor"))
        )
      ),
      br(),
      br(),
      bsButton(ns("reset"), "Resetear Gráfico", type = "toggle", value = TRUE,
               icon("bars"), style = "primary", size = "large"
      ),
      bsButton(ns("controlador02"), "Aplicar todos los cambios", type = "toggle", value = TRUE,
               icon("bars"), style = "primary", size = "large"
      )
    )
    
    
    
    
    
  })
  
  valores_usuario <-   eventReactive(aplicador_logico(), ignoreNULL = FALSE, {
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    if(is.null(minibase())) return(NULL)
    if(is.null(valores_iniciales())) return(NULL)
    
    valores <- list()
    
    if(!is.null(input$x_min)) valores[[1]] <- input$x_min else valores[[1]] <- valores_iniciales()$x_min
    if(!is.null(input$x_max)) valores[[2]] <- input$x_max else valores[[2]] <- valores_iniciales()$x_max
    if(!is.null(input$y_min)) valores[[3]] <- input$y_min else valores[[3]] <- valores_iniciales()$y_min
    if(!is.null(input$y_max)) valores[[4]] <- input$y_max else valores[[4]] <- valores_iniciales()$y_max
    if(!is.null(input$xlab))  valores[[5]] <- input$xlab  else valores[[5]] <- valores_iniciales()$xlab
    if(!is.null(input$ylab))  valores[[6]] <- input$ylab  else valores[[6]] <- valores_iniciales()$ylab
    if(!is.null(input$ayuda)) valores[[7]] <- input$ayuda else valores[[7]] <- valores_iniciales()$ayuda
    # if(!is.null(input$col_1)) valores[[8]] <- input$col_1 else valores[[8]] <- valores_iniciales()$color
    if(!is.null(colores_usuario())) valores$color <- colores_usuario() else valores$color <- valores_iniciales()$color
    
    
    # Nombre de la lista, mismo nombre que por defecto
    names(valores) <- names(valores_iniciales())
    
    # Correccion para los valores que min y max de cada eje Y
    if(!is.null(valores$y_min)) if(valores$y_min > min(minibase()[,2])) valores$y_min <- min(minibase()[,2])
    if(!is.null(valores$y_max)) if(valores$y_max < max(minibase()[,2])) valores$y_max <- max(minibase()[,2])
    
    
    
    return(valores)
  })
  
  
  
  
  output$grafico01 <- renderPlot({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    if(is.null(valores_usuario())) return(NULL)
    
    
    
    
    graficos_qc(minibase = minibase(),
                tipo_grafico = "boxplot", # boxplot
                cols = valores_usuario()$color,
                xlab = valores_usuario()$xlab,
                ylab = valores_usuario()$ylab,
                ylim = c(valores_usuario()$y_min, valores_usuario()$y_max)
    )
    # FACTOR <- as.character(minibase()[,1])
    # VR <- minibase()[,2]
    # boxplot(VR ~ FACTOR, col = "red")
    
    #  cols = c("red", "blue"))
    
    
  })
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  output$armado_grafico <- renderUI({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    div(
      h2("Gráfico de Media y Desvío Estándard"),
      fluidRow(
        column(6,
               plotOutput(ns("grafico01"))
        ),
        column(6,
               bsButton(inputId = ns("controlador01"),
                        label = "Mostrar/Ocultar opciones gráficas",
                        icon = icon("bars"),
                        type = "toggle",
                        # value = FALSE,
                        value = TRUE,
                        style = "primary",
                        size = "large"
               ), br(),br(), br(),
               conditionalPanel(condition = "input.controlador01", ns = ns,
                                div(id = ns("James01"), uiOutput(ns("menu_general01")))
               )
               
        )
        
      )
    )
    
  })
  
}



