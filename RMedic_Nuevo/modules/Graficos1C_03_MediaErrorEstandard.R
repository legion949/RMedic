


Graficos1C_03_MediaErrorEstandard_UI <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("armado_grafico"))
  
  
}






## Segmento del server
Graficos1C_03_MediaErrorEstandard_SERVER <- function(input, output, session, 
                                                      minibase, 
                                                      batalla_naval,
                                                      decimales,
                                                      casoRMedic,
                                                      tablas_1c) {
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  
  
  
  # xxchange <- reactive({
  #   paste(ylab_interno())
  # })
  
  
  
  colores_usuario <- reactive({
    
    
    cantidad <- 1
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
  
  
  reseteo_logico <- reactiveVal(F)
  aplicador_logico <- reactiveVal(F)
  
  
  observeEvent(input$controlador01, {
    
    shinyjs::toggle(ns("James01"), asis = T, anim = TRUE, animType = "fade")
    
  })
  
  
  observeEvent(input$controlador03, {
    
    
    aplicador_logico(!aplicador_logico())
    
  })
  
  
  observeEvent(input$reset, {
    
    reseteo_logico(!reseteo_logico())
    delay(400,     aplicador_logico(!aplicador_logico()))
  })
  
  
  
  
  
  
  
  
  
  
  observeEvent(input$ylab, {
    
    
  
    if(input$ylab == colnames(minibase())[1]) {
      
      if(input$ylab != valores_usuario()$ylab) {
        
        delay(1000, aplicador_logico(!aplicador_logico()))
        
        #  reseteo_logico(!reseteo_logico())
      }
    }
    
    # reseteo_logico(!reseteo_logico())
    
  })
  
  
  
  # Variable criterio de inclusion
  observeEvent(reseteo_logico(),{
    
    updateRadioButtons(session,
                       inputId = "ayuda",
                       label = "Ayuda en el gráfico...",
                       choices = c("Sin detalle" = F,
                                   "Agregar especificaciones" = T
                       ),
                       selected = F
    )
    
    
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
    
    label_armado <- "Color..."
    colores_internos <- valores_iniciales()$color
    cantidad <- length(colores_internos)
    
    lapply(1:cantidad, function(i) {
      
      nombre_input <- paste("col", i, sep="_")
      
      colourpicker::updateColourInput(session,
                                      inputId = nombre_input,
                                      label = label_armado[i],
                                      value = colores_internos[i])
      
    })
    
    
    #  delay(100,     aplicador_logico(!aplicador_logico()))
    
    
    
  })
  
  
  
  
  # Salida de colores
  output$MODcolor <- renderUI({
    
    label_armado <- "Color..."
    colores_internos <- valores_iniciales()$color
    cantidad <- length(colores_internos)
    
    lapply(1:cantidad, function(i) {
      
      nombre_input <- paste("col", i, sep="_")
      div(
        colourpicker::colourInput(inputId = ns(nombre_input),
                                  label = label_armado[i], 
                                  value = colores_internos[i]), br()
      )
      
    })
    
  })
  
  
  
  
  output$texto_ayudaMax_y <- renderText({
    texto <- "El límite superior del eje Y debe ser igual o mayor al máximo
    valor de la variable."
    
    if(input$y_max < max(minibase()[,1])) return(texto) else return(NULL)
    
  })
  
  
  output$texto_ayudaMin_y <- renderText({
    texto <- "El límite inferior del eje Y debe ser igual o menor al mínimo 
    valor de la variable."
    
    if(input$y_min > min(minibase()[,1])) return(texto) else return(NULL)
    
  })
  
  
  output$menu_general01 <- renderUI({
    
    div(
      fluidRow(
        column(6,
               radioButtons(inputId = ns("ayuda"),
                            label = "Ayuda en el gráfico...",
                            choices = c("Sin detalle" = F,
                                        "Agregar especificaciones" = T
                            )
               )
        ),
        column(6,
               uiOutput(ns("MODcolor"))
        )
      ),
      br(),
      fluidRow(
        column(6,
               numericInput(inputId = ns("y_min"),
                            label = "Mínimo eje Y", 
                            value = valores_iniciales()$y_min,
                            min = NA,
                            max = valores_iniciales()$y_min
               ),
               textOutput(ns("texto_ayudaMin_y"))
        ),
        column(6,
               numericInput(inputId = ns("y_max"),
                            label = "Máximo eje Y", 
                            value = valores_iniciales()$y_max,
                            min = valores_iniciales()$y_max,
                            max = NA
               ),
               textOutput(ns("texto_ayudaMax_y"))
        )
      ),
      br(),
      fluidRow(
        column(6,
               textInput(inputId = ns("xlab"),
                         label = "Rótulo eje X",
                         value = valores_iniciales()$xlab
               )
        ),
        column(6, 
               textInput(inputId = ns("ylab"),
                         label = "Rótulo eje Y",
                         value = valores_iniciales()$ylab
               )
        )
      ),
      br(),
      br(),
      bsButton(ns("reset"), "Resetear", type = "toggle", value = TRUE,
               icon("bars"), style = "primary", size = "large"
      ),
      bsButton(ns("controlador03"), "Aplicar todos los cambios", type = "toggle", value = TRUE,
               icon("bars"), style = "primary", size = "large"
      )
    )
    
    
    
    
    
  })
  
  
  
  
  
  
  
  
  valores_iniciales <-  reactive({
    
    if(is.null(minibase())) return(NULL)
    
    
    valores <- list(x_min = NULL,
                    x_max = NULL,
                    y_min = min(minibase()[1]),
                    y_max = max(minibase()[1]),
                    xlab = "",
                    ylab = colnames(minibase())[1],
                    ayuda = F,
                    color = c("#FF0000")
    )
    
    
    
    
    return(valores)
  })
  
  
  
  valores_usuario <-   eventReactive(aplicador_logico(), ignoreNULL = FALSE, {
    
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
    if(!is.null(valores$y_min)) if(valores$y_min > min(minibase()[,1])) valores$y_min <- min(minibase()[,1])
    if(!is.null(valores$y_max)) if(valores$y_max < max(minibase()[,1])) valores$y_max <- max(minibase()[,1])
    
    
    
    return(valores)
  })
  
  
  
  
  output$grafico01 <- renderPlot({
    
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 2) return(NULL)
    if(is.null(valores_usuario())) return(NULL)
    
    texto01 <- c("Media - E.E.", "Media", "Media + E.E.")
    
    media <- as.numeric(as.character(tablas_1c()[[1]][1,2]))
    # media
    
    desvio_estandard <- as.numeric(as.character(tablas_1c()[[1]][1,3]))
    # desvio_estandard
    
    error_estandard <- as.numeric(as.character(tablas_1c()[[6]][1,5]))
    
    tipos_intervalos <- c("1", "2", "3", "90%", "95%", "99%")
    coeficientes <- c(1, 2, 3, 1.96, 2.45, 3.77)
    matriz_valores <- matrix(NA, 2, length(tipos_intervalos))
    names(matriz_valores) <- tipos_intervalos
    matriz_valores[1,] <- media - coeficientes*error_estandard
    matriz_valores[2,] <- media + coeficientes*error_estandard
    
    valores_x <- rep(1, nrow(minibase()))
    plot( x = valores_x, y = minibase()[,1], col = "white", 
          xlim = c(0.5, 1.5), 
          ylim = c(valores_usuario()$y_min, valores_usuario()$y_max),
          xlab = valores_usuario()$xlab, ylab = valores_usuario()$ylab,
          xaxt = "n")
    lines(c(1,1), matriz_valores[,1],
          lwd = 3)
    points(valores_x[1], media,  cex = 2,  col=valores_usuario()$color,  pch=19)  
    
    if (valores_usuario()$ayuda) {
      
      rejunte <- c(matriz_valores[1,1], media, matriz_valores[2,1])
      
      text((rep(1, 3)+0.1), rejunte, texto01, pos = 4, cex = 1.5)
      text((rep(1, 3)-0.1), rejunte, rejunte, pos = 2, cex = 1.5)
    }
    
  })
  
  
  
  
  
  
  
  output$armado_grafico <- renderUI({
    
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 2) return(NULL)
    div(
      h2("Gráfico de Media y Error Estándard"),
      fluidRow(
        column(6,
               plotOutput(ns("grafico01"))
        ),
        column(6,
               bsButton(inputId = ns("controlador01"), 
                        label = "Mostrar/Ocultar opciones gráficas",
                        icon = icon("bars"), 
                        type = "toggle", 
                        value = FALSE,
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