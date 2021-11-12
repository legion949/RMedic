


Graficos1C_08_Puntos_UI <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("armado_grafico"))
  
  
}






## Segmento del server
Graficos1C_08_Puntos_SERVER <- function(input, output, session, 
                                         minibase, 
                                         batalla_naval,
                                         decimales,
                                         casoRMedic) {
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  
  
  
  
  colores_seleccionados <- reactiveVal({
    
    mis_colores <- "red"
    names(mis_colores) <- paste0("color", c(1:length(mis_colores)))
    
    mis_colores
  })
  
  
  
  
  
  
  
  
  
  
  # xxchange <- reactive({
  #   paste(ylab_interno())
  # })
  
  
  
  colores_seleccionados2 <- reactive({
    # if (is.null(DF_interna())) return(NULL)
    
    
    
    
    
    
    cantidad <- 1
    armado <- "Color..."
    
    
    
    mis_colores <- rep(NA, cantidad)
    
    
    if(length(mis_colores) == 0) return(NULL)
    
    
    for(i in 1:cantidad){ 
      nombre_input <- paste("col", i, sep="_")
      #   cat("nombre_input: ", nombre_input, "\n" )
      #   cat("input[[nombre_input]]: ", input[[nombre_input]], "\n" )
      if(is.null(input[[nombre_input]])) return(NULL)
      
      mis_colores[i] <- input[[nombre_input]]
      
    }
    
    
    #   cat("mis_colores:", mis_colores, "\n")
    return(mis_colores)
  })
  
  
  reseteo_logico <- reactiveVal(F)
  aplicador_logico <- reactiveVal(F)
  
  
  observeEvent(input$controlador01, {
    
    shinyjs::toggle(ns("James01"), asis = T, anim = TRUE, animType = "fade")
    
  })
  
  
  observeEvent(input$controlador03, {
    
    if(length(colores_seleccionados2()) > 0) {
      
      
      colores_seleccionados(colores_seleccionados2())
      
    }
    aplicador_logico(!aplicador_logico())
    # reseteo_logico(!reseteo_logico())
  })
  
  observeEvent(input$reset, {
    
    reseteo_logico(!reseteo_logico())
    delay(400,     aplicador_logico(!aplicador_logico()))
  })
  
  observeEvent(colnames(minibase())[1], {
    
    reseteo_logico(!reseteo_logico())
    #   aplicador_logico(!aplicador_logico())
    #     delay(1000,     aplicador_logico(!aplicador_logico()))
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
                       inputId = "max",
                       label = "Máximo eje Y", 
                       value = valores_iniciales()$max,
                       min = valores_iniciales()$max,
                       max = NA
    )
    
    
    updateNumericInput(session,
                       inputId = "min",
                       label = "Mínimo eje Y", 
                       value = valores_iniciales()$min,
                       min = NA,
                       max = valores_iniciales()$min
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
  
  
  if (1 == 2) {
    # Variable criterio de inclusion
    observeEvent(input[["max"]],{
      
      if(input[["max"]] < max(minibase()[,1])) {
        
        updateNumericInput(session, 
                           inputId = "max",
                           label = "Máximo eje Y", 
                           value = valores_iniciales()$max,
                           max = NA,
                           min = valores_iniciales()$max
        )
        
        
        
      }
    })
    
    
    # Variable criterio de inclusion
    observeEvent(input[["min"]],{
      
      if(input[["min"]] > min(minibase()[,1])) {
        
        updateNumericInput(session, 
                           inputId = "min",
                           label = "Mínimo eje Y", 
                           value = valores_iniciales()$min,
                           max = valores_iniciales()$min,
                           min = NA
        )
        
        
        
      }
    })
  }
  
  output$texto_ayudaMax <- renderText({
    texto <- "El límite superior del eje Y debe ser igual o mayor al máximo
    valor de la variable."
    
    if(input$max < max(minibase()[,1])) return(texto) else return(NULL)
    
  })
  
  
  output$texto_ayudaMin <- renderText({
    texto <- "El límite inferior del eje Y debe ser igual o menor al mínimo 
    valor de la variable."
    
    if(input$min > min(minibase()[,1])) return(texto) else return(NULL)
    
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
               ),
               br(),
               numericInput(inputId = ns("max"),
                            label = "Máximo eje Y", 
                            value = valores_iniciales()$max,
                            min = valores_iniciales()$max,
                            max = NA
               ),
               textOutput(ns("texto_ayudaMax")),
               br(),
               numericInput(inputId = ns("min"),
                            label = "Mínimo eje Y", 
                            value = valores_iniciales()$min,
                            min = NA,
                            max = valores_iniciales()$min
               ),
               textOutput(ns("texto_ayudaMin")),
               br()
        ),
        column(6,
               uiOutput(ns("MODcolor")),
               br(),
               textInput(inputId = ns("ylab"),
                         label = "Rótulo eje Y",
                         value = valores_iniciales()$ylab
               ),
               br(),
               textInput(inputId = ns("xlab"),
                         label = "Rótulo eje X",
                         value = valores_iniciales()$xlab
               ),
               br()
               
        )
      ),
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
    
    
    valores <- list(min(minibase()[1]),
                    max(minibase()[1]),
                    colnames(minibase())[1],
                    "",
                    F,
                    c("#FF0000")
    )
    
    names(valores) <- c("min", "max", "ylab", "xlab", "ayuda", "color")
    
    
    return(valores)
  })
  
  
  # valores_usuario <-  reactive({
  #   
  #   if(is.null(minibase())) return(NULL)
  #   if(is.null(input$min)) return(NULL)
  #   if(is.null(input$max)) return(NULL)
  #   if(is.null(input$ylab)) return(NULL)
  #   if(is.null(input$xlab)) return(NULL)
  #   if(is.null(input$ayuda)) return(NULL)
  #   if(is.null(input$color_1)) return(NULL)
  #   
  #   valores <- list(input$min,
  #                   input$max,
  #                   input$ylab,
  #                   input$xlab,
  #                   input$ayuda,
  #                   input$col_1)
  #   
  #   names(valores) <- c("min", "max", "ylab", "xlab", "ayuda", "color")
  #   
  #   
  #   if(valores[[1]] > min(minibase()[,1])) valores[[1]] <- min(minibase()[,1])
  #   if(valores[[2]] < max(minibase()[,1])) valores[[2]] <- max(minibase()[,1])
  #   
  #   
  #   
  #   return(valores)
  # })
  # 
  
  # valores_usuario <-   eventReactive(input$controlador03, ignoreNULL = FALSE, {
  valores_usuario <-   eventReactive(aplicador_logico(), ignoreNULL = FALSE, {
    
    if(is.null(minibase())) return(NULL)
    if(is.null(valores_iniciales())) return(NULL)
    
    valores <- list()
    
    if(!is.null(input$min)) valores[[1]] <- input$min else valores[[1]] <- valores_iniciales()$min
    if(!is.null(input$max)) valores[[2]] <- input$max else valores[[2]] <- valores_iniciales()$max
    if(!is.null(input$ylab)) valores[[3]] <- input$ylab else valores[[3]] <- valores_iniciales()$ylab
    if(!is.null(input$xlab)) valores[[4]] <- input$xlab else valores[[4]] <- valores_iniciales()$xlab
    if(!is.null(input$ayuda)) valores[[5]] <- input$ayuda else valores[[5]] <- valores_iniciales()$ayuda
    if(!is.null(input$col_1)) valores[[6]] <- input$col_1 else valores[[6]] <- valores_iniciales()$color
    
    
    # if(is.null(input$min)) return(NULL)
    # if(is.null(input$max)) return(NULL)
    # if(is.null(input$ylab)) return(NULL)
    # if(is.null(input$xlab)) return(NULL)
    # if(is.null(input$ayuda)) return(NULL)
    # if(is.null(input$col_1)) return(NULL)
    
    # valores <- list(input$min,
    #                 input$max,
    #                 input$ylab,
    #                 input$xlab,
    #                 input$ayuda,
    #                 input$col_1)
    
    names(valores) <- c("min", "max", "ylab", "xlab", "ayuda", "color")
    
    
    if(valores[[1]] > min(minibase()[,1])) valores[[1]] <- min(minibase()[,1])
    if(valores[[2]] < max(minibase()[,1])) valores[[2]] <- max(minibase()[,1])
    
    
    
    return(valores)
  })
  
  
  
  
  output$grafico01 <- renderPlot({
    
    
    
    coordenadas <-    boxplot(minibase()[1], ylim = c(valores_usuario()$min, valores_usuario()$max),
                              ylab = valores_usuario()$ylab, xlab = valores_usuario()$xlab,
                              col = valores_usuario()$color)
    
    
    texto01 <- c("Mínimo", "Q1", "Q2 (Mediana)", "Q3", "Máximo")
    texto02 <- c("25%", "25%", "25%", "25%")
    
    coordenadasY <- coordenadas$stats
    
    
    
    
    if (valores_usuario()$ayuda) {
      
      text(1.25, coordenadas$stats, texto01, pos = 4)
      
      mediasY <- c()
      for(k in 1:(length(coordenadasY)-1)) mediasY[k] <- mean(coordenadasY[c(k, (k+1))])
      
      
      text(1.25, coordenadasY, texto01, pos = 4)
      
      colores <- rep(c("red", "blue"), 2)
      
      pos01 <- 0.70
      pos02 <- pos01 - 0.06
      pos03 <- pos01 - 0.12
      
      for(k in 1:(length(coordenadasY)-1)) {
        
        
        lines(x = c(pos01, pos01), y = coordenadasY[c(k, (k+1))], col = colores[k], lwd = 4)
        lines(x = c(pos03, pos03), y = coordenadasY[c(k, (k+1))], col = colores[k], lwd = 4)
        
        # lines(x = c(pos01, pos03), y = coordenadasY[c(k, (k+1))], col = colores[k], lwd = 4)
        
      }
      
      
      for(k in 1:length(coordenadasY)) {
        
        
        #  lines(x = c(pos01, pos01), y = coordenadasY[c(k, (k+1))], col = colores[k], lwd = 4)
        #  lines(x = c(pos03, pos03), y = coordenadasY[c(k, (k+1))], col = colores[k], lwd = 4)
        
        lines(x = c(pos01, pos03), y = rep(coordenadasY[k], 2), col = "black", lwd = 4, lty = 2)
        
      }
      
      text(pos02, mediasY, texto02, srt = 90)
      
    }
    
  })
  
  
  
  
  
  
  
  output$armado_grafico <- renderUI({
    
    div(
      h2("Gráfico de Boxplot"),
      fluidRow(
        column(6,
               plotOutput(ns("grafico01"))
        ),
        column(6,
               bsButton(inputId = ns("controlador01"), 
                        label = "Mostrar/Ocultar opciones gráficas",
                        icon = icon("bars"), 
                        type = "toggle", 
                        value = TRUE,
                        style = "primary", 
                        size = "large"
               ), br(),br(), br(),
               div(id = ns("James01"), uiOutput(ns("menu_general01")))
               
               
        )
        
      )
    )
  })
  
}