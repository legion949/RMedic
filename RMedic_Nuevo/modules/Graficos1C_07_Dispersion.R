


Graficos1C_07_Dispersion_UI <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("armado_grafico"))
  
  
}






## Segmento del server
Graficos1C_07_Dispersion_SERVER <- function(input, output, session, 
                                        minibase, 
                                        batalla_naval,
                                        decimales,
                                        casoRMedic,
                                        tablas_1c) {
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  # Control interno 01
  control_interno01 <- reactive({
    
    if(is.null(casoRMedic())) return(FALSE)
    if(casoRMedic() != 2) return(FALSE)
    
    # Return Exitoso
    return(TRUE)
  })
  
  # Tabla de Medidas Resumen
  medidas_resumen <- reactive({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    
    
    # Nota: al valor input$x_breaks lo tuve que poner
    #      como na.omit(input$x_breaks)[1] por que algunas veces
    #      otorga un vector con dos valores, pero uno de ellos es NA.
    
    
    
    salida <-  RMedic_1c_tablas(input_base =  minibase(),
                                input_decimales = decimales(),
                                input_min = NULL,
                                input_max = NULL,
                                input_breaks = NULL,
                                input_side = NULL
    )[[1]]
    
    
    
    # Return Exitoso
    return(salida)
    
    
  })  
  
  # Tabla de Frecuencias!
  # Para este grafico de puntos hacen falta!
  tabla_frecuencias <- reactive({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    table(minibase()[,1])
    
    
  })
  
  # Generacion de una base interna para el grafico
  base_interna <- reactive({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    valores_x <- rep(names(tabla_frecuencias()), tabla_frecuencias()) 
    valores_x <- as.numeric(valores_x)
    valores_x
    
    valores_y <- c()
    for(k in 1:length(tabla_frecuencias())) valores_y <- c(valores_y, c(1:tabla_frecuencias()[k]))
    
    # Armamos un objeto que funcione como base interna
    base_interna <- as.data.frame(cbind(valores_x, valores_y))
    colnames(base_interna) <- c("valores_x", "valores_y")
    
    # Return exitoso
    return(base_interna)
    
  })
  
  
  # Objetos reactivos de contorl
  reseteo_logico <- reactiveVal(F)
  aplicador_logico <- reactiveVal(F)
  
  
  # Valores iniciales
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
  
  
  
  # xxchange <- reactive({
  #   paste(ylab_interno())
  # })
  
  
  # Colores seleccionados por el usuario
  colores_usuario <- reactive({
    
    # Nota: la cantidad de colores seleccionados por el usuario por ser
    #        una cantidad que puede ser dinamica para algunas partes de RMedic.
    #        Por eso reunimos todos los valores de todos los input que estan
    #        relacionados aqui, para luego ingresar esta info dentro de
    #        valores_usuaio()
    
    
    colores_internos <- valores_iniciales()$color
    cantidad <- length(colores_internos)
    armado <- "Color..."
    
    
    
    mis_colores <- rep(NA, cantidad)
    
    
    if(length(mis_colores) == 0) return(NULL)
    
    # Recolectamos todos los colores en un vector
    for(i in 1:cantidad){ 
      nombre_input <- paste("col", i, sep="_")
      if(is.null(input[[nombre_input]])) return(NULL)
      
      mis_colores[i] <- input[[nombre_input]]
      
    }
    
    
    # Return Exitoso
    return(mis_colores)
  })
  
  # Valores elegidos por el usuario
  valores_usuario <-   eventReactive(aplicador_logico(), ignoreNULL = FALSE, {
    
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    valores <- list()
    
    # Valores X
    if(!is.null(input$x_min)) valores[[1]] <- input$x_min else valores[[1]] <- valores_iniciales()$x_min
    if(!is.null(input$x_max)) valores[[2]] <- input$x_max else valores[[2]] <- valores_iniciales()$x_max
    
    # Valores Y
    if(!is.null(input$y_min)) valores[[3]] <- input$y_min else valores[[3]] <- valores_iniciales()$y_min
    if(!is.null(input$y_max)) valores[[4]] <- input$y_max else valores[[4]] <- valores_iniciales()$y_max
    
    
    # Rotulos
    if(!is.null(input$xlab))  valores[[5]] <- input$xlab  else valores[[5]] <- valores_iniciales()$xlab
    if(!is.null(input$ylab))  valores[[6]] <- input$ylab  else valores[[6]] <- valores_iniciales()$ylab
    
    # Ayuda
    if(!is.null(input$ayuda)) valores[[7]] <- input$ayuda else valores[[7]] <- valores_iniciales()$ayuda
    
    # Colores
    # if(!is.null(input$col_1)) valores[[8]] <- input$col_1 else valores[[8]] <- valores_iniciales()$color
    if(!is.null(colores_usuario())) valores$color <- colores_usuario() else valores$color <- valores_iniciales()$color
    
    
    # Nombre de la lista, mismo nombre que por defecto
    names(valores) <- names(valores_iniciales())
    
    # Correccion de Valores
    # # Correccion para los valores que min y max de cada eje
    if(!is.null(valores$x_min)) if(valores$x_min > min(minibase()[,1])) valores$x_min <- min(minibase()[,1])
    if(!is.null(valores$x_max)) if(valores$x_max < max(minibase()[,1])) valores$x_max <- max(minibase()[,1])
    if(!is.null(valores$y_min)) if(valores$y_min < 0) valores$y_min <- 0
    if(!is.null(valores$y_max)) if(valores$y_max < max(tabla_frecuencias())) valores$y_max <- max(tabla_frecuencias())
    
    
    # Return exitoso
    return(valores)
  })
  
  
  # Controlador 01
  observeEvent(input$controlador01, {
    
    delay(100, shinyjs::toggle(ns("James01"), asis = T, anim = TRUE, animType = "fade"))
    
  })
  
  # Controlador 02
  observeEvent(input$controlador02, {
    
    if(!is.null(input$controlador01)) {
      if(input$controlador01) {
        aplicador_logico(!aplicador_logico())
      }
    }
  })
  
  # Reseteo
  observeEvent(input$reset, {
    
    if(is.null(input$controlador01)) return(NULL)
    if(!input$controlador01) return(NULL)
    
    # Damos la orden de setear!
    reseteo_logico(!reseteo_logico())
    
    # Luego de un delay de 400 milisegundos la pagina tomara los
    # valores reseteados para generarla nuevamente
    delay(400,     aplicador_logico(!aplicador_logico()))
  })
  
  
  
  
  
  
  
  # Reseteo forzado
  observeEvent(input$ylab, {
    
    
    # Este reseteo forzado ocurre por que cuando cambia de variable
    # no termina de resetear los valores del grafico anterior. 
    # Entre otras cosas pasaba que como ylab salia la nueva variable, 
    # pero no el resto de los valores, entonces cuando pasa que
    # ylab es una columna elegida, se resetea todo.
    #
    # Seria un golazo encontrar otra forma para que al cambiar de variable
    # efectivamente resetee tooodo.
    if(input$ylab == colnames(minibase())[1]) {
      
      if(input$ylab != valores_usuario()$ylab) {
        
        delay(1000, aplicador_logico(!aplicador_logico()))
        
        
      }
    }
    
    
    
  })
  
  
  
  # Variable criterio de inclusion
  observeEvent(reseteo_logico(),{
    
    
    freezeReactiveValue(input, "x_max")
    updateNumericInput(session,
                       inputId = "x_max",
                       label = "Máximo eje X", 
                       value = valores_iniciales()$x_max,
                       min = valores_iniciales()$x_max,
                       max = NA
    )
    
    
    freezeReactiveValue(input, "x_min")
    updateNumericInput(session,
                       inputId = "x_min",
                       label = "Mínimo eje X", 
                       value = valores_iniciales()$x_min,
                       min = NA,
                       max = valores_iniciales()$x_min
    )
    
    freezeReactiveValue(input, "y_max")
    updateNumericInput(session,
                       inputId = "y_max",
                       label = "Máximo eje Y", 
                       value = valores_iniciales()$y_max,
                       min = valores_iniciales()$y_max,
                       max = NA
    )
    
    freezeReactiveValue(input, "y_min")
    updateNumericInput(session,
                       inputId = "y_min",
                       label = "Mínimo eje Y", 
                       value = valores_iniciales()$y_min,
                       min = NA,
                       max = valores_iniciales()$y_min
    )
    
    
    freezeReactiveValue(input, "ylab")
    updateTextInput(session,
                    inputId = "ylab",
                    label = "Rótulo eje Y",
                    value = valores_iniciales()$ylab
    )
    
    freezeReactiveValue(input, "xlab")
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
      
      freezeReactiveValue(input, nombre_input)
      colourpicker::updateColourInput(session,
                                      inputId = nombre_input,
                                      label = label_armado[i],
                                      value = colores_internos[i])
      
    })
    
    
    
    
    
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
  
  
  output$texto_ayudaMin_x <- renderText({
    texto <- paste0("El límite inferior del eje X debe ser igual o menor al mínimo 
    valor de la variable. ", "En este caso debe ser igual o menor a ",
                    valores_iniciales()$x_min, ".")
    
    if(input$x_min > valores_iniciales()$x_min) return(texto) else return(NULL)
    
  })
  
  
  output$texto_ayudaMax_x <- renderText({
    texto <- paste0("El límite superior del eje X debe ser igual o mayor al máximo
    de la variable. ", "En este caso debe ser igual o mayor a ",
                    valores_iniciales()$x_max, ".")
    
    if(input$x_max < valores_iniciales()$x_max) return(texto) else return(NULL)
    
  })
  
  
  
  
  output$texto_ayudaMax_y <- renderText({
    texto <- paste0("El límite superior del eje Y debe ser igual o mayor al máximo
    valor de frecuencias para las variables. En este caso debe ser mayor o igual a ",
                    max(tabla_frecuencias()), ".")
    
    if(input$y_max < max(tabla_frecuencias())) return(texto) else return(NULL)
    
  })
  
  
  output$texto_ayudaMin_y <- renderText({
    texto <- "El límite inferior del eje Y debe ser igual o mayor a cero ya que
    el eje Y representa a las frecuencias de los valores de la variable."
    
    if(input$y_min < 0) return(texto) else return(NULL)
    
  })
  
  
  output$menu_general01 <- renderUI({
    
    
    
    div(
      fluidRow(
        column(6),
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
                            min = 0,
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
      bsButton(ns("controlador02"), "Aplicar todos los cambios", type = "toggle",
               value = TRUE, icon("bars"), style = "primary", size = "large"
      )
    )
    
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$grafico01 <- renderPlot({
    
    # if(is.null(casoRMedic())) return(NULL)
    #  if(casoRMedic() != 2) return(NULL)
    #  if(is.null(valores_usuario())) return(NULL)
    #  if(is.null(base_interna())) return(NULL)
    
    # # Grafico de puntos
    # plot(x = base_interna()$valores_x, y = base_interna()$valores_y, 
    #      col = valores_usuario()$color, 
    #      xlim =c(valores_usuario()$x_min, valores_usuario()$x_max),
    #      ylim =c(valores_usuario()$y_min, valores_usuario()$y_max),
    #      xlab = valores_usuario()$xlab,
    #      ylab = valores_usuario()$ylab,
    #      cex = 2,  
    #      pch = 19)
    
    plot( x = rep(1, nrow(minibase())), y = minibase()[,1], col = valores_usuario()$color, 
          xlim = c(0.5, 1.5), 
          ylim = c(valores_usuario()$y_min, valores_usuario()$y_max),
          xlab = valores_usuario()$xlab, ylab = valores_usuario()$ylab,
          xaxt = "n")
    
  })
  
  
  
  
  
  
  
  output$armado_grafico <- renderUI({
    
    #  if(is.null(casoRMedic())) return(NULL)
    #  if(casoRMedic() != 2) return(NULL)
    #  if(is.null(valores_usuario())) return(NULL)
    
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