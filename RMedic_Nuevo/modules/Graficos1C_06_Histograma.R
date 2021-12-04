


Graficos1C_06_Histograma_UI <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("armado_grafico"))
  
  
}






## Segmento del server
Graficos1C_06_Histograma_SERVER <- function(input, output, session, 
                                            minibase,
                                            decimales,
                                            control_ejecucion,
                                            tablas_1c) {
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  # Control interno 01
  control_interno01 <- reactive({
    
    if(is.null(control_ejecucion())) return(FALSE)
    else return(control_ejecucion())
  })
  
  
  
  # Tabla inicial para generar el histograma
  tabla_histograma_inicial <- reactive({
    
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
    )
    
    
    
    
    salida[[9]][,2] <- as.character(salida[[9]][,2])
    salida[[9]][,3] <- as.character(salida[[9]][,3])
    salida[[9]][,5] <- as.character(salida[[9]][,5])
    
    rownames(salida[[9]]) <- c(1:nrow(salida[[9]]))
    
    # Return Exitoso
    return(salida[[9]])
    
    
  }) 
  
  
  
  # "n" de la muestra
  n_total <- reactive({
    # Control interno 01
    if(!control_interno01()) return(NULL)
    as.numeric(as.character(tabla_histograma_inicial()[1,3]))
    
  })
  
  
  
  # Frecuencia maxima del histograma
  frecuencia_maxima <- reactive({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    # Por problemas tecnicos de recursividad entre
    # la tabla, los input, y el grafico, y no pude marcar
    # un valor maximo. Por eso, simplemente impongo un valor 0.
    
    return(0)
    
  })
  
  # Cantidad de categorias de la tabla inicial
  cantidad_de_categorias_inicial <- reactive({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    nrow(tabla_histograma_inicial())
  })
  
  
  # Valores de corte de la tabla inicial
  valores_de_cortes_inicial <- reactive({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    inicio <- tabla_histograma_inicial()[,1]
    
    metralla <- unlist(strsplit(inicio, ";"))
    metralla <- gsub(" ", "", metralla)
    metralla <- gsub("[(]", "", metralla)
    metralla <- gsub("[)]", "", metralla)
    metralla <- gsub("[[]", "", metralla)
    metralla <- gsub("[]]", "", metralla)
    #metralla <- metralla[c(T, F)]
    metralla <- as.numeric(metralla)
    metralla <- unique(metralla)
    metralla <- sort(metralla)
    
    return(metralla)
    
    
  })
  
  # # # Nuevo Histograma
  # Tabla del histograma final
  tabla_histograma_final <- reactive({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    
    
    # Nota: al valor input$x_breaks lo tuve que poner
    #      como na.omit(input$x_breaks)[1] por que algunas veces
    #      otorga un vector con dos valores, pero uno de ellos es NA.
    
    
    salida <-  RMedic_1c_tablas(input_base =  minibase(),
                                input_decimales = decimales(),
                                input_min = valores_usuario()$x_min,
                                input_max = valores_usuario()$x_max,
                                input_breaks = valores_usuario()$x_breaks,
                                input_side = valores_usuario()$x_side
    )
    
    # salida <-  RMedic_1c_tablas(input_base =  minibase(),
    #                             input_decimales = decimales(),
    #                             input_min = input$x_min,
    #                             input_max = input$x_max,
    #                             input_breaks = na.omit(input$x_breaks)[1],
    #                             input_side = input$x_side
    # )
    
    
    salida[[9]][,2] <- as.character(salida[[9]][,2])
    salida[[9]][,3] <- as.character(salida[[9]][,3])
    salida[[9]][,5] <- as.character(salida[[9]][,5])
    
    rownames(salida[[9]]) <- c(1:nrow(salida[[9]]))
    
    # Return Exitoso
    return(salida[[9]])
    
    
  })  
  
  # Valores de corte final
  valores_de_cortes_final <- reactive({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    inicio <- tabla_histograma_final()[,1]
    
    metralla <- unlist(strsplit(inicio, ";"))
    metralla <- gsub(" ", "", metralla)
    metralla <- gsub("[(]", "", metralla)
    metralla <- gsub("[)]", "", metralla)
    metralla <- gsub("[[]", "", metralla)
    metralla <- gsub("[]]", "", metralla)
    #metralla <- metralla[c(T, F)]
    metralla <- as.numeric(metralla)
    metralla <- unique(metralla)
    metralla <- sort(metralla)
    
    return(metralla)
    
    
  })
  
  
  
  output$tabla_histograma_final <- renderTable(rownames = TRUE, align= "c",{
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    tabla_histograma_final()
  })
  
  
  
  # Objetos reactivos de contorl
  reseteo_logico <- reactiveVal(F)
  aplicador_logico <- reactiveVal(F)
  
  
  # Valores iniciales
  valores_iniciales <-  reactive({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    if(is.null(minibase())) return(NULL)
    if(is.null(tabla_histograma_inicial())) return(NULL)
    if(is.null(frecuencia_maxima())) return(NULL)
    #############
    
    
    
    valores <- list(x_min = min(valores_de_cortes_inicial()), #  min(minibase()[,1]),
                    x_max = max(valores_de_cortes_inicial()),
                    y_min = 0,
                    y_max = n_total(),
                    #  y_limite = frecuencia_maxima(),
                    y_limite = 0,
                    x_side = T, 
                    x_breaks = cantidad_de_categorias_inicial(),
                    xlab = colnames(minibase())[1],
                    ylab = "Frecuencia",
                    color = c("#FF0000")
                    #  color = c("#FF0000", "#00FF00")
    )
    
    
    
    return(valores)
  })
  
  # Colores seleccionados por el usuario
  colores_usuario <- reactive({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
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
  
  
  
  valores_usuario <-   eventReactive(aplicador_logico(), ignoreNULL = FALSE, {
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    if(is.null(minibase())) return(NULL)
    if(is.null(valores_iniciales())) return(NULL)
    # if(is.null(colores_usuario())) return(NULL)
    
    
    valores <- valores_iniciales()
    
    # Valores X
    if(!is.null(input$x_min)) valores$x_min <- input$x_min else valores$x_min <- valores_iniciales()$x_min
    if(!is.null(input$x_max)) valores$x_max <- input$x_max else valores$x_max <- valores_iniciales()$x_max
    
    # Valores Y
    if(!is.null(input$y_min)) valores$y_min <- input$y_min else valores$y_min <- valores_iniciales()$y_min
    if(!is.null(input$y_max)) valores$y_max <- input$y_max else valores$y_max <- valores_iniciales()$y_max
    if(!is.null(input$y_limite)) valores$y_limite <- input$y_limite else valores$y_limite <- valores_iniciales()$y_limite
    
    # Lado de los corchetes (x_side)
    if(!is.null(input$x_side)) valores$x_side <- input$x_side else valores$x_side <- valores_iniciales()$x_side
    
    # Cantidad de categorias (x_breaks)
    if(!is.null(input$x_breaks)) valores$x_breaks <- input$x_breaks else valores$x_breaks <- valores_iniciales()$x_breaks
    
    # Rotulos
    if(!is.null(input$xlab))  valores$xlab <- input$xlab  else valores$xlab <- valores_iniciales()$xlab
    if(!is.null(input$ylab))  valores$ylab <- input$ylab  else valores$ylab <- valores_iniciales()$ylab
    
    
    # Color
    if(!is.null(colores_usuario())) valores$color <- colores_usuario() else valores$color <- valores_iniciales()$color
    
    
    
    
    # Correccion interna para los valores que estan fuera de rango
    # # La variable no puede ser recategorizada en menos de una categoria
    if(!is.null(valores$x_breaks)) if(valores$x_breaks < 1) valores$x_breaks <- 1
    
    # # El minimo del eje X debe ser mayor o igual al minimo de la variable
    if(!is.null(valores$x_min)) if(valores$x_min > valores_iniciales()$x_min) valores$x_min <- valores_iniciales()$x_min
    
    # # El maximo del eje Y debe ser mayor o igual al maximo de la variable
    if(!is.null(valores$x_max)) if(valores$x_max < valores_iniciales()$x_max) valores$x_max <- valores_iniciales()$x_max
    
    
    
    
    return(valores)
  })
  
  
  
  
  observeEvent(minibase(), {
    
    reseteo_logico(!reseteo_logico())
    delay(400,     aplicador_logico(!aplicador_logico()))
    # aplicador_logico(!aplicador_logico())
    
  })
  
  
  # # Controlador 01
  # observeEvent(input$controlador01, {
  #   
  #   delay(100, shinyjs::toggle(ns("James01"), asis = T, anim = TRUE, animType = "fade"))
  #   
  # })
  
  
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
  
  
  # # Reseteo forzado
  # observeEvent(input$ylab, {
  #   
  #   
  #   # Este reseteo forzado ocurre por que cuando cambia de variable
  #   # no termina de resetear los valores del grafico anterior. 
  #   # Entre otras cosas pasaba que como ylab salia la nueva variable, 
  #   # pero no el resto de los valores, entonces cuando pasa que
  #   # ylab es una columna elegida, se resetea todo.
  #   #
  #   # Seria un golazo encontrar otra forma para que al cambiar de variable
  #   # efectivamente resetee tooodo.
  #   if(input$ylab == colnames(minibase())[1]) {
  #     
  #     if(input$ylab != valores_usuario()$ylab) {
  #       
  #       delay(1000, aplicador_logico(!aplicador_logico()))
  #       
  #       
  #     }
  #   }
  #   
  #   
  #   
  # })
  # 
  
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
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    
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
  
  
  # Minimo eje X
  output$texto_ayudaMin_x <- renderText({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    texto <- paste0("El límite inferior del eje X debe ser igual o menor al mínimo 
    valor de la variable. ", "En este caso debe ser igual o menor a ",
                    valores_iniciales()$x_min, ".")
    
    if(input$x_min > valores_iniciales()$x_min) return(texto) else return(NULL)
    
  })
  
  # Maximo eje X
  output$texto_ayudaMax_x <- renderText({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    texto <- paste0("El límite superior del eje X debe ser igual o mayor al máximo
    de la variable. ", "En este caso debe ser igual o mayor a ",
                    valores_iniciales()$x_max, ".")
    
    if(input$x_max < valores_iniciales()$x_max) return(texto) else return(NULL)
    
  })
  
  # Minimo eje Y
  output$texto_ayudaMin_y <- renderText({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    texto <- "El límite inferior del eje Y debe ser igual o mayor a cero ya que
    el eje Y representa a las frecuencias de los valores de la variable."
    
    if(input$y_min < valores_iniciales()$y_min) return(texto) else return(NULL)
    
  })

  # Maximo eje Y
  output$texto_ayudaMax_y <- renderText({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    texto <- paste0("El límite superior del eje Y debe ser igual o mayor al máximo
    valor de frecuencias para las variables. En este caso debe ser mayor o igual a ",
                    max(tabla_histograma_final()), ".")
    
    if(input$y_max < max(tabla_histograma_final())) return(texto) else return(NULL)
    
  })
  
  
  # Cantidad de categorias
  output$texto_x_breaks <- renderText({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    texto <- "La cantidad mínima de categorías posibles es 1."
    
    if(input$x_breaks < 1) return(texto)
    
    
  })
  
  
  
  # Controladores para el histograma
  output$menu_general01 <- renderUI({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    cantidad_cortes <- nclass.Sturges(minibase()[,1])
    tabla <- table(minibase()[,1])
    cantidad_categorias <- length(names(tabla))
    if(cantidad_categorias < cantidad_cortes) cantidad_cortes <- cantidad_categorias
    
    div(
      fluidRow(
        column(6),
        column(6, 
               uiOutput(ns("MODcolor"))
        )
      ), br(), br(),
        fluidRow(
            column(6,
                   numericInput(
                     inputId = ns("x_min"),
                     label = "Valor mínimo eje X: ",
                     value = valores_iniciales()$x_min,
                     min = NA,
                     max = valores_iniciales()$x_min,
                     step = 0.01
                   ),
                   textOutput(ns("texto_x_min"))
            ),
            column(6,
                   numericInput(
                     inputId = ns("x_max"),
                     label = "Valor máximo eje X: ",
                     value = valores_iniciales()$x_max,
                     min = valores_iniciales()$x_max,
                     max = NA,
                     step = 0.01
                   ),
                   textOutput(ns("texto_x_max"))
            )
          ),
          br(), br(),
          fluidRow(
            column(6,
                   numericInput(
                     inputId = ns("y_min"),
                     label = "Valor mínimo eje Y: ",
                     value = valores_iniciales()$y_min,
                     max = valores_iniciales()$y_min,
                     min = valores_iniciales()$y_min,
                     step = 0.01
                   ),
                   textOutput(ns("texto_y_min"))
            ),
            column(6,
                   numericInput(
                     inputId = ns("y_max"),
                     label = "Valor máximo Y: ",
                     value = valores_iniciales()$y_max,
                     min = valores_iniciales()$y_limite,
                     max = NA,
                     step = 0.01
                   ),
                   textOutput(ns("texto_y_max"))
            )
          ),
          br(), br(),
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
          fluidRow(
            column(6,
                   radioButtons(inputId = ns("x_side"), 
                                label = "Cierre del intervalo: ", 
                                choices = c("A la Derecha" = T , "A la Izquierda" = F),
                                selected = valores_iniciales()$x_side
                                
                   )
            ),
            column(6, 
                   numericInput(
                     inputId = ns("x_breaks"),
                     label = "Cantidad de intervalos: ",
                     value = valores_iniciales()$x_breaks,
                     min = 1,
                     max = NA,
                     step = 1,
                     width = NULL
                   ),
                   textOutput(ns("texto_x_breaks"))
            )
          ),
          br(), br(),
          bsButton(ns("reset"), "Resetear Gráfico", type = "toggle", value = TRUE,
                   icon("bars"), style = "primary", size = "large"
          ),
          bsButton(ns("controlador02"), "Aplicar todos los cambios", type = "toggle", value = TRUE,
                   icon("bars"), style = "primary", size = "large"
          )
      )
  
    
    
  })
  
  
  
  
  output$grafico01 <- renderPlot({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    # Suprimimos un cartel del warning que sale al generar el histograma.
    # No es un error, es solo un aviso.
    suppressWarnings(
      
      hist(minibase()[,1], 
           col = valores_usuario()$color[1],
           main = "",
           xlab = valores_usuario()$xlab,
           ylab = valores_usuario()$ylab,
           xlim = c(min(valores_de_cortes_final()), max(valores_de_cortes_final())),
           #  xlim = c(valores_usuario()$x_min, valores_usuario()$x_max),
           ylim = c(valores_usuario()$y_min, valores_usuario()$y_max),
           include.lowest = TRUE, # No cambiar!
           #   right = valores_usuario_especiales()$x_side,
           freq =TRUE,
           #   freq =FALSE,
           xaxt = "n",
           #   breaks = length(valores_de_cortes())
           breaks = valores_de_cortes_final()
      )
    )
    
    axis(side=1, at= valores_de_cortes_final(), labels = TRUE)
    
    
    
    
  })
  
  
  
  
  
  
  output$armado_grafico <- renderUI({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    
     # if(is.null(valores_usuario())) return(NULL)
    
    div(
      h2("Histograma"),
      fluidRow(
        column(7,
               plotOutput(ns("grafico01")),
               br(),
               br(),
               h2("Distribución de Frecuencias"),
               tableOutput(ns("tabla_histograma_final"))
        ),
        column(5,
               bsButton(inputId = ns("controlador01"), 
                        label = "Mostrar/Ocultar opciones gráficas",
                        icon = icon("bars"), 
                        type = "toggle", 
                      #  value = FALSE,
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