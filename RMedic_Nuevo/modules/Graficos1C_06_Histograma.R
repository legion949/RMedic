## Segmento del UI
Graficos1C_06_Histograma_UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("rejunte_grafico"))
  
  
}




## Segmento del server
Graficos1C_06_Histograma_SERVER <- function(input, output, session, 
                                            minibase,
                                            batalla_naval,
                                            decimales,
                                            casoRMedic,
                                            tablas_1c) {
  
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
 
  tabla_histograma_inicial <- reactive({
    
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 2) return(NULL)
    
    
    
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
 
  n_total <- reactive({
    
    as.numeric(as.character(tabla_histograma_inicial()[1,3]))
    
  })
  
  frecuencia_maxima <- reactive({
    
    0
    
  })
  
  cantidad_de_categorias_inicial <- reactive({
    nrow(tabla_histograma_inicial())
  })
  
  valores_de_cortes_inicial <- reactive({
    
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 2) return(NULL)
    
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
  
  # Todas las tablas 1C
  tabla_histograma_final <- reactive({
    
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 2) return(NULL)
    
    
    
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
  
  valores_de_cortes_final <- reactive({
    
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 2) return(NULL)
    
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
  
  
  
  MEGA_ARMADO <- reactive({
    
  parte01 <- paste0(valores_de_cortes_final(), sep="-")
  
  parte02 <- unlist(valores_usuario())

  parte03 <- c(parte01, parte02)
      
  dim(parte03) <- c(1, length(parte03))
  
  # colnames(parte03) <- c(1:length(parte01), names(valores_usuario()))
  parte03
  })
  
  output$MEGA_ARMADO <- renderTable({
    
    MEGA_ARMADO()
  })
  
  
  output$tabla_histograma_final <- renderTable(rownames = TRUE, align= "c",{
    
    tabla_histograma_final()
  })
  
  
  
  valores_iniciales <-  reactive({
    
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 2) return(NULL)
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
                    color = c("#FF0000")
                  #  color = c("#FF0000", "#00FF00")
    )
    
    
    
    return(valores)
  })
  
  ################################################
  reseteo_logico <- reactiveVal(F)
  aplicador_logico <- reactiveVal(F)
  
  observeEvent(input$controlador02, {
    
    
    aplicador_logico(!aplicador_logico())
    
  })
  
  
  observeEvent(input$reset, {
    
    reseteo_logico(!reseteo_logico())
    delay(400,     aplicador_logico(!aplicador_logico()))
  })
  
  # Reseteo
  
  observeEvent(reseteo_logico(),{
    
  
    label_armado <- "Color..."
    colores_internos <- valores_iniciales()$color[1]
    cantidad <- length(colores_internos)
    
    lapply(1:cantidad, function(i) {
      
      nombre_input <- paste("col", i, sep="_")
      
      colourpicker::updateColourInput(session,
                                      inputId = nombre_input,
                                      label = label_armado[i],
                                      value = colores_internos[i])
      
    })
    
    updateNumericInput(session,
                       inputId = "x_min",
                       label = "Valor mínimo eje X",
                       value = valores_iniciales()$x_min,
                       min = NA,
                       max = valores_iniciales()$x_min
    )
    
    updateNumericInput(session,
                       inputId = "x_max",
                       label = "Valor máximo eje X",
                       value = valores_iniciales()$x_max,
                       min = valores_iniciales()$x_max,
                       max = NA
    )
    
    
    updateNumericInput(session,
                       inputId = "y_min",
                       label = "Valor mínimo eje Y: ",
                       value = valores_iniciales()$y_min,
                       max = valores_iniciales()$y_min,
                       min = valores_iniciales()$y_min,
                       step = 0.01
    )
    
    
    updateNumericInput(session,
                       inputId = "y_max",
                       label = "Valor máximo Y: ",
                       value = valores_iniciales()$y_max,
                       min = valores_iniciales()$y_limite,
                       max = NA,
                       step = 0.01
    )
    # 
    # 
    
    # 
    # 
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
    
    
    
    updateRadioButtons(session,
                       inputId = "x_side", 
                       label = "Cierre del intervalo: ", 
                       choices = c("A la Derecha" = T , 
                                   "A la Izquierda" = F),
                       selected = valores_iniciales()$x_side
    )
    
    
    updateNumericInput(session,
                       inputId = "x_breaks",
                       label = "Cantidad de intervalos: ",
                       value = valores_iniciales()$x_breaks,
                       min = 1,
                       max = NA,
                       step = 1
    )
    
    
    
   
    
    # label_armado <- "Color..."
    # colores_internos <- valores_iniciales()$color
    # cantidad <- length(colores_internos)
    # 
    # lapply(1:cantidad, function(i) {
    # 
    #   nombre_input <- paste("col", i, sep="_")
    # 
    #   freezeReactiveValue(input, "nombre_input")
    #   colourpicker::updateColourInput(session,
    #                                   inputId = nombre_input,
    #                                   label = label_armado[i],
    #                                  # value = colores_internos[i])
    #                                  value = "#FF0000")
    # 
    # })
    
    
  
  })
  
  
  
  if ( 1 == 2) {
  # Valores nuevos aplicados...
  observeEvent(aplicador_logico(),{
    
    
    
    updateNumericInput(session,
                       inputId = "x_min",
                       label = "Valor mínimo eje X",
                       value = valores_usuario()$x_min,
                       min = NA,
                       max = valores_usuario()$x_min
    )
    
    updateNumericInput(session,
                       inputId = "x_max",
                       label = "Valor máximo eje X",
                       value = valores_usuario()$x_max,
                       min = valores_usuario()$x_max,
                       max = NA
    )
    
    
    updateNumericInput(session,
                       inputId = "y_min",
                       label = "Valor mínimo eje Y: ",
                       value = valores_usuario()$y_min,
                       max = valores_usuario()$y_min,
                       min = valores_usuario()$y_min,
                       step = 0.01
    )
    
    
    updateNumericInput(session,
                       inputId = "y_max",
                       label = "Valor máximo Y: ",
                       value = valores_usuario()$y_max,
                       min = valores_usuario()$y_limite,
                       max = NA,
                       step = 0.01
    )
    # 
    # 
    
    # 
    # 
    updateTextInput(session,
                    inputId = "ylab",
                    label = "Rótulo eje Y",
                    value = valores_usuario()$ylab
    )
    
    updateTextInput(session,
                    inputId = "xlab",
                    label = "Rótulo eje X",
                    value = valores_usuario()$xlab
    )
    
    
    
    updateRadioButtons(session,
                       inputId = "x_side", 
                       label = "Cierre del intervalo: ", 
                       choices = c("A la Derecha" = T , 
                                   "A la Izquierda" = F),
                       selected = valores_usuario()$x_side
    )
    
    
    updateNumericInput(session,
                       inputId = "x_breaks",
                       label = "Cantidad de intervalos: ",
                       value = valores_usuario()$x_breaks,
                       min = 1,
                       max = NA,
                       step = 1
    )
    
  })
  ##################################################################
  }
  
  
 
  valores_usuario <-   eventReactive(aplicador_logico(), ignoreNULL = FALSE, {
    
    if(is.null(minibase())) return(NULL)
    if(is.null(valores_iniciales())) return(NULL)
   # if(is.null(colores_seleccionados())) return(NULL)
    
    
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
    
    # Color
    if(!is.null(colores_seleccionados())) valores$color <- colores_seleccionados() else valores$color <- valores_iniciales()$color

    
      
    
    # Correccion interna para los valores que estan fuera de rango
    # # La variable no puede ser recategorizada en menos de una categoria
    if(!is.null(valores$x_breaks)) if(valores$x_breaks < 1) valores$x_breaks <- 1
    
    # El minimo del eje X debe ser mayor o igual al minimo de la variable
    if(!is.null(valores$x_min)) if(valores$x_min > valores_iniciales()$x_min) valores$x_min <- valores_iniciales()$x_min
    
    # El maximo del eje Y debe ser mayor o igual al maximo de la variable
    if(!is.null(valores$x_max)) if(valores$x_max < valores_iniciales()$x_max) valores$x_max <- valores_iniciales()$x_max
    
    
    
  
    return(valores)
  })
  
  

  observeEvent(input$controlador01, {
    
    shinyjs::toggle(ns("James01"), asis = T, anim = TRUE, animType = "fade")
    
  })
  
  
 
  
  # Salida de colores
  output$MODcolor <- renderUI({
    
 # ACAA PARAAAAAAAAAA  
# Queda ver si cambio todo... para que los input tomen a "valores_iniciales()"
# y yo aparte hago un update de los valores como cuando hace reseteo, pero para
# llevar a cabo la implementacion!

    
    colores_internos <- valores_iniciales()$color
 #   colores_internos <- valores_usuario()$color
 #   colores_internos <- "#FF0000"
    
    cantidad <- length(colores_internos)
    ordenamiento <- c(1:cantidad)
    label_armado <- paste0("Color ", ordenamiento)
    if(cantidad == 1) label_armado <- "Color..." 
    
    nombre_input <- paste("col", ordenamiento, sep="_")
    


    lapply(1:cantidad, function(i) {
      
      div(
        colourpicker::colourInput(inputId = ns(nombre_input[i]),
                                  label = label_armado[i], 
                                  value = colores_internos[i]), br()
      )
      
    })
    
  })
  
 
  
#  colores_seleccionados <-  eventReactive(input$controlador02,({
    
  colores_seleccionados <-  reactive({
    
   # colores_internos <- valores_iniciales()$color
   # cantidad <- length(colores_internos)
  #  cantidad <- 1
    
    cantidad <- length(valores_iniciales()$color)
    
    ordenamiento <- c(1:cantidad)
    label_armado <- paste0("Color ", ordenamiento)
    if(cantidad == 1) label_armado <- "Color..." 
    
    nombre_input <- paste("col", ordenamiento, sep="_")
    
    
    

    mis_colores <- c()
    for(i in 1:cantidad){ 
  
      
      if(is.null(input[[nombre_input[i]]])) return(NULL)
      
      mis_colores[i] <- input[[nombre_input[i]]]
      
      cat(input[[nombre_input[i]]], "\n")
    }
    
    # Return Exitoso
    return(mis_colores)
    
  })
  
  
  # TExtos
  output$texto_x_min <- renderText({
    texto <- paste0("El límite inferior del eje X debe ser igual o menor al mínimo 
    valor de la variable, que es ", valores_iniciales()$x_min, ".")
    
    if(!(input$x_min <= valores_iniciales()$x_min)) return(texto) else return(NULL)
    
  })
  
  
  output$texto_x_max <- renderText({
    texto <- paste0("El límite superior del eje X debe ser igual o mayor al máximo 
    valor de la variable, que es ", valores_iniciales()$x_max, ".")
    
    if(!(input$x_max >= valores_iniciales()$x_max)) return(texto) else return(NULL)
    
  })
  
  output$texto_y_min <- renderText({
    texto <- paste0("El límite inferior del eje Y representa debe ser igual a
                     en este tipo de gráfico.")
    
    if(!(input$y_min == 0)) return(texto) else return(NULL)
    
  })
  
  
  output$texto_y_max <- renderText({
    texto <- paste0("El límite superior del eje Y debe ser igual o mayor a cero.")
    
    if(!(input$y_max >= 0)) return(texto) else return(NULL)
    
  })
  
  
  output$texto_x_breaks <- renderText({
    texto <- "La cantidad mínima de categorías posibles es 1."
  
    cat("input$x_breaks: ", input$x_breaks, "\n")
    
    if(input$x_breaks < 1) return(texto)
    
  
  })
  
  

  
 output$Controlador_1c_RMedic <- renderUI({
    
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 2) return(NULL)
    
    cantidad_cortes <- nclass.Sturges(minibase()[,1])
    tabla <- table(minibase()[,1])
    cantidad_categorias <- length(names(tabla))
    if(cantidad_categorias < cantidad_cortes) cantidad_cortes <- cantidad_categorias
    
    div(
      bsButton(inputId = ns("controlador01"), 
               label = "Mostrar/Ocultar opciones gráficas",
               icon = icon("bars"), 
               type = "toggle", 
               value = FALSE,
               style = "primary", 
               size = "large"
      ), br(),
      br(), 
      br(),
    div(id = ns("James01"), 
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
      fluidRow(
        column(6),
        column(6, 
               uiOutput(ns("MODcolor"))
        )
        ), br(), br(),
      bsButton(ns("reset"), "Resetear", type = "toggle", value = TRUE,
               icon("bars"), style = "primary", size = "large"
      ),
      bsButton(ns("controlador02"), "Aplicar todos los cambios", type = "toggle", value = TRUE,
               icon("bars"), style = "primary", size = "large"
      )
     )
    )
    
    
  })
  
  
  if (1 == 2) {
  # Variable criterio de inclusion
  observeEvent(input[["x_min"]],{
    
    if(input[["x_min"]] > min(minibase()[,1])) {
      
      updateNumericInput(session, inputId = "x_min",
                         label = "Valor mínimo eje X: ",
                         value = min(minibase()[,1]),
                         min = NA,
                         max = min(minibase()[,1]),
                         step = 0.01
      )
      
      
      
    }
  })
  
  
  # Variable criterio de inclusion
  observeEvent(input[["x_max"]],{
    
    if(input[["x_max"]] < max(minibase()[,1])) {
      
      updateNumericInput(session, inputId = "x_max",
                         label = "Valor máximo: ",
                         value = max(minibase()[,1]),
                         min = max(minibase()[,1]),
                         max = NA,
                         step = 0.01
      )
      
      
      
    }
  })
  }
  
 
 
 output$grafico01 <- renderPlot({
   
   if(is.null(casoRMedic())) return(NULL)
   if(casoRMedic() != 2) return(NULL)
   ###   if(is.null(valores_usuario())) return(NULL)
   
  cat("valores_usuario()$color[1]: ", valores_usuario()$color[1], "\n")
   
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
   
   axis(side=1, at= valores_de_cortes_final(), labels = TRUE)
   
   
 
   
 })
 
 
 
  
  output$rejunte_grafico <- renderUI({
    
    # Especificaciones de cumplimiento
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 2) return(NULL)
    
    # Si es el caso 1, seguimos!
    div(
      fluidRow(
      column(7,
      h2("Histograma"),
      plotOutput(ns("grafico01")),
      h2("Distribución de Frecuencias"),
      tableOutput(ns("tabla_histograma_final"))
      ),
      column(5,
      uiOutput(ns("Controlador_1c_RMedic")),
      )
      ),
      br(), br(),
   #   tableOutput(ns("MEGA_ARMADO"))
   br()
    )
    
  })
  
  
  
  
  
  
  
  
  
}


