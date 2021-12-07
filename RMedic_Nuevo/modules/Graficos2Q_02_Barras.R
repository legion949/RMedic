


Graficos2Q_02_Barras_UI <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("armado_grafico"))
  
  
}






## Segmento del server
Graficos2Q_02_Barras_SERVER <- function(input, output, session, 
                                        minibase,
                                        decimales,
                                        control_ejecucion,
                                        tablas_2q) {
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  # Control interno 01
  control_interno01 <- reactive({
    
    if(is.null(control_ejecucion())) return(FALSE)
    else return(control_ejecucion())
  })
  
  
  # Tabla FA
  tabla_fa <- reactive({
    
    if(is.null(tablas_2q())) return(NULL)
    
    tabla_fa <- tablas_2q()[[1]][[2]][[1]]
    tabla_fa <- as.matrix(tabla_fa)
    tabla_fa
    
  })
  
  
  
  # Tabla FR
  tabla_fr <- reactive({
    
    if(is.null(tablas_2q())) return(NULL)
    
    tabla_fr <- tablas_2q()[[1]][[5]][[3]]
    tabla_fr <- as.matrix(tabla_fr)
    tabla_fr <- tabla_fr[-nrow(tabla_fr),]
    tabla_fr
    
  })
  
  
  
  # Tabla Porcentajes
  tabla_porcentajes <- reactive({
    
    if(is.null(tablas_2q())) return(NULL)
    
    tabla_porcentajes <- tablas_2q()[[1]][[5]][[4]]
    tabla_porcentajes <- tabla_porcentajes[-nrow(tabla_porcentajes), ]
    tabla_porcentajes <- as.matrix(tabla_porcentajes)
    # tabla_porcentajes <- tabla_porcentajes[, -ncol(tabla_porcentajes)]
    tabla_porcentajes <- gsub("%", "", tabla_porcentajes)
    tabla_porcentajes[1,1] <- as.character(tabla_porcentajes[1,1])
    
    tabla_porcentajes2 <- matrix(NA, nrow(tabla_porcentajes), ncol(tabla_porcentajes))
    for (k in 1:nrow(tabla_porcentajes2)) tabla_porcentajes2[k,] <- as.numeric(tabla_porcentajes[k,])
    colnames(tabla_porcentajes2) <- colnames(tabla_porcentajes)
    rownames(tabla_porcentajes2) <- rownames(tabla_porcentajes)
    tabla_porcentajes2
    
  })
  
  
  # Categorias Filas
  categorias_filas <- reactive({
    
    rownames(tabla_fa())
    
  })
  
  
  # Categorias columnas
  categorias_columnas <- reactive({
    
    colnames(tabla_fa())
    
  })
  
  
  # Cantidad de categorias Var1
  cantidad_categorias_filas <- reactive({
    
    length(categorias_filas())
    
  })
  
  
  
  # Cantidad de categorias Var2
  cantidad_categorias_columnas <- reactive({
    
    length(categorias_columnas())
    
  })
  
  
  # Valor maximo del eje Y...
  valor_maximo <- reactive({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    #   if(is.null(tablas_1q())) return(NULL)
    
    # Valores finales
    candidato_maximo <- c()
    step <- c()
    
    # FA
    candidato_maximo[1] <- max(tabla_fa())
    
    # FR
    candidato_maximo[2] <- 1
    
    # Porcentaje
    candidato_maximo[3] <- 100
    
    # Modificaciones para FA
    {
      div1 <- candidato_maximo[1] / 10
      div2 <- candidato_maximo[1] %/% 10
      cantidad_digitos <- str_count(div2)
      
      
      
      step[1] <- 10^cantidad_digitos
      if (div2 == 0) step[1] <- 1
    }
    
    # Detalles para FR y Porcentaje
    step[2] <- 0.1
    step[3] <- 10
    
    
    if(div1 > div2) {
      candidato_maximo[1] <- (div2 + 1)*step[1]
      # return(candidato)
      
      
    }
    
    
    
    return(list(candidato_maximo, step))
    
  })
  
  
  
  # Salida de colores
  output$MODcolor <- renderUI({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    
    cantidad <- cantidad_categorias_filas()
    label_armado <- paste0("Categoría '", as.vector(categorias_filas()), "'")
    
  
    
    
    
    # colores_internos <- rep("#FF0000", cantidad)
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
  
  
  colores_usuario <- reactive({
    
    
    cantidad <- cantidad_categorias_filas()
    mis_colores <- rep(NA, cantidad)
    
    
    if(length(mis_colores) == 0) return(NULL)
    
    
    for(i in 1:cantidad){
      nombre_input <- paste("col", i, sep="_")
      if(is.null(input[[nombre_input]])) return(NULL)
      
      mis_colores[i] <- input[[nombre_input]]
      
    }
    
    
    
    return(mis_colores)
  })
  
  ############################################################################
  # Objetos reactivos de contorl
  reseteo_logico <- reactiveVal(F)
  aplicador_logico <- reactiveVal(F)
  
  
  xxchange <- reactive({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    paste(input$controlador02)
  })
  
  observeEvent(xxchange(), {
    
    
    aplicador_logico(!aplicador_logico())
    
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
    
    #  reset("coronacion", asis = F)
    #  reset("graf_1q_barras_CantidadColores", asis = F)
    
    
    #  reset("tipo_grafico1", asis = F)
    reset("tipo_grafico1", asis = F)
    reset("tipo_grafico2", asis = F)
    reset("leyenda", asis = F)
    reset("graf_2q_barras_max_FA", asis = F)
    # reset("graf_2q_barras_max_FR", asis = F)
    # reset("graf_2q_barras_max_PORCENTAJE", asis = F)
    reset("graf_2q_barras_ylab_FA", asis = F)
    reset("graf_2q_barras_ylab_FR", asis = F)
    reset("graf_2q_barras_ylab_PORCENTAJE", asis = F)
    reset("graf_2q_barras_xlab", asis = F)
    
    cantidad <- cantidad_categorias_filas()
    colores_internos <- rainbow(cantidad)
    label_armado <- paste0("Categoría '", as.vector(categorias_filas()), "'")
    
    lapply(1:cantidad, function(i) {
      
     nombre_input <- paste("col", i, sep="_")
     # reset(nombre_input, asis = F)
      
      colourpicker::updateColourInput(session,
                                      inputId = nombre_input,
                                      label = label_armado[i],
                                      value = colores_internos[i])
      
    })
    
  })
  
  
  
  #############################################################################
  
  valores_usuario_FA <-   eventReactive(aplicador_logico(), ignoreNULL = FALSE, {
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    if(is.null(colores_usuario())) return(NULL)
    if(is.null(input$leyenda)) return(NULL)
    
    # 
    # if(is.null(input$graf_1q_barras_max_FA)) return(NULL)
    # if(is.null(input$graf_1q_barras_ylab_FA)) return(NULL)
    # if(is.null(input$graf_1q_barras_xlab_FA)) return(NULL)
    # 
    
    valores <- list(col = colores_usuario(),
                    beside = as.logical(input$tipo_grafico2),
                    xlab = input$graf_2q_barras_xlab,
                    ylab = input$graf_2q_barras_ylab_FA,
                    ylim = c(0, input$graf_2q_barras_max_FA),
                    leyenda = as.logical(input$leyenda)
                    
    )
    
    
    return(valores)
  })
  
  valores_usuario_FR <-   eventReactive(aplicador_logico(), ignoreNULL = FALSE, {
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    if(is.null(colores_usuario())) return(NULL)
    if(is.null(input$leyenda)) return(NULL)
    
    # 
    # if(is.null(input$graf_1q_barras_max_FA)) return(NULL)
    # if(is.null(input$graf_1q_barras_ylab_FA)) return(NULL)
    # if(is.null(input$graf_1q_barras_xlab_FA)) return(NULL)
    # 
    
    valores <- list(col = colores_usuario(),
                    beside = as.logical(input$tipo_grafico2),
                    xlab = input$graf_2q_barras_xlab,
                    ylab = input$graf_2q_barras_ylab_FR,
                    ylim = c(0, 1),
                    leyenda = as.logical(input$leyenda)
                    
    )
    
    
    return(valores)
  })
  
  valores_usuario_PORCENTAJE <-   eventReactive(aplicador_logico(), ignoreNULL = FALSE, {
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    if(is.null(colores_usuario())) return(NULL)
    if(is.null(input$leyenda)) return(NULL)
    
    # 
    # if(is.null(input$graf_1q_barras_max_FA)) return(NULL)
    # if(is.null(input$graf_1q_barras_ylab_FA)) return(NULL)
    # if(is.null(input$graf_1q_barras_xlab_FA)) return(NULL)
    # 
    
    valores <- list(col = colores_usuario(),
                    beside = as.logical(input$tipo_grafico2),
                    xlab = input$graf_2q_barras_xlab,
                    ylab = input$graf_2q_barras_ylab_PORCENTAJE,
                    ylim = c(0, 100),
                    leyenda = as.logical(input$leyenda)
                    
    )
    
    
    return(valores)
  })
  
  
  
  
  output$grafico01 <- renderPlot({
    
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    if(is.null(tabla_fa())) return(NULL)
    if(is.null(valores_usuario_FA())) return(NULL)
    
    barplot(tabla_fa(),
            col = valores_usuario_FA()$col,
            beside = valores_usuario_FA()$beside,
            xlab = valores_usuario_FA()$xlab,
            ylab = valores_usuario_FA()$ylab,
            ylim = valores_usuario_FA()$ylim
           )
    
   # cat("valores_usuario_FA()$leyenda: ", valores_usuario_FA()$leyenda, "\n\n")
    
    if(valores_usuario_FA()$leyenda) legend("topright", categorias_filas(),
                                            col= valores_usuario_FA()$col, lwd=8)

  })
  
  
  
  output$grafico02 <- renderPlot({
    
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    if(is.null(tabla_fr())) return(NULL)
    if(is.null(valores_usuario_FR())) return(NULL)
    
    barplot(tabla_fr(),
            col = valores_usuario_FR()$col,
            beside = valores_usuario_FR()$beside,
            xlab = valores_usuario_FR()$xlab,
            ylab = valores_usuario_FR()$ylab,
            ylim = valores_usuario_FR()$ylim
    )
    
    # cat("valores_usuario_FA()$leyenda: ", valores_usuario_FA()$leyenda, "\n\n")
    
    if(valores_usuario_FR()$leyenda) legend("topright", categorias_filas(),
                                            col= valores_usuario_FR()$col, lwd=8)
    
  })
  
  
  output$grafico03 <- renderPlot({
    
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    if(is.null(tabla_porcentajes())) return(NULL)
    if(is.null(valores_usuario_PORCENTAJE())) return(NULL)
    
    barplot(tabla_porcentajes(),
            col = valores_usuario_PORCENTAJE()$col,
            beside = valores_usuario_PORCENTAJE()$beside,
            xlab = valores_usuario_PORCENTAJE()$xlab,
            ylab = valores_usuario_PORCENTAJE()$ylab,
            ylim = valores_usuario_PORCENTAJE()$ylim
    )
    
    # cat("valores_usuario_FA()$leyenda: ", valores_usuario_FA()$leyenda, "\n\n")
    
    if(valores_usuario_PORCENTAJE()$leyenda) legend("topright", categorias_filas(),
                                            col= valores_usuario_PORCENTAJE()$col, lwd=8)
    
  })
  
  
  
  
  output$menu_general01 <- renderUI({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    div(
      br(),
      radioButtons(inputId = ns("tipo_grafico2"),
                   label = "Tipo de barras: ",
                   choices = c("Apilado" = FALSE,
                               "Particionado" = TRUE)
      ),
      br(),
      radioButtons(inputId = ns("leyenda"),
                   label = "Leyenda: ",
                   choices = c("Ocultar" = FALSE,
                               "Agregar" = TRUE)
      ),
      br(),
      textInput(inputId = ns("graf_2q_barras_xlab"),
                label = "Rótulo eje X",
                value = ""),
      br(),
      conditionalPanel(condition = "input.tipo_grafico1 == 1", ns = ns,
          textInput(inputId = ns("graf_2q_barras_ylab_FA"),
                label = "Rótulo eje Y1",
                value = "Frecuencia"),
          br(),
          numericInput(inputId = ns("graf_2q_barras_max_FA"),
                       label = "Frecuencias Absolutas - Máximo del eje Y",
                       value = valor_maximo()[[1]][1],
                       step = valor_maximo()[[2]][1]
                       
          )
          ),
     
      conditionalPanel(condition = "input.tipo_grafico1 == 2", ns = ns,
                       textInput(inputId = ns("graf_2q_barras_ylab_FR"),
                                 label = "Rótulo eje Y2",
                                 value = "Frecuencia Relativa"),
                       br()
      ),
      conditionalPanel(condition = "input.tipo_grafico1 == 3", ns = ns,
                       textInput(inputId = ns("graf_2q_barras_ylab_PORCENTAJE"),
                                 label = "Rótulo eje Y3",
                                 value = "Porcentaje"),
                       br()
      ),
      br(),
      uiOutput(ns("MODcolor")),
      br(),
      bsButton(ns("controlador02"), "Aplicar cambios", 
               type = "toggle", value = TRUE,
               icon("bars"), style = "primary", size = "large"
      ), br(),
      bsButton(ns("reset"), "Resetear gráfico", type = "toggle", value = TRUE,
               icon("bars"), style = "primary", size = "large"
      ),
    )
    
    
    
    
    
  })
  
  
  
  
  
  output$armado_grafico <- renderUI({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
   # if(is.null(input$tipo_grafico1)) return(NULL)
    
    
    div(
      h2("Gráfico de Barras"), br(),
      fluidRow(
        column(6,
               conditionalPanel(condition = "input.tipo_grafico1 == 1", ns = ns,
                  h2("Frecuencias Absolutas"),
                  plotOutput(ns("grafico01"))),
               conditionalPanel(condition = "input.tipo_grafico1 == 2", ns = ns,
                                h2("Frecuencias Relativas"),
                                plotOutput(ns("grafico02"))),
               conditionalPanel(condition = "input.tipo_grafico1 == 3", ns = ns,
                                h2("Porcentajes"),
                                plotOutput(ns("grafico03")))
        ),
        column(3,
               radioButtons(inputId = ns("tipo_grafico1"),
                            label = "Tipo de frecuencias: ",
                            choices = c("Frecuencias Absolutas" = 1,
                                        "Frecuencias Relativas" = 2,
                                        "Porcentajes" = 3)
               )
               ),
        column(3,
               bsButton(inputId = ns("controlador01"), 
                        label = "Mostrar/Ocultar opciones",
                        icon = icon("bars"), 
                        type = "toggle", 
                        #   value = FALSE,
                        value = TRUE,
                        style = "primary", 
                        size = "large"
               ),
               br(),
               conditionalPanel(condition = "input.controlador01", ns = ns,
                                div(id = ns("James01"), uiOutput(ns("menu_general01")))
               )
               )
      )
    )
    
    
            
    
  })
  
  
 
}


