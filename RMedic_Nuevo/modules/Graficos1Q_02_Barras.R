


Graficos1Q_02_Barras_UI <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("rejunte_barras"))

  
}






## Segmento del server
Graficos1Q_02_Barras_SERVER <- function(input, output, session, 
                              minibase, 
                              batalla_naval,
                              decimales,
                              casoRMedic,
                              DF_interna) {
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  # Cantidad de categorias
  cantidad_categorias <- reactive({
    return(nrow(DF_interna()))
    
  })
 
 

  # Valor maximo del eje Y...
  valor_maximo <- reactive({
    
 #   if(is.null(DF_interna())) return(NULL)
    
    # Valores finales
    candidato_maximo <- c()
    step <- c()
    
    # FA
    candidato_maximo[1] <- max(as.numeric(as.character(DF_interna()[,2])))
    
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
  
  
 
  # Coronacion de las barras...
  corona <- reactive({
    
    la_corona <- list()
    
    if(is.null(input$coronacion)) {
      la_corona[[1]] <- rep("", cantidad_categorias())
      la_corona[[2]] <- la_corona[[1]]
      la_corona[[3]] <- la_corona[[1]]
    } else
      if(input$coronacion == 1) {
        
        la_corona[[1]] <- rep("", cantidad_categorias())
        la_corona[[2]] <- la_corona[[1]]
        la_corona[[3]] <- la_corona[[1]]
      } else 
        if(input$coronacion == 2) {
          
          la_corona[[1]] <- as.character(as.vector(DF_interna()[,2]))
          la_corona[[2]] <- as.character(as.vector(DF_interna()[,5]))
          la_corona[[3]] <- as.character(as.vector(DF_interna()[,6]))
        } else
          if(input$coronacion == 3) {
            
            la_corona[[1]] <- as.character(as.vector(DF_interna()[,2]))
            la_corona[[2]] <- la_corona[[1]]
            la_corona[[3]] <- la_corona[[1]]
          } else
            if(input$coronacion == 4) {
              
              la_corona[[1]] <- as.character(as.vector(DF_interna()[,4]))
              la_corona[[2]] <- la_corona[[1]]
              la_corona[[3]] <- la_corona[[1]]
            } else
              if(input$coronacion == 5) {
                
                la_corona[[1]] <- as.character(as.vector(DF_interna()[,5]))
                la_corona[[2]] <- la_corona[[1]]
                la_corona[[3]] <- la_corona[[1]]
              } else
                if(input$coronacion == 6) {
                  
                  la_corona[[1]] <- as.character(as.vector(DF_interna()[,6]))
                  la_corona[[2]] <- la_corona[[1]]
                  la_corona[[3]] <- la_corona[[1]]
                } else
                  if(input$coronacion == 7) {
                    
                    la_corona[[1]] <- as.character(as.vector(DF_interna()[,7]))
                    la_corona[[2]] <- la_corona[[1]]
                    la_corona[[3]] <- la_corona[[1]]
                  } 
    
    return(la_corona)
  }) 
  
  
  
  
  observeEvent(input$controlador01, {
    
    shinyjs::toggle(ns("James04"), asis = T, anim = TRUE, animType = "fade")
    
  })
  
  observeEvent(input$controlador02, {
    shinyjs::toggle(ns("James01"), asis = T, anim = TRUE, animType = "slide")
    shinyjs::toggle(ns("James02"), asis = T, anim = TRUE, animType = "slide")
    shinyjs::toggle(ns("James03"), asis = T, anim = TRUE, animType = "slide")
    
  })
  
  
  
  colores_seleccionados <- reactiveVal({
    
    mis_colores <- "red"
    names(mis_colores) <- paste0("color", c(1:length(mis_colores)))
    
    mis_colores
  })
  
  
  colores_seleccionados2 <- reactive({
   # if (is.null(DF_interna())) return(NULL)
    
    cantidad <- c()
    
    if (is.null(input$graf_1q_barras_CantidadColores)){
      
      cantidad <- 1
      mis_colores <- "#FF0000"
      return(mis_colores)
    } 
    
    
    
    cantidad <- c()
    if (input$graf_1q_barras_CantidadColores == "1"){
      cantidad <- 1
      armado <- "Color..."
    } else
      if(input$graf_1q_barras_CantidadColores == "2"){
        cantidad <- nrow(DF_interna())
        armado <- as.vector(DF_interna()[,1])
        
      }
    

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
  
  
  
  observeEvent(input$controlador03, {
    
    if(length(colores_seleccionados2()) > 0) {
      #  colores_seleccionados("blue")
      #  nombre_input <- paste("col", 1, sep="_")
      #  cat("input[[nombre_input]]: ", input[[nombre_input]], "\n")
      
      colores_seleccionados(colores_seleccionados2())
    }
  })
  
  # Salida de colores
  output$MODcolor <- renderUI({
    
#    if (is.null(DF_interna())) return(NULL)
#    if (is.null(input$graf_1q_barras_CantidadColores)) return(NULL)
    
    
    
    cantidad <- c()
    if (input$graf_1q_barras_CantidadColores == "1"){
      cantidad <- 1
      label_armado <- "Color..."
    } else
      if(input$graf_1q_barras_CantidadColores == "2"){
        cantidad <- nrow(DF_interna())
        label_armado <- paste0("Categoría '", as.vector(DF_interna()[,1]), "'")
        
      }
    
    # colores_internos <- rainbow(cantidad)
    # if(cantidad == 1) colores_internos <- "#FF0000"
    colores_internos <- rep("#FF0000", cantidad)
    
    lapply(1:cantidad, function(i) {
      
      nombre_input <- paste("col", i, sep="_")
      div(
        colourpicker::colourInput(inputId = ns(nombre_input),
                                  label = label_armado[i], 
                                  value = colores_internos[i]), br()
      )
      
    })
    
  })
  
  
  
  output$menu_general01 <- renderUI({
    
    div(
      radioButtons(inputId = ns("coronacion"),
                   label = "Detalle sobre las barras...",
                   choices = c("Sin detalle" = 1,
                               "Detalle correspondiente" = 2,
                               "Frecuencias Absolutas" = 3,
                               "Cociente al total " = 4,
                               "Frecuencias Relativas" = 5,
                               "Porcentajes" = 6,
                               "FA (%)" = 7)
      ),
      br(),
      radioButtons(inputId = ns("graf_1q_barras_CantidadColores"),
                   label = "Coloración General",
                   choices = c("Color único" = 1, 
                               "Un color por categoría" = 2)
      ),
      br(),
      uiOutput(ns("MODcolor")),
      br(),
      bsButton(ns("controlador03"), "Aplicar cambio de color", type = "toggle", value = TRUE,
               icon("bars"), style = "primary", size = "large"
      )
    )
    
    
    
    
    
  })
  
  
  
  
  valores_por_defecto_FA <-   eventReactive(input$controlador_grafico01, ignoreNULL = FALSE, {
    
    valores <- list(input$graf_1q_barras_max_FA,
                    input$graf_1q_barras_ylab_FA, 
                    input$graf_1q_barras_xlab_FA
    )
    
    names(valores) <- c("MaxY", "Ylab", "Xlab")
    
    return(valores)
  })
  
  valores_por_defecto_FR <-   eventReactive(input$controlador_grafico02, ignoreNULL = FALSE, {
    
    valores <- list(input$graf_1q_barras_max_FR,
                    input$graf_1q_barras_ylab_FR, 
                    input$graf_1q_barras_xlab_FR
    )
    
    names(valores) <- c("MaxY", "Ylab", "Xlab")
    
    return(valores)
  })
  
  
  valores_por_defecto_PORCENTAJE <-   eventReactive(input$controlador_grafico03, ignoreNULL = FALSE, {
    
    valores <- list(input$graf_1q_barras_max_PORCENTAJE,
                    input$graf_1q_barras_ylab_PORCENTAJE, 
                    input$graf_1q_barras_xlab_PORCENTAJE
    )
    
    names(valores) <- c("MaxY", "Ylab", "Xlab")
    
    return(valores)
  })
  
  
  
  
  output$grafico01 <- renderPlot({
    

    pos <- 2
    
    
    max_y <- valores_por_defecto_FA()[[1]]
    lab_ejey <- valores_por_defecto_FA()[[2]]
    lab_ejex <- valores_por_defecto_FA()[[3]]
    categorias <- DF_interna()[,1]
    mis_colores <- colores_seleccionados()
    la_corona <- corona()[[1]]
    
    # Datos a graficar
    seleccion <- as.numeric(as.character(
      gsub("%", "", DF_interna()[,pos])))
    
    if(length(seleccion) == 0) return(NULL)
    
    # Nombre de cada categoria 
    names(seleccion) <- categorias
    
    
    # Grafico de barras
    
    coord_x <-   barplot(seleccion, 
                         ylab = lab_ejey, 
                         ylim=c(0, max_y),
                         xlab = lab_ejex,
                         col = mis_colores)
    
    text(coord_x, (seleccion + (0.08*max_y)) , la_corona)
  })
  
  
  output$grafico02 <- renderPlot({
    
    pos <- 5
    
    
    max_y <- valores_por_defecto_FR()[[1]]
    lab_ejey <- valores_por_defecto_FR()[[2]]
    lab_ejex <- valores_por_defecto_FR()[[3]]
    categorias <- DF_interna()[,1]
    mis_colores <- colores_seleccionados()
    la_corona <- corona()[[2]]
    
    # Datos a graficar
    seleccion <- as.numeric(as.character(
      gsub("%", "", DF_interna()[,pos])))
    
    if(length(seleccion) == 0) return(NULL)
    
    # Nombre de cada categoria 
    names(seleccion) <- categorias
    
    
    # Grafico de barras
    
    coord_x <-  barplot(seleccion, 
                        ylab = lab_ejey, 
                        ylim=c(0, max_y),
                        xlab = lab_ejex,
                        col = mis_colores)
    
    text(coord_x, (seleccion + (0.08*max_y)) , la_corona)
    
  })
  
  
  
  output$grafico03 <- renderPlot({
    
    pos <- 6
    
    
    max_y <- valores_por_defecto_PORCENTAJE()[[1]]
    lab_ejey <- valores_por_defecto_PORCENTAJE()[[2]]
    lab_ejex <- valores_por_defecto_PORCENTAJE()[[3]]
    categorias <- DF_interna()[,1]
    mis_colores <- colores_seleccionados()
    la_corona <- corona()[[3]]
    
    # Datos a graficar
    seleccion <- as.numeric(as.character(
      gsub("%", "", DF_interna()[,pos])))
    
    if(length(seleccion) == 0) return(NULL)
    
    # Nombre de cada categoria 
    names(seleccion) <- categorias
    
    
    # Grafico de barras
    
    coord_x <-   barplot(seleccion, 
                         ylab = lab_ejey, 
                         ylim=c(0, max_y),
                         xlab = lab_ejex,
                         col = mis_colores)
    
    text(coord_x, (seleccion + (0.08*max_y)) , la_corona)
    
  })
  
  




  output$rejunte_barras <- renderUI({
    
    div(
      fluidRow(
        column(4,
               bsButton(inputId = ns("controlador01"), 
                        label = "Mostrar/Ocultar opciones gráficas generales",
                        icon = icon("bars"), 
                        type = "toggle", 
                        value = FALSE,
                        style = "primary", 
                        size = "large"
               )
        ),
        column(4),
        column(4,
               bsButton(inputId = ns("controlador02"),
                        label = "Mostrar/Ocultar opciones gráficas específicas",
                        icon = icon("bars"), 
                        type = "toggle", 
                        value = FALSE,
                        style = "primary", 
                        size = "large"
               )
        )
      ),br(),
      
      h2("Gráfico de Barras"),
      fluidRow(
        column(4,
               conditionalPanel(condition = "input.controlador01", ns = ns,
                                
               div(id = ns("James04"), uiOutput(ns("menu_general01")))
               )
        ),
        column(8,
               fluidRow(
                 column(6,
                        h3("Barras para Frecuencias Absolutas"),
                        plotOutput(ns("grafico01"))
                 ),
                 column(6,
                        conditionalPanel(condition = "input.controlador02", ns = ns,
                                         
                        div(id = ns("James01"),
                            h3("Cambios específicos"),
                            numericInput(inputId = ns("graf_1q_barras_max_FA"),
                                         label = "Frecuencias Absolutas - Máximo del eje Y",
                                         value = valor_maximo()[[1]][1],
                                         step = valor_maximo()[[2]][1]
                                        
                            ),
                            br(),
                            textInput(inputId = ns("graf_1q_barras_ylab_FA"),
                                      label = "Rótulo eje Y",
                                      value = "Frecuencia"),
                            br(),
                            textInput(inputId = ns("graf_1q_barras_xlab_FA"),
                                      label = "Rótulo eje X",
                                      value = colnames(minibase())[1]),
                            br(),
                            bsButton(ns("controlador_grafico01"), "Aplicar cambios", type = "toggle", value = FALSE,
                                     icon("bars"), style = "primary", size = "large"
                            ),
                            br(),
                            
                            
                        )
                        )
                 )
               ),
               br(),
               fluidRow(
                 column(6,
                        h3("Barras para Frecuencias Relativas"),
                        plotOutput(ns("grafico02"))
                 ),
                 column(6,
                        conditionalPanel(condition = "input.controlador02", ns = ns,
                                         
                        div(id = ns("James02"),
                            h3("Cambios específicos"),
                            numericInput(inputId = ns("graf_1q_barras_max_FR"),
                                         label = "Frecuencias Relativas - Máximo del eje Y",
                                         value = valor_maximo()[[1]][2],
                                         step = valor_maximo()[[2]][2],
                                         min = 0,
                                         max = 1
                            ),
                            br(),
                            textInput(inputId = ns("graf_1q_barras_ylab_FR"),
                                      label = "Rótulo eje Y",
                                      value = "Frecuencia Relativa"),
                            br(),
                            textInput(inputId = ns("graf_1q_barras_xlab_FR"),
                                      label = "Rótulo eje X",
                                      value = colnames(minibase())[1]),
                            br(),
                            bsButton(ns("controlador_grafico02"), "Aplicar cambios", type = "toggle", value = FALSE,
                                     icon("bars"), style = "primary", size = "large"
                            ),
                            br(),
                            
                        ) 
                        )
                 )
               ),
               br(),
               fluidRow(
                 column(6,
                        h3("Barras para Porcentajes"),
                        plotOutput(ns("grafico03"))
                 ),
                 column(6,
                        conditionalPanel(condition = "input.controlador02", ns = ns,
                                         
                        div(id = ns("James03"),
                            h3("Cambios específicos"),
                            numericInput(inputId = ns("graf_1q_barras_max_PORCENTAJE"),
                                         label = "Porcentaje - Máximo del eje Y",
                                         value = valor_maximo()[[1]][3],
                                         step = valor_maximo()[[2]][3],
                                         min = 0,
                                         max = 100
                            ),
                            br(),
                            textInput(inputId = ns("graf_1q_barras_ylab_PORCENTAJE"),
                                      label = "Rótulo eje Y",
                                      value = "Porcentaje"),
                            br(),
                            textInput(inputId = ns("graf_1q_barras_xlab_PORCENTAJE"),
                                      label = "Rótulo eje X",
                                      value = colnames(minibase())[1]),
                            br(),
                            bsButton(ns("controlador_grafico03"), "Aplicar cambios", type = "toggle", value = FALSE,
                                     icon("bars"), style = "primary", size = "large"
                            ),
                            br()
                            
                        ) 
                        )
                 )
               )
        )
      )
    )
    
  })
  
}