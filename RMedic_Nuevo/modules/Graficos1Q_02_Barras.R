


Graficos1Q_02_Barras_UI <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("armado_grafico"))
  
  
}






## Segmento del server
Graficos1Q_02_Barras_SERVER <- function(input, output, session, 
                                            minibase,
                                            decimales,
                                            control_ejecucion,
                                            tablas_1q) {
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  # Control interno 01
  control_interno01 <- reactive({
    
    if(is.null(control_ejecucion())) return(FALSE)
    else return(control_ejecucion())
  })
  
  
  # Cantidad de categorias
  cantidad_categorias <- reactive({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    return(nrow(tablas_1q()))

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
    candidato_maximo[1] <- max(as.numeric(as.character(tablas_1q()[,2])))

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

    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    # Control interno 02
    if(is.null(input$coronacion)) return(NULL)
    
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

          la_corona[[1]] <- as.character(as.vector(tablas_1q()[,2]))
          la_corona[[2]] <- as.character(as.vector(tablas_1q()[,5]))
          la_corona[[3]] <- as.character(as.vector(tablas_1q()[,6]))
        } else
          if(input$coronacion == 3) {

            la_corona[[1]] <- as.character(as.vector(tablas_1q()[,2]))
            la_corona[[2]] <- la_corona[[1]]
            la_corona[[3]] <- la_corona[[1]]
          } else
            if(input$coronacion == 4) {

              la_corona[[1]] <- as.character(as.vector(tablas_1q()[,4]))
              la_corona[[2]] <- la_corona[[1]]
              la_corona[[3]] <- la_corona[[1]]
            } else
              if(input$coronacion == 5) {

                la_corona[[1]] <- as.character(as.vector(tablas_1q()[,5]))
                la_corona[[2]] <- la_corona[[1]]
                la_corona[[3]] <- la_corona[[1]]
              } else
                if(input$coronacion == 6) {

                  la_corona[[1]] <- as.character(as.vector(tablas_1q()[,6]))
                  la_corona[[2]] <- la_corona[[1]]
                  la_corona[[3]] <- la_corona[[1]]
                } else
                  if(input$coronacion == 7) {

                    la_corona[[1]] <- as.character(as.vector(tablas_1q()[,7]))
                    la_corona[[2]] <- la_corona[[1]]
                    la_corona[[3]] <- la_corona[[1]]
                  }

    return(la_corona)
  })


  # Objetos reactivos de contorl
  reseteo_logico <- reactiveVal(F)
  aplicador_logico <- reactiveVal(F)


  observeEvent(minibase(), {
    
    reseteo_logico(!reseteo_logico())
    delay(400,     aplicador_logico(!aplicador_logico()))
    # aplicador_logico(!aplicador_logico())
    
  })
  
  
 
  colores_usuario <- reactive({

    # Control interno 01
    if(!control_interno01()) return(NULL)
    
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
        cantidad <- nrow(tablas_1q())
        armado <- as.vector(tablas_1q()[,1])
        
      }
    
    
    mis_colores <- rep(NA, cantidad)
    
    
    if(length(mis_colores) == 0) return(NULL)
    
    
    for(i in 1:cantidad){
      nombre_input <- paste("col", i, sep="_")
      if(is.null(input[[nombre_input]])) return(NULL)
      
      mis_colores[i] <- input[[nombre_input]]
      
    }
    
    

    return(mis_colores)
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
    reset("graf_1q_barras_CantidadColores", asis = F)
    
    # FA
    reset("graf_1q_barras_max_FA", asis = F)
    reset("graf_1q_barras_ylab_FA", asis = F)
    reset("graf_1q_barras_xlab_FA", asis = F)
    

    # FR
    reset("graf_1q_barras_max_FR", asis = F)
    reset("graf_1q_barras_ylab_FR", asis = F)
    reset("graf_1q_barras_xlab_FR", asis = F)

    
    # Porcentaje
    reset("graf_1q_barras_max_PORCENTAJE", asis = F)
    reset("graf_1q_barras_ylab_PORCENTAJE", asis = F)
    reset("graf_1q_barras_xlab_PORCENTAJE", asis = F)
    

  })
  
  
  
  xxchange <- reactive({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    paste(input$controlador02, input$controlador_grafico01B, 
          input$controlador_grafico02B, input$controlador_grafico03B)
  })
  
  observeEvent(xxchange(), {
    
    
    aplicador_logico(!aplicador_logico())
    
  })
  
  
  
  # Salida de colores
  output$MODcolor <- renderUI({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    
    
    cantidad <- c()
    if (input$graf_1q_barras_CantidadColores == "1"){
      cantidad <- 1
      label_armado <- "Color..."
    } else
      if(input$graf_1q_barras_CantidadColores == "2"){
        cantidad <- nrow(tablas_1q())
        label_armado <- paste0("Categoría '", as.vector(tablas_1q()[,1]), "'")
        
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
  
  
 
  valores_usuario_general <-   eventReactive(aplicador_logico(), ignoreNULL = FALSE, {
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    if(is.null(colores_usuario())) mis_colores <- "red" else
      if(is.na(colores_usuario())) mis_colores <- "red" else mis_colores <- colores_usuario()
    
   
    
    valores <- list(corona(),
                    mis_colores
                    
                    
    )
    
    names(valores) <- c("corona", "colores")
    
    return(valores)
  })
  
  
  valores_usuario_FA <-   eventReactive(aplicador_logico(), ignoreNULL = FALSE, {
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    if(is.null(input$graf_1q_barras_max_FA)) return(NULL)
    if(is.null(input$graf_1q_barras_ylab_FA)) return(NULL)
    if(is.null(input$graf_1q_barras_xlab_FA)) return(NULL)

    
    valores <- list(input$graf_1q_barras_max_FA,
                    input$graf_1q_barras_ylab_FA,
                    input$graf_1q_barras_xlab_FA

    )
    
    names(valores) <- c("y_max", "ylab", "xlab")
    
    return(valores)
  })
  
  valores_usuario_FR <-   eventReactive(aplicador_logico(), ignoreNULL = FALSE, {
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    if(is.null(input$graf_1q_barras_max_FR)) return(NULL)
    if(is.null(input$graf_1q_barras_ylab_FR)) return(NULL)
    if(is.null(input$graf_1q_barras_xlab_FR)) return(NULL)
    
    
    valores <- list(input$graf_1q_barras_max_FR,
                    input$graf_1q_barras_ylab_FR,
                    input$graf_1q_barras_xlab_FR
                    
    )
    
    names(valores) <- c("y_max", "ylab", "xlab")
    
    return(valores)
  })
  
  valores_usuario_PORCENTAJE <-   eventReactive(aplicador_logico(), ignoreNULL = FALSE, {
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    if(is.null(input$graf_1q_barras_max_PORCENTAJE)) return(NULL)
    if(is.null(input$graf_1q_barras_ylab_PORCENTAJE)) return(NULL)
    if(is.null(input$graf_1q_barras_xlab_PORCENTAJE)) return(NULL)
    
    
    valores <- list(input$graf_1q_barras_max_PORCENTAJE,
                    input$graf_1q_barras_ylab_PORCENTAJE,
                    input$graf_1q_barras_xlab_PORCENTAJE
                    
    )
    
    names(valores) <- c("y_max", "ylab", "xlab")
    
    return(valores)
  })
  
  
  output$menu_general01 <- renderUI({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
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
      bsButton(ns("controlador02"), "Aplicar cambios generales", 
               type = "toggle", value = TRUE,
               icon("bars"), style = "primary", size = "large"
      ), br(),
      bsButton(ns("reset"), "Resetear todo", type = "toggle", value = TRUE,
               icon("bars"), style = "primary", size = "large"
      ),
    )
    
    
    
    
    
  })
  
  
  
  
  output$menu_especifico01 <- renderUI({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    if(is.null(valor_maximo())) return(NULL)
    
    div(id = ns("James02"),
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
        bsButton(ns("controlador_grafico01B"), "Aplicar cambios específicos", 
                 type = "toggle", value = TRUE,
                 icon("bars"), style = "primary", size = "large"
        ),
        br(),
        
        
    )
  })
  
  output$menu_especifico02 <- renderUI({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    if(is.null(valor_maximo())) return(NULL)
    
    div(id = ns("James03"),
        numericInput(inputId = ns("graf_1q_barras_max_FR"),
                     label = "Frecuencias Relativas - Máximo del eje Y",
                     value = valor_maximo()[[1]][2],
                     step = valor_maximo()[[2]][2]
                     
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
        bsButton(ns("controlador_grafico02B"), "Aplicar cambios específicos", 
                 type = "toggle", value = TRUE,
                 icon("bars"), style = "primary", size = "large"
        ),
        br(),
        
        
    )
  })
  
  output$menu_especifico03 <- renderUI({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    if(is.null(valor_maximo())) return(NULL)
    
    div(id = ns("James04"),
        numericInput(inputId = ns("graf_1q_barras_max_PORCENTAJE"),
                     label = "Porcentaje - Máximo del eje Y",
                     value = valor_maximo()[[1]][3],
                     step = valor_maximo()[[2]][3]
                     
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
        bsButton(ns("controlador_grafico03B"), "Aplicar cambios específicos", 
                 type = "toggle", value = TRUE,
                 icon("bars"), style = "primary", size = "large"
        ),
        br(),
        
        
    )
  })
  
  output$grafico01 <- renderPlot({
    
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    # Control interno 01
    if(is.null(valores_usuario_FA())) return(NULL)
    
   
    pos <- 2
    
   
    max_y <- valores_usuario_FA()$y_max
    
    # lab_ejey <- valores_por_defecto_FA()[[2]]
    # lab_ejex <- valores_por_defecto_FA()[[3]]
     categorias <- tablas_1q()[,1]
    # mis_colores <- colores_seleccionados()
    # la_corona <- corona()[[1]]
    
    # Datos a graficar
    seleccion <- as.numeric(as.character(
      gsub("%", "", tablas_1q()[,pos])))
    
    if(length(seleccion) == 0) return(NULL)
    
    # Nombre de cada categoria
    names(seleccion) <- categorias
    
    
    # Grafico de barras
 # mis_colores <- colores_usuario()
      
     coord_x <- barplot(seleccion, 
           col = valores_usuario_general()$colores,
           ylab = valores_usuario_FA()$ylab,
           xlab = valores_usuario_FA()$xlab,
           ylim = c(0, valores_usuario_FA()$y_max)
           )
    
    # Agregar texto
  #  text(coord_x, (seleccion) , valores_usuario_general()$corona)
    
    
    text(coord_x, (seleccion + (0.08*max_y)) , valores_usuario_general()$corona[[1]])
    
    # coord_x <-   barplot(seleccion,
    #                      ylab = lab_ejey,
    #                      ylim=c(0, max_y),
    #                      xlab = lab_ejex,
    #                      col = mis_colores)
    # 
    # text(coord_x, (seleccion + (0.08*max_y)) , la_corona)
    
  })
  
  
  
  output$grafico02 <- renderPlot({
    
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    # Control interno 01
    if(is.null(valores_usuario_FR())) return(NULL)
    
    
    pos <- 5
    
   
    max_y <- valores_usuario_FR()$y_max
    
    # lab_ejey <- valores_por_defecto_FA()[[2]]
    # lab_ejex <- valores_por_defecto_FA()[[3]]
    categorias <- tablas_1q()[,1]
    # mis_colores <- colores_seleccionados()
    #la_corona <- corona()[[2]]
    
    # Datos a graficar
    seleccion <- as.numeric(as.character(
      gsub("%", "", tablas_1q()[,pos])))
    
    if(length(seleccion) == 0) return(NULL)
    
    # Nombre de cada categoria
    names(seleccion) <- categorias
    
    
    # Grafico de barras
    # mis_colores <- colores_usuario()
    
    coord_x <- barplot(seleccion, 
                       col = valores_usuario_general()$colores,
                       ylab = valores_usuario_FR()$ylab,
                       xlab = valores_usuario_FR()$xlab,
                       ylim = c(0, valores_usuario_FR()$y_max)
    )
    
    # Agregar texto
    #  text(coord_x, (seleccion) , valores_usuario_general()$corona)
    
    
    text(coord_x, (seleccion + (0.08*max_y)) , valores_usuario_general()$corona[[2]])
    
    # coord_x <-   barplot(seleccion,
    #                      ylab = lab_ejey,
    #                      ylim=c(0, max_y),
    #                      xlab = lab_ejex,
    #                      col = mis_colores)
    # 
    # text(coord_x, (seleccion + (0.08*max_y)) , la_corona)
    
  })
  
  
  output$grafico03 <- renderPlot({
    
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    # Control interno 01
    if(is.null(valores_usuario_PORCENTAJE())) return(NULL)
    
    
    pos <- 6
    
    
    max_y <- valores_usuario_PORCENTAJE()$y_max
    
    # lab_ejey <- valores_por_defecto_FA()[[2]]
    # lab_ejex <- valores_por_defecto_FA()[[3]]
    categorias <- tablas_1q()[,1]
    # mis_colores <- colores_seleccionados()
    la_corona <- corona()[[3]]
    
    # Datos a graficar
    seleccion <- as.numeric(as.character(
      gsub("%", "", tablas_1q()[,pos])))
    
    if(length(seleccion) == 0) return(NULL)
    
    # Nombre de cada categoria
    names(seleccion) <- categorias
    
    
    # Grafico de barras
    # mis_colores <- colores_usuario()
    
    coord_x <- barplot(seleccion, 
                       col = valores_usuario_general()$colores,
                       ylab = valores_usuario_PORCENTAJE()$ylab,
                       xlab = valores_usuario_PORCENTAJE()$xlab,
                       ylim = c(0, valores_usuario_PORCENTAJE()$y_max)
    )
    
    # Agregar texto
    #  text(coord_x, (seleccion) , valores_usuario_general()$corona)
    
    
    text(coord_x, (seleccion + (0.08*max_y)) , valores_usuario_general()$corona[[3]])
    
    # coord_x <-   barplot(seleccion,
    #                      ylab = lab_ejey,
    #                      ylim=c(0, max_y),
    #                      xlab = lab_ejex,
    #                      col = mis_colores)
    # 
    # text(coord_x, (seleccion + (0.08*max_y)) , la_corona)
    
  })
  
  output$armado_grafico <- renderUI({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    
    # if(is.null(valores_usuario())) return(NULL)
    
    div(
      h2("Gráfico de Barras"), br(),
      fluidRow(
        column(9,
          fluidRow(
            column(6,
               h2("Frecuencias Absolutas"),
               plotOutput(ns("grafico01"))
        ),
        column(6,
               bsButton(inputId = ns("controlador_grafico01A"), 
                        label = "Mostrar/Ocultar Opciones Específicas",
                        icon = icon("bars"), 
                        type = "toggle", 
                        #   value = FALSE,
                        value = TRUE,
                        style = "primary", 
                        size = "large"
               ), br(),br(), br(),
               conditionalPanel(condition = "input.controlador_grafico01A", ns = ns,
                                div(id = ns("James02"), br(), uiOutput(ns("menu_especifico01")))
               ))
        ), # Fin Graf1 - Inicio de Graf02
        br(),br(),br(),
        fluidRow(
          column(6,
                 h2("Frecuencias Relativas"),
                 plotOutput(ns("grafico02"))
          ),
          column(6,
                 br(),
                 # bsButton(inputId = ns("controlador_grafico02A"), 
                 #          label = "Mostrar/Ocultar Opciones Específicas",
                 #          icon = icon("bars"), 
                 #          type = "toggle", 
                 #          #   value = FALSE,
                 #          value = TRUE,
                 #          style = "primary", 
                 #          size = "large"
                 # ), 
                 br(),br(), br(),
                 conditionalPanel(condition = "input.controlador_grafico01A", ns = ns,
                                  div(id = ns("James03"), br(), uiOutput(ns("menu_especifico02")))
                 ))
        ), # Fin Graf2 - Empieza el 3
        br(),br(),br(),
        fluidRow(
          column(6,
                 h2("Porcentajes"),
                 plotOutput(ns("grafico03"))
          ),
          column(6,
                 br(),
                 # bsButton(inputId = ns("controlador_grafico03A"), 
                 #          label = "Mostrar/Ocultar Opciones Específicas",
                 #          icon = icon("bars"), 
                 #          type = "toggle", 
                 #          #   value = FALSE,
                 #          value = TRUE,
                 #          style = "primary", 
                 #          size = "large"
                 # ), 
                 br(),br(), br(),
                 conditionalPanel(condition = "input.controlador_grafico01A", ns = ns,
                                  div(id = ns("James04"), br(),uiOutput(ns("menu_especifico03")))
                 ))
        ), # Fin Graf3
        )
        ,
        column(3,
               bsButton(inputId = ns("controlador01"), 
                        label = "Mostrar/Ocultar opciones generales",
                        icon = icon("bars"), 
                        type = "toggle", 
                        #   value = FALSE,
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


