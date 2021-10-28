## Segmento del UI
Graficos1Q_UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("SeccionGraficos1Q"))
  
  
}




## Segmento del server
Graficos1Q_SERVER <- function(input, output, session, 
                            minibase, 
                            batalla_naval,
                            decimales) {
  
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  # Caso 1: 1Q
  casoRMedic <- reactive({
    
    if(is.null(batalla_naval())) return(NULL)
    if(is.null(batalla_naval()[[4]])) return(NULL)
    if(length(batalla_naval()[[4]]) == 0) return(NULL)
    if(batalla_naval()[[4]] == '') return(NULL)
    casoRMedic <- batalla_naval()[[4]]
    #casoRMedic <- as.numeric(as.character(as.vector(batalla_naval()[[4]])))
    casoRMedic
    
  })
  
 
  DF_interna <-  reactive({RMedic_1q_tablas(minibase(), decimales())[[1]] })
  
  valor_maximo <- reactive({
    
    if(is.null(DF_interna())) return(NULL)
    
    candidato_maximo <- max(as.numeric(as.character(DF_interna()[,2])))
    
    library(stringr)
    
    div1 <- candidato_maximo / 10
    div2 <- candidato_maximo %/% 10
    cantidad_digitos <- str_count(div2)
    
    step <- 10^cantidad_digitos
    if (div2 == 0) step <- 1
    
  #  if(div1 == div2) return(candidato)
    
    if(div1 > div2) {
      candidato_maximo <- (div2 + 1)*step
     # return(candidato)
      
      
    }
    

  #  if(!is.null(input$graf_1q_barras_max)) if(input$graf_1q_barras_max > candidato) cantidato <- input$graf_1q_barras_max
    
    return(list(candidato_maximo, step))
      
  })
    
  
  cantidad_categorias <- reactive({
    if(is.null(DF_interna())) return(NULL)
    
    return(nrow(DF_interna()))
    
  })
  
    
  
  
# Input de Colores
colores_seleccionados <- reactive({

      if (is.null(DF_interna())) return(NULL)
      if (is.null(input$graf_1q_barras_CantidadColores)) return(NULL)



          cantidad <- c()
          if (input$graf_1q_barras_CantidadColores == "1"){
            cantidad <- 1
            armado <- "Color..."
          } else
            if(input$graf_1q_barras_CantidadColores == "2"){
              cantidad <- nrow(DF_interna())
              armado <- as.vector(DF_interna()[,1])

            }

          cat("cantidad:", cantidad, "\n")
          
          mis_colores <- rep(NA, cantidad)
          
          cat("input$col_1: ", input$col_1, "\n")
        # lapply(1:cantidad, function(i) {

          if(length(mis_colores) == 0) return(NULL)
          
          for(i in 1:cantidad){ 
                nombre_input <- paste("col", i, sep="_")
                cat("nombre_input: ", nombre_input, "\n" )
                cat("input[[nombre_input]]: ", input[[nombre_input]], "\n" )
                if(is.null(input[[nombre_input]])) return(NULL)
                
              mis_colores[i] <- input[[nombre_input]]

         }
         
         cat("mis_colores:", mis_colores, "\n")
         return(mis_colores)
      })
  
  
  # Salida de colores
  output$MODcolor <- renderUI({
    
    if (is.null(DF_interna())) return(NULL)
    if (is.null(input$graf_1q_barras_CantidadColores)) return(NULL)
    
    
    
    cantidad <- c()
    if (input$graf_1q_barras_CantidadColores == "1"){
      cantidad <- 1
      label_armado <- "Color..."
    } else
      if(input$graf_1q_barras_CantidadColores == "2"){
        cantidad <- nrow(DF_interna())
        label_armado <- paste0("Categoría '", as.vector(DF_interna()[,1]), "'")
        
      }
    
    
    lapply(1:cantidad, function(i) {
      
      nombre_input <- paste("col", i, sep="_")
      div(
        colourpicker::colourInput(inputId = ns(nombre_input),
                    label = label_armado[i], 
                    value = "#FF0000"), br()
      )
      
    })
    
  })
  
  
   
  output$armado_barras_1q <- renderUI({
    
    div(
       numericInput(inputId = ns("graf_1q_barras_max"),
                    label = "Máximo del eje Y",
                    value = valor_maximo()[[1]],
                    step = valor_maximo()[[2]]
                    ),
       radioButtons(inputId = ns("graf_1q_barras_CantidadColores"),
                    label = "Coloración General",
                    choices = c("Color único" = 1, 
                                "Un color por categoría" = 2)
                    ),
       # colourpicker::colourInput(inputId = ns("color"), 
       #                           label = "Color...",
       #                           showColour = "both",
       #                           value = "#FF0000"
       #                            ),
       uiOutput(ns("MODcolor"))
      #)
    )
  
    
    
    
    
  })
  
  
 
  
  

  intermediario <- reactive({
    
    if(is.null(input$graf_1q_barras_max)) return(NULL)
    
    nombres <- c("Columnas", "Label", "Min", "Max")
    referencia <- matrix(NA, 3, length(nombres))
    colnames(referencia) <- nombres
    
    referencia[,1] <- c(2, 5, 6)
    referencia[,2] <- c("Frecuencia", "Frecuencia Relativa", "Porcentajes")
    referencia[,3] <- c(0, 0, 0)
    referencia[,4] <- c(input$graf_1q_barras_max, 1, 100)

    
    referencia <- as.vector(referencia[as.numeric(1), ])
    

    
    
    return(referencia)
  })

  
  

  output$grafico_barras_1q <- renderPlot({
    
    if(is.null(DF_interna())) return(NULL)
    if(is.null(colores_seleccionados())) return(NULL)
    
    # Label
   lab_ejex <- colnames(DF_interna())[1]
    
    # Datos a graficar
    seleccion <- as.numeric(as.character(
      gsub("%", "", DF_interna()[,as.numeric(intermediario()[1])])))
    
   if(length(seleccion) == 0) return(NULL)
    
    # Nombre de cada categoria 
   names(seleccion) <- DF_interna()[,1]
   
  
   # Grafico de barras
    barplot(seleccion, ylab = intermediario()[2], 
            ylim=c(0, as.numeric(intermediario()[4])),
            xlab = lab_ejex,
            col = colores_seleccionados())

  })
 
 
  output$grafico_tortas_1q <- renderPlot({
    
    pie(table(mtcars[,2]))
  })
  
  
 
  output$SeccionGraficos1Q <- renderUI({
    
    # Especificaciones de cumplimiento
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 1) return(NULL)
    
    
    
    # Si es el caso 1, seguimos!
    div(
      h2("RMedic - Gráficos para 1 Variable Categórica"),
      tabsetPanel(id = ns("Graficos_1q"),
                  tabPanel(title = "RMedic Help!", value = 1,
                           fluidRow(
                             column(4, 
                                    radioButtons(inputId = "help_graficos_1q",
                                                 label = h3("Selección de Ayuda Automática"),
                                                 choices = c("RMedic Here!" = 1,
                                                             "Barras" = 2,
                                                             "Tortas" = 3)
                                    )
                             ),
                             column(8,
                                    br(),
                                    conditionalPanel(condition = "input.help_graficos_1q == 1",
                                                     div(
                                                       h3("RMedic Here!"),
                                                       HTML(
                      "Los gráficos más utilizados aplicados a una variable categórica son:<br/>
                      - Gráfico de <b>Barras</b>.<br/>
                      - Gráfico de <b>Torgas</b>.<br/>
                      Seleccionando la ayuda de cada uno encontrarás un resumen con
                      detalles teóricos y estructura de la base de datos.<br/>
                      Estos te ayudarán a determinar si estas herramientas pueden ser
                      aplicadas en tu trabajo."
                                                       )
                                                     )
                                    ),
                                    conditionalPanel(condition = "input.help_graficos_1q == 2",
                                                     div(
                                                       h3("Gráfico de Barras"),
                                                       HTML(
                            "Agregar texto aquí.<br/><br/>
                          
                            Se aplica sobre una columna de la base de datos.<br/>
                            La variable debe contener al menos una categoría."
                                                       )
                                                     )
                                    ),
                                    conditionalPanel(condition = "input.help_graficos_1q == 3",
                                                     div(
                                                       h3("Gráfico de Tortas"),
                                                       HTML(
                                                         "Agregar texto aquí.<br/><br/>
                          
                            Se aplica sobre una columna de la base de datos.<br/>
                            La variable debe contener al menos una categoría."
                                                       )
                                                     )
                                    ),
                             )
                           )
                  ),
                  tabPanel(title = "Barras", value = 2,
                           # h3(textOutput(ns("Salida_texto_1q_RMedic_01"))),
                           # tableOutput(ns("Salida_tabla_1q_RMedic_01")),
                           # br()
                           "Gráfico de Barras",
                           fluidRow(
                             column(4,
                                    uiOutput(ns("armado_barras_1q"))
                                    ),
                             column(8,
                                    plotOutput(ns("grafico_barras_1q"))
                             )
                           )
                           ),
                  tabPanel(title = "Tortas", value = 3,
                           # h3(textOutput(ns("Salida_texto_1q_RMedic_02"))),
                           # tableOutput(ns("Salida_tabla_1q_RMedic_02")),
                           # br(),
                           # h3(textOutput(ns("Salida_texto_1q_RMedic_03"))),
                           # tableOutput(ns("Salida_tabla_1q_RMedic_03")),
                           # br(),
                           # h3(textOutput(ns("Salida_texto_1q_RMedic_04"))),
                           # tableOutput(ns("Salida_tabla_1q_RMedic_04")),
                           # br() 
                           "Gráfico de Tortas",
                           plotOutput(ns("grafico_tortas_1q"))
                           )
      ),
    )
  })
  
  
  
  
  
  
}


