## Segmento del UI
Tablas2C_UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("SeccionTablas2C"))
  
  
}




## Segmento del server
Tablas2C_SERVER <- function(input, output, session, 
                            minibase, 
                            batalla_naval,
                            decimales) {
  
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  # Caso 4: 2C
  casoRMedic <- reactive({
    
    if(is.null(batalla_naval())) return(NULL)
    if(is.null(batalla_naval()[[4]])) return(NULL)
    if(length(batalla_naval()[[4]]) == 0) return(NULL)
    if(batalla_naval()[[4]] == '') return(NULL)
    casoRMedic <- batalla_naval()[[4]]
    #casoRMedic <- as.numeric(as.character(as.vector(batalla_naval()[[4]])))
    casoRMedic
    
  })
  
  # Todas las tablas 2C
  Reactive_tabla_2c_RMedic <- reactive({
    
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 4) return(NULL)
    
    
    
    # Nota: al valor input$x_breaks lo tuve que poner
    #      como na.omit(input$x_breaks)[1] por que algunas veces
    #      otorga un vector con dos valores, pero uno de ellos es NA.
    
    
    
    salida <-  RMedic_2c_tablas(input_base =  minibase(),
                                input_decimales = decimales(),
                                input_min1 = input$x_min1,
                                input_max1 = input$x_max1,
                                input_breaks1 = na.omit(input$x_breaks1)[1],
                                input_side1 = input$x_side1,
                                input_min2 = input$x_min2,
                                input_max2 = input$x_max2,
                                input_breaks2 = na.omit(input$x_breaks2)[1],
                                input_side2 = input$x_side2
    )
    
    
    
    salida[[11]][,2] <- as.character(salida[[11]][,2])
    salida[[11]][,3] <- as.character(salida[[11]][,3])
    salida[[11]][,5] <- as.character(salida[[11]][,5])
    
    salida[[12]][,2] <- as.character(salida[[12]][,2])
    salida[[12]][,3] <- as.character(salida[[12]][,3])
    salida[[12]][,5] <- as.character(salida[[12]][,5])
    # Return Exitoso
    return(salida)
    
    
  })  
  
  
  
  # Cantidad de tablas
  cantidad_tablas <- reactive({
    
    if(is.null(Reactive_tabla_2c_RMedic)) return(NULL)
    
    # Return Exitoso
    return(length(Reactive_tabla_2c_RMedic()))
  })
  
  # Create all renderTables!!!     
  observe(
    lapply(c(1:cantidad_tablas()), function(i) {
      
      nombre_fusion1 <- paste0('Salida_texto_2c_RMedic_', CifrasPerfectas(i))
      nombre_fusion2 <- paste0('Salida_tabla_2c_RMedic_', CifrasPerfectas(i))
      
      # El rotulo de cada tabla       
      output[[nombre_fusion1]] <- renderText({
        names(Reactive_tabla_2c_RMedic())[i]
      })
      
      # Cada tabla
      output[[nombre_fusion2]] <- renderTable(digits = decimales(), align= "c",{
        Reactive_tabla_2c_RMedic()[[i]]
      })
      
      
      
    })
  ) 
  
  
 
  # Controlador1 - Var 1 
  {
  output$Controlador1_2c_RMedic <- renderUI({
    
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 4) return(NULL)
    
    cantidad_cortes <- nclass.Sturges(minibase()[,1])
    tabla <- table(minibase()[,1])
    cantidad_categorias <- length(names(tabla))
    if(cantidad_categorias < cantidad_cortes) cantidad_cortes <- cantidad_categorias
    
    div(
      fluidRow(
        column(4,
               numericInput(
                 inputId = ns("x_min1"),
                 label = "Valor mínimo: ",
                 value = min(minibase()[,1]),
                 min = NA,
                 max = min(minibase()[,1]),
                 step = 0.01,
                 width = NULL
               ),
               numericInput(
                 inputId = ns("x_max1"),
                 label = "Valor máximo: ",
                 value = max(minibase()[,1]),
                 min = max(minibase()[,1]),
                 max = NA,
                 step = 0.01,
                 width = NULL
               )
        ),
        column(4,
               radioButtons(inputId = ns("x_side1"), 
                            label = "Cierre del intervalo: ", choices = c("A la Derecha" = T , "A la Izquierda" = F)
               )
        ),
        column(4, 
               numericInput(
                 inputId = ns("x_breaks1"),
                 label = "Cantidad de intervalos: ",
                 value = cantidad_cortes,
                 min = 1,
                 max = NA,
                 step = 1,
                 width = NULL
               )
        )
      )
    )
    
  })
  
  
  
  
  # Variable criterio de inclusion
  observeEvent(input[[ns("x_min1")]],{
    
    if(input[[ns("x_min1")]] > min(minibase()[,1])) {
      
      updateNumericInput(session, inputId = ns("x_min1"),
                         label = "Valor mínimo: ",
                         value = min(minibase()[,1]),
                         min = NA,
                         max = min(minibase()[,1]),
                         step = 0.01
      )
      
      
      
    }
  })
  
  
  # Variable criterio de inclusion
  observeEvent(input[[ns("x_max1")]],{
    
    if(input[[ns("x_max1")]] < max(minibase()[,1])) {
      
      updateNumericInput(session, inputId = ns("x_max1"),
                         label = "Valor máximo: ",
                         value = max(minibase()[,1]),
                         min = max(minibase()[,1]),
                         max = NA,
                         step = 0.01
      )
      
      
      
    }
  })
  
  }
  ##########################
  
  
  # Controlador2 - Var 2 
  {
    output$Controlador2_2c_RMedic <- renderUI({
      
      if(is.null(casoRMedic())) return(NULL)
      if(casoRMedic() != 4) return(NULL)
      
      cantidad_cortes <- nclass.Sturges(minibase()[,2])
      tabla <- table(minibase()[,2])
      cantidad_categorias <- length(names(tabla))
      if(cantidad_categorias < cantidad_cortes) cantidad_cortes <- cantidad_categorias
      
      div(
        fluidRow(
          column(4,
                 numericInput(
                   inputId = ns("x_min2"),
                   label = "Valor mínimo: ",
                   value = min(minibase()[,2]),
                   min = NA,
                   max = min(minibase()[,2]),
                   step = 0.01,
                   width = NULL
                 ),
                 numericInput(
                   inputId = ns("x_max2"),
                   label = "Valor máximo: ",
                   value = max(minibase()[,2]),
                   min = max(minibase()[,2]),
                   max = NA,
                   step = 0.01,
                   width = NULL
                 )
          ),
          column(4,
                 radioButtons(inputId = ns("x_side2"), 
                              label = "Cierre del intervalo: ", choices = c("A la Derecha" = T , "A la Izquierda" = F)
                 )
          ),
          column(4, 
                 numericInput(
                   inputId = ns("x_breaks2"),
                   label = "Cantidad de intervalos: ",
                   value = cantidad_cortes,
                   min = 1,
                   max = NA,
                   step = 1,
                   width = NULL
                 )
          )
        )
      )
      
    })
    
    
    
    
    # Variable criterio de inclusion
    observeEvent(input[[ns("x_min2")]],{
      
      if(input[[ns("x_min2")]] > min(minibase()[,2])) {
        
        updateNumericInput(session, inputId = ns("x_min2"),
                           label = "Valor mínimo: ",
                           value = min(minibase()[,2]),
                           min = NA,
                           max = min(minibase()[,2]),
                           step = 0.01
        )
        
        
        
      }
    })
    
    
    # Variable criterio de inclusion
    observeEvent(input[[ns("x_max2")]],{
      
      if(input[[ns("x_max2")]] < max(minibase()[,2])) {
        
        updateNumericInput(session, inputId = ns("x_max2"),
                           label = "Valor máximo: ",
                           value = max(minibase()[,2]),
                           min = max(minibase()[,2]),
                           max = NA,
                           step = 0.01
        )
        
        
        
      }
    })
    
  }
  ##########################
  
  
  output$SeccionTablas2C <- renderUI({
    
    # Especificaciones de cumplimiento
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 4) return(NULL)
    
    # Si es el caso 4, seguimos!
    div(
      h2("RMedic - Tablas para 2 Variables Numéricas"),
      tabsetPanel(id = "Tablas_2c",
                  tabPanel("RMedic Help!", value = 1,
                           fluidRow(
                             column(4,
                                    radioButtons(inputId = "help_tablas_2c",
                                                 label = h3("Selección de Ayuda Automática"),
                                                 choices = c("RMedic Here!" = 1,
                                                             "Medidas Resumen" = 2,
                                                             "Medidas de Posición" = 3,
                                                             "Medidas de Dispersión" = 4,
                                                             "Intervalos de Confianza" = 5,
                                                             "Distribución de Frecuencias" = 6)
                                    )),
                             column(8,
                                    br(),
                                    conditionalPanel(condition = "input.help_tablas_2c == 1", 
                                                     div(
                                                       h3("RMedic Here!"),
                                                       HTML(
                                                         "Las tablas más utilizadas aplicadas a una variable numérica son:<br/>
                      - Tablas de <b>Medidas Resumen</b>.<br/>
                      - Tablas de <b>Medidas Posición</b>.<br/>
                      - Tablas de <b>Medidas Dispersión</b>.<br/>
                      - Tablas de <b>Medidas Intervalos de Confianza</b>.<br/>
                      - Tablas de <b>Distribución de Frecuencias</b>.<br/>
                      Seleccionando la ayuda de cada una encontrarás un resumen con
                      detalles teóricos y estructura de la base de datos.<br/>
                      Estos te ayudarán a determinar si estas herramientas pueden ser
                      aplicadas en tu trabajo."
                                                       )
                                                     )),
                                    conditionalPanel(condition = "input.help_tablas_2c == 2", 
                                                     div(
                                                       h3("Medidas Resumen"),
                                                       HTML(
                                                         "Las tablas de <b>Medidas Resumen</b> contienen solo algunos de los componentes
                      que pueden encontrarse en las tablas de <b>Medidas de Posición</b> y <b>Medidas de Dispersión</b>.<br/>
                      En este caso RMedic detalla media, desvío estándard y n.<br/>"
                                                       )
                                                     )
                                    ),
                                    conditionalPanel(condition = "input.help_tablas_2c == 3", 
                                                     div(
                                                       h3("Medidas de Posición"),
                                                       HTML(
                                                         "Las <b>Medidas Posición</b> son un conjunto de estimaciones que podrían
                      representarse como un punto en una recta, siendo la recta nuestra variable.<br/>
                      Las estimaciones clásicas son: mínimo, media, mediana y máximo.<br/>
                      RMedic agrega: cuartiles, deciles y percentiles.<br/><br/>
                      Si bien 'n' (cantidad de datos) no es una medida de posición, es un detalle sumamente importante que debe estar siempre
                      en todo tabla.<br/><br/>
                      Se aplica sobre una columna de la base de datos.<br/>
                      La variable debe contener al menos un datos."
                                                       )
                                                     )),
                                    conditionalPanel(condition = "input.help_tablas_2c == 4", 
                                                     div(
                                                       h3("Medidas de Dispersión"),
                                                       HTML(
                                                         "Las <b>Medidas Dispersión</b> son un conjunto de estimaciones 
                      manifiestan la variabilidad de nuestros datos.<br/>
                      Las estimaciones clásicas son: rango, varianza, desvío estándard y error estándard.<br/>
                      RMedic agrega además: Rango Intercuartílico y Desviación Intercuartílica.<br/><br/>
                      Si bien 'n' (cantidad de datos) no es una medida de posición, es un detalle sumamente importante que debe estar siempre
                      en todo tabla.<br/><br/>
                      Se aplica sobre una columna de la base de datos.<br/>
                      La variable debe contener al menos dos datos.")
                                                     )
                                    ),
                                    conditionalPanel(condition = "input.help_tablas_2c == 5", 
                                                     div(
                                                       h3("Intervalos de Confinza"),
                                                       HTML("El <b>'Intervalo de Confianza'</b> para la media es un rango de valores que tiene cierta
                                               probabilidad de contener a la media poblacional.<br/>
                                               El intervalo de confianza se genera relacionando a la media y el error estándard en una fórmula.<br/>
                                               Se presentan intervalos del 90%, 95% y 99%. <br/>
                                               De estas 3 posibilidades debiera elegir el usuario solo uno de ellas. <br/>
                                               El más utilizado es el intervalo del 95%, pero la elección correcta depende de cada trabajo
                                               y cada área del conocimiento. <br/>
                                               Si no sabe cuál elegir, debería buscar en publicaciones de su área
                                               para saber qué intervalo de confianza debería elegir.<br/><br/>
                                                    Se aplica sobre una columna de la base de datos.<br/>
                                                    La variable debe contener al menos dos datos."
                                                       )
                                                     )
                                    ),
                                    conditionalPanel(condition = "input.help_tablas_2c == 6", 
                                                     div(
                                                       h3("Distribución de Frecuencias"),
                                                       HTML(
                                                         "La tabla de <b>'Distribución de Frecuencias'</b> para una variable numérica
           se genera a partir de recategorizar la información de la variable en intervalos.<br/>
           Se toma al valor máximo y mínimo de la variable, y a ese rango de valores
           es francionado una cantidad de intervalor equidistantes.<br/>
           La cantidad de intervalos óptima responde a una fórmula, que es la fórmula de Sturges.<br/>
           RMedic internamente calcula la cantidad de intervalos óptima y genera automáticamente la tabla.<br/>
           Cada dato de la variable original cae dentro de un único intervalo. A partir de allí se generan
           frecuencias absolutas (FA), total, cociente, frecuencias relativas (FR) y porcentajes de los intervalos.
           Se agrega además una columna que fusiona las frecuencias absolutas con los porcentajes.<br/><br/>
           Se aplica sobre una columna de la base de datos.<br/>
           La variable debe contener al menos un dato."
                                                         
                                                       )
                                                     ))
                             )
                           )
                  ),
                  tabPanel("Medidas Resumen", value = 2,
                           h3(textOutput(ns("Salida_texto_2c_RMedic_01"))),
                           tableOutput(ns("Salida_tabla_2c_RMedic_01")),
                           br()),
                  tabPanel("Medidas de Posición", value = 3,
                           h3(textOutput(ns("Salida_texto_2c_RMedic_02"))),
                           tableOutput(ns("Salida_tabla_2c_RMedic_02")),
                           br(),
                           h3(textOutput(ns("Salida_texto_2c_RMedic_03"))),
                           tableOutput(ns("Salida_tabla_2c_RMedic_03")),
                           br(),
                           h3(textOutput(ns("Salida_texto_2c_RMedic_04"))),
                           tableOutput(ns("Salida_tabla_2c_RMedic_04")),
                           br(),
                           h3(textOutput(ns("Salida_texto_2c_RMedic_05"))),
                           tableOutput(ns("Salida_tabla_2c_RMedic_05")),
                           br()),
                  tabPanel("Medidas de Dispersión", value = 4,
                           h3(textOutput(ns("Salida_texto_2c_RMedic_06"))),
                           tableOutput(ns("Salida_tabla_2c_RMedic_06")),
                           br(),
                           h3(textOutput(ns("Salida_texto_2c_RMedic_07"))),
                           tableOutput(ns("Salida_tabla_2c_RMedic_07")),
                           br()),
                  tabPanel("Intervalos de Confianza", value = 5,
                           h3(textOutput(ns("Salida_texto_2c_RMedic_08"))),
                           tableOutput(ns("Salida_tabla_2c_RMedic_08")),
                           br(),
                           h3(textOutput(ns("Salida_texto_2c_RMedic_09"))),
                           tableOutput(ns("Salida_tabla_2c_RMedic_09")),
                           br(),
                           h3(textOutput(ns("Salida_texto_2c_RMedic_10"))),
                           tableOutput(ns("Salida_tabla_2c_RMedic_10")),
                           br()
                           ),
                  tabPanel("Distribución de Frecuencias", value = 6,
                           h3(textOutput(ns("Salida_texto_2c_RMedic_11"))),
                           tableOutput(ns("Salida_tabla_2c_RMedic_11")),
                           uiOutput(ns("Controlador1_2c_RMedic")),
                           br(),
                           h3(textOutput(ns("Salida_texto_2c_RMedic_12"))),
                           tableOutput(ns("Salida_tabla_2c_RMedic_12")),
                           uiOutput(ns("Controlador2_2c_RMedic")),
                           br(),
                           
                  )
      )
    )
  })
  
  
  
  
  
  
  
  
  
}


