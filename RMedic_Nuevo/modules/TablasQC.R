## Segmento del UI
TablasQC_UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("SeccionTablasQC"))
  
  
}




## Segmento del server
TablasQC_SERVER <- function(input, output, session, 
                            minibase, 
                            batalla_naval,
                            decimales) {
  
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  # Caso 5: QC
  casoRMedic <- reactive({
    
    if(is.null(batalla_naval())) return(NULL)
    if(is.null(batalla_naval()[[4]])) return(NULL)
    if(length(batalla_naval()[[4]]) == 0) return(NULL)
    if(batalla_naval()[[4]] == '') return(NULL)
    casoRMedic <- batalla_naval()[[4]]
    #casoRMedic <- as.numeric(as.character(as.vector(batalla_naval()[[4]])))
    casoRMedic
    
  })
  
  
  Rotulo_recategorizacion <- reactive({
    
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 5) return(NULL)
    
    referencias <- colnames(minibase())
    
    armado <- paste0("Recategorización para la variable '", referencias[2], "'")
    
    armado <- HTML(armado)
    
    return(armado)
  })
  
  Referencias_var_qc <- reactive({
    
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 5) return(NULL)
    
    referencias <- colnames(minibase())
    
    armado <- paste0(paste0("En filas: ", referencias[1], "(Categórica)"), 
                     "<br/>", 
                     paste0("En columnas: ", referencias[2], " (Recategorizada)")
                    )
    
    armado <- HTML(armado)
    
    return(armado)
  })
  
  output$Controlador_qc_RMedic <- renderUI({
    
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 5) return(NULL)
    
    cantidad_cortes <- nclass.Sturges(minibase()[,2])
    tabla <- table(minibase()[,2])
    cantidad_categorias <- length(names(tabla))
    if(cantidad_categorias < cantidad_cortes) cantidad_cortes <- cantidad_categorias
    
    div(
      fluidRow(
        column(4,
               numericInput(
                 inputId = ns("x_min"),
                 label = "Valor mínimo: ",
                 value = min(minibase()[,2]),
                 min = NA,
                 max = min(minibase()[,2]),
                 step = 0.01,
                 width = NULL
               ),
               numericInput(
                 inputId = ns("x_max"),
                 label = "Valor máximo: ",
                 value = max(minibase()[,2]),
                 min = max(minibase()[,2]),
                 max = NA,
                 step = 0.01,
                 width = NULL
               )
        ),
        column(4,
               radioButtons(inputId = ns("x_side"), 
                            label = "Cierre del intervalo: ", choices = c("A la Derecha" = T , "A la Izquierda" = F)
               )
        ),
        column(4, 
               numericInput(
                 inputId = ns("x_breaks"),
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
  observeEvent(input[[ns("x_min")]],{
    
    if(input[[ns("x_min")]] > min(minibase()[,2])) {
      
      updateNumericInput(session, inputId = ns("x_min"),
                         label = "Valor mínimo: ",
                         value = min(minibase()[,2]),
                         min = NA,
                         max = min(minibase()[,2]),
                         step = 0.01
      )
      
      
      
    }
  })
  
  
  # Variable criterio de inclusion
  observeEvent(input[[ns("x_max")]],{
    
    if(input[[ns("x_max")]] < max(minibase()[,2])) {
      
      updateNumericInput(session, inputId = ns("x_max"),
                         label = "Valor máximo: ",
                         value = max(minibase()[,2]),
                         min = max(minibase()[,2]),
                         max = NA,
                         step = 0.01
      )
      
      
      
    }
  })
  
  
  # Todas las tablas QC
  Reactive_tabla_qc_RMedic <- reactive({
    
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 5) return(NULL)
    
    
    
    # Nota: al valor input$x_breaks lo tuve que poner
    #      como na.omit(input$x_breaks)[1] por que algunas veces
    #      otorga un vector con dos valores, pero uno de ellos es NA.
    
    
    
    salida <-  RMedic_qc_tablas(input_base =  minibase(),
                                input_decimales = decimales(),
                                input_min = input$x_min,
                                input_max = input$x_max,
                                input_breaks = na.omit(input$x_breaks)[1],
                                input_side = input$x_side
    )
    
    # salida[[11]] <- as.matrix(table(minibase()))
    # salida[[11]] <- as.matrix(salida[[11]])
     salida[[11]][1,1] <- as.character(salida[[11]][1,1])
    
    # for(k in 1:length(salida[[11]])) {
    # salida[[11]][[k]][,2] <- as.character(salida[[11]][[k]][,2])
    # salida[[11]][[k]][,3] <- as.character(salida[[11]][[k]][,3])
    # salida[[11]][[k]][,5] <- as.character(salida[[11]][[k]][,5])
    # }
    
    # salida <- salida[[c(1:10)]]
    
    # Return Exitoso
    return(salida)
    
    
  })  
  
  
  
  # Cantidad de tablas
  cantidad_tablas <- reactive({
    
    if(is.null(Reactive_tabla_qc_RMedic)) return(NULL)
    
    # Return Exitoso
    return(length(Reactive_tabla_qc_RMedic()))
  })
  
  # Create all renderTables!!!     
  observe(
    lapply(c(1:cantidad_tablas()), function(i) {
      
      nombre_fusion1 <- paste0('Salida_texto_qc_RMedic_', CifrasPerfectas(i))
      nombre_fusion2 <- paste0('Salida_tabla_qc_RMedic_', CifrasPerfectas(i))
      
      # El rotulo de cada tabla       
      output[[nombre_fusion1]] <- renderText({
        names(Reactive_tabla_qc_RMedic())[i]
      })
      
      status_rownames <- F
      if(i >= 11) status_rownames <- T
      
      decimales_internos <- decimales()
      if(i == 1) decimales_internos <- 0
      # Cada tabla
      output[[nombre_fusion2]] <- renderTable(digits = decimales_internos, align= "c", rownames = status_rownames, {
        Reactive_tabla_qc_RMedic()[[i]]
        
      })
      
      
      
    })
  )
  
    
 
  
  
  
  
  
  
  output$SeccionTablasQC <- renderUI({
    
    # Especificaciones de cumplimiento
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 5) return(NULL)
    
    # Si es el caso 5, seguimos!
    div(
      h2("RMedic - Tablas para 1 Variable Categórica y 1 Variable Numérica, o viceversa"),
      tabsetPanel(id = "Tablas_qc",
                  tabPanel("RMedic Help!", value = 1,
                           fluidRow(
                             column(4,
                                    radioButtons(inputId = "help_tablas_qc",
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
                                    conditionalPanel(condition = "input.help_tablas_qc == 1", 
                                                     div(
                                                       h3("RMedic Here!"),
                                                       HTML(
                                                         "Las tablas más utilizadas aplicadas a una variable numérica particionada
                                                         por una variable categórica son:<br/>
                      - Tablas de <b>Medidas Resumen</b>.<br/>
                      - Tablas de <b>Medidas Posición</b>.<br/>
                      - Tablas de <b>Medidas Dispersión</b>.<br/>
                      - Tablas de <b>Medidas Intervalos de Confianza</b>.<br/>
                      - Tablas de <b>Distribución de Frecuencias</b>.<br/><br/>
                      
                      En todos los casos RMedic agrega a demás de las medidas de cada partición, las medidas generales de la variable sin particionar
                      dentro de la misma tabla. De esa manera se puede comparar cada categoría con las medidas de todo el conjunto de datos. <br/><br/>
                      Seleccionando la ayuda de cada una encontrarás un resumen con
                      detalles teóricos y estructura de la base de datos.<br/>
                      Estos te ayudarán a determinar si estas herramientas pueden ser
                      aplicadas en tu trabajo."
                                                       )
                                                     )),
                                    conditionalPanel(condition = "input.help_tablas_qc == 2", 
                                                     div(
                                                       h3("Medidas Resumen"),
                                                       HTML(
                                                         "Las tablas de <b>Medidas Resumen</b> contienen solo algunos de los componentes
                      que pueden encontrarse en las tablas de <b>Medidas de Posición</b> y <b>Medidas de Dispersión</b>.<br/>
                      En este caso RMedic detalla media, desvío estándard y n; para cada categoría y en general.<br/>"
                                                       )
                                                     )
                                    ),
                                    conditionalPanel(condition = "input.help_tablas_qc == 3", 
                                                     div(
                                                       h3("Medidas de Posición"),
                                                       HTML(
                                                         "Las <b>Medidas Posición</b> son un conjunto de estimaciones que podrían
                      representarse como un punto en una recta, siendo la recta nuestra variable numérica.<br/>
                      Las estimaciones clásicas son: mínimo, media, mediana y máximo.<br/>
                      RMedic agrega: cuartiles, deciles y percentiles.<br/><br/>
                      Todo es detallado para cada categoría de la variable categórica y para el general.<br/><br/>
                      Si bien 'n' (cantidad de datos) no es una medida de posición, es un detalle sumamente importante que debe estar siempre
                      en todo tabla.<br/><br/>
                      Las medidas de posición particioandas se aplican sobre dos columnas de la base de datos, siendo una numérica y la otra categórica, o viceversa.<br/>
                      Solo se trabaja con las filas que simultáneamente tienen un dato en ambas variables de manera simultánea.<br/>
                      "
                                                       )
                                                     )),
                                    conditionalPanel(condition = "input.help_tablas_qc == 4", 
                                                     div(
                                                       h3("Medidas de Dispersión"),
                                                       HTML(
                                                         "Las <b>Medidas Dispersión</b> son un conjunto de estimaciones 
                      manifiestan la variabilidad de nuestros datos.<br/>
                      Las estimaciones clásicas son: rango, varianza, desvío estándard y error estándard.<br/>
                      RMedic agrega además: Rango Intercuartílico y Desviación Intercuartílica.<br/><br/>
                      Si bien 'n' (cantidad de datos) no es una medida de posición, es un detalle sumamente importante que debe estar siempre
                      en todo tabla.<br/><br/>
                      Las medidas de dispersión particioandas se aplican sobre dos columnas de la base de datos, siendo una numérica y la otra categórica, o viceversa.<br/>
                      Solo se trabaja con las filas que simultáneamente tienen un dato en ambas variables de manera simultánea.<br/>
")
                                                     )
                                    ),
                                    conditionalPanel(condition = "input.help_tablas_qc == 5", 
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
                                               Los Intervalos de Confianza particioandos se aplican sobre dos columnas de la base de datos, siendo una numérica y la otra categórica, o viceversa.<br/>
                                                Solo se trabaja con las filas que simultáneamente tienen un dato en ambas variables de manera simultánea.<br/>
"
                                                       )
                                                     )
                                    ),
                                    conditionalPanel(condition = "input.help_tablas_qc == 6", 
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
                           h3(textOutput(ns("Salida_texto_qc_RMedic_01"))),
                           tableOutput(ns("Salida_tabla_qc_RMedic_01")),
                           br()),
                  tabPanel("Medidas de Posición", value = 3,
                           h3(textOutput(ns("Salida_texto_qc_RMedic_02"))),
                           tableOutput(ns("Salida_tabla_qc_RMedic_02")),
                           br(),
                           h3(textOutput(ns("Salida_texto_qc_RMedic_03"))),
                           tableOutput(ns("Salida_tabla_qc_RMedic_03")),
                           br(),
                           h3(textOutput(ns("Salida_texto_qc_RMedic_04"))),
                           tableOutput(ns("Salida_tabla_qc_RMedic_04")),
                           br(),
                           h3(textOutput(ns("Salida_texto_qc_RMedic_05"))),
                           tableOutput(ns("Salida_tabla_qc_RMedic_05")),
                           br()),
                  tabPanel("Medidas de Dispersión", value = 4,
                           h3(textOutput(ns("Salida_texto_qc_RMedic_06"))),
                           tableOutput(ns("Salida_tabla_qc_RMedic_06")),
                           br(),
                           h3(textOutput(ns("Salida_texto_qc_RMedic_07"))),
                           tableOutput(ns("Salida_tabla_qc_RMedic_07")),
                           br()),
                  tabPanel("Intervalos de Confianza", value = 5,
                           h3(textOutput(ns("Salida_texto_qc_RMedic_08"))),
                           tableOutput(ns("Salida_tabla_qc_RMedic_08")),
                           br(),
                           h3(textOutput(ns("Salida_texto_qc_RMedic_09"))),
                           tableOutput(ns("Salida_tabla_qc_RMedic_09")),
                           br(),
                           h3(textOutput(ns("Salida_texto_qc_RMedic_10"))),
                           tableOutput(ns("Salida_tabla_qc_RMedic_10")),
                           br()
                  ),
                  tabPanel("Distribución de Frecuencias", value = 6,
                           h3(Rotulo_recategorizacion()),
                           uiOutput(ns("Controlador_qc_RMedic")),
                           br(), 
                           h3(textOutput(ns("Salida_texto_qc_RMedic_11"))),
                           Referencias_var_qc(),
                           tableOutput(ns("Salida_tabla_qc_RMedic_11")),
                           br(), 
                           h3(textOutput(ns("Salida_texto_qc_RMedic_12"))),
                           Referencias_var_qc(),
                           tableOutput(ns("Salida_tabla_qc_RMedic_12")),
                           br(), 
                           h3(textOutput(ns("Salida_texto_qc_RMedic_13"))),
                           Referencias_var_qc(),
                           tableOutput(ns("Salida_tabla_qc_RMedic_13")),
                           br(), 
                           h3(textOutput(ns("Salida_texto_qc_RMedic_14"))),
                           Referencias_var_qc(),
                           tableOutput(ns("Salida_tabla_qc_RMedic_14"))
                           # br(),
                           # h3(textOutput(ns("Salida_texto_2c_RMedic_12"))),
                           # tableOutput(ns("Salida_tabla_2c_RMedic_12")),
                           # uiOutput(ns("Controlador2_2c_RMedic")),
                           # br(),
                          # "Vacío por el momento!"
                           
                  )
      )
    )
  })
  
  
  
  
  
  
  
  
  
}


