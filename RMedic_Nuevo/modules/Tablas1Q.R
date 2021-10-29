## Segmento del UI
Tablas1Q_UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("SeccionTablas1Q"))
 
 
}




## Segmento del server
Tablas1Q_SERVER <- function(input, output, session, 
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
  
  # Todas las tablas 1Q
  Reactive_tabla_1q_RMedic <- reactive({
    
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 1) return(NULL)
   
    
    salida <-  RMedic_1q_tablas(minibase(), decimales())
    
 
     

    # Return Exitoso
    return(salida)
    
    
  })  
  
  

    # Cantidad de tablas
   cantidad_tablas <- reactive({
     
     if(is.null(Reactive_tabla_1q_RMedic)) return(NULL)
     
      # Return Exitoso
      return(length(Reactive_tabla_1q_RMedic()))
   })
   
   # Create all renderTables!!!     
   observe(
          lapply(c(1:cantidad_tablas()), function(i) {
            
    nombre_fusion1 <- paste0('Salida_texto_1q_RMedic_', CifrasPerfectas(i))
    nombre_fusion2 <- paste0('Salida_tabla_1q_RMedic_', CifrasPerfectas(i))
    
    # El rotulo de cada tabla       
    output[[nombre_fusion1]] <- renderText({
      names(Reactive_tabla_1q_RMedic())[i]
    })
    
    # Cada tabla
    output[[nombre_fusion2]] <- renderTable(digits = decimales(), align= "c",{
      Reactive_tabla_1q_RMedic()[[i]]
    })
            
           
            
           })
   ) 
   
        
        
     
    
   output$SeccionTablas1Q <- renderUI({
     
     # Especificaciones de cumplimiento
     if(is.null(casoRMedic())) return(NULL)
     if(casoRMedic() != 1) return(NULL)
     
     
     
     # Si es el caso 1, seguimos!
     div(
       h2("RMedic - Tablas para 1 Variable Categórica"),
       tabsetPanel(id = ns("Tablas_1q"),
                   tabPanel(title = "RMedic Help!", value = 1,
                            fluidRow(
                              column(4, 
                            radioButtons(inputId = "help_tablas_1q",
                                         label = h3("Selección de Ayuda Automática"),
                                         choices = c("RMedic Here!" = 1,
                                                     "Distribución de Frecuencias" = 2,
                                                     "Intervalos de Confianza" = 3)
                                         )
                            ),
                            column(8,
                            br(),
                            conditionalPanel(condition = "input.help_tablas_1q == 1",
                     div(
                       h3("RMedic Here!"),
                       HTML(
                      "Las tablas más utilizadas aplicadas a una variable categórica son:<br/>
                      - Tablas de <b>'Distribución de Frecuencias'</b>.<br/>
                      - Tablas de los <b>'Intervalos de Confianza'</b> para el porcentaje.<br/>
                      Seleccionando la ayuda de cada una encontrarás un miniresumen con
                      detalles teóricos y estructura de la base de datos.<br/>
                      Estos te ayudarán a determinar si estas herramientas pueden ser
                      aplicadas en tu trabajo."
                      )
                      )
                      ),
                            conditionalPanel(condition = "input.help_tablas_1q == 2",
                                             div(
                                               h3("Distribución de Frecuencias"),
                                               HTML("Las tablas de <b>'Distribución de Frecuencias'</b> es uno de los
                                               formatos clásicos más utilizados en revistas y publicaciones para la presentación y resumen de la información de 1 variable categórica. <br/>
                                               Sobre cada una de las categorías que se encuentran dentro de la variable seleccionada
                                               se detalla frecuencias absolutas (FA), total, cociente, frecuencias relativas (FR) y porcentajes (%).<br/>
                                               RMedic agrega además una columna muy útil que fusiona la información de
                                               las frecuencias absolutas y el porcentaje.<br/><br/>
                                                    Se aplica sobre una columna de la base de datos.<br/>
                                                    La variable debe contener al menos una categoría."
                                             )
                                             )
                                             ),
                            conditionalPanel(condition = "input.help_tablas_1q == 3",
                                             div(
                                               h3("Intervalo de Confianza"),
                                               HTML("El <b>'Intervalo de Confianza'</b> para el porcentaje es un rango de valores que tiene cierta
                                               probabilidad de contener al porcentaje problacional de cada una de las categorías
                                               de la variable categórica seleccionada. <br/>
                                               Se presentan intervalos del 90%, 95% y 99%. <br/>
                                               De estas 3 posibilidades debiera elegir el usuario solo uno de ellas. <br/>
                                               El más utilizado es el intervalo del 95%, pero la elección correcta depende de cada trabajo
                                               y cada área del conocimiento. <br/>
                                               Si no sabe cuál elegir, debería buscar en publicaciones de su área
                                               para saber qué intervalo de confianza debería elegir.<br/><br/>
                                                    Se aplica sobre una columna de la base de datos.<br/>
                                                    La variable debe contener al menos una categoría."
                                               )
                                                    )
                                             ),
                            )
                            )
                            ),
                   tabPanel(title = "Distribución de Frecuencias", value = 2,
                            h3(textOutput(ns("Salida_texto_1q_RMedic_01"))),
                            tableOutput(ns("Salida_tabla_1q_RMedic_01")),
                            br()),
                   tabPanel(title = "Intervalos de Confianza", value = 3,
                            h3(textOutput(ns("Salida_texto_1q_RMedic_02"))),
                            tableOutput(ns("Salida_tabla_1q_RMedic_02")),
                            br(),
                            h3(textOutput(ns("Salida_texto_1q_RMedic_03"))),
                            tableOutput(ns("Salida_tabla_1q_RMedic_03")),
                            br(),
                            h3(textOutput(ns("Salida_texto_1q_RMedic_04"))),
                            tableOutput(ns("Salida_tabla_1q_RMedic_04")),
                            br())
       ),
     )
   })
 
  
 
  
  
  
}


