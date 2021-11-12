## Segmento del UI
Graficos1Q_UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("SeccionGraficos1Q"))
  
  
}




## Segmento del server
Graficos1Q_SERVER <- function(input, output, session, 
                            minibase, 
                            batalla_naval,
                            casoRMedic = casoRMedic,
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

  
  callModule(module = Graficos1Q_02_Barras_SERVER, id =  "graficos03B",
             minibase = minibase,
             batalla_naval = batalla_naval,
             decimales = decimales,
             casoRMedic = casoRMedic,
             DF_interna = DF_interna)
  
  
  callModule(module = Graficos1Q_03_Tortas_SERVER, id =  "graficos03C",
             minibase = minibase,
             batalla_naval = batalla_naval,
             decimales = decimales,
             casoRMedic = casoRMedic,
             DF_interna = DF_interna)
  
  
 
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
                           Graficos1Q_02_Barras_UI(ns("graficos03B"))
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
                         
                           #plotOutput(ns("grafico_tortas_1q"))
                           Graficos1Q_03_Tortas_UI(ns("graficos03C"))
                           )
      ),
    )
  })
  
  
  
  
  
  
}


