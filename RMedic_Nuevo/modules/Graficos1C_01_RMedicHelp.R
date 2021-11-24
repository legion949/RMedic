


Graficos1C_01_RMedicHelp_UI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(4,
           radioButtons(inputId = "help_graficos_1c",
                        label = h3("Selección de Ayuda Automática"),
                        choices = c("RMedic Here!" = 1,
                                    "Media y Desvío Estándard" = 2,
                                    "Media y Error Estándard" = 3,
                                    "Boxplot" = 4,
                                    "Violín plot" = 5,
                                    "Histograma" = 6,
                                    "Dispersión" = 7,
                                    "Puntos" = 8)
           )),
    column(8,
           conditionalPanel(condition = "input.help_graficos_1c == 1", 
                            div(
                              h3("RMedic Here!"),
                              HTML(
                                "Los gráficos más utilizados aplicados a una variable numérica son:<br/>
                      - Gráfico de <b>Media y Desvío Estándard</b>.<br/>
                      - Gráfico de <b>Media y Error Estándard</b>.<br/>
                      - Gráfico de <b>Boxplot</b>.<br/>
                      - Gráfico de <b>Violín plot</b>.<br/>
                      - Gráfico de <b>Histograma</b>.<br/>
                      - Gráfico de <b>Dispersión</b>.<br/>
                      - Gráfico de <b>Puntos</b>.<br/>
                      Seleccionando la ayuda de cada uno encontrarás un resumen con
                      detalles teóricos y estructura de la base de datos.<br/>
                      Estos te ayudarán a determinar si estas herramientas pueden ser
                      aplicadas en tu trabajo."
                              )
                            )
                            ),
           conditionalPanel(condition = "input.help_graficos_1c == 2", 
                            div(
                              h3("Gráfico de Medias y Desvío Estándard"),
                              HTML(
                                "El gráfico de media y desvío estándard...")
                              )
                            ),
           conditionalPanel(condition = "input.help_graficos_1c == 3", 
                            div(
                              h3("Gráfico de Medias y Error Estándard"),
                              HTML(
                                "El gráfico de media y error estándard...")
                            )
           ),
           conditionalPanel(condition = "input.help_graficos_1c == 4", 
                            div(
                              h3("Boxplot"),
                              HTML(
                                "El gráfico boxplot...")
                            )
           ),
           conditionalPanel(condition = "input.help_graficos_1c == 5", 
                            div(
                              h3("Violin Plot"),
                              HTML(
                                "El gráfico denominado Violín Plot...")
                            )
           ),
           conditionalPanel(condition = "input.help_graficos_1c == 6", 
                            div(
                              h3("Histograma"),
                              HTML(
                                "El histograma...")
                            )
           ),
           conditionalPanel(condition = "input.help_graficos_1c == 7", 
                            div(
                              h3("Dispersión"),
                              HTML(
                                "El gráfico de dispersión...")
                            )
           ),
           conditionalPanel(condition = "input.help_graficos_1c == 8", 
                            div(
                              h3("Puntos"),
                              HTML(
                                "El gráfico de puntos...")
                            )
           ),
           )
           )
  
  
  
}






## Segmento del server
Graficos1C_01_RMedicHelp_SERVER <- function(input, output, session,
                                            minibase, 
                                            batalla_naval,
                                            decimales,
                                            casoRMedic,
                                            tablas_1c) {
  
  
  
  
  
  
  
  
  
  
  
  
}