


Ho1C_01_RMedicHelp_UI <- function(id) {
  
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
                      - Gráfico de <b>Violín</b>.<br/>
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
                                "Se presenta un gráfico donde se observa el valor
                                de la media (promedio) y el intervalo de: 
                                media más un desvío estándard, media menos un desvío estándard."
                                )
                              )
                            ),
           conditionalPanel(condition = "input.help_graficos_1c == 3", 
                            div(
                              h3("Gráfico de Medias y Error Estándard"),
                              HTML(
                                "Se presenta un gráfico donde se observa el valor
                                de la media (promedio) y el intervalo de: 
                                media más un error estándard, media menos un error estándard."
                                )
                            )
           ),
           conditionalPanel(condition = "input.help_graficos_1c == 4", 
                            div(
                              h3("Boxplot"),
                              HTML(
                                "Se presenta un gráfico boxplot (caja) o también llamado 
                                box & whisker plot (caja y bigote). La caja se extiende desde 
                                el cuartilo 1 al cuartilo 3, los bigotes se extienden desde el 
                                cuartilo 1 al mínimo y del cuartilo 3 al máximo. Dentro la caja 
                                la mediana es detallada como una línea oscura."
                              )
                            )
           ),
           conditionalPanel(condition = "input.help_graficos_1c == 5", 
                            div(
                              h3("Violín Plot"),
                              HTML(
                                "Se presenta un gráfico de violín. La caja negra interior 
                                corresponde a la caja del boxplot. La mediana es presentada 
                                como un punto blanco dentro de la caja. Simultáneamente 
                                se grafica hacia los margenes laterales la distribución de 
                                la variable. La silueta de la distribución llega hasta 
                                el valor mínimo y máximo de la variable."
                              )
                            )
           ),
           conditionalPanel(condition = "input.help_graficos_1c == 6", 
                            div(
                              h3("Histograma"),
                              HTML(
                                "Se presenta un histograma de frecuencias. La cantidad de 
                                intervalos corresponde a la estimación otorgada por el 
                                cálculo de Sturges. El usuario puede cambiar la cantidad 
                                de intervalos. Los valores extremos de los intervalos pueden 
                                ser abiertos o cerrados a elección del usuario.")
                            )
           ),
           conditionalPanel(condition = "input.help_graficos_1c == 7", 
                            div(
                              h3("Dispersión"),
                              HTML(
                                "Se presenta un gráfico donde todos los valores 
                                de la variable están alineados y son representados por una 
                                circunsferencia."
                                )
                            )
           ),
           conditionalPanel(condition = "input.help_graficos_1c == 8", 
                            div(
                              h3("Puntos"),
                              HTML(
                                "Se presenta un gráfico donde todos los valores 
                                de la variable están alineados respecto al eje X, y 
                                son apilados en la medida de su frecuencia."
                              )
                            )
           ),
           )
           )
  
  
  
}






## Segmento del server
Ho1C_01_RMedicHelp_SERVER <- function(input, output, session,
                                            minibase,
                                            decimales,
                                            control_ejecucion,
                                            tablas_1c,
                                            alfa) {
  
  
  
  
  
  
  
  
  
  
  
  
}







