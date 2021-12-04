


GraficosQC_01_RMedicHelp_UI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(4,
           radioButtons(inputId = "help_graficos_qc",
                        label = h3("Selección de Ayuda Automática"),
                        choices = c("RMedic Here!" = 1,
                                    "Media y Desvío Estándard" = 2,
                                    "Media y Error Estándard" = 3,
                                    "Boxplot" = 4,
                                    "Violín plot" = 5,
                                    "Dispersión" = 6)
           )),
    column(8,
           conditionalPanel(condition = "input.help_graficos_qc == 1", 
                            div(
                              h3("RMedic Here!"),
                              HTML(
                                "Los gráficos más utilizados aplicados a dos variables siendo 1 categórica y 1 numérica son:<br/>
                      - Gráfico de <b>Media y Desvío Estándard</b>.<br/>
                      - Gráfico de <b>Media y Error Estándard</b>.<br/>
                      - Gráfico de <b>Boxplot</b>.<br/>
                      - Gráfico de <b>Violín</b>.<br/>
                      - Gráfico de <b>Dispersión</b>.<br/>
                      Seleccionando la ayuda de cada uno encontrarás un resumen con
                      detalles teóricos y estructura de la base de datos.<br/>
                      Estos te ayudarán a determinar si estas herramientas pueden ser
                      aplicadas en tu trabajo."
                              )
                            )
                            ),
           conditionalPanel(condition = "input.help_graficos_qc == 2", 
                            div(
                              h3("Gráfico de Medias y Desvío Estándard"),
                              HTML(
                                "Se presenta un gráfico donde se observa el valor
                                de la media (promedio) y el intervalo de: 
                                media más un desvío estándard, media menos un desvío estándard."
                                )
                              )
                            ),
           conditionalPanel(condition = "input.help_graficos_qc == 3", 
                            div(
                              h3("Gráfico de Medias y Error Estándard"),
                              HTML(
                                "Se presenta un gráfico donde se observa el valor
                                de la media (promedio) y el intervalo de: 
                                media más un error estándard, media menos un error estándard."
                                )
                            )
           ),
           conditionalPanel(condition = "input.help_graficos_qc == 4", 
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
           conditionalPanel(condition = "input.help_graficos_qc == 5", 
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
           conditionalPanel(condition = "input.help_graficos_qc == 6", 
                            div(
                              h3("Dispersión"),
                              HTML(
                                "Se presenta un gráfico donde todos los valores 
                                de la variable están alineados y son representados por una 
                                circunsferencia."
                                )
                            )
           )
           )
           )
  
  
  
}






## Segmento del server
GraficosQC_01_RMedicHelp_SERVER <- function(input, output, session,
                                            minibase,
                                            decimales,
                                            control_ejecucion,
                                            tabla_qc) {
  
  
  
  
  
  
  
  
  
  
  
  
}