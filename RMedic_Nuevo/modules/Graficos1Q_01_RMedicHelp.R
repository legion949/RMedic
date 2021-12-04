





Graficos1Q_01_RMedicHelp_UI <- function(id) {
  
  ns <- NS(id)
  
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
                      - Gráfico de <b>Tortas</b>.<br/>
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
                                "Se presenta un gráfico que manifiesta las categorías 
                            de la variable en el eje X. La altura de las barras 
                            representa las frecuencias de las categorías en el eje Y."
                              )
                            )
           ),
           conditionalPanel(condition = "input.help_graficos_1q == 3",
                            div(
                              h3("Gráfico de Tortas"),
                              HTML(
                                "Se presenta un gráfico que manifiesta las categorías 
                            de una variable cualitativa como porciones de una torta.
                            El tamaño de cada porción manifiesta la frecuencia de cada 
                            categoría."
                              )
                            )
           ),
    )
  )
  
  
  
}






## Segmento del server
Graficos1Q_01_RMedicHelp_SERVER <- function(input, output, session,
                                            minibase,
                                            decimales,
                                            control_ejecucion,
                                            tablas_1q) {
  
  
  
  
  
  
  
  
  
  
  
  
}







