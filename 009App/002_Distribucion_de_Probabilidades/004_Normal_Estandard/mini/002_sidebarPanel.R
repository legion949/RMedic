  conditionalPanel("1 == 1",
                   withMathJax(),
                   h3("Detalles Gráficos"),
                   selectInput("color", "Color:",
                               c("Naranja" = "orange",
                                 "Rojo" = "red",
                                 "Verde" = "green",
                                 "Azul" = "blue", 
                                 "Amarillo" = "yellow", 
                                 "Negro" = "black",
                                 "Celeste" = "skyblue"),multiple=FALSE),
                   
                   # # # DECIMALES
                   numericInput("decimales", "Máx. Decimales:", min=0,  max=1e20, step=1, value=4),  
                   br(),
                   br(),
                   h3("Parámetros Poblacionales"),
                   h2("$$\\mu = 0$$"),
                   h2("$$\\sigma = 1$$"),
                   br(),
                   h3("Valores Z"),
                   
                   #       sliderInput("bins",
                   #                   "Cantidad de muestras de la distribución Normal:",
                   #                   min = 1,
                   #                   max = 200,
                   #                   value = 200,
                   #                   locale="us"),
                   #       
                   #      Agregado para seleccionar el rango!!!!
                   #       sliderInput("range", 
                   #                   label = "Rango de Valores Z",
                   #                   min = -4, max = 4, value = c(-1.85, 2.36),
                   #                   step=0.01),
                   
                   fluidRow(
                     column(6,
                            radioButtons(inputId = 'aver1', label='Valor Z izquierdo',
                                         choices=c("-Inf"=TRUE,
                                                   "Un valor"=FALSE),
                                         selected = FALSE)
                            ),
                     column(6,
                            conditionalPanel('input.aver1 == FALSE',
                                             numericInput("z_izq", "Valor Z izquierdo (Zi):", min=-1e20,  max=1e20, step= 0.01, value=-1.96)
                            ))
                   ),
                   
                   fluidRow(
                     column(6,
                            radioButtons(inputId = 'aver2', label='Valor Z derecho',
                                         choices=c("Un valor"=FALSE,
                                                   "+Inf"=TRUE),
                                         selected = FALSE)
                     ),
                     column(6,
                            conditionalPanel('input.aver2 == FALSE',
                                             numericInput("z_der", "Valor Z derecho (Zd):", min=-1e20,  max=1e20, step= 0.01, value=1.96)
                            ))
                   ),
                   
                   
                   

                   # 
                   # sliderInput("range", label = "Range:",
                   #             min = -5, max = 5, 
                   #             step=0.01, value = c(-1.96, 1.96),
                   #             format="####.00",
                   #             pre="ANTES", post="DESPUES"),
                   br()
                   
                   )