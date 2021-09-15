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
                   h3("Parámetros Muestrales"),
                   numericInput("df", "Grados de Libertad (n-1):", min=1,  max=1e20, step= 1,  value=13),
                   
                   fluidRow(
                     column(6,
                            radioButtons(inputId = 'aver1', label='Valor t izquierdo',
                                         choices=c("-Inf"=TRUE,
                                                   "Un valor"=FALSE),
                                         selected = FALSE)
                            ),
                     column(6,
                            conditionalPanel('input.aver1 == FALSE',
                     # uiOutput("t_izq")
                     numericInput("t_izq", "Valor t izquierdo (ti):", min=-1e20,  max=1e20, step= 0.01, value=-1.96)
                            ))
                   ),
                   
                   fluidRow(
                     column(6,
                            radioButtons(inputId = 'aver2', label='Valor t derecho',
                                         choices=c("Un valor"=FALSE,
                                                   "+Inf"=TRUE),
                                         selected = FALSE)
                     ),
                     column(6,
                            conditionalPanel('input.aver2 == FALSE',
                                 #uiOutput("t_der")
                            numericInput("t_der", "Valor t derecho (td):", min=-1e20,  max=1e20, step= 0.01, value=1.96)
                                             )
                            )
                   ),
                   
                   
                   

                   # 
                   # sliderInput("range", label = "Range:",
                   #             min = -5, max = 5, 
                   #             step=0.01, value = c(-1.96, 1.96),
                   #             format="####.00",
                   #             pre="ANTES", post="DESPUES"),
                   br()
                   
                   )