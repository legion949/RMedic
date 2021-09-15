  conditionalPanel("1 == 1",
                   withMathJax(),
                   h3("Detalles Gráficos"),
                   fluidRow(
                     column(4,
                            selectInput("variable", "Color:",
                                        c("Naranja" = "orange",
                                          "Rojo" = "red",
                                          "Verde" = "green",
                                          "Azul" = "blue", 
                                          "Amarillo" = "yellow", 
                                          "Negro" = "black",
                                          "Celeste" = "skyblue"),multiple=FALSE)),
                     column(4,
                            # # # DECIMALES
                            numericInput("decimales", "Máx. Decimales:", min=0,  max=1e20, step=1, value=4)  
                     )),
                   br(), 
                   br(),
                   h3("Parámetros"),
                   fluidRow(
                     column(6,
                        numericInput("dfA", "Grados de Libertad A (nA -1):", min=2,  max=1e20, step= 1,  value=5)),
                     column(6,
                        numericInput("dfB", "Grados de Libertad B (nB -1):", min=2,  max=1e20, step= 1,  value=12))),
                   fluidRow(
                     column(6,
                        numericInput("f_izq", "Valor F izquierdo (Fi):", min=0,  max=1e20, step= 0.01, value=1.17)),
                     column(6,
                            fluidRow(
                              column(6,
                                     radioButtons(inputId = 'aver2', label='Valor F derecho',
                                                  choices=c("Un valor"=FALSE,
                                                            "+Inf"=TRUE),
                                                  selected = FALSE)
                              ),
                              column(6,
                                     conditionalPanel('input.aver2 == FALSE',
                                                      numericInput("f_der", "Chi Der:", min=0,  max=1e20, step= 0.01, value=8)
                                     ))
                            )))
                  
                   
                   )