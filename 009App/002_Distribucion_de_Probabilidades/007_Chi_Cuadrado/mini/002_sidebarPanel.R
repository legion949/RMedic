  conditionalPanel("1 == 1",
                  # withMathJax(),
                  h3("Detalles Gráficos"),
                  fluidRow(
                    column(6,
                   selectInput("variable", "Color:",
                               c("Naranja" = "orange",
                                 "Rojo" = "red",
                                 "Verde" = "green",
                                 "Azul" = "blue", 
                                 "Amarillo" = "yellow", 
                                 "Negro" = "black",
                                 "Celeste" = "skyblue"),multiple=FALSE)),
                   column(6,
                  # # # DECIMALES
                  numericInput("decimales", "Máx. Decimales:", min=0,  max=1e20, step=1, value=4)  
                  )),
                   br(),
                   br(),
                  
                  h3("Parámetros"),
                  fluidRow(
                    column(6,
                  numericInput("df", "Grados de Libertad:", min=1,  max=1e20, step= 1, value=5)), 
                  column(6, 
                  numericInput("chi_izq", "Chi Izq:", min=0,  max=1e20, step= 0.01, value=1.17)
                  )),
                  fluidRow(
                    column(6,
                           radioButtons(inputId = 'aver2', label='Valor chi derecho',
                                        choices=c("Un valor"=FALSE,
                                                  "+Inf"=TRUE),
                                        selected = FALSE)
                    ),
                    column(6,
                           conditionalPanel('input.aver2 == FALSE',
                           numericInput("chi_der", "Chi Der:", min=0,  max=1e20, step= 0.01, value=8)
                           ))
                  )
                         
                
                   # br(),
                   # br(),
                   # numericInput("varB", "Varianza Muestral de B:", min=0,  max=1e20, step= 0.01,  value=2.45),
                   # numericInput("dfB", "Grados de Libertad de B (nB -1):", min=1,  max=1e20, step= 1,  value=28)
                   
                   
                   )