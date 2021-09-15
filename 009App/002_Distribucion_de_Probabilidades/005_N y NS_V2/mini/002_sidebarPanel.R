  conditionalPanel("1 == 1",
                   withMathJax(),
                   h3("Detalles Gráficos"),
                   fluidRow(
                     column(4,
                   selectInput("color", "Color:",
                               c("Naranja" = "orange",
                                 "Rojo" = "red",
                                 "Verde" = "green",
                                 "Azul" = "blue", 
                                 "Amarillo" = "yellow", 
                                 "Negro" = "black",
                                 "Celeste" = "skyblue"),multiple=FALSE)),
                   
                   # # # DECIMALES
                   column(4,
                   numericInput("decimales", "Máx. Decimales:", min=0,  max=1e20, step=1, value=2))
                   
                  ),
                   
                   br(),
                   
                
                   br(),
                   h3("Parámetros Poblacionales"),
                   fluidRow(
                     column(4,
                        numericInput("mu", "Media Poblacional:", min=-1e20,  max=1e20, step= 0.01, value= 10.17)),
                     column(4,
                          numericInput("sigma_cuad", "Varianza Poblacional:", min=0,  max=1e20, step= 0.01,  value=2.45))),
                   
                   br(),
                   h3("Resolución"),
                  radioButtons("forma", "Forma de cálculo:",
                               c("Menores que..." = "menor",
                                 "Mayores que..." = "mayor",
                                 "Entre..." = "entre"),
                               selected = "menor"),
                   
                   
                 #  checkboxInput("calc2", "Calcular...", FALSE),
                   
               #   numericInput("n", "Tamaño Muestral:", min=1,  max=1e20, step=1, value=10),
               #    numericInput("alfa", "Valor Alfa:", min=0,  max=1, step= 0.01, value=0.05)
               br()
                   
                   
                   )