  conditionalPanel("1 == 1",
                   selectInput("variable", "Color:",
                               c("Naranja" = "orange",
                                 "Rojo" = "red",
                                 "Verde" = "green",
                                 "Azul" = "blue", 
                                 "Amarillo" = "yellow", 
                                 "Negro" = "black",
                                 "Celeste" = "skyblue"),multiple=FALSE),
                   
                   #       sliderInput("bins",
                   #                   "Cantidad de muestras de la distribución Normal:",
                   #                   min = 1,
                   #                   max = 200,
                   #                   value = 200,
                   #                   locale="us"),
                   #       
                   # Agregado para seleccionar el rango!!!!
                   #       sliderInput("range", 
                   #                   label = "Rango de Valores Z",
                   #                   min = -4, max = 4, value = c(-1.85, 2.36),
                   #                   step=0.01),
                   br(),
                   numericInput("mu", "Media Poblacional:", min=-1e20,  max=1e20, step= 0.01, value= 106),
                   numericInput("sigma_cuad", "Varianza Poblacional:", min=0,  max=1e20, step= 0.01,  value=64),
               #   numericInput("n", "Tamaño Muestral:", min=1,  max=1e20, step=1, value=10),
               #    numericInput("alfa", "Valor Alfa:", min=0,  max=1, step= 0.01, value=0.05)
               
               br(),
               radioButtons("opciones", "Datos conocidos:",
                            c("Valor de la variable" = "opc1",
                              "Valor Z" = "opc2",
                              "Probabilidad" = "opc3"),
                            selected = "opc1"),
               br(),
               radioButtons("forma", "Forma de cálculo:",
                            c("Menores que..." = "menor",
                              "Mayores que..." = "mayor",
                              "Entre..." = "entre"),
                               selected = "menor"),
               br(),
               uiOutput("gmenu3")
               
                   
                   
                   )