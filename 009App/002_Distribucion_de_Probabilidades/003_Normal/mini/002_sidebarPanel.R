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
                   numericInput("decimales", "Máx. Decimales:", min=0,  max=1e20, step=1, value=2), 
                   
                   selectInput("unidades", "Unidades:",
                               c("Metro (m)" = "m",
                                 "Centrímetro (cm)" = "cm",
                                 "Kilómetro (Km)" = "Km",
                                 "Gramo (g)" = "g",
                                 "Voltios" = "v",
                                 "Joules" = "J"),multiple=FALSE),
                   
                   br(),
                   
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
                   h3("Parámetros Poblacionales"),
                   numericInput("mu", "Media Poblacional:", min=-1e20,  max=1e20, step= 0.01, value= 10.17),
                   numericInput("sigma_cuad", "Varianza Poblacional:", min=0,  max=1e20, step= 0.01,  value=2.45),
               #   numericInput("n", "Tamaño Muestral:", min=1,  max=1e20, step=1, value=10),
               #    numericInput("alfa", "Valor Alfa:", min=0,  max=1, step= 0.01, value=0.05)
               br()
                   
                   
                   )