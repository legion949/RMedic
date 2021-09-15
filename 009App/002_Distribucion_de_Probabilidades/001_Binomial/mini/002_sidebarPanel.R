  conditionalPanel("1 == 1",
                   # # # AYUDA
                   checkboxInput('ex5_visible', 'Mostrar AYUDA!', FALSE),
                   uiOutput('ex5'),
                   
                   # # # COLORES
                   selectInput("variable_color", "Color:",
                               c("Naranja" = "orange",
                                 "Rojo" = "red",
                                 "Verde" = "green",
                                 "Azul" = "blue", 
                                 "Amarillo" = "yellow", 
                                 "Negro" = "black",
                                 "Celeste" = "skyblue"),multiple=FALSE, inicio$color_inicial),
                   
                   # # # DECIMALES
                   numericInput("decimales", "Máx. Decimales:", min=0,  max=1e20, step=1, value=inicio$decimales),  
                   br(),
                   
                   # # # Valor p
                   numericInput("p", "Prob de Evento favorable (p):", min=0,  max=1, step=0.01, value=inicio$valor_prob ),
                   
                   # # # Valor de "n"
                   numericInput("n", "Tamaño Muestral (n):", min=1,  max=1e20, step=1, value=inicio$total),
                   #numericInput("x", "Eventos Favorables (x):", min=1,  max=1e20, step=1, value=2),  
                   
                   # # # Valor de "x"
                   textInput("x", "Eventos Favorables (x):", inicio$casos),
                   
                   br(),
                   
                   # # # Salida del detalle de la ecuación
                   checkboxInput('ex6_visible', 'Detalles de ecuación', FALSE),
                   # # #uiOutput('ex6'),
                   
                   # # # Salida de un ejercicio como ejemplo realizado
                   checkboxInput('ex7_visible', 'Ejemplo de Resolución', FALSE),
                   # # #uiOutput('ex6'),
                   
                   # # # Salida de todos los ejercicios que pidio
                   # checkboxInput('ex8_visible', 'Resolución Paso a Paso', FALSE),
                   # # #uiOutput('ex6'),
                   
                   
                   br())
