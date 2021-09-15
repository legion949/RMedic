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
                                 "Celeste" = "skyblue"),multiple=FALSE),
                   
                   
                   # # # DECIMALES
                   numericInput("decimales", "Máx. Decimales:", min=0,  max=1e20, step=1, value=inicio$decimales),  
                   br(),  
                   
                   br(),
                   # numericInput("p", "Prob de Evento favorable (p):", min=0,  max=1, step=0.01, value=0.17),
                   # numericInput("n", "Tamaño Muestral (n):", min=1,  max=1e20, step=1, value=10),
                   numericInput("lambda", "Lambda:", min=0,  max=1e20, step=0.01, value=inicio$lambda),  
                   
                   # # # Valor de "x"
                   textInput("x", "Eventos Favorables (x):", inicio$casos),
                   
                   br(),
                   br(),
                   
                   # # # Salida del detalle de la ecuación
                   checkboxInput('ex6_visible', 'Detalles de ecuación', FALSE),
                   # # #uiOutput('ex6'),
                   
                   # # # Salida de un ejercicio como ejemplo realizado
                   checkboxInput('ex7_visible', 'Ejemplo de Resolución', FALSE),
                   # # #uiOutput('ex6'),
                   
                   # # # Salida de todos los ejercicios que pidio
               #    checkboxInput('ex8_visible', 'Resolución Paso a Paso', FALSE),
                   # # #uiOutput('ex6'),
                   
                   br())