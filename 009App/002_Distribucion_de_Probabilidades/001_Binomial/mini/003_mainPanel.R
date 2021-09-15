conditionalPanel("1 == 1", 
                 # # # Pestañas
                 # h3("Gráficos"),
                 h3("Distribución Binomial"),
                 tabsetPanel(type = "tabs", 
                             tabPanel("Distribución Binomial", plotOutput("distPlot1")), 
                             tabPanel("Distribución Binomial Acumulada", plotOutput("distPlot2"))
                             # tabPanel("Summary", verbatimTextOutput("summary")), 
                             # tabPanel("Table", tableOutput("table"))
                 ),
                 br(),
                 br(),
                 
                 # Salida... ecuacion binomial
                 uiOutput('ex6'),
                 
                 # Salida... un ejercicio a mano
                 uiOutput('ex7'),
                 
                 # Salida... todos los ejercicios a mano
              #   uiOutput('ex8'),
                 
                 br(),
                 br(),
                 
                 # # # RESOLUCION
                 h3("Resolución"),
                 
                 # # # Tabla de los valores que pidio...
                 tableOutput("Values1"),
                 br(),
                 br(),
                 
                 # # # Tabla de Probabilidades completa
                 h3("Tabla de Probabilidades Completa"),
                 tableOutput("Values2"),
                 br(),
                 br(),
                 
                 # # # Textos de control
                 textOutput("text1"),
                 textOutput("text2"),
                 #       # textOutput("text3"),
                 #       # textOutput("text4"),
                 #       # textOutput("text5"),
                 #       # textOutput("text6"),
                 #       
          #       h3("Lo que falta..."),
          #      h4("Bibliografia y fórmulas"),
                 # # # checkboxInput('ex6_visible', 'Show Example 5', FALSE),
                 # # 3uiOutput('ex6'),
                 #       #  h1(paste("La probabilidad comprendida entre", YA,sep="")),
                 #       
                 #       
                 #       h3("Resolución (Mucho más desarrollada)"),
                 #       h4(textOutput("Libro")),
                 #       textOutput("Resol"),
                 #       uiOutput("ex6"),
                 #       uiOutput("ex7"),
                 #       #       
                 #       h3("Elementos Teóricos (1)"),
                 #       textOutput("text7"),
                 #       #       
                 #       #       
                 #       h3("Elementos Teóricos (2)"),
                 #       textOutput("text8"),
                 #       #       
                 #       h3("Conceptos Claves"),
                 #       textOutput("text9"),
                 #       #       # uiOutput("ex6"),
                 #       #       # uiOutput("ex7"),
                 #       #       
                 #       
                 h3()
) 
