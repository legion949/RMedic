conditionalPanel("1 == 1", 
                 h3("Distribución Poisson"),
                 tabsetPanel(type = "tabs", 
                             tabPanel("Distribución de Poisson", plotOutput("distPlot1")), 
                             tabPanel("Distribución de Poisson Acumulada", plotOutput("distPlot2"))
                             # tabPanel("Summary", verbatimTextOutput("summary")), 
                             # tabPanel("Table", tableOutput("table"))
                 ),
                 #)
                 #)
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
                 #       
                 #     
                 #       plotOutput("distPlot2"),
                 h3("Probabilidad de un valor de X"),
                 tableOutput("values1"),
                 h3("Resolución (Tabla de Probabilidades)"),
                 tableOutput("values2"),
                 # tableOutput("values3"),
                 #       plotOutput("distPlot1"),
                 #       # p("A ver que pasa...", textOutput("mu")),
                 textOutput("text1"),
                 textOutput("text2"),
                 #       # textOutput("text3"),
                 #       # textOutput("text4"),
                 #       # textOutput("text5"),
                 #       # textOutput("text6"),
                 #       
                 #       #  h1(paste("La probabilidad comprendida entre", YA,sep="")),
                 #       
                 #       
          #       h3("Lo que falta..."),
                 #    h4("Falta agregar script para obtener la probabilidad en un rango... Por ejemplo la probabilidad de x entre 2 y 8..."),
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
