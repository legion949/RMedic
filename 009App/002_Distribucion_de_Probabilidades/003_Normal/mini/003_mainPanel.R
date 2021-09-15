conditionalPanel("1 == 1", 
                 withMathJax(),
                 h3("Distribución Normal"),
                 plotOutput("distPlot1"),
                 # tabsetPanel(type = "tabs", 
                 #             tabPanel("Variable Original", plotOutput("distPlot1")), 
                 #             tabPanel("Variable Z", plotOutput("distPlot2"))
                 #             # tabPanel("Summary", verbatimTextOutput("summary")), 
                 #             # tabPanel("Table", tableOutput("table"))
                 # ),
                 # #)
                 #)
                 
                 #       
                 #     
                 #       plotOutput("distPlot2"),
                 tableOutput("values1"),
              #   tableOutput("values2"),
              #   tableOutput("values3"),
                 #       plotOutput("distPlot1"),
                 #       # p("A ver que pasa...", textOutput("mu")),
                 #       # textOutput("text1"),
                 #       # textOutput("text2"),
                 #       # textOutput("text3"),
                 #       # textOutput("text4"),
                 #       # textOutput("text5"),
                 #       # textOutput("text6"),
                 #       
                 #       #  h1(paste("La probabilidad comprendida entre", YA,sep="")),
                 #       
                 #       
                 # h3("Resolución (Mucho más desarrollada)"),
                 # h4(textOutput("Libro")),
                 # textOutput("Resol"),
                 # uiOutput("ex6"),
                 # uiOutput("ex7"),
                 # #       
                 # h3("Elementos Teóricos (1)"),
                 # textOutput("text7"),
                 # #       
                 # #       
                 # h3("Elementos Teóricos (2)"),
                 # textOutput("text8"),
                 # #       
                 # h3("Conceptos Claves"),
                 # textOutput("text9"),
                 # #       # uiOutput("ex6"),
                 # #       # uiOutput("ex7"),
                 # #       
                 # #       
                  h3()
                 
                 
) 