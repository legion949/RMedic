conditionalPanel("1 == 1", 
                 h3("Distribución t Estándard"),
                 textOutput("ER2"),
                 textOutput("ER1"),
                 plotOutput("distPlot"),
                 tableOutput("values"), 
                 br(),
                 h1("Resolución General"),
                 h4(uiOutput("ex7")),
                 h3("Resolución Extendida"),
                 textOutput("text1"),
                 textOutput("text2"),
                 # 
             #     h3(textOutput("text3")),
          
                 br(),
                 br(),
                 
                 h3()
                 
                 
) 
