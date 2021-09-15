conditionalPanel("1 == 1", 
                 withMathJax(),
                 h3("Distribución Normal y Normal Estándard"),
                 tabsetPanel(type = "tabs", 
                             tabPanel("Variable Original",
                                      plotOutput("distPlot1")),
                             tabPanel("Valores Z",
                                      plotOutput("distPlot2")),
              #               tabPanel("Resolución")),
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
             #    tableOutput("values1"),
                 h3()
                 
                 
) )
