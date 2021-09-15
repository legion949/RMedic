conditionalPanel("1 == 1", 
                 h3("Distribución Normal"),
                 tabsetPanel(type = "tabs", 
                             tabPanel("Variable Original", plotOutput("distPlot1")), 
                             tabPanel("Variable Z", plotOutput("distPlot2"))
                         
                 ),
                 tableOutput("values222"),
                 tableOutput("values111"),
                 uiOutput("text"),
           
                 #       
          
                 h3()
                 
                 
) 