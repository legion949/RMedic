conditionalPanel("1 == 1", 
                 h3("Intervalor de Confinaza para Distribución Normal"),
                 tabsetPanel(type = "tabs", 
                             tabPanel("Variable Original", plotOutput("distPlot1")), 
                             tabPanel("Variable Z", plotOutput("distPlot2"))
                         
                 ),
            
                 tableOutput("values111"),
                 tableOutput("values222"),
                 uiOutput("text"),
           
                 #       
          
                 h3()
                 
                 
) 