conditionalPanel("1 == 1", 
                 h3("Distribución t de Student"),
                 tabsetPanel(type = "tabs", 
                            # tabPanel("Variable Original", plotOutput("distPlot333")), 
                             tabPanel("Variable t", plotOutput("distPlot222"))
                         
                 ),
            
             #    tableOutput("values111"),
                 tableOutput("values222"),
                 tableOutput("values333"),
          #       uiOutput("text"),
           
                 #       
          
                 h3()
                 
                 
) 