
source(file="mini/004_lib.R", local= T)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Distribución Normal"),
  
  # Lenguaje Matematico
  withMathJax(),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
       
      withMathJax(),
      uiOutput("gmenu1")
    ),
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
      
      uiOutput("gmenu2")
      
    )
    
    
    
  )
))




#  if (!is.na(input$decimal1)) input$range[1] <- input$decimal1
