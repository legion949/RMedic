
# 
# library(shiny)
library(shinyjs)

# Define UI for dataset viewer application
fluidPage(theme = "styles.css", 
  
  # Application title.
  titlePanel("R+Medic"),

  sidebarLayout(
    sidebarPanel(
   
      # Cargamos el Main Panel
      uiOutput("uiSideBarPanel")
      

    ),
    
    # Show a summary of the dataset and an HTML table with the
    # requested number of observations. Note the use of the h4
    # function to provide an additional header above each output
    # section.
    mainPanel(
      uiOutput('uiMainPanel')
    )
  )
)

