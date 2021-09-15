



# Define UI for dataset viewer application
fluidPage(theme = "styles.css", 
  
  # Application title.
  titlePanel("R+Medic"),

  sidebarLayout(
    sidebarPanel(
   
      # Cargamos el Main Panel
      uiOutput("uiSideBarPanel") # Esto esta en 003Panel
      

    ),
    
    mainPanel(
      uiOutput('uiMainPanel') # Esto esta en 004serverALL.R
    )
  )
)

