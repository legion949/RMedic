



# Define UI for dataset viewer application
fluidPage(
  #theme = "../../sss/styles.css", 
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  # Application title.
  titlePanel("R+Medic"),

  sidebarLayout(
    sidebarPanel(
    
      # Cargamos el Siderbar Panel
      uiOutput("uiSideBarPanel")
      

    ),
    
    
    mainPanel(
     
     uiOutput('uiMainPanel')
    )
  )
)

