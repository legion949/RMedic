
# 
# library(shiny)
# library(shinyjs)

# Define UI for dataset viewer application
fluidPage(theme = "styles.css", 
  
  # Application title.
  titlePanel("R+Medic"),

  sidebarLayout(
    sidebarPanel(
   
      # Cargamos el Main Panel
      uiOutput("uiSideBarPanel")  # Este objeto esta en "003PANEL"
      

    ),
    
    mainPanel(
     uiOutput('uiMainPanel'),  # Esto esta en 005serverALL.R
    #  uiOutput('uiMainPanel_005CONTROL'),
    #  tabsetPanel(uiOutput("uiMainPanel_005CONTROL")),
      br(), 
      br(), 
      br()
    )
  )
)

