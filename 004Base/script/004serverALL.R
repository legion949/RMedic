


# Server del SideBarPanel
source("script/004svSideBarPanel.R", local = T)$value




# Server del MainPanel
source("script/004svMainPanel.R", local = T)$value







# Objeto Salida ui para el MainPanel
output$uiMainPanel <- renderUI({
  
  uiOutput("SALIDAmenuBASE")
  
})