


# Server del SideBarPanel
source("script/005svSideBarPanel.R", local = T)$value



# Server de la Pestania "Base de Datos"
source("../004Base/script/004svMainPanel.R", local = T)$value


# Server del MainPanel de la pestania "Control"
source("script/005svMainPanel.R", local = T)$value







# Objeto Salida ui para el MainPanel
output$uiMainPanel <- renderUI({
  
  uiOutput("SALIDAmenuCONTROL")
  
})