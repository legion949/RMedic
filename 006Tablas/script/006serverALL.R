

# Server del SideBarPanel
source("script/006svSideBarPanel.R", local = T)$value



# Server de la Pestania "Base de Datos"
source("../004Base/script/004svMainPanel.R", local = T)$value

# Server de la Pestania "Control"
source("../005Control/script/005svMainPanel.R", local = T)$value


# Server NOX
source("../ZZZ_NOX/NOX.R", local = T)$value
source("../ZZZ_NOX/NOX_01.R", local = T)$value
source("../ZZZ_NOX/NOX_02.R", local = T)$value
source("../ZZZ_NOX/NOX_03.R", local = T)$value
source("../ZZZ_NOX/NOX_04.R", local = T)$value
source("../ZZZ_NOX/NOX_05.R", local = T)$value # Planeta Goku

# Server del Desc General
source("script/006svDescGeneral.R", local = T)$value




# Server del MainPanel Local
source("script/006svMainPanel.R", local = T)$value


# Objeto Salida ui para el MainPanel
output$uiMainPanel <- renderUI({
  
  fluidPage(
  uiOutput("SALIDAmenuDESCRIPTIVAS"),
#  uiOutput("planeta_tablas")
uiOutput("planeta_goku")
  )
  
})