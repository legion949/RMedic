

# Server del SideBarPanel
source("script/007svSideBarPanel.R", local = T)$value



# Server de la Pestania "Base de Datos"
source("../004Base/script/004svMainPanel.R", local = T)$value

# Server de la Pestania "Control"
source("../005Control/script/005svMainPanel.R", local = T)$value

# Carga NOX
source("../ZZZ_NOX/NOX.R", local = T)$value
source("../ZZZ_NOX/NOX_01.R", local = T)$value
source("../ZZZ_NOX/NOX_02.R", local = T)$value
source("../ZZZ_NOX/NOX_03.R", local = T)$value
source("../ZZZ_NOX/NOX_04.R", local = T)$value
source("../ZZZ_NOX/NOX_05.R", local = T)$value

# Server de la Pestania "Tablas"
source("../006Tablas/script/006svDescGeneral.R", local = T)$value
source("../006Tablas/script/006svMainPanel.R", local = T)$value


# Server elementos Graficos Generales
source("script/007svGrafGeneral_01.R", local = T)$value
source("script/007svGrafGeneral_02.R", local = T)$value
source("script/007svGrafGeneral_03.R", local = T)$value

# Server Main Panel de Graficos
source("script/007svMainPanel.R", local = T)$value


# # Server elementos Barras Q1
# source("../001Q/001BarrasQ/script/002svMainPanel.R", local = T)$value
# 
# 
# # Server elementos Tortas Q1
# source("../001Q/002TortasQ/script/002svMainPanel.R", local = T)$value
# 
# 
# 
# # Server elementos MyEEplot C1
# source("../002C/001HistC/script/002svMainPanel.R", local = T)$value
# 
# # Server elementos Boxplot C1
# source("../002C/002BoxplotC/script/002svMainPanel.R", local = T)$value
# 
# 
# 
# # Server elementos Displot C1
# source("../002C/003DispersionC/script/002svMainPanel.R", local = T)$value
# 
# 
# 
# # Server elementos Puntosplot C1
# source("../002C/004PuntosC/script/002svMainPanel.R", local = T)$value
# 
# 
# 
# 
# # Server elementos MyDplot C1
# source("../002C/005MyDC/script/002svMainPanel.R", local = T)$value
# 
# 
# 
# # Server elementos MyEEplot C1
# source("../002C/006MyEEC/script/002svMainPanel.R", local = T)$value
# 
# 
# 
# # Server elementos Barras2 QQ
# 
#  source("../004QQ/001Barras2QQ/script/002svMainPanel.R", local = T)$value
# #                       004Q/001Barras2QQ/script/002svBarras2Plot
# 
# 
# 
# 
# # Server elementos XY C2
# source("../005CC/001xyCC/script/002svMainPanel.R", local = T)$value
# 
# 
# 
# # Server elementos XY3d C2
# source("../005CC/002xy3dCC/script/002svMainPanel.R", local = T)$value
# 
# 
# 
# # script/contents/003QC/001Boxplot2QC/script/
# 
# # Server elementos Boxplot2 QC
# source("../003QC/001Boxplot2QC/script/002svMainPanel.R", local = T)$value
# 
# 
# 
# # Server elementos Media y Desvio2 QC
# source("../003QC/002MyD2QC/script/002svMainPanel.R", local = T)$value
# 
# 
# # Server elementos Media y Desvio2 QC
# source("../003QC/003MyEE2QC/script/002svMainPanel.R", local = T)$value
# 
# 
# 
# # Server del MainPanel Local
 source("script/007svMainPanel.R", local = T)$value


# Objeto Salida ui para el MainPanel
output$uiMainPanel <- renderUI({
  
  fluidPage(
  uiOutput("SALIDAmenuGRAFICOS"),
  #tableOutput("SAYA")
  uiOutput("planeta_goku")
 ## uiOutput("planeta_graf")
  )
  
})