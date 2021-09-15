


############################################################################



# Server del SideBarPanel
source("script/002svSideBarPanel.R", local = T)$value


# Server de la Pestania "Base de Datos"
source("../004Base/script/002svMainPanel.R", local = T)$value


# 
# 
# # Server de la Pestania "Control"
# source("002Control/script/002svMainPanel.R", local = T)$value
# 
# 
# # Server elementos Graficos Generales
# source("003Graficos/script/002svGrafGeneral.R", local = T)$value
# 
# 
# # Server elementos Barras Q1
# source("003Graficos/script/contents/001Q/001BarrasQ/script/002svMainPanel.R", local = T)$value
# 
# 
# # Server elementos Tortas Q1
# source("003Graficos/script/contents/001Q/002TortasQ/script/002svMainPanel.R", local = T)$value
# 
# # Puesto es Exceso los siguientes 2...
# source("003Graficos/script/contents/001Q/001BarrasQ/script/002svBarrasPlot.R", local = T)$value
# source("003Graficos/script/contents/001Q/001BarrasQ/script/002svBarrasGGplot.R", local = T)$value
# 
# # Puesto es Exceso los siguientes 2...
# source("003Graficos/script/contents/001Q/002TortasQ/script/002svTortasPlot.R", local = T)$value
# source("003Graficos/script/contents/001Q/002TortasQ/script/002svTortasGGplot.R", local = T)$value
# 
# 
# # Grafico de Histograma
# source("003Graficos/script/contents/002C/001HistC/script/002svMainPanel.R", local = T)$value
# source("003Graficos/script/contents/002C/001HistC/script/002svHistPlot.R", local = T)$value
# 
# 
# # Grafico de Boxplot
# source("003Graficos/script/contents/002C/002BoxplotC/script/002svMainPanel.R", local = T)$value
# source("003Graficos/script/contents/002C/002BoxplotC/script/002svBoxPlot.R", local = T)$value
# 
# 
# # Grafico de DIspersion
# source("003Graficos/script/contents/002C/003DispersionC/script/002svMainPanel.R", local = T)$value
# source("003Graficos/script/contents/002C/003DispersionC/script/002svDisPlot.R", local = T)$value
# 
# # Grafico de Puntos
# source("003Graficos/script/contents/002C/004PuntosC/script/002svMainPanel.R", local = T)$value
# source("003Graficos/script/contents/002C/004PuntosC/script/002svPuntosPlot.R", local = T)$value
# 
# 
# # Grafico de MyD
# source("003Graficos/script/contents/002C/005MyDC/script/002svMainPanel.R", local = T)$value
# source("003Graficos/script/contents/002C/005MyDC/script/002svMyDPlot.R", local = T)$value
# 
# 
# # Grafico de MyEE
# source("003Graficos/script/contents/002C/006MyEEC/script/002svMainPanel.R", local = T)$value
# source("003Graficos/script/contents/002C/006MyEEC/script/002svMyEEPlot.R", local = T)$value
# 
# 
# # Grafico de Barras2QQ
# source("003Graficos/script/contents/004QQ/001Barras2QQ/script/002svMainPanel.R", local = T)$value
# source("003Graficos/script/contents/004QQ/001Barras2QQ/script/002svBarras2Plot.R", local = T)$value
# 
# 
# # Grafico de XY CC
# source("003Graficos/script/contents/005CC/001xyCC/script/002svMainPanel.R", local = T)$value
# source("003Graficos/script/contents/005CC/001xyCC/script/002svxyPlot.R", local = T)$value
# 
# 
# # Grafico de XY CC
# source("003Graficos/script/contents/005CC/002xy3dCC/script/002svMainPanel.R", local = T)$value
# source("003Graficos/script/contents/005CC/002xy3dCC/script/002svxy3dPlot.R", local = T)$value
# 
# 
# # Grafico Boxplot QC
# source("003Graficos/script/contents/003QC/001Boxplot2QC/script/002svMainPanel.R", local = T)$value
# source("003Graficos/script/contents/003QC/001Boxplot2QC/script/002svBox2Plot.R", local = T)$value
# 
# 
# # Grafico Media y Desvio QC
# source("003Graficos/script/contents/003QC/002MyD2QC/script/002svMainPanel.R", local = T)$value
# source("003Graficos/script/contents/003QC/002MyD2QC/script/002svMyD2Plot.R", local = T)$value
# 
# 
# # Grafico Media y EE QC
# source("003Graficos/script/contents/003QC/003MyEE2QC/script/002svMainPanel.R", local = T)$value
# source("003Graficos/script/contents/003QC/003MyEE2QC/script/002svMyEE2Plot.R", local = T)$value
# 
# 
# 
# # # Server de la Pestania "Graficos"
# source("003Graficos/script/002svMainPanel.R", local = T)$value
# 
# 
# Server del MainPanel Local
source("script/002svMainPanel.R", local = T)$value







# Server de Barras GGplot
# source("script/002svBarrasGGplot.R", local = T)$value


############################################################################






# Objeto Salida ui para el MainPanel
output$uiMainPanel <- renderUI({
  
  uiOutput("SALIDAmenuR")
  
})