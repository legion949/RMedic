
options(encoding = "UTF-8")
options(shiny.maxRequestSize = 500*1024^2)


library(shiny)
library(shinyjs)
library(shinyBS)
library(bslib)
library(datasets)
library(DT)
library(htmltools)
library(openxlsx)
library(stringr)
library(vioplot)

source("uiCode.R")

source("lib.R")
source("functionsHo.R")

source("modules/SideBarBase.R")
source("modules/BatallaNaval.R")
source("modules/BatallaNaval2.R")
source("modules/MiniBase.R")

source("modules/Tablas1Q.R")
source("modules/Tablas1C.R")
source("modules/Tablas2Q.R")
source("modules/Tablas2C.R")
source("modules/TablasQC.R")

source("modules/Graficos1Q_ALL.R")
source("modules/Graficos1Q_01_RMedicHelp.R")
source("modules/Graficos1Q_02_Barras.R")
source("modules/Graficos1Q_03_Tortas.R")

source("modules/Graficos1C_ALL.R")
source("modules/Graficos1C_01_RMedicHelp.R")
source("modules/Graficos1C_02_MediaDesvioEstandard.R")
source("modules/Graficos1C_03_MediaErrorEstandard.R")
source("modules/Graficos1C_04_Boxplot.R")
source("modules/Graficos1C_05_Violinplot.R")
source("modules/Graficos1C_06_Histograma.R")
source("modules/Graficos1C_07_Dispersion.R")
source("modules/Graficos1C_08_Puntos.R")

# Graficos 2Q
source("modules/Graficos2Q_ALL.R")
source("modules/Graficos2Q_01_RMedicHelp.R")
source("modules/Graficos2Q_02_Barras.R")


# Graficos 2C
source("modules/Graficos2C_ALL.R")
source("modules/Graficos2C_01_RMedicHelp.R")
source("modules/Graficos2C_02_XY.R")
source("modules/Graficos2C_03_MediaDesvioEstandard.R")
source("modules/Graficos2C_04_MediaErrorEstandard.R")
source("modules/Graficos2C_05_Boxplot.R")
source("modules/Graficos2C_06_Violinplot.R")
source("modules/Graficos2C_07_Dispersion.R")
source("modules/Graficos2C_08_Conectores.R")



# Graficos QC
source("modules/GraficosQC_ALL.R")
source("modules/GraficosQC_01_RMedicHelp.R")
source("modules/GraficosQC_02_MediaDesvioEstandard.R")
source("modules/GraficosQC_03_MediaErrorEstandard.R")
source("modules/GraficosQC_04_Boxplot.R")
source("modules/GraficosQC_05_Violinplot.R")
source("modules/GraficosQC_06_Dispersion.R")

#####################################################

source("modules/Ho1Q_ALL.R")
source("modules/Ho1Q_01_RMedicHelp.R")
source("modules/Ho1Q_02_TestDeProporciones.R")
source("modules/Ho1Q_03_Tortas.R")

#####################################################

source("modules2/ModuleBase.R")
source("modules2/ModuleControl.R")
source("modules2/ModuleTablas.R")
source("modules2/ModuleGraficos.R")
source("modules2/ModuleHo.R")


