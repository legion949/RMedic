
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
library(coin)
library(car)
library(agricolae)
library(gplots)

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
source("modules/Ho1Q_02_TestDeUnaProporcion.R")
source("modules/Ho1Q_03_TestDeUniformidad.R")


source("modules/Ho1C_ALL.R")
source("modules/Ho1C_01_RMedicHelp.R")
source("modules/Ho1C_02_TestTUnaMuestra.R")
source("modules/Ho1C_03_TestWilcoxonUnaMuestra.R")
source("modules/Ho1C_04_TestNormalidadShapiroWilk.R")
source("modules/Ho1C_05_TestChiCuadradoUnaMuestra.R")
# source("modules/Ho1Q_03_TestDeUniformidad.R")


source("modules/Ho2Q_ALL.R")
source("modules/Ho2Q_01_RMedicHelp.R")
source("modules/Ho2Q_02_TestDeDosProporciones.R")

# Ho 2C
source("modules/Ho2C_ALL.R")
source("modules/Ho2C_01_RMedicHelp.R")
source("modules/Ho2C_05_TestTApareado.R")
source("modules/Ho2C_06_TestWilcoxonApareado.R")
source("modules/Ho2C_07_TestHomogenedadDeVarianzasFisher.R")
source("modules/Ho2C_08_TestHomogenedadDeVarianzasBartlett.R")
source("modules/Ho2C_09_TestHomogenedadDeVarianzasLevene.R")
#####################################################

# Ho QC
source("modules/HoQC_ALL.R")
source("modules/HoQC_01_RMedicHelp.R")
source("modules/HoQC_02_TestTDosMuestrasIndependientes.R")
source("modules/HoQC_03_TestWilcoxonDosMuestrasIndependientes.R")
source("modules/HoQC_04_TestAnova1Factor.R")
source("modules/HoQC_07_TestHomogeneidadDeVarianzasFisher.R")
source("modules/HoQC_08_TestHomogeneidadDeVarianzasBartlett.R")
source("modules/HoQC_09_TestHomogeneidadDeVarianzasLevene.R")
source("modules/HoQC_10_TestNormalidadShapiroWilkParticionado.R")




source("modules2/ModuleBase.R")
source("modules2/ModuleControl.R")
source("modules2/ModuleTablas.R")
source("modules2/ModuleGraficos.R")
source("modules2/ModuleHo.R")


