
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

source("uiCode.R")

source("lib.R")

source("modules/SideBarBase.R")
source("modules/BatallaNaval.R")
source("modules/MiniBase.R")
source("modules/Tablas1Q.R")
source("modules/Tablas1C.R")

source("modules2/ModuleBase.R")
source("modules2/ModuleControl.R")
source("modules2/ModuleTablas.R")
source("modules2/ModuleGraficos.R")
source("modules2/ModuleHo.R")


