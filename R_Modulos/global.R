
options(encoding = "UTF-8")
options(shiny.maxRequestSize = 500*1024^2)


library(shiny)
library(shinyBS)
library(shinyjs)
library(bslib)

source("uiCode.R")

source("lib.R")

source("modules/SideBarBase.R")
source("modules/BatallaNaval.R")
source("modules/MiniBase.R")
source("modules/Tablas1Q.R")
source("modules/Tablas1C.R")

