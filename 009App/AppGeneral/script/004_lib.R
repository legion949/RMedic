
# Librerias
{
  library(shiny)
} # Fin Librerias
#######################################################


# setwd("/home/david/ARN/PROYECTOS/AULA VIRTUAL/ESTADISTICA/pagmod/mod2")
library(shinyjs)
# List Files

detalle1 <- "AppGeneral"
detalle2 <- "001RmedicPage"

my_dir <- getwd()
metralla <- strsplit(my_dir, "/")[[1]]
metralla <- metralla[length(metralla)]

if (metralla == detalle1) ubi <- "../002_Distribucion_de_Probabilidades"
if (metralla == detalle2) ubi <- "../009App/002_Distribucion_de_Probabilidades"

  
# ubi <- "../002_Distribucion_de_Probabilidades"
files <- list.files(ubi)

saf <- c("001_server.R", "002_sidebarPanel.R", "003_mainPanel.R", "004_lib.R")

for (n in 1:length(files)) {
  
  new_dir <- paste0(ubi, "/", files[n])
  files2 <- list.files(new_dir)
  dt <- files2 == "mini"
  
  contador <- 0
  if (sum(dt) == 1) {
    
    new_dir2 <- paste0(new_dir, "/mini")
    script_files <- list.files(new_dir2)
    for (n in 1:length(saf)) {
      
      dt <- script_files == saf[n]
      if (sum(dt) == 1) contador <- contador + 1
      
    }
    
  }
  
  
  if (contador != length(saf)) files[n] <- NA
}

files <- na.omit(files)

origen <- getwd()
dir_origen <- getwd()