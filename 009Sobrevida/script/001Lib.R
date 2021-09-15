# libraries.R


require(shiny)
library(shinyjs)
# library(RCurl)
# library(XML)
# library(gdata)
# library(XLConnect)
# library(readxl)
library(DT)
# library(datasets)
library(ggplot2)
library(gplots)
library(Hmisc)
library(plotly)

AVER <- data.frame(matrix(NA, 4, 3))

AVER[,1] <- c(1:nrow(AVER))
AVER[,2] <- c(1.2, "A", "1..4", "1,3")
AVER[,3] <- c("A b", "Z Z", " ", "  ")




namel<-function (vec){
  tmp<-as.list(vec)
  names(tmp)<-as.character(unlist(vec))
  tmp
}




