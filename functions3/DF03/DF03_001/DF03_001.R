DF03 <- function(input_dataset=NULL, input_decimales=2, input_breaks="Sturges", input_right=TRUE){
  
  MINI <- input_dataset
  MINI <- MINI[,1]
  
  GRAFICO_INFO <-   hist(MINI, 
                         breaks=input_breaks,
                         plot=FALSE,
                         right = input_right)

  
  GRAFICO_INFO$xname <- colnames(input_dataset)  
  
  GRAFICO_INFO
  
} # Fin Function
############################################################################


if (1 == 2) {
MINI <- TIJERA(mtcars, 1)
input_breaks <- "Sturges"
input_right <- TRUE

DF03(ESTA, 2, "Sturges", TRUE)

}