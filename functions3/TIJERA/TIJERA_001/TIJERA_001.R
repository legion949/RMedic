TIJERA <- function(input_base, input_columna){
  
  # Si el input_columna es un numero...
  if (is.numeric(input_columna)){
    
    MINI <- input_base[,input_columna]
    MINI <- na.omit(MINI)
    MINI <- data.frame(MINI)  
    colnames(MINI) <- colnames(input_base)[input_columna]
    
  } # Fin is.numeric()
  
  
  # Si el input_columna es un numero...
  if (is.character(input_columna)){
    
    orden <- c(1:ncol(input_base))
    nombres <- colnames(input_base)
    detec <- nombres == input_columna
    numero_columna <- orden[detec]
    
    
    MINI <- input_base[,numero_columna]
    MINI <- na.omit(MINI)
    MINI <- data.frame(MINI)  
    colnames(MINI) <- colnames(input_base)[numero_columna]
    
    
  } # Fin is.numeric()
  
  return(MINI)
  
  
  
}
##################################################################################################



