
list2df <- function(input_lista = NULL) {  
  ###

  # Convierte una lista en un data frame
  # Cada item de la lista debe ser un data.frame o una matrix con el mismo nombre
  # en las columnas
  
  # Las junta todas en un solo data.frame
  
  
  for (n in 1:length(input_lista)) {
    
    if (n == 1) armado <- input_lista[[n]]
    
    if (n > 1) armado <- rbind(armado, input_lista[[n]])
    
    
  }
  
  
  return(armado)

} # Fin list2df





if (1 == 2) {

  
  
  
} 


