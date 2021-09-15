

control_1c <- function(input_base = NULL, input_cadena = NULL){
  
  # # # Funcionamiento por defecto
  {
    ###
    
    if (is.null(input_cadena)) input_cadena <- TRUE
    
    
    
    
    # Armamos el siguiente eslabon
    output_cadena <- input_cadena
    
    ###
  } # Fin Funcionamiento
  ################################################
  
  
  # # # Controles 1
  {
    ###
    
    # Verificar que input_base no sea nulo
    if(output_cadena && is.null(input_base)) {
      cat("\n", "Error control_1c: input_base no debe ser nulo", "\n")
      output_cadena <- FALSE
    }  
    
    # Verificar si es un data frame
    if(output_cadena && !is.data.frame(input_base)) {
      cat("\n", "Error control_1c: input_base debe ser un data.frame", "\n")
      output_cadena <- FALSE
    }
    
    # Verificar si tiene al menos una columna
    if(output_cadena && ncol(input_base) == 0) {
      cat("\n", "Error control_1c: input_base no tiene columnas")
      output_cadena <- FALSE
    }
    
    # Verificar si tiene mas de 1 columna
    if(output_cadena && ncol(input_base) > 1) {
      cat("\n", "Error control_1c: input_base debe ser solo una columna")
      output_cadena <- FALSE
    }
    
    # Verificar si es no tiene datos
    if(output_cadena && nrow(input_base) == 0) {
      cat("\n", "Error control_1c: input_base no presenta filas")
      output_cadena <- FALSE
    }
    
    # Verificar que sea numerica
    if(output_cadena && !is.numeric(input_base[,1])) {
      cat("\n", "Error control_1c: input_base debe ser numérica.", "\n", "Utilice la solapa 'Control' sobre esta variable.", "\n")
      output_cadena <- FALSE
    }
    
    
    ###      
  } # Fin Controles 1
  ###################################################################
  
  # # # Salida
  {
    ###
    
    salida <- output_cadena
    
    names(salida) <- c("output_cadena")
    
    return(salida)
    
    
    ###  
  } # Fin Salida
  ##################
  
  
}



