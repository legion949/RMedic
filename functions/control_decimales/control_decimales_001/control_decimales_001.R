

control_decimales <- function(input_decimales = NULL, input_cadena = NULL){
  
  # # # Funcionamiento por defecto
  {
    ###
    
    if (is.null(input_cadena)) input_cadena <- TRUE
    
    
    
    
    # Armamos el siguiente eslabon
    output_cadena <- input_cadena
    
    ###
  } # Fin Funcionamiento
  ################################################
  
  
  # # # Controles  - input_decimales
  {
    ###
    
    # Verificar que input_decimales es un vector
    if(output_cadena && !is.vector(input_decimales)) {
      cat("Error input_decimales: input_decimales debe ser un vector", "\n")
      output_cadena <- FALSE
    }
    
    # Verificar que input_decimales es un solo elemento
    if(output_cadena && length(input_decimales) > 1) {
      cat("Error input_decimales: input_decimales debe ser un solo número", "\n")
      output_cadena <- FALSE
    }
    
    # Verificar que input_decimales es un numero
    if(output_cadena && is.numeric(input_decimales) == FALSE) {
      cat("Error input_decimales: input_decimales debe ser un número", "\n")
      output_cadena <- FALSE
    }
    
    # Verificar que input_decimales no es "NA"
    if(output_cadena && is.na(input_decimales)) {
      cat("Error input_decimales: input_decimales no debe ser 'NA'", "\n")
      output_cadena <- FALSE
    }
    
    
    # Verificar que input_decimales no es "NaN"
    if(output_cadena && is.nan(input_decimales)) {
      cat("Error input_decimales: input_decimales no debe ser 'NaN'", "\n")
      output_cadena <- FALSE
    }
    
    ###      
  } # Fin Controles 
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



