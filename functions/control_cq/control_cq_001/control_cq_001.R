

control_cq <- function(input_base = NULL, input_cadena = NULL){
  
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
      cat("Error control_qc: input_base no debe ser nulo", "\n")
      output_cadena <- FALSE
    }  
    
    # Verificar si es un data frame
    if(output_cadena && !is.data.frame(input_base)) {
      cat("Error control_qc: input_base debe ser un data.frame", "\n")
      output_cadena <- FALSE
    }
    
    # Verificar si tiene al menos una columna
    if(output_cadena && ncol(input_base) == 0) {
      cat("Error control_qc: input_base no tiene columnas")
      output_cadena <- FALSE
    }
    
    # Verificar si tiene una cantidad diferente de 2 columnas
    if(output_cadena && ncol(input_base) != 2) {
      cat("Error control_cq: input_base debe tener dos columnas")
      output_cadena <- FALSE
    }
    
    # Verificar si es no tiene datos
    if(output_cadena && nrow(input_base) == 0) {
      stop("Error control_cq: input_base no presenta filas")
      output_cadena <- FALSE
    }
    
    # Verificar si la 1ra columna es numerica
    if(output_cadena && !is.numeric(input_base[,1])) {
      stop("Error control_cq: input_base columna 1 debe ser numérica")
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



