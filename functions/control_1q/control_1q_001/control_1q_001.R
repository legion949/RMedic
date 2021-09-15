

control_1q <- function(input_base = NULL, input_cadena = NULL){
  
  # # # Explicacion
  {
  ###
      # La funcion "control_1q()" es una funcion de control sobre input_base.
      # Verifica que input_base sea un dataframe de solo una columna, con al menos una fila.
      # Otorgara avisos si el objeto input_base:
      # 1- Es nulo
      # 2- No es un data.frame
      # 3- Tiene cero columnas
      # 4- Tienes más de 1 columna
      # 5- No tiene filas (nrow(input_base))
    
    # Aclaramos que el punto 5 es ver las filas que tiene input_base, y no 
    # ver si las filas ingresadas son vacias o no.

    
      # Si se cumple todo, la funcion devuelve un "TRUE". 
      # Si al menos un detalle falta, devuelve un "FALSE".
      # El control se realiza si el objeto input_cadena es TRUE.
      # Si input_cadena es FALSE quiere decir que ya hay un paso previo de una funcion
      # externa que no se cumple, y por lo tanto carece de sentido realizar las tareas, devolviendo
      # directamente un FALSE control_1q() como resultado.
        
  ###    
  } # Fin Explicacion
  ############################################################
  
  
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
      cat("Error control_1q: input_base no debe ser nulo", "\n")
      output_cadena <- FALSE
    }  
    
    # Verificar si es un data frame
    if(output_cadena && !is.data.frame(input_base)) {
      cat("Error control_1q: input_base debe ser un data.frame", "\n")
      output_cadena <- FALSE
    }
    
    # Verificar si tiene al menos una columna
    if(output_cadena && ncol(input_base) == 0) {
      cat("Error control_1q: input_base no tiene columnas.")
      output_cadena <- FALSE
    }
    
    # Verificar si tiene mas de 1 columna
    if(output_cadena && ncol(input_base) > 1) {
      cat("Error control_1q: input_base debe ser solo una columna")
      output_cadena <- FALSE
    }
    
    # Verificar si es no tiene datos
    if(output_cadena && nrow(input_base) == 0) {
      stop("Error control_1q: input_base no presenta filas")
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



