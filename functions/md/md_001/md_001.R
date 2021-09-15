


md <- function(input_base = NULL, input_decimales = NULL, input_cadena = NULL) {
  
  # Input originales 
  {
  ###
    
    input_originales <- list(input_decimales, input_cadena)
    names(input_originales) <- c("input_decimales", "input_cadena")
  
  ###    
  } # Fin Argumentos originales
  ##############################################################################
  
  
  # # # Funcionamiento por defecto
  {
  ###
    
    if (is.null(input_cadena)) input_cadena <- TRUE
    
    
    # Decimales por defecto
    if (is.null(input_decimales)) input_decimales <- 2
    
    # Armamos el siguiente eslabon
    output_cadena <- input_cadena
    
  ###
  } # Fin Funcionamiento
  ################################################
  
  
  # # # Control 1 - input_base
  {
  ###
    veredicto1 <- control_1c(input_base = input_base, input_cadena = output_cadena)
    output_cadena <- veredicto1

  ###  
  } # Fin Control 1 - input_base
  #################################################
  
  
  # # # Control 2 - input_decimales
  {
  ###
    
    # Hacemos el control de decimales
    veredicto2 <- control_decimales(input_decimales = input_decimales, input_cadena = output_cadena)
    
    # Si no pasa el control numerico, asignamos un nuevo valor para input_decimales
    # pero guardamos el original por si hace falta
    if (veredicto2 == FALSE){
      input_decimales_original <- input_decimales
      input_decimales <- 2
    }
    
    # Si paso el control anterior... guardamos esta nueva opinion
    if (veredicto1) output_cadena <- veredicto2

  ###  
  } # Fin Control 2 - input_decimales
  ############################################################################
  
  
  # # # Modificaciones, Controles 3, y base "NO DATA"
  {
  ###
    
    # Si todo va OK...
    if (output_cadena) {
      
      # Creamos "mini"  
      mini <- na.omit(input_base)
      
      # Vemos que mini tenga filas
      if (nrow(mini) == 0) {
        cat("Error mp: 'mini' sin filas", "\n")
        output_cadena <- FALSE
      }
      
    } # Fin if Si todo va OK...
    ####################################################
    
    # Si algo no esta OK...
    if (output_cadena == FALSE){
      mini <- as.data.frame(mtcars[1])
      colnames(mini) <- "No Data"
      cat("Error mp: 'mini' externo agregado (NO DATA)", "\n")
    } # Fin si algo no esta OK
    ### #####################################################
    
    mini_vector <- mini[,1]
    
  ###  
  } # Fin Modificaciones y Controles 2
  ############################################################################
  
  
  # # # Medidas de Dispersion
  {
  ###
    
    # Tabla 1 
    {
    ###
      
      
      nombres_elementos <- c("Variable", "Varianza", "Desvío Estándard", "Error Estándard", "Coeficiente de Variación", "n")
      
      tabla1_md <- as.data.frame(matrix(NA,1, length(nombres_elementos)))
      colnames(tabla1_md) <- nombres_elementos
      
      
      # Varianza
      varianza <- var(mini_vector)
      varianza <- round2(varianza, input_decimales)
      
      # Desvio
      desvio <- sd(mini_vector)

      # Tamanio muestral
      n_muestra <- length(mini_vector)
      
      # Error estandard
      ee <- desvio/sqrt(n_muestra)

      # Media
      media <- mean(mini_vector)
      
      # Coeficiente de Variacion
      cv <- desvio/media
      
      
      # Primero sacamos el desvio...
      # Sin redondear el desvio, lo usamos para sacar el error estandard...
      # Y luego redondeamos los dos.
      # Esto es para sacar mejor al EE... por que sino sacas el DE... lo redondeas...
      # lo usas para sacar el EE y lo volves a redondear.
      # Lo mismo con el CV.
      
      desvio <- round2(desvio, input_decimales)
      ee <- round2(ee, input_decimales)
      cv <- round2(cv, input_decimales)

      
      
      tabla1_md[,1] <- colnames(mini)
      tabla1_md[,2] <- varianza
      tabla1_md[,3] <- desvio
      tabla1_md[,4] <- ee
      tabla1_md[,5] <- cv
      tabla1_md[,6] <- n_muestra
      
    ###
    } # Fin Tabla 1
    ############################################################
    
    
    # Tabla 2
    {
    ###
      
      celdas_vacias <- nrow(input_base) - nrow(mini)
      
      tabla2_md <- as.data.frame(matrix(NA, 1, (ncol(tabla1_md) + 1)))
      
      for (n in 1:ncol(tabla1_md)) tabla2_md[1,n] <- tabla1_md[1,n]
      
      colnames(tabla2_md) <- c(colnames(tabla1_md), "Celdas Vacías")
      tabla2_md[ncol(tabla2_md)] <- celdas_vacias
      
    ###  
    } # Fin Tabla 2
    ############################################################
    
  
  ###
  } # Fin Medidas de Posicion
  ############################################################################
  
  
  # # # Mis Tablas
  {
  ###
    
    mis_tablas <- list(tabla1_md, tabla2_md)
    names(mis_tablas) <- c("tabla1_md", "tabla2_md")
    
  ###  
  } # Fin Mis Tablas
  ###########################################################################
  
  
  
  # Cambios "NO DATA" o "Errores"
  {
  ###
    
    # Si no es valido trabajar... Y estamos en "NO DATA"
    if (output_cadena == FALSE) {
      
      cambio1 <- "Sin datos en la Columna"
      cambio2 <- "Modificar input_decimales"
      
      # Damos aviso si es algo de los datos o los decimales
      cambio_aplicado <- cambio1
      if (veredicto1 == TRUE && veredicto2 == FALSE) cambio_aplicado <- cambio2
      
      
      
      # Cambiamos los valores por avisos
      for (n in 1:length(mis_tablas)) {
        
        esta_tabla <- mis_tablas[[n]]
        
        estas_dimensiones <- dim(esta_tabla)
        tabla_no_data <- as.data.frame(matrix(cambio_aplicado, estas_dimensiones[1], estas_dimensiones[2]))
        colnames(tabla_no_data) <- colnames(esta_tabla)
        esta_tabla <- tabla_no_data
        
        
        mis_tablas[[n]]  <- esta_tabla
        
      } # Fin for n
      
    }
    
  ###   
  } # Fin Cambios "NO DATA" o "Errores"
  ##################################################
  
  
  
  # # # Salida
  {
  ###
    
    salida <- list(mis_tablas, output_cadena, input_originales)
    names(salida) <- c("md", "output_cadena", "input_originales")
    
    return(salida)
    
  ###  
  } # Salida
  ############################################################################
  
  
  
  
  
} # Fin function md()
########################################################################################################





if (1 == 2) {
  
  
  input_base <- as.data.frame(mtcars[1]) 
  # Ejemplo...
  SALE <-     md(input_base, 3)
  
  SALE$md$tabla1_md
  
}


