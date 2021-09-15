

percentiles <- function(input_base=NULL, input_busqueda = NULL, input_decimales = NULL, input_cadena = NULL) {
  
  
 
  
  # Input originales 
  {
    ###
    
    input_originales <- list(input_busqueda, input_decimales, input_cadena)
    names(input_originales) <- c("input_busqueda", "input_decimales", "input_cadena")
    
    ###    
  } # Fin Argumentos originales
  ##############################################################################
  
  
  # # # Funcionamiento por defecto
  {
    ###
    
    # Busqueda por defecto
    if (is.null(input_cadena)) input_busqueda = c(5, 10, 90, 95)
    
    # Cadena por defecto
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
  
  
  # # # Control 3 - input_busqueda
  {
    ###
    
    # Hacemos el control de input_busqueda
    veredicto3 <- TRUE

    # Si estuvo bien el control anterior... Seguimos...    
    if (veredicto2 == TRUE){
      
      # Verificar que input_busqueda no sea nulo
      if(veredicto3 && is.null(input_busqueda)) {
        cat("\n", "Error percentiles: input_busqueda no debe ser nulo", "\n")
        veredicto3 <- FALSE
      }  
      
      # Verificar que input_busqueda sea vector
      if(output_cadena && !is.vector(input_busqueda)) {
        cat("\n", "Error percentiles: input_busqueda debe ser un vector'", "\n")
        veredicto3 <- FALSE
      }
      
      
      # Verificar que input_busqueda debe ser numerico
      if(output_cadena && !is.numeric(input_busqueda)) {
        cat("\n", "Error percentiles: input_busqueda debe ser numérico'", "\n")
        veredicto3 <- FALSE
      }
      
      
      # Verificar que input_busqueda tenga elementos
      if(output_cadena && length(input_busqueda) == 0) {
        cat("\n", "Error percentiles: input_busqueda debe tener al menos un elemento'", "\n")
        veredicto3 <- FALSE
      }
      
    }
    
    # Si paso el control anterior... guardamos esta nueva opinion
    if (veredicto2) output_cadena <- veredicto3
    
    
    # Si paso el control anterior... guardamos esta nueva opinion
    if (veredicto2) output_cadena <- veredicto3
    
    ###  
  } # Fin Control 3 - input_busqueda
  ############################################################################
  
  
  # # # Modificaciones, Controles 4, y base "NO DATA"
  {
    ###
    
    # Si todo va OK...
    if (output_cadena) {
      
      # Creamos "mini"  
      mini <- na.omit(input_base)
      
      # Vemos que mini tenga filas
      if (nrow(mini) == 0) {
        cat("Error percentiles: 'mini' sin filas", "\n")
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
  
  
  
  # # # Percentiles
  {
  ###
  
  # Si tiene al menos un dato... y es numerica
  nombres_columnas <- c("Variable", paste0(input_busqueda, "%"), "n")
  tabla_percentiles <- as.data.frame(matrix(NA, 1, length(nombres_columnas)))
  colnames(tabla_percentiles) <- nombres_columnas
  
  percentiles <- quantile(mini_vector,  probs = input_busqueda/100)
  
  tabla_percentiles[1,1] <- colnames(input_base)
  tabla_percentiles[1, c(2:(ncol(tabla_percentiles)-1))] <- percentiles
  tabla_percentiles[1,ncol(tabla_percentiles)] <- length(mini_vector)
  
  
  ###
  } # Fin Percentiles
  ##############################################################
  
  
  
  # # # Mis Tablas
  {
    ###
    
    mis_tablas <- list(tabla_percentiles)
    names(mis_tablas) <- c("tabla_percentiles")
    
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
      cambio3 <- "Modificar input_busqueda"
      
      # Damos aviso si es algo de los datos o los decimales
      
      # Por defecto, el cambio indicado es el cambio1
      cambio_aplicado <- cambio1
      
      # Si esta bien el 1, y esta mal el dos...
      if (veredicto1 == TRUE && veredicto2 == FALSE) cambio_aplicado <- cambio2
      
      # Si esta bien el 2, y esta mal el 3...
      if (veredicto2 == TRUE && veredicto3 == FALSE) cambio_aplicado <- cambio3
      
      
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
    names(salida) <- c("percentiles", "output_cadena", "input_originales")
    
    return(salida)
    
    ###  
  } # Salida
  ############################################################################   
  
  
  
} # Fin CUANTILES
