


percentiles2 <- function(input_base = NULL, input_busqueda = NULL, input_decimales = NULL, input_cadena = NULL) {
  
  # percentiles2: Percentiles simultaneos
  
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
    if (is.null(input_busqueda)) input_busqueda <- c(5, 10, 90, 95)
    
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
    # Vemos si todas las columnas de input_base son numericas
    
    vector1 <- rep(NA, ncol(input_base))
    
    for (n in 1:ncol(input_base)) {
      
      recorte <- input_base[n]
      vector1[n] <- control_1c(input_base = recorte, input_cadena = output_cadena)
      
    }
    veredicto1 <- sum(vector1) == length(vector1)
    output_cadena <- veredicto1
    
    ###  
  } # Fin Control 1 - input_base
  #################################################
  
  
  # # # Control 2 - input_decimales
  {
    ###
    
    # Hacemos el control de input_decimales
    veredicto2 <- control_decimales(input_decimales = input_decimales, input_cadena = output_cadena)
    
    
    # Si no pasa el control de input_decimales, asignamos un nuevo valor
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
        cat("Error percentiles2: 'mini' sin filas", "\n")
        output_cadena <- FALSE
      }
      
    } # Fin if Si todo va OK...
    ####################################################
    
    # Si algo no esta OK...
    if (output_cadena == FALSE){
      estas_columnas <- rep(1, ncol(input_base))
      mini <- as.data.frame(mtcars[estas_columnas])
      colnames(mini) <- "No Data"
      cat("Error percentiles2: 'mini' externo agregado (NO DATA)", "\n")
    }
    ### 
    
    
  } # Fin Modificaciones y Controles 2
  ############################################################################
  
  
  # # # Percentiles Simultaneos
  {
  ###
    
    # Todos los percentiles separadas
    {
      ###
      
      todas_per2 <- list()
      
      for (n in 1:ncol(mini)){
        recorte <- mini[n]
        todas_per2[[n]] <- percentiles(input_base = recorte, input_busqueda = input_busqueda, input_decimales = input_decimales, input_cadena = output_cadena)
      }  
      
      
      
      for (n in 1:length(todas_per2)) {
        
        if ( n == 1) tabla_per2 <- todas_per2[[n]]$percentiles$tabla_percentiles
        
        if (n > 1) tabla_per2 <- rbind(tabla_per2, todas_per2[[n]]$percentiles$tabla_percentiles)
      }
      
     
      
      ###    
    } # Fin Todas
    ###########################################
    
    
    
  ###
  } # Fin Percentiles Simultaneos
  ############################################################################
  
  
  # # #Mis Tablas
  {
    ###
    
    mis_tablas <- list(tabla_per2)
    names(mis_tablas) <- c("tabla_per2")
    
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
      
    } # Fin si no es valido trabajar...
    ########################################
    
    ###   
  } # Fin Cambios "NO DATA" o "Errores"
  ##################################################
  
  
  # # # Salida
  {
    ###
    
    salida <- list(mis_tablas, output_cadena, input_originales)
    names(salida) <- c("perc2", "output_cadena", "input_originales")
    
    return(salida)
    
    ###  
  } # Salida
  ############################################################################
  
  
  
  
  
} # Fin function percentiles2()
########################################################################################################





if (1 == 2) {
  
  
  input_base <- as.data.frame(mtcars[c(1,2)]) 
  input_decimales = 2
  input_cadena = TRUE
  input_busqueda <- c(5, 10, 90, 95)
  # Ejemplo...
  
  SALE <-     percentiles2(input_base = input_base, input_decimales = input_decimales, input_cadena = input_cadena)
  
  SALE$mps$tabla1_mps
  
  SALE$mps$tabla2_mps
  
  SALE$mps$tabla3_mps
}


