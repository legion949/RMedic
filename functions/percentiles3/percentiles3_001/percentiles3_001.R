


percentiles3 <- function(input_base = NULL, input_busqueda = NULL, input_decimales = NULL, input_cadena = NULL) {
  
  # percentiles3: Percentiles particionados
  
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
    veredicto1 <- control_cq(input_base = input_base, input_cadena = output_cadena)
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
        cat("\n", "Error percentiles3: input_busqueda no debe ser nulo", "\n")
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
  
  
  # # # Modificaciones, Controles 3, y base "NO DATA"
  {
    ###
    
    # Si todo va OK...
    if (output_cadena) {
      
      # Si cumple todo... Falta la columna 2 sea de tipo factor.
      if (is.factor(input_base[,2]) == FALSE) input_base[,2] <- as.factor(as.character(input_base[,2]))
      
      # Creamos "mini"  
      mini <- na.omit(input_base)
      
      # Vemos que mini tenga filas
      if (nrow(mini) == 0) {
        cat("Error percentiles3: 'mini' sin filas", "\n")
        output_cadena <- FALSE
      }
      
      
      
    } # Fin if Si todo va OK...
    ####################################################
    
    # Si algo no esta OK...
    if (output_cadena == FALSE){
      mini <- as.data.frame(mtcars[c(1,2)])
      colnames(mini) <- c("No Data", "No Data")
      mini[,2] <- as.factor(as.character(mini[,2]))
      cat("Error percentiles3: 'mini' externo agregado (NO DATA)", "\n")
    } # Fin si algo no esta OK
    #########################################################
    
    VR <- mini[1]
    FACTOR <- mini[2]
    
    ###  
  } # Fin Modificaciones y Controles 2
  ############################################################################
  
  
  # # # VR y FACTOR
  {
    ###
    
    VR <- mini[1]
    FACTOR <- mini[2]
    
    vector_VR <- mini[,1]
    vector_FACTOR <- mini[,2]
    
    
    ###  
  } # Fin VR y FACTOR
  #############################################
  
  
  # # # Percentiles Particionadas
  {
    ###
    
    # Objetos de utilidad general
    {
      ###
      
      niveles_factor <- levels(vector_FACTOR)
      cantidad_niveles <- length(niveles_factor)
      
      ###  
    } # Fin Objetos
    ####################################
    
    
    # Tabla 1 
    {
      ###
      
      # Medidas Generales
      tabla_percentiles_general <- percentiles(VR, input_decimales = input_decimales, input_busqueda = input_busqueda)$percentiles$tabla_percentiles
      
      # # Medidas de Posicion Particionadas
       tabla1_percp <- as.data.frame(matrix(NA, cantidad_niveles , ncol(tabla_percentiles_general)))
       colnames(tabla1_percp) <- colnames(tabla_percentiles_general)
       colnames(tabla1_percp)[1] <- colnames(FACTOR)
       
       

         for(j in 1:cantidad_niveles) {
           
           dt <- NULL
           recorte <- NULL
           
           dt <- FACTOR == levels(FACTOR[,1])[j]
           recorte <- VR[dt, 1]
           dim(recorte) <- c(length(recorte), 1)
           recorte <- as.data.frame(recorte)
           colnames(recorte) <- levels(FACTOR[,1])[j]

           tabla1_percp[j,] <- percentiles(recorte, input_decimales, input_busqueda = input_busqueda)$percentiles$tabla_percentiles
           
         } # FIn for j
         

      # 
      # Agregamos las medidas generales
      tabla_percentiles_general[1,1] <- " --Medidas Generales--"
      colnames(tabla_percentiles_general)[1] <- colnames(FACTOR)

      tabla1_percp <- rbind(tabla1_percp, tabla_percentiles_general)
      colnames(tabla1_percp)[1] <- paste0("Variable: ", colnames(FACTOR))

   
      ###
    } # Fin Tabla 1
    ############################################################
    
    
    # Tabla 2
    {
      ###
      
      # Le sacamos las medidas del general
      tabla2_percp <- tabla1_percp[-nrow(tabla1_percp), ]
      ###  
    } # Fin Tabla 2
    ############################################################
    
    
    ###
  } # Fin Medidas de Posicion Particionadas
  ############################################################################
  
  
  # # #Mis Tablas
  {
    ###
    
    mis_tablas <- list(tabla1_percp, tabla2_percp)
    names(mis_tablas) <- c("tabla1_percp", "tabla2_percp")
    
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
    names(salida) <- c("perc3", "output_cadena", "input_originales")
    
    return(salida)
    
    ###  
  } # Salida
  ############################################################################
  
  
  
  
  
} # Fin function percentiles3()
########################################################################################################





if (1 == 2) {
  
  
  input_base <- as.data.frame(mtcars[c(1,2)]) 
  input_base[,2] <- as.factor(as.character(input_base[,2]))
  input_decimales = 2
  input_cadena = TRUE
  input_busqueda <- c(5, 10, 90, 95)
  # Ejemplo...
  
  SALE <-     percentiles3(input_base = input_base, input_decimales = input_decimales, input_busqueda = input_busqueda, input_cadena = input_cadena)
  
  SALE$perc3$tabla1_percp
}


