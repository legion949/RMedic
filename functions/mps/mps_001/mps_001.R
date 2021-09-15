


mps <- function(input_base = NULL, input_decimales = NULL, input_cadena = NULL) {
 
  # mps: Medidas de Posicion Simultaneas
   
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
  
  
  # # # Modificaciones, Controles 3, y base "NO DATA"
  {
    ###
    
    # Si todo va OK...
    if (output_cadena) {
      
      # Creamos "mini"  
      mini <- na.omit(input_base)
      
      # Vemos que mini tenga filas
      if (nrow(mini) == 0) {
        cat("Error mps: 'mini' sin filas", "\n")
        output_cadena <- FALSE
      }
      
    } # Fin if Si todo va OK...
    ####################################################
    
    # Si algo no esta OK...
    if (output_cadena == FALSE){
      estas_columnas <- rep(1, ncol(input_base))
      mini <- as.data.frame(mtcars[estas_columnas])
      colnames(mini) <- "No Data"
      cat("Error mps: 'mini' externo agregado (NO DATA)", "\n")
    }
    ### 
    

  } # Fin Modificaciones y Controles 2
  ############################################################################
  
  
  # # # Medidas de Posicion
  {
    ###
    
    # Todas las Medidas de Posicion separadas
    {
    ###
   
      tablas_mps <- list()
      
      for (n in 1:ncol(mini)){
        recorte <- mini[n]
        tablas_mps[[n]] <- mp(input_base = recorte, input_decimales = input_decimales, input_cadena = output_cadena)
      }  
      names(tablas_mps) <- paste0("var",c(1:ncol(mini)))
      
    ###    
    } # Fin Todas
    ###########################################
    
    
    
    
    # Tabla 1 
    {
      ###
      
      for (n in 1:length(tablas_mps)) {
        
        if (n == 1) {
          
          tabla1_mps <- tablas_mps[[n]]$mp$tabla1_mp
          
        } else {
        
          tabla1_mps <- rbind(tabla1_mps, tablas_mps[[n]]$mp$tabla1_mp)
        }
      } # Fin for n
       
   
      
      ###
    } # Fin Tabla 1
    ############################################################
    
    
    # Tabla 2
    {
      ###
      
      for (n in 1:length(tablas_mps)) {
        
        if (n == 1) {
          
          tabla2_mps <- tablas_mps[[n]]$mp$tabla2_mp
          
        } else {
          
          tabla2_mps <- rbind(tabla2_mps, tablas_mps[[n]]$mp$tabla2_mp)
        }
      } # Fin for n
      
      ###  
    } # Fin Tabla 2
    ############################################################
    
    
    # Tabla 3
    {
      ###
      
      tabla3_mps <- list()
      
      
      for (n in 1:nrow(tablas_mps[[1]]$mp$tabla3_mp)) {
        for (k in 1:length(tablas_mps)) {  
          
        if (k == 1) {
          
          
          tabla_armada <- tablas_mps[[k]]$mp$tabla3_mp[n,]
       
          
          tabla3_mps[[n]] <- tabla_armada
        } else {
        
          tabla_armada <- tablas_mps[[k]]$mp$tabla3_mp[n,]
        
          
            
          tabla3_mps[[n]] <- rbind(tabla3_mps[[n]], tabla_armada)
        }
      } # Fin for n
        rownames(tabla3_mps[[n]]) <- c(1:nrow(tabla3_mps[[n]]))
      } # Fin for k
      
      ###  
    } # Fin Tabla 3
    ############################################################
    
    
    
    
    ###
  } # Fin Medidas de Posicion
  ############################################################################
  
  
  
  # # #Mis Tablas
  {
    ###
    
    mis_tablas <- list(tabla1_mps, tabla2_mps, tabla3_mps)
    names(mis_tablas) <- c("tabla1_mps", "tabla2_mps", "tabla3_mps")
    
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
      
    } # Fin si no es valido trabajar...
    ########################################
    
    ###   
  } # Fin Cambios "NO DATA" o "Errores"
  ##################################################
  
  
  # # # Salida
  {
    ###
    
    salida <- list(mis_tablas, output_cadena, input_originales)
    names(salida) <- c("mps", "output_cadena", "input_originales")
    
    return(salida)
    
    ###  
  } # Salida
  ############################################################################
  
  
  
  
  
} # Fin function mp()
########################################################################################################





if (1 == 2) {
  
  
  input_base <- as.data.frame(mtcars[c(1,2)]) 
  input_decimales = 2
  input_cadena = TRUE
  # Ejemplo...
  
  SALE <-     mps(input_base = input_base, input_decimales = input_decimales, input_cadena = input_cadena)
  
  SALE$mps$tabla1_mps
  
  SALE$mps$tabla2_mps
  
  SALE$mps$tabla3_mps
}


