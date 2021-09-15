



mpp <- function(input_base = NULL, input_decimales = 2, input_cadena = NULL) {
  
  
  
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
        cat("Error mpp: 'mini' sin filas", "\n")
        output_cadena <- FALSE
      }
      
      
      
    } # Fin if Si todo va OK...
    ####################################################
    
    # Si algo no esta OK...
    if (output_cadena == FALSE){
      mini <- as.data.frame(mtcars[c(1,2)])
      colnames(mini) <- c("No Data", "No Data")
      mini[,2] <- as.factor(as.character(mini[,2]))
      cat("Error mpp: 'mini' externo agregado (NO DATA)", "\n")
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
  
  
  # # # Medidas de Dispersion Particionadas
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
      tabla_mp_general <- mp(VR, input_decimales = input_decimales)$mp$tabla1_mp
      
      # Medidas de Posicion Particionadas
      tabla1_mpp <- as.data.frame(matrix(NA, cantidad_niveles , ncol(tabla_mp_general)))
      colnames(tabla1_mpp) <- colnames(tabla_mp_general)
      colnames(tabla1_mpp)[1] <- colnames(FACTOR)
      
      
      # Segun la cantidad de niveles
      if (cantidad_niveles > 0) {
        for(j in 1:cantidad_niveles) {
          
          dt <- NULL
          recorte <- NULL
          
          dt <- FACTOR == levels(FACTOR[,1])[j]
          recorte <- VR[dt, 1]
          dim(recorte) <- c(length(recorte), 1)
          recorte <- as.data.frame(recorte)
          colnames(recorte) <- levels(FACTOR[,1])[j]
          
          tabla1_mpp[j,] <- mp(recorte, input_decimales)$mp$tabla1_mp
          
        } # FIn for j
        
      } # Fin if cantidad > 0
      
      # Agregamos las medidas generales
        tabla_mp_general[1,1] <- " --Medidas Generales--"
        colnames(tabla_mp_general)[1] <- colnames(FACTOR)
        
        tabla1_mpp <- rbind(tabla1_mpp, tabla_mp_general)
        colnames(tabla1_mpp)[1] <- paste0("Variable: ", colnames(FACTOR))      
   
      
    ###
    } # Fin Tabla 1
    ############################################################
    
    
    # Tabla 2
    {
    ###
      
      # Le sacamos las medidas del general
      tabla2_mpp <- tabla1_mpp[-nrow(tabla1_mpp), ]
      
    ###  
    } # Fin Tabla 2
    ############################################################
    
    
    # Tabla 3
    {
    ###
      
      # Medidas Generales
      tabla_ic_general <- mp(VR, input_decimales = input_decimales)$mp$tabla3_mp
      
      # Agregamos las medidas generales
      tabla_ic_general[,1] <- " --IC General--"
      colnames(tabla_ic_general)[1] <- "Variable"
         
      # IC Particionados
      tabla3_icp <- list()
     
    
          
        for (n in 1:cantidad_niveles) {
       
            dt <- NULL
            recorte <- NULL
            
            dt <- FACTOR == levels(FACTOR[,1])[n]
            recorte <- VR[dt, 1]
            dim(recorte) <- c(length(recorte), 1)
            recorte <- as.data.frame(recorte)
            colnames(recorte) <- levels(FACTOR[,1])[n]
            
            tabla3_icp[[n]] <- mp(recorte, input_decimales)$mp$tabla3_mp
            
           
            
          } # FIn for n
          
# 
#       nueva_pos <- (length(tabla3_icp) + 1)
#       tabla3_icp[[nueva_pos]] <- tabla_ic_general
      
     # tabla3_icp <- rbind(tabla3_icp, tabla_ic_general)
      
      
      
      # IC Particionados V2
      tabla3_icp2 <- list()
      
      for (n in 1:nrow(tabla_ic_general)) {
       for (k in 1:length(tabla3_icp)) {
           
      
          if (k == 1) tabla_armada <- tabla3_icp[[k]][n,]
        
          if (k > 1) tabla_armada <- rbind(tabla_armada, tabla3_icp[[k]][n,])
      
          if (k == length(tabla3_icp)){
            tabla_armada <- rbind(tabla_armada, tabla_ic_general[n,])
            rownames(tabla_armada) <- c(1:nrow(tabla_armada))
            colnames(tabla_armada)[1] <- paste0("Variable: ", colnames(FACTOR))
            tabla3_icp2[[n]] <- tabla_armada
          }
      } # Fin for k
        # rownames(tabla3_icp2[[k]]) <- c(1:nrow(tabla3_icp2[[k]]))
        # 
      } # Fin for n
      
          
          
        
   
    ###  
    } # Tabla 3
    ####################################################################
    
    
  ###
  } # Fin Medidas de Posicion Particionadas
  ############################################################################
  
  
  # # # Mis Tablas
  {
  ###
    
    mis_tablas <- list(tabla1_mpp, tabla2_mpp, tabla3_icp2)
    names(mis_tablas) <- c("tabla1_mpp", "tabla2_mpp", "tabla3_icp2")
    
  ###  
  } # Fin Mis Tablas
  ###########################################################################
  
  
  # # # Cambios "NO DATA" o "Errores"
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
    names(salida) <- c("mpp", "output_cadena", "input_originales")
    
    return(salida)
    
  ###  
  } # Salida
  ############################################################################
  
  
}



if (1 == 2) {
  
  
  
#  BASE <- mtcars[,c(1,2)]
  BASE <- brm[,c(3,2)]
  input_base <- BASE
  input_base[,2] <- as.factor(as.character(input_base[,2]))
  input_decimales <- 2
  input_cadena <- NULL
  
  AVER <- mpp(input_base = input_base, input_decimales = input_decimales)
  
  
}


