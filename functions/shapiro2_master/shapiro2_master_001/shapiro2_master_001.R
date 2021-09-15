

shapiro2_master <- function(input_base = NULL, input_decimales = NULL, input_alfa = NULL, input_cadena = NULL){
  
  
  
  # Input originales 
  {
    ###
    
    input_originales <- list(input_decimales, input_alfa, input_cadena)
    names(input_originales) <- c("input_decimales", "input_alfa", "input_cadena")
    
    ###    
  } # Fin Argumentos originales
  ##############################################################################
  
  
  # # # Funcionamiento por defecto
  {
    ###

    # Decimales por defecto
    if (is.null(input_decimales)) input_decimales <- 2
    
    # Valor alfa por defecto
    if (is.null(input_alfa)) input_alfa <- 0.05

    # Cadena por defecto
    if (is.null(input_cadena)) input_cadena <- TRUE
    
    # Armamos el siguiente eslabon
    output_cadena <- input_cadena
    
    ###
  } # Fin Funcionamiento
  ################################################
  
  
  # # # Control 1 - input_base
  {
    ###
    if(output_cadena) {
    veredicto1 <- control_cq(input_base = input_base, input_cadena = output_cadena)
    output_cadena <- veredicto1
    }
    ###  
  } # Fin Control 1 - input_base
  #################################################
  
  
  # # # Control 2 - input_decimales
  {
    ###
    if(output_cadena) {
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
    }
    ###  
  } # Fin Control 2 - input_decimales
  ############################################################################
  
  
  # # # Control 3 y 4 - Detalles del test de Normalidad
  {
    ###
  
    # Si ya paso los anteriores, ahora debe pasar un control especifico
    # de la funcion. En este caso, hay dos cosas...
    # El factor debe tener al menos 1 nivel...

    # Control 3 - Al menos una categoria en la 2da columna
    if(output_cadena) {
      
      veredicto3 <- TRUE
      
      # Verificar que input_base tiene al menos una categoria
      if(veredicto3 && length(names(table(input_base[,2]))) < 1) {
        cat("\n", "Error shapiro2_master: input_base debe tener al menos una categoría en la 2da columna.", "\n")
        veredicto3 <- FALSE
      }  
    }
      
    # Control 4 - Al menos una categoria en la 2da columna
    if(output_cadena) {
      
      veredicto4 <- TRUE
      if(veredicto4 && sd(input_base[,1]) == 0) {
        cat("\n", "Error shapiro2_master: input_base presenta valores constantes en su 1ra columna.", "\n")
        veredicto4 <- FALSE
      }  
      
      
      
      # Si paso el control anterior... guardamos esta nueva opinion
      if (veredicto3) output_cadena <- veredicto4
      
    }
    

    ###
  }
  ###########################################################
  
  
  
  # # # Modificaciones (as.factor())
  {
  ###
    
    # Si todo va OK...
    if (output_cadena) {
      
      # Si cumple todo... Falta la columna 2 sea de tipo factor.
      # Si no es factor, la convertimos en factor.
      if (is.factor(input_base[,2]) == FALSE) input_base[,2] <- as.factor(as.character(input_base[,2]))
  ###
  }
  #########################################################################################################
    
      # Creamos "mini"  
      mini <- na.omit(input_base)
      
      # Vemos que mini tenga filas
      if (nrow(mini) == 0) {
        cat("Error shapiro2_master: 'mini' sin filas", "\n")
        output_cadena <- FALSE
      }
      
      
      
    } # Fin if Si todo va OK...
  ####################################################
    
  
  # # # Minibase 
  {
  ###
    mini <- na.omit(input_base)
    
    
  ###  
  } # Fin Minibase
  #################################################
  
  
  # # # Control 5 y 6 - Detalles del test de Normalidad
  {
  ###
    
    # Si ya paso los anteriores, ahora debe pasar un control especifico
    # de la funcion. En este caso, hay dos cosas...
    # El factor debe tener al menos 1 nivel...
    
    # Control 5 - Al menos una categoria en la 2da columna
    if(output_cadena) {
      
      veredicto5 <- TRUE
      
      # Verificar que input_base tiene al menos una categoria
      if(veredicto5 && length(names(table(mini[,2]))) < 1) {
        cat("\n", "Error shapiro2_master: 'mini' debe tener al menos una categoría en la 2da columna.", "\n")
        veredicto5 <- FALSE
      }  
      # Si paso el control anterior... guardamos esta nueva opinion
      if (veredicto4) output_cadena <- veredicto5
    }
    
    # Control 6 - Al menos una categoria en la 2da columna
    if(output_cadena) {
      
      veredicto6 <- TRUE
      if(veredicto6 && sd(input_base[,1]) == 0) {
        cat("\n", "Error shapiro2_master: 'mini' presenta valores constantes en su 1ra columna.", "\n")
        veredicto6 <- FALSE
      }  
      
      # Si paso el control anterior... guardamos esta nueva opinion
      if (veredicto5) output_cadena <- veredicto6
      
     
      
    }
    
    
  ###
  }
  ###########################################################
  
  
  # # # Base "NO DATA"
  {
  ###
    
    # Si algo no esta OK...
    if (output_cadena == FALSE){
      mini <- as.data.frame(mtcars[c(1,2)])
      colnames(mini) <- c("No Data", "No Data")
      mini[,2] <- as.factor(as.character(mini[,2]))
      cat("Error shapiro2_master: 'mini' externo agregado (NO DATA)", "\n")
    } # Fin si algo no esta OK
  
  ###
  } # Fin Base "NO DATA"
  ##################################################################
    
  
  
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
  
  
    # # # Resolucion
  {
    ###
    
    
    # Tabla 1: Test de Normalidad
    {
      ###
      
      niveles_factor <- levels(vector_FACTOR)
      varianzas_niveles <- tapply(vector_VR, vector_FACTOR, var)
      dt_no_constantes <- varianzas_niveles != 0
      
      tabla_global <- shapiro_master( input_base = VR, 
                      input_decimales = input_decimales,
                      input_alfa = input_alfa)$tablas$Test_Normalidad
      
  
      lista_normalidad <- list()
      
      # Generamos todos los analisis
      for (n in 1:length(niveles_factor)) {
        
        dt_categoria <- vector_FACTOR == niveles_factor[n]
        
        VR_MOD <- mini[dt_categoria, ]
        VR_MOD2 <- VR_MOD[1]
        
        lista_normalidad[[n]] <- shapiro_master( input_base = VR_MOD2, 
                                                 input_decimales = input_decimales,
                                                 input_alfa = input_alfa)$tablas$Test_Normalidad
        
        
        lista_normalidad[[n]][1,1] <- niveles_factor[n]
      }
      
      
      
      
      tabla_armada <- list2df(lista_normalidad)
      
      tabla_armada2 <- rbind(tabla_armada, tabla_global)
      
      agregado <- rep(colnames(mini)[1], nrow(tabla_armada2))
      tabla_resumen <- cbind(agregado, tabla_armada2)
      
      nombre1 <- "Variable"
      nombre2 <- paste0("Grupo de '", colnames(mini)[2], "'")
      colnames(tabla_resumen)[c(1,2)] <- c(nombre1, nombre2)
      tabla_resumen[nrow(tabla_resumen), 2] <- "--General--"
      
      
      tabla01 <- tabla_resumen
      
    ###  
    } # Fin Tabla 1
    ################################################
    
    
    # Tabla 2: Test Chi Cuadrado de Uniformidad
    {
    ###
      
       
      
      tabla02 <- tabla01[-nrow(tabla01), ]
      
      
      ###
    } # Fin Tabla 2
    ##################################
    
    
    # Frase 1: Valido hacer el test
    {
      ###
      frase01 <- ""
      if (veredicto3 == FALSE) frase01 <- ""
      ###  
    } # Fin Frase 1
    ##########################################
    
    
    
    # Frase 2: Interpretacion Estadistica
    {
      ###
      
      texto1 <- ""
      
      
      
      texto2 <- ""
      
      
      
      texto3 <- ""
      
      frase02 <- ""
   
      ###    
    } # Fin Frase 2
    ##############################################
    
    
    ###  
  } # Fin Resolucion
  ################################################################
  
  
  # Mis Tablas
  {
    ###
    
    # Reunimos las tablas
    mis_tablas <- list(tabla01, tabla02)
    names(mis_tablas) <- c("Test_Normalidad", "Test_Normalidad2")
    
    ###    
  }
  ##########################################################################
  
  
  
  # Mis Frases
  {
    ###
    
    # Reunimos las tablas
    mis_frases <- list(frase01, frase02)
    names(mis_frases) <- c("Validacion", "Explicacion")
    
    ###    
  }
  ##########################################################################
  
  
  
  # # # Cambios "NO DATA" o "Errores"
  {
    ###
    
    # 1) Cambios en Tablas
    {
      ###
      
      
      # Si hay errores... Y estamos en "NO DATA"
      if (output_cadena == FALSE) {
        
        cambio0 <- "Test Cancelado"
        cambio1 <- "Sin datos en la Columna"
        cambio2 <- "Modificar input_decimales"
        cambio3 <- "Se necesitan al menos 3 datos"
        cambio4 <- "Constante"
        cambio5 <- "Se necesitan al menos 3 datos"
        cambio6 <- "Constante"
        
        # Damos aviso si es algo de los datos o los decimales
        cambio_aplicado <- cambio0
        if (input_cadena == TRUE && veredicto1 == FALSE) cambio_aplicado <- cambio1
        else if (veredicto1 == TRUE && veredicto2 == FALSE) cambio_aplicado <- cambio2
        else if (veredicto1 == TRUE && veredicto2 == FALSE) cambio_aplicado <- cambio2
        else if (veredicto2 == TRUE && veredicto3 == FALSE) cambio_aplicado <- cambio3
        else if (veredicto3 == TRUE && veredicto4 == FALSE) cambio_aplicado <- cambio4
        else if (veredicto2 == TRUE && veredicto3 == FALSE) cambio_aplicado <- cambio5
        else if (veredicto3 == TRUE && veredicto4 == FALSE) cambio_aplicado <- cambio6
        
        # Cambiamos los valores por avisos
        for (n in 1:length(mis_tablas)) {
          
          esta_tabla <- mis_tablas[[n]]
          
          #if (is.list(esta_tabla)) next
          estas_dimensiones <- dim(esta_tabla)
          tabla_no_data <- as.data.frame(matrix(cambio_aplicado, estas_dimensiones[1], estas_dimensiones[2]))
          colnames(tabla_no_data) <- colnames(esta_tabla)
          for (k in 1:ncol(tabla_no_data)) tabla_no_data[,k] <- as.character(tabla_no_data[,k]) 
          tabla_no_data[1,1] <- colnames(input_base)
          esta_tabla <- as.data.frame(tabla_no_data)
          
          mis_tablas[[n]]  <- esta_tabla
          
        } # Fin for n
        
      } # Fin if si hay errores...
      #############################################################
      
      ###
    } # Fin 1) Cambios en Tablas
    ###########################################
    
    
    
    # 2) Cambios en frases
    {
      ###
      
      # Si hay errores... Y estamos en "NO DATA"
      if (output_cadena == FALSE) {
        
        frase_cambio0 <- "Test Cancelado por 'input_cadena'"
        frase_cambio1 <- "Sin datos en la Columna. No puede realizarse el test."
        frase_cambio2 <- "Valores decimales incorrectos en 'input_decimales'"
        frase_cambio3 <- "Se necesitan al menos 3 datos para poder realizar el test de Normalidad de Shapiro-Wilks"
        frase_cambio4 <- "La variable es constante. No puede realizarse el test sobre una constante."
        frase_cambio5 <- "Se necesitan al menos 3 datos para poder realizar el test de Normalidad de Shapiro-Wilks."
        frase_cambio6 <- "La variable es constante. No puede realizarse el test sobre una constante."
        
        
        # Damos aviso si es algo de los datos o los decimales
        frase_cambio <- frase_cambio0
        if (input_cadena == TRUE && veredicto1 == FALSE) frase_cambio <- frase_cambio1
        else if (veredicto1 == TRUE && veredicto2 == FALSE) frase_cambio <- frase_cambio2
        else if (veredicto1 == TRUE && veredicto2 == FALSE) frase_cambio <- frase_cambio2
        else if (veredicto2 == TRUE && veredicto3 == FALSE) frase_cambio <- frase_cambio3
        else if (veredicto3 == TRUE && veredicto4 == FALSE) frase_cambio <- frase_cambio4
        else if (veredicto2 == TRUE && veredicto3 == FALSE) frase_cambio <- frase_cambio5
        else if (veredicto3 == TRUE && veredicto4 == FALSE) frase_cambio <- frase_cambio6
        
        
        mis_frases[[1]] <- frase_cambio
        mis_frases[[2]] <- "No pueden ser obtenidos valores estadísticos."
        
        
      } # Fin if si hay errores...
      #############################################################  
      
      ###
    } # Fin 2) Cambios en frases
    ###########################################################
    
    
  } # Fin Cambios "NO DATA" o "Errores"
  ##################################################
  
  
  # # # Salida
  {
    ###
    
    salida <- list(mis_tablas, mis_frases, output_cadena, input_originales)
    
    names(salida) <- c("tablas", "frases", "output_cadena", "input_originales")
    return(salida)
    
    
    ###  
  } # Fin Salida
  ##################
  
}



if ( 1 == 2) {
  
  input_base <- mtcars[,c(1,2)]
  input_base[,2] <- as.factor(input_base[,2])
  input_decimales <- NULL
  input_alfa <- NULL
  input_cadena <- NULL
  
  aver <- shapiro2_master(input_base = input_base, input_decimales = input_decimales, input_alfa = input_alfa, 
                          input_cadena = input_cadena)
  
  
  aver$tablas$Test_Normalidad
  aver$frases$Explicacion
  aver$frases$Validacion
  
}