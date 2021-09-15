

shapiro_master <- function(input_base = NULL, input_decimales = NULL, input_alfa = NULL, input_cadena = NULL){
  
  
  
  # # # Input originales 
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
    
    # Alfa por defecto
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
    
    # Control 1 - Control Cuantitativo
    if (output_cadena) {
    veredicto1 <- control_1c(input_base = input_base, input_cadena = output_cadena)
    output_cadena <- veredicto1
    }
    ###  
  } # Fin Control 1 - input_base
  #################################################
  
  
  # # # Control 2 - input_decimales
  {
  ###
    
    # Control 2 - Decimales correctos
    if (output_cadena) {
    # Hacemos el control de input_decimales
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
  
  
  # # # Control 3 y 4 - Detalles para Test de Normalidad Shapiro-Wilk
  {
  ###

    # Control 3 - input_base con al menos 3 datos
    if (output_cadena){
      
      veredicto3 <- TRUE
      
      # Verificar que input_base tenga al menos 3 datos
      if(veredicto3 && nrow(input_base) < 3) {
        cat("\n", "Error shapiro_master: input_base debe tener al menos tres filas.", "\n")
        veredicto3 <- FALSE
      }  
      
      # Si paso el control anterior... guardamos esta nueva opinion
      if (veredicto2) output_cadena <- veredicto3
      
      
    }
    
    # Control 4 - input_base sin valores constantes
    if (output_cadena){

      veredicto4 <- TRUE
      
      # Verificar que input_base no sea constante
      if(veredicto4 && var(input_base[,1]) == 0) {
        cat("\n", "Error shapiro_master: input_base no debe ser constante.", "\n")
        veredicto4 <- FALSE
      }  
  
      # Si paso el control anterior... guardamos esta nueva opinion
      if (veredicto3) output_cadena <- veredicto4
          
    }

  ###  
  }
  ####################################################################
  
  
  # # # Creacion de Minibase
  {
  ###
    if (output_cadena) {
      # Creamos "mini"  
      mini <- na.omit(input_base)
    }
  ###  
  } # Fin Creacion Minibase
  ###########################################
  
  
  # # # Control 5 y 6 - Control sobre minibase (al menos 3 filas) 
  {
  ###
    
    # Control 5 - Objeto 'mini' con al menos 3 filas
    if (output_cadena) {
  

     
      veredicto5 <- TRUE
      if (nrow(mini) < 3) {
        veredicto5 <- FALSE
        cat("Error shapiro_master: 'mini' debe tener al menos tres filas", "\n")
      }
      
      output_cadena <- veredicto5
    }
    
    
    # Control 6 - mini sin valores constantes
    if (output_cadena){
      
      veredicto4 <- TRUE
      
      # Verificar que input_base no sea nulo
      if(veredicto4 && var(input_base[,1]) == 0) {
        cat("\n", "Error shapiro_master: mini no debe ser constante.", "\n")
        veredicto4 <- FALSE
      }  
      
      # Si paso el control anterior... guardamos esta nueva opinion
      if (veredicto3) output_cadena <- veredicto4
      
    }
    
  ###      
  } # Fin if Si todo va OK...
  ###################################################
    
  
  # # # Base "NO DATA"
  {
  ###
  
    # Si hay algun problema...
    if (output_cadena == FALSE){
      mini <- as.data.frame(mtcars[1])
      colnames(mini) <- "No Data"
      cat("Error shapiro_master: 'mini' externo agregado (NO DATA)", "\n")
      
  ###    
  } # Fin if si hay problemas
  ##########################################################
    
    
    ### 
  } # Fin Modificaciones y Controles 4, y base "NO DATA"
  ################################################################
  
  
  # # # VR
  {
    ###
    
    VR <- mini[1]
    
    vector_VR <- mini[,1]
    
    
    ###  
  } # Fin VR
  #############################################
  
  
  # # # Resolucion
  {
    ###
    
    
    # Tabla 1: Test de Normalidad de Shapiro-Wilk
    {
      ###
      
      test <- shapiro.test( x = vector_VR)
      
      n_muestra <- nrow(mini)
      estadistico <- round2(test$statistic, input_decimales)
      valor_p_interno <- round2(test$p.value, input_decimales)
      valor_p_externo <- valor_p_interno
      if (valor_p_interno < 0.01) valor_p_externo <- "<<0.01"
      gl <- test$parameter
      decision <- "No Rechazo Ho"
      normalidad <- "Si"
      
      
      # Si valor p es menor que alfa
      if (valor_p_interno < input_alfa) {
        decision <- "Rechazo Ho"
        normalidad <- "No"
      }
      
      nombre_columnas <- c("Variable", "n", "Test", "Estadístico (W)", "Grados de Libertad", "Valor p", "Alfa", "Decisión", "¿Posee distribución Normal?")
      tabla_test <- as.data.frame(matrix(NA, 1 , length(nombre_columnas)))
      colnames(tabla_test) <- nombre_columnas
      
      tabla_test[1,1] <- colnames(mini)
      tabla_test[1,2] <- n_muestra
      tabla_test[1,3] <- "Normalidad de Shapiro-Wilk"
      tabla_test[1,4] <- estadistico
      tabla_test[1,5] <- "No corresponde"
      tabla_test[1,6] <- valor_p_externo
      tabla_test[1,7] <- input_alfa
      tabla_test[1,8] <- decision
      tabla_test[1,9] <- normalidad
      
      tabla01 <- tabla_test    
      
      
      ###  
    }
    ################################################
    
  
    # Frase 1: Valido hacer el test de Normalidad de Shapiro-Wilk
    {
      ###
      frase01 <- ""
      
      # if (veredicto1 == FALSE) frase01 <- "No es posible aplicar el test de normalidad a estos datos - control_1c()"
      #   else if (veredicto2 == FALSE) frase01 <- "Referencia de redondeo incorrecta. Cambie el valor de redondeo."
      #     else if (veredicto3 == FALSE) frase01 <- "La variable debe tener al menos tres datos para poder realizar el Test de Normalidad de Shapiro-Wilk."
      #       else if (veredicto4 == FALSE) frase01 <- "La variable es constante. No es posible aplicar el test de Normalidad de Shapiro-Wilk."
      #         else if (veredicto5 == FALSE) frase01 <- "No es posible realizar el test. Se tienen menos de tres filas con datos de la variable."
      #           else if (veredicto6 == FALSE) frase01 <- "La variable es constante. No es posible aplicar el test de Normalidad de Shapiro-Wilk."
              
      ###  
    } # Fin Frase 1
    ##########################################
    
    
    
    # Frase 2: Interpretacion Estadistica
    {
      ###
      
      texto1 <- c("El valor p es mayor que el valor de ", paste0("alfa=", input_alfa, "."),"<br/>",
                  "No se rechaza la Ho.", "<br/>",
                  "La distrubución de la variable ", paste0("'", colnames(input_base), "'"), " es estadísticamente igual a la distribución Normal.", "<br/>")
      
      
      
      texto2 <- c("El valor p es igual que el valor de ", paste0("alfa=", input_alfa, "."),"<br/>",
                  "No se rechaza la Ho.", "<br/>",
                  "La distrubución de la variable ", paste0("'", colnames(input_base), "'"), " es estadísticamente igual a la distribución Normal.", "<br/>")
      
      
      
      texto3 <- c("El valor p es menor que el valor de ", paste0("alfa=", input_alfa, "."),"<br/>",
                  "Se rechaza la Ho.", "<br/>",
                  "La distribución de la variable ", paste0("'", colnames(input_base), "'"), " es estadísticamente diferente a la distribución Normal.", "<br/>")
      
      
      frase02 <- ""
      # Si p es mayor... igual... o menor... que alfa
      if (valor_p_interno > input_alfa) frase02 <- texto1
        else if (valor_p_interno == input_alfa) frase02 <- texto2
          else if(valor_p_interno < input_alfa) frase02 <- texto3
      
      
    
      
      
      
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
    mis_tablas <- list(tabla01)
    names(mis_tablas) <- c("Test_Normalidad")
    
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
      # if (input_cadena == TRUE && veredicto1 == FALSE) cambio_aplicado <- cambio1
      #   else if (veredicto1 == TRUE && veredicto2 == FALSE) cambio_aplicado <- cambio2
      #     else if (veredicto1 == TRUE && veredicto2 == FALSE) cambio_aplicado <- cambio2
      #       else if (veredicto2 == TRUE && veredicto3 == FALSE) cambio_aplicado <- cambio3
      #         else if (veredicto3 == TRUE && veredicto4 == FALSE) cambio_aplicado <- cambio4
      #           else if (veredicto2 == TRUE && veredicto3 == FALSE) cambio_aplicado <- cambio5
      #             else if (veredicto3 == TRUE && veredicto4 == FALSE) cambio_aplicado <- cambio6
      
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
        frase_cambio1 <- "Sin datos numéricos en la columna seleccionada.</br> No puede realizarse el test. </br> Verifique en 'Control' que la variable posee datos y es numérica."
        frase_cambio2 <- "Valores decimales incorrectos en 'input_decimales'. Modifique el valor ingresado en decimales."
        frase_cambio3 <- "El test de normalidad no puede ser realizado. Se necesitan al menos 3 datos para poder realizar el test de Normalidad de Shapiro-Wilk"
        frase_cambio4 <- "La variable es constante. No puede realizarse el test de normalidad sobre una variable constante."
        frase_cambio5 <- "Se necesitan al menos 3 datos para poder realizar el test de Normalidad de Shapiro-Wilk."
        frase_cambio6 <- "La variable es constante. No puede realizarse el test de normalidad sobre una constante."
        
                
        # Damos aviso si es algo de los datos o los decimales
        if (input_cadena == FALSE) frase_cambio <- frase_cambio0
          else if (input_cadena == TRUE && veredicto1 == FALSE) frase_cambio <- frase_cambio1
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
  
  
  
} # Fin function Shapiro Wilk



if ( 1 == 2) {
  

  input_base <- as.factor(mtcars[,2])
  dim(input_base) <- c(length(input_base), 1)
  input_base <- as.data.frame(input_base)
  
  input_decimales <- NULL
  input_alfa <- NULL
  input_cadena <- FALSE
  
  salida <- shapiro_master(input_base = input_base, input_decimales = input_decimales, input_alfa = input_alfa, input_cadena = input_cadena)
  
  salida$tablas$Test_Normalidad
  
  salida$frases$Validacion
  
  salida$frases$Explicacion
  
} # Fin if