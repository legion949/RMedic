

uniformidad_master <- function(input_base = NULL, input_decimales = NULL, input_alfa = NULL, input_cadena = NULL){
  
  
  
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
    veredicto1 <- control_1q(input_base = input_base, input_cadena = output_cadena)
    output_cadena <- veredicto1
    
    ###  
  } # Fin Control 1 - input_base
  #################################################
  
  
  # # # Control 2 - input_decimales
  {
    ###
    
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
    
    ###  
  } # Fin Control 2 - input_decimales
  ############################################################################
  
  
  # # # Control3 - Detalles del test de Uniformidad
  {
  ###
    # Si no pasa el control numerico, asignamos un nuevo valor para input_decimales
    # pero guardamos el original por si hace falta
    if (veredicto2 == TRUE){

      veredicto3 <- TRUE
      
      # Verificar que input_base no sea nulo
      if(veredicto3 && length(names(table(input_base))) < 2) {
        cat("\n", "Error uniformidad_master: input_base debe tener al menos dos categorías.", "\n")
        veredicto3 <- FALSE
      }  
      
   }
    
    # Si paso el control anterior... guardamos esta nueva opinion
    if (veredicto2) output_cadena <- veredicto3
  ###
  }
  ###########################################################
  
  
  # # # Modificaciones y Controles 4, y base "NO DATA"
  {
    ###
    
    # Si todo va OK...
    if (output_cadena) {
      
       # Creamos "mini"  
      mini <- na.omit(input_base)
      
      # Vemos que mini tenga filas
      if (nrow(mini) == 0) {
        cat("Error uniformidad_master: 'mini' sin filas", "\n")
        output_cadena <- FALSE
      }
      
    } # Fin if Si todo va OK...
    ###################################################
    
    # Si hay algun problema...
    if (output_cadena == FALSE){
      mini <- as.data.frame(mtcars[1])
      colnames(mini) <- "No Data"
      cat("Error uniformidad_master: 'mini' externo agregado (NO DATA)", "\n")
    } # Fin if si hay problemas
    ##########################################################
    
    
    ### 
  } # Fin Modificaciones y Controles 4, y base "NO DATA"
  ################################################################
  
  
  
  # # # Resolucion
  {
    ###
    
    
    # Tabla 1: Distribucion de Frecuencias
    {
      ###
      
     tabla01 <- df01(input_base, input_decimales)$df01$DF2
      
    ###  
    }
    ################################################
    
    
    # Tabla 2: Test Chi Cuadrado de Uniformidad
    {
    ###
      
      # Frecuencais Absolutas
      fa <- table(mini)
      
      # Tamanio de la muestra
      n_muestra <- sum(fa)
      
      # Test Chi Cuadrado de Uniformidad
      test <- chisq.test(fa)
      
      # Armado interno de objetos
      estadistico <- round2(test$statistic, input_decimales)
      valor_p_interno <- round2(test$p.value, input_decimales)
      valor_p_externo <- valor_p_interno
      if (valor_p_interno < 0.01) valor_p_externo <- "<<0.01"
      gl <- test$parameter
      gl <- as.integer(gl)
      
      decision <- "No Rechazo Ho"
      resumen <- "No"
      
      # Si valor p es menor que alfa
      if (valor_p_interno < input_alfa) decision <- "Rechazo Ho"
      if (valor_p_interno < input_alfa) resumen <- "Si"
            
      nombre_columnas <- c("Variable", "n", "Test", "Estadístico (Chi)", "Grados de Libertad", "Valor p", "Decisión", "¿Existen diferencias entre las proporciones de las categorías?")
      tabla_test <- as.data.frame(matrix(NA, 1 , length(nombre_columnas)))
      colnames(tabla_test) <- nombre_columnas
      
      tabla_test[1,1] <- colnames(mini)
      tabla_test[1,2] <- n_muestra
      tabla_test[1,3] <- "Chi Cuadrado"
      tabla_test[1,4] <- estadistico
      tabla_test[1,5] <- gl
      tabla_test[1,6] <- valor_p_externo
      tabla_test[1,7] <- decision
      tabla_test[1,8] <- resumen
      
      tabla02 <- tabla_test    
      
      
    ###
    } # Fin Tabla 2
    ##################################
    
    
    # Frase 1: Valido hacer el test
    {
    ###
        frase01 <- ""
        if (veredicto3 == FALSE) frase01 <- "No es posible aplicar el test de uniformidad a estos datos."
    ###  
    } # Fin Frase 1
    ##########################################
    
    
    
    # Frase 2: Interpretacion Estadistica
    {
    ###
    
      texto1 <- c("El valor p es mayor que el valor de alfa=", paste0(input_alfa, "."),"<br/>",
                     "No se rechaza la Ho.", "<br/>",
                     "Las proporciones de todas las categorías son estadísticamente iguales.", "<br/>")
      
      
      
      texto2 <- c("El valor p es igual que el valor de alfa=", paste0(input_alfa, "."),"<br/>",
                  "No se rechaza la Ho.", "<br/>",
                  "Las proporciones de todas las categorías son estadísticamente iguales.", "<br/>")
      
      
      
      texto3 <- c("El valor p es menor que el valor de alfa=", paste0(input_alfa, "."),"<br/>",
                  "Se rechaza la Ho.", "<br/>",
                  "Al menos una de las categorías presenta una proporción estadísticamente diferente.", "<br/>")
      
      
      # Si p es mayor... igual... o menor... que alfa
      if (valor_p_interno > input_alfa) frase02 <- texto1
        else if (valor_p_interno == input_alfa) frase02 <- texto2
          else if(valor_p_interno < input_alfa) frase02 <- texto3
      
            
      # Si p es menor que alfa...
      if (valor_p_interno < input_alfa) frase02 <- texto2
      
      
      
      
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
    names(mis_tablas) <- c("DF", "Test_Uniformidad")
    
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
    
    
    # Si hay errores... Y estamos en "NO DATA"
    if (output_cadena == FALSE) {
      
      cambio1 <- "Sin datos en la Columna"
      cambio2 <- "Modificar input_decimales"
      
      # Damos aviso si es algo de los datos o los decimales
      cambio_aplicado <- cambio1
      if (veredicto1 == TRUE && veredicto2 == FALSE) cambio_aplicado <- cambio2
      
      
      
      # Cambiamos los valores por avisos
      for (n in 1:length(mis_tablas)) {
        
        esta_tabla <- mis_tablas[[n]]
        
        if (is.list(esta_tabla)) next
        estas_dimensiones <- dim(esta_tabla)
        tabla_no_data <- as.data.frame(matrix(cambio_aplicado, estas_dimensiones[1], estas_dimensiones[2]))
        colnames(tabla_no_data) <- colnames(esta_tabla)
        esta_tabla <- tabla_no_data
        
        mis_tablas[[n]]  <- esta_tabla
        
      } # Fin for n
      
    } # Fin if si hay errores...
    #############################################################
    
    
    ###   
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



if (1 == 2) {
  
  
  input_base <- mtcars[2]
  input_decimales <- NULL
  input_cadena <- NULL
  input_alfa <- NULL
  
  TEST <- uniformidad_master(input_base = input_base, input_decimales = input_decimales, 
                             input_alfa = input_alfa, input_cadena = input_cadena)
  
  TEST$tablas$Test_Uniformidad
  TEST$frases$Validacion
  TEST$frases$Explicacion
  
}