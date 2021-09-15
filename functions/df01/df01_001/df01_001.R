


df01 <- function(input_base = NULL, input_decimales = NULL, input_cadena = NULL) {
  
  
  # # # Input originales 
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
  
  
  # # # Modificaciones y Controles 3, y base "NO DATA"
  {
  ###
    
    # Si todo va OK...
    if (output_cadena) {
      
    # Si cumple todo... Falta que sea de tipo factor.
    # Si es factor... la dejamos como está...
    # Sino, al obligamos...
    if (is.factor(input_base[,1]) == FALSE) input_base[,1] <- as.factor(as.character(input_base[,1]))
  
    # Creamos "mini"  
    mini <- na.omit(input_base)
  
    # Vemos que mini tenga filas
    if (nrow(mini) == 0) {
    cat("Error df01: 'mini' sin filas", "\n")
    output_cadena <- FALSE
    }
    
    } # Fin if Si todo va OK...
    ###################################################
    
    # Si hay algun problema...
    if (output_cadena == FALSE){
      mini <- as.data.frame(mtcars[2])
      mini[,1] <- as.factor(as.character(mini[,1]))
      colnames(mini) <- "No Data"
      cat("Error df01: 'mini' externo agregado (NO DATA)", "\n")
    } # Fin if si hay problemas
    ##########################################################
    
    
  ### 
  } # Fin Modificaciones y Controles 3, y base "NO DATA"
  ################################################################
  
  
  
  # # # Resolucion
  {
  ###
    

      # Tabla 1: Distribucion de Frecuencias
      {
      ###
        
        # Frecuencais Absolutas
        fa <- table(mini)
        
        # Total Absoluto
        n_total <- sum(fa)
        grupos <- as.character(names(fa))
        salida_n_total <- rep(n_total, length(fa))
  
        # Cociente
        cociente <- paste0(fa, "/", n_total)
        
        # Frecuencias Relativas
        fr <- round2((fa/n_total), input_decimales)
        if (n_total == 0) fr <- rep(0, length(grupos))
        
        
        # Porcentajes
        porcentaje <- fr*100
        
        # Porcentaje2
        porcentaje2 <- paste(porcentaje, "%", sep="")
        
        # Frec. Abs. Acumuladas
        faa <- cumsum(fa)
        
        # Frec. Rel. Acumuladas
        fra <- cumsum(fr)
        
        # Cociente Acumulado
        cocientea <- paste0(faa, "/", n_total)
        
        # Porcentajes Acumulados
        pa <- fra*100
        
        # Porcentajes Acumulados 2
        pa2 <- paste(pa, "%", sep="")
        
        # Tabla de Distribucion de Frecuencias
        tabla01 <- cbind(grupos, fa, salida_n_total, cociente, fr,  porcentaje, porcentaje2, faa, fra, cocientea, pa, pa2)
        tabla01 <- as.data.frame(tabla01)
        nombres_tabla01 <- c(colnames(mini), "FA", "Total", "Cociente", "FR", "%", "%%", "FAA", "FRA", "CocienteAcum", "%Acum", "%%Acum")
        colnames(tabla01) <- nombres_tabla01
        
        numericas <- c(2, 3, 5, 6, 8, 9, 11)
        for(n in 1:length(numericas)) tabla01[,numericas[n]] <- as.numeric(as.character(tabla01[,numericas[n]])) 
        
        

      ###
      } # Fin Tabla 1
      ##################################

    
      # Tabla 2: Intervalos de Confianza
      {
      ###
        
      p <- fr
      q <- rep(NA, length(p))
      for (n in 1:length(q)) q[n] <- sum(p[-n])
      n <- n_total  
      
      alfa <- c(0.10, 0.05, 0.01)
      
      
      tablas2_ic <- list()
      
      for (k in 1:length(alfa)) {
        
      este_alfa <- alfa[k]
      este_alfa_2 <- este_alfa/2
      confianza <- paste0((1 - este_alfa)*100, "%")
      
      Z <- qnorm(1-este_alfa_2)
      Z <- round2(Z, input_decimales)
      
      desvio <- Z*sqrt((p*q)/n)
      desvio <- round2(desvio, input_decimales)
      
      li_prop <- p - desvio
      ls_prop <- p + desvio
      
      dt_li <- li_prop < 0
      dt_ls <- ls_prop > 1
      
      if (sum(dt_li) > 0) li_prop[dt_li] <- 0
      if (sum(dt_ls) > 0) ls_prop[dt_ls] <- 1
      
        
      li_porcentaje <- paste0((li_prop*100), "%")
      ls_porcentaje <- paste0((ls_prop*100), "%")
      
      
      tabla02 <- cbind(grupos, porcentaje2, li_porcentaje, ls_porcentaje)
      colnames(tabla02) <- c(colnames(mini), "%", paste0("LI", confianza), paste0("LS", confianza))
      tabla02 <- as.data.frame(tabla02)
      rownames(tabla02) <- rownames(tabla01)
     
      
      if (output_cadena == FALSE) {
        
        estas_dimensiones <- dim(tabla02)
        tabla_no_data02 <- as.data.frame(matrix("Sin datos en la Columna", estas_dimensiones[1], estas_dimensiones[2]))
        colnames(tabla_no_data02) <- colnames(tabla02)
        tabla02 <- tabla_no_data02
        
      }
       
      tablas2_ic[[k]] <- tabla02
      
      }
      
     
  
      ###
      } # Fin Tabla 2
      ##################################
      
    
      # Tabla 3: Fusion
      {
      ###
        fusion <- paste0(porcentaje2, "(", fa, ")")
        
        tabla03 <- cbind(grupos, fusion)
        tabla03 <- as.data.frame(tabla03)
        nombres_tabla03 <- c(colnames(mini), "%(FA)")
        colnames(tabla03) <- nombres_tabla03
        
      ###  
      } # Fin Tabla 3
      ##################################
    
      # Tabla 4: Recorte de la Tabla 1
      {
      ###
        estas_columnas1 <- c(1,2,3,4,5,7)
        tabla04 <- tabla01[, estas_columnas1]
        
        estas_columnas2 <- c(2,5,6)
        colnames(tabla04)[estas_columnas2] <- c("Frecuencia Absoluta", "Frecuencia Relativa", "%")
      ###  
      } # Fin Tabla 4
      ########################################
    
    
    
  ###  
  } # Fin Resolucion
  ################################################################
  
  
  # Mis Tablas
  {
  ###
    
    # Reunimos las tablas
    mis_tablas <- list(tabla01, tablas2_ic, tabla03, tabla04)
    names(mis_tablas) <- c("DF", "IC", "Fusion", "DF2")
    
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
  
    salida <- list(mis_tablas, output_cadena, input_originales)
    
    names(salida) <- c("df01", "output_cadena", "input_originales")
    return(salida)
    
      
  ###  
  } # Fin Salida
  ##################
  
  
  
  
  
} # Fin function


# 
if (1 == 2) {
  # Ejemplo
  input_base <- as.data.frame(mtcars[2]) 
  input_decimales <- 2
  SALIDA <- df01(input_base,2)
  SALIDA$df01$IC
  
  
}

