


df02 <- function(input_base = NULL, input_decimales = NULL, input_cadena = NULL) {
  
  
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
    
  # Funcionamiento por defecto
  if (is.null(input_cadena)) input_cadena <- TRUE
  
  # Decimales por defecto
  if (is.null(input_decimales)) input_decimales <- 2
   

  # Armamos el siguiente eslabon
  output_cadena <- input_cadena
  
  ###
  } # Fin Funcionamiento
  ##########################################################
  
  
  # # # Control 1 - input_base
  {
  ###
    veredicto1 <- control_2q(input_base = input_base, input_cadena = output_cadena)
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
  
  
  # # # Modificaciones, Controles 2, y base "NO DATA"
  {
  ###
    
    # Si todo va OK...
    if (output_cadena) {
      
      # Si cumple todo... Falta cada columna sea de tipo factor.
      if (is.factor(input_base[,1]) == FALSE) input_base[,1] <- as.factor(as.character(input_base[,1]))
      if (is.factor(input_base[,2]) == FALSE) input_base[,2] <- as.factor(as.character(input_base[,2]))
      
      # Creamos "mini"  
      mini <- na.omit(input_base)
      
      # Vemos que mini tenga filas
      if (nrow(mini) == 0) {
        cat("Error df02: 'mini' sin filas", "\n")
        output_cadena <- FALSE
      }
      
    } # Fin if Si todo va OK...
    ###########################
    
    # Si no hay datos o tienen errores...
    if (output_cadena == FALSE){
      mini <- as.data.frame(mtcars[c(8,2)])
      mini[,1] <- as.factor(as.character(mini[,1]))
      mini[,2] <- as.factor(as.character(mini[,2]))
      colnames(mini) <- "No Data"
      cat("Error df02: 'mini' externo agregado (NO DATA)", "\n")
    }
  ### 
  } # Fin Modificaciones
  ################################################################    
  
 
  # # # Resolucion
  {
  ###

    # 1) Marginales al total
    {
    ###
      
    
      
    # Detalles Iniciales y fa
    fa <- table(mini)
    n_total <- sum(fa)
    fa_interno <- fa
    m_col <- colSums(fa)
    m_row <- rowSums(fa)
    
    # fa marginal
    fa_marginal <- cbind(fa, m_row)
    fa_marginal <- rbind(fa_marginal, c(m_col, n_total))
    colnames(fa_marginal)[ncol(fa_marginal)] <- c("Total por Filas")
    rownames(fa_marginal)[nrow(fa_marginal)] <- c("Total por Columnas")
    
    # Cociente al Total con marginales
    cociente_marginal <- fa_marginal
    for(n in 1:ncol(cociente_marginal)) cociente_marginal[,n] <- paste0(cociente_marginal[,n], "/", n_total)
    
    
    # Frecuencias Relativas al Total con Marginales
    fr_marginal <- fa_marginal/n_total
    fr_marginal <- round2(fr_marginal, input_decimales)
    
    # Si es "NaN" le ponemos un cero... Hay que hacerlo dos veces
    for (n in 1:ncol(fr_marginal)) if (is.nan(fr_marginal[n])) fr_marginal[n,] <- rep(0, nrow(fr_marginal))
    for (k in 1:nrow(fr_marginal)) if (is.nan(fr_marginal[k])) fr_marginal[,k] <- rep(0, ncol(fr_marginal))
      
    m_col2 <- fr_marginal[nrow(fr_marginal),-ncol(fr_marginal)]
    m_row2 <- fr_marginal[-nrow(fr_marginal),ncol(fr_marginal)]
    totales_raros <- c(sum(m_col2), sum(m_row2))
    diferencia <- abs(totales_raros - 1)
    dt_diferencia <- max(diferencia) == diferencia
    orden_diferencia <- c(1,2)[dt_diferencia]
    if (length(orden_diferencia) == 2) orden_diferencia <- orden_diferencia[1]
    

    # En "fr"... los marginales al total por filas y columnas deben sumar 1 en cada caso.
    # Por temas de redondeo... puede que uno sea igual a 1 y el otro no... o que ambos sean diferentes de 1.
    # En ese caso deberiamos avisarle que las sumas marginales no estan dando como debieran... y que
    # debe cambiar los decimales para traajar...
    
    # # Deteccion de redondeo incorrecto
     fr_interno <- fr_marginal
    # if (input_aviso == TRUE) if (totales_raros[orden_diferencia] != 1) fr_marginal[nrow(fr_marginal), ncol(fr_marginal)] <- paste0(fr_marginal[nrow(fr_marginal), ncol(fr_marginal)], "(Redondeo Incorrecto)")  
        
    # Porcentajes al Total con marginales
    porcentaje_marginal <- fr_interno
    porcentaje_marginal <- porcentaje_marginal*100
    #dim(PORCENTAJE) <- dim(FR_interno)
    
    porcentaje2_marginal <- porcentaje_marginal
    for(n in 1:ncol(porcentaje2_marginal)) for (k in 1:nrow(porcentaje2_marginal)) {
      porcentaje2_marginal[k,n] <- paste(porcentaje2_marginal[k,n], "%", sep="")
    }
    
    # Guardamos unos porcentajes internos...
    porcentaje_interno <- porcentaje_marginal
    
    # # Si tuvo problemas con las "fr" dejamos constancia tambien en la tabla de porcentajes
    # if (input_aviso == TRUE) if (totales_raros[orden_diferencia] != 1) porcentaje_marginal[nrow(porcentaje_marginal), ncol(porcentaje_marginal)] <- paste0(porcentaje_marginal[nrow(porcentaje_marginal), ncol(porcentaje_marginal)], "(Redondeo Incorrecto)")  
    
    ###
    } # Fin Marginales al total
    ###################################################################################
    
    
    # 2) Todo por filas
    {
    
    # FA por filas
    fa_filas <- fa
    total_filas <- rowSums(fa)
    fa_filas <- cbind(fa_filas, total_filas)
    colnames(fa_filas)[ncol(fa_filas)] <- "Total por Filas"
    
    # Cociente por filas
    cociente_filas <- fa_filas
    for (n in 1:nrow(cociente_filas)) cociente_filas[n,] <- paste0(cociente_filas[n,], "/", total_filas[n]) 
      
    # "fr" por filas
    fr_filas <- fa_filas
    for (n in 1:nrow(cociente_filas)) {
      
      fr_filas[n, ] <- fr_filas[n, ]/total_filas[n]
      fr_filas[n, ] <- round2(fr_filas[n, ], 2)
      
      if (total_filas[n] == 0) fr_filas[n, ] <- rep(0, length(fr_filas[n, ]))
      
    }
    
    fr_filas_interno <- fr_filas
    totales_fr_filas <- fr_filas[,ncol(fr_filas)]
    dt_fr_filas <- totales_fr_filas != 1
#    if (input_aviso == TRUE) fr_filas[dt_fr_filas] <- paste0(fr_filas[dt_fr_filas], "(Redondeo Incorrecto)")
    
    
    
    # "porcentaje" por filas
    porcentaje_filas <- fr_filas_interno*100
    
    porcentaje2_filas <-  porcentaje_filas
    for (n in 1:nrow(porcentaje2_filas)) porcentaje2_filas[n,] <- paste0(porcentaje2_filas[n,], "%")
#     porcentaje_filas[dt_fr_filas] <- paste0(porcentaje_filas[dt_fr_filas], "(Redondeo Incorrecto)")
    
    ###
    } # Todo por filas
    ###################################################################
   
    
    # 3) Todo por columnas
    {
    ###
      
    
    # FA por columnas
    fa_columnas <- fa
    total_columnas <- colSums(fa)
    fa_columnas <- rbind(fa_columnas, total_columnas)
    rownames(fa_columnas)[nrow(fa_columnas)] <- "Total por columnas"
    
    # Cociente por columnas
    cociente_columnas <- fa_columnas
    for (n in 1:ncol(cociente_columnas)) cociente_columnas[,n] <- paste0(cociente_columnas[,n], "/", total_columnas[n]) 
    
    # "fr" por columnas
    fr_columnas <- fa_columnas
    for (n in 1:ncol(cociente_columnas)) {
      
      fr_columnas[,n ] <- fr_columnas[,n ]/total_columnas[n]
      fr_columnas[,n ] <- round2(fr_columnas[,n ], 2)
      if (total_columnas[n] == 0) fr_columnas[,n ] <- rep(0, length(fr_columnas[,n ]))
    }
    
    fr_columnas_interno <- fr_columnas
    totales_fr_columnas <- fr_columnas[nrow(fr_columnas),]
    dt_fr_columnas <- totales_fr_columnas != 1
#    if (input_aviso == TRUE) fr_columnas[dt_fr_columnas] <- paste0(fr_columnas[dt_fr_columnas], "(Redondeo Incorrecto)")
    
    
    
    # "porcentaje" por columnas
    porcentaje_columnas <- fr_columnas_interno*100
    
    porcentaje2_columnas <- porcentaje_columnas
    for (n in 1:ncol(porcentaje2_columnas)) porcentaje2_columnas[,n] <- paste0(porcentaje2_columnas[,n], "%")
#    if (input_aviso == TRUE)  porcentaje_columnas[dt_fr_columnas] <- paste0(porcentaje_columnas[dt_fr_columnas], "(Redondeo Incorrecto)")
    
    
    ###
    } # Todo por columnas
    #################################################
    
    
    # 4) Clasico
    {
    ###
    
    # FA Clasico  
    fa_clasico <- fa_marginal[-nrow( fa_marginal), - ncol( fa_marginal)]
    if(is.null(dim(fa_clasico))) {
      
    dim(fa_clasico) <- c((nrow(fa_marginal)-1), (ncol(fa_marginal)-1)) 
    fa_clasico <- as.data.frame(fa_clasico)
    rownames(fa_clasico) <- rownames(fa_marginal)[-nrow(fa_marginal)]
    colnames(fa_clasico) <- colnames(fa_marginal)[-ncol(fa_marginal)]
    }    
        
    # Cociente Clasico
    cociente_clasico <- cociente_marginal[-nrow( fa_marginal), - ncol( fa_marginal)]    
    if(is.null(dim(cociente_clasico))) {
      
      dim(cociente_clasico) <- c((nrow(fa_marginal)-1), (ncol(fa_marginal)-1)) 
      cociente_clasico <- as.data.frame(cociente_clasico)
      rownames(cociente_clasico) <- rownames(fa_marginal)[-nrow(fa_marginal)]
      colnames(cociente_clasico) <- colnames(fa_marginal)[-ncol(fa_marginal)]
    }   
        
    # FR Clasico
    fr_clasico <- fr_marginal[-nrow( fa_marginal), - ncol( fa_marginal)] 
    if(is.null(dim(fr_clasico))) {
      
      dim(fr_clasico) <- c((nrow(fa_marginal)-1), (ncol(fa_marginal)-1)) 
      fr_clasico <- as.data.frame(fr_clasico)
      rownames(fr_clasico) <- rownames(fa_marginal)[-nrow(fa_marginal)]
      colnames(fr_clasico) <- colnames(fa_marginal)[-ncol(fa_marginal)]
    }  
    
    # %
    porcentaje_clasico <- porcentaje_marginal[-nrow( fa_marginal), - ncol( fa_marginal)] 
    if(is.null(dim(porcentaje_clasico))) {
      
      dim(porcentaje_clasico) <- c((nrow(fa_marginal)-1), (ncol(fa_marginal)-1)) 
      porcentaje_clasico <- as.data.frame(porcentaje_clasico)
      rownames(porcentaje_clasico) <- rownames(fa_marginal)[-nrow(fa_marginal)]
      colnames(porcentaje_clasico) <- colnames(fa_marginal)[-ncol(fa_marginal)]
    }  

    # %%
    porcentaje2_clasico <- porcentaje2_marginal[-nrow( fa_marginal), - ncol( fa_marginal)] 
    if(is.null(dim(porcentaje2_clasico))) {
      
      dim(porcentaje2_clasico) <- c((nrow(fa_marginal)-1), (ncol(fa_marginal)-1)) 
      porcentaje2_clasico <- as.data.frame(porcentaje2_clasico)
      rownames(porcentaje_clasico) <- rownames(fa_marginal)[-nrow(fa_marginal)]
      colnames(porcentaje_clasico) <- colnames(fa_marginal)[-ncol(fa_marginal)]
    }  
    
    
    ###  
    } # Clasico
    ##################################

  
  ###
  } # Fin Resolucion
  ##################################################################
  
  
  # # # Objetos Intermedios
  {
  ###  
  
    input_columnas <- c(2,5)  
    
    # Una funcion propia...
    # Para convertir en "character" algunas columnas de varias tablas en una lista
    char_machine <- function(input_lista = NULL, input_columnas = NULL) {
    
    for (k in 1:length(input_columnas)) {
      for (n in 1:ncol(input_lista[[input_columnas[k]]])) {
        
        input_lista[[input_columnas[k]]][,n] <- as.character(input_lista[[input_columnas[k]]][,n])
        
      } # Fin for n
    } # Fin for k
    
        
    return(input_lista)
    } 
      
  
    
  
    
    # Todo lo CLASICO 
    {
    ###
      
    CLASICO <- list()
    CLASICO[[1]] <- as.data.frame(fa_clasico)
    CLASICO[[2]] <- as.data.frame(cociente_clasico)
    CLASICO[[3]] <- as.data.frame(fr_clasico)
    CLASICO[[4]] <- as.data.frame(porcentaje_clasico)
    CLASICO[[5]] <- as.data.frame(porcentaje2_clasico)
    names(CLASICO) <- c("FA", "Cociente", "FR", "%", "%%")
    CLASICO <- char_machine(CLASICO, input_columnas)
  
    ###
    } # Fin Todo lo CLASICO
    ############################################################################
    
    
    # Todo al TOTAL
    {
    ###
      
    TOTAL <- list()
    TOTAL[[1]] <- as.data.frame(fa_marginal)
    TOTAL[[2]] <- as.data.frame(cociente_marginal)
    TOTAL[[3]] <- as.data.frame(fr_marginal)
    TOTAL[[4]] <- as.data.frame(porcentaje_marginal)
    TOTAL[[5]] <- as.data.frame(porcentaje2_marginal)
    names(TOTAL) <- c("FA", "Cociente", "FR", "%", "%%")
    TOTAL <- char_machine(TOTAL, input_columnas)
 
    ###
    } # Todo al TOTAL
    ############################################################################
    
    
    
    # Todo por FILAS
    {
    ###
      
    
    FILAS <- list()
    FILAS[[1]] <- as.data.frame(fa_filas)
    FILAS[[2]] <- as.data.frame(cociente_filas)
    FILAS[[3]] <- as.data.frame(fr_filas)
    FILAS[[4]] <- as.data.frame(porcentaje_filas)
    FILAS[[5]] <- as.data.frame(porcentaje2_filas)
    names(FILAS) <- c("FA", "Cociente", "FR", "%", "%%")
    FILAS <- char_machine(FILAS, input_columnas)
    
    ###
    } # Todo por filas
    ############################################################################
    
    
    
    # Todo por COLUMNAS
    {
    ###
      
    COLUMNAS <- list()
    COLUMNAS[[1]] <- as.data.frame(fa_columnas)
    COLUMNAS[[2]] <- as.data.frame(cociente_columnas)
    COLUMNAS[[3]] <- as.data.frame(fr_columnas)
    COLUMNAS[[4]] <- as.data.frame(porcentaje_columnas)
    COLUMNAS[[5]] <- as.data.frame(porcentaje2_columnas)
    names(COLUMNAS) <- c("FA", "Cociente", "FR", "%", "%%")    
    COLUMNAS <- char_machine(COLUMNAS, input_columnas)
    
    ###
    } # Fin Todo por COLUMNAS
    ############################################################################
    
    
    # SIMPLE
    {
    ###
      
    grupos_filas <- rownames(fa)
    grupos_columnas <- colnames(fa)
    cantidad_filas <- length(grupos_filas)*length(grupos_columnas)
    nombres_columnas <- c(colnames(input_base), "Frecuencia Absoluta", "Total", "Cociente", "Frecuencia Relativa", "%", "%%")
    
    
    SIMPLE <- as.data.frame(matrix(NA, cantidad_filas, length(nombres_columnas)))
    colnames(SIMPLE) <- nombres_columnas
    
    contador_externo <- 0
    for (k in 1:length(grupos_filas)) {
      for (n in 1:length(grupos_columnas)) {
        contador_externo <- contador_externo + 1
        
        SIMPLE[contador_externo,1] <- grupos_filas[k]
        SIMPLE[contador_externo,2] <- grupos_columnas[n]        
        SIMPLE[contador_externo,3] <- TOTAL[[1]][k,n]    
        SIMPLE[contador_externo,4] <- sum(TOTAL[[1]]) 
        SIMPLE[contador_externo,5] <- TOTAL[[2]][k,n]       
        SIMPLE[contador_externo,6] <- TOTAL[[3]][k,n]       
        SIMPLE[contador_externo,7] <- TOTAL[[4]][k,n]       
        SIMPLE[contador_externo,8] <- TOTAL[[5]][k,n] 
        
      } # Fin for n
    } # Fin for k
    
    #SIMPLE <- list(SIMPLE)  
    
    ###
    } # Fin SIMPLE
    ############################################################################
    
    
    
    # SIMPLE2
    {
      ###
      SIMPLE2 <- SIMPLE[,-7]
      colnames(SIMPLE2)[ncol(SIMPLE2)] <- "%"
      SIMPLE2[,3] <- as.character(SIMPLE2[,3])
      SIMPLE2[,4] <- as.character(SIMPLE2[,4])
      ###
    } # Fin SIMPLE
    ############################################################################
    
  ###
  } # Fin Objetos Intermedios
  ##############################################################################
  
  
  # # # Mis Tablas y cambios "NO DATA" y "Errores"
  {
  ###
    
    mis_tablas <- list(CLASICO, TOTAL, FILAS, COLUMNAS, SIMPLE, SIMPLE2)
    names(mis_tablas) <- c("Clasico", "Al total", "Por filas", "Por columnas", "Simple Entrada", "Simple2")  
    
    
   
    
  ###  
  } # Fin Mis Tablas
  ########################################################
  
  
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
      for (n in 1:length(mis_tablas)) for (k in 1:length(mis_tablas[[n]])) {
        
        esta_tabla <- mis_tablas[[n]][[k]]
        
        estas_dimensiones <- dim(esta_tabla)
        tabla_no_data <- as.data.frame(matrix(cambio_aplicado, estas_dimensiones[1], estas_dimensiones[2]))
        colnames(tabla_no_data) <- colnames(esta_tabla)
        esta_tabla <- tabla_no_data
        
        mis_tablas[[n]][[k]]  <- esta_tabla
        
      } # Fin for n
      
    } # Fin si no es valido trabajar
    
  ###   
  } # Fin Cambios "NO DATA" o "Errores"
  ##################################################
  
  
  # # # Objetos Finales
  {
  ###
    
    referencias <- list()
    referencias[[1]] <- paste0("En filas: ", colnames(mini)[1])
    referencias[[2]] <- paste0("En columnas: ", colnames(mini)[2])
    
  ###  
  } # Fin Objetos Finales
  ###############################
  
  
  # # # Salida
  {
  ###
    
    salida <- list(mis_tablas, referencias, output_cadena, input_originales)
    names(salida) <- c("df02", "referencias", "output_cadena", "input_originales")
    
    return(salida)
    
    
  ###  
  } # Fin Salida
  ##################

    

  
  
  
} # Fin function



if (2 == 1) {
  
  # # Ejemplo
  BASE <- mtcars[,c(2,8)]
  input_base <- BASE
  input_decimales <- 2
  input_marginal <- TRUE
  input_talk <- FALSE
  
AVER <-   df02(BASE,"&")
AVER$df02$Clasico$FA


AVER2 <-   df02(BASE,2)
AVER2$df02

}