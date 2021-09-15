DF02 <- function(input_base=NULL, input_decimales= 2, input_marginal=FALSE) {
  

   
    # Tomamos la columna... y le hacemos un na.omit()  
    Variables <- colnames(input_base)
    MINI <- input_base
    MINI <- na.omit(MINI)
    
    mentime <- FALSE
    
    # Si no hay datos, le metemos unos datos de R...
    if (length(dim(MINI)) == 2 && dim(MINI)[1] == 0) {
      
      mentime <- TRUE
      MINI <- mtcars[, c(8,2)]
      MINI[,1] <- as.character(MINI[,1])
      MINI[,2] <- as.character(MINI[,2])
      colnames(MINI) <- colnames(input_base)
    } 
    
  
    # MINI <-mtcars[c(2,8)]
    # MINI[,1] <- as.character(MINI[,1])
    # MINI[,2] <- as.character(MINI[,2])
    # input_decimales <- 2
      
    # Frecuencais Absolutas
    FA <- table(MINI)
    N_TOTAL <- sum(FA)
    FA_interno <- FA
    m_col <- colSums(FA)
    m_row <- rowSums(FA)
    FA <- cbind(FA, m_row)
    FA <- rbind(FA, c(m_col, N_TOTAL))
    colnames(FA)[ncol(FA)] <- c("Total por Filas")
    rownames(FA)[nrow(FA)] <- c("Total por Columnas")
    
    # Cociente al Total
    COCIENTE <- FA
    for(n in 1:ncol(COCIENTE)) COCIENTE[,n] <- paste0(COCIENTE[,n], "/", N_TOTAL)
    
    
    # Frecuencias Relativas al Total
    FR <- FA_interno/N_TOTAL
    FR <- round2(FR, input_decimales)
    m_col2 <- colSums(FR)
    m_row2 <- rowSums(FR)
    totales_raros <- c(sum(m_col2), sum(m_row2))
    diferencia <- abs(totales_raros - 1)
    dt_diferencia <- max(diferencia) == diferencia
    orden_diferencia <- c(1,2)[dt_diferencia]
    if (length(orden_diferencia) == 2) orden_diferencia <- orden_diferencia[1]
    
    FR <- cbind(FR, m_row2)
    FR <- rbind(FR, c(m_col2, totales_raros[orden_diferencia]))
    colnames(FR)[ncol(FR)] <- c("Total por Filas")
    rownames(FR)[nrow(FR)] <- c("Total por Columnas")
    FR_interno <- FR
    if (totales_raros[orden_diferencia] != 1) FR[nrow(FR), ncol(FR)] <- paste0(FR[nrow(FR), ncol(FR)], "(Redondeo Incorrecto)")  
        
    # Porcentajes al Total
    PORCENTAJE <- FR_interno*100
    for(n in 1:ncol(PORCENTAJE)) for (k in 1:nrow(PORCENTAJE))
    PORCENTAJE[k,n] <- paste(PORCENTAJE[k,n], "%", sep="")
    PORCENTAJE_interno <- PORCENTAJE
    if (totales_raros[orden_diferencia] != 1) PORCENTAJE[nrow(FR), ncol(FR)] <- paste0(PORCENTAJE[nrow(FR), ncol(FR)], "(Redondeo Incorrecto)")  
    

    # Cociente por filas
    COCIENTE_F <- FA[-nrow(FA), -ncol(FA)]
    for (n in 1:length(m_row))      COCIENTE_F[n,] <- paste0(COCIENTE_F[n,], "/", m_row[n])
    COCIENTE_F <- cbind(COCIENTE_F, paste0(m_row, "/", m_row))
    colnames(COCIENTE_F)[ncol(COCIENTE_F)] <- "Totales"

    
    
    # Frecuencias Relativas por filas
    FR_F <- FA[-nrow(FA), -ncol(FA)]
    for (n in 1:length(m_row))      FR_F[n,] <- FR_F[n,]/m_row[n]
    FR_F <- round2(FR_F, input_decimales)
    sumas_filas <- rowSums(FR_F)  
    dt_filas3 <- sumas_filas != 1
    FR_F <- cbind(FR_F, sumas_filas)
    colnames(FR_F)[ncol(FR_F)] <- "Totales"
    FR_F_interno <- FR_F
    if (sum(dt_filas3) > 0) FR_F[dt_filas3,ncol(FR_F)] <- paste0(sumas_filas[dt_filas3], "***(Debe usar más decimales para redondear)")
    

    # Porcentajes por filas
    PORCENTAJE_F <- FR_F_interno*100
    for(n in 1:ncol(PORCENTAJE_F))  PORCENTAJE_F[,n] <- paste(PORCENTAJE_F[,n], "%", sep="")
    PORCENTAJE_F_interno <- PORCENTAJE_F
    if (sum(dt_filas3) > 0) PORCENTAJE_F[dt_filas3,ncol(FR_F)] <- paste0(sumas_filas[dt_filas3], "***(Debe usar más decimales para redondear)")
    
    
################    
    
    
    # Cociente por columnas
    COCIENTE_C <- FA[-nrow(FA), -ncol(FA)]
    for (n in 1:length(m_col))      COCIENTE_C[,n] <- paste0(COCIENTE_C[,n], "/", m_col[n])
    COCIENTE_C <- rbind(COCIENTE_C, paste0(m_col, "/", m_col))
    rownames(COCIENTE_C)[nrow(COCIENTE_C)] <- "Totales"

    
    
    # Frecuencias Relativas por columnas
    FR_C <- FA[-nrow(FA), -ncol(FA)]
    for (n in 1:length(m_col))      FR_C[,n] <- FR_C[,n]/m_col[n]
    FR_C <- round2(FR_C, input_decimales)
    sumas_columnas <- colSums(FR_C)  
    dt_columnas4 <- sumas_columnas != 1
    FR_C <- rbind(FR_C, sumas_columnas)
    rownames(FR_C)[nrow(FR_C)] <- "Totales"
    FR_C_interno <- FR_C
    if (sum(dt_columnas4) > 0) FR_C[nrow(FR_C), dt_columnas4] <- paste0(sumas_columnas[dt_columnas4], "***(Debe usar más decimales para redondear)")
    
    
    # Porcentajes por Columnas
    PORCENTAJE_C <- FR_C_interno*100
    for(n in 1:nrow(PORCENTAJE_C))  PORCENTAJE_C[n,] <- paste(PORCENTAJE_C[n,], "%", sep="")
    PORCENTAJE_C_interno <- PORCENTAJE_C
    if (sum(dt_columnas4) > 0) FR_C[nrow(FR_C), dt_columnas4] <- paste0(sumas_columnas[dt_columnas4], "***(Debe usar más decimales para redondear)")
    
    
##############################################################################################
    
    
    CLASICO <- list()
    CLASICO[[1]] <- FA[-nrow(FA), - ncol(FA)]
    CLASICO[[2]] <- COCIENTE[-nrow(COCIENTE), - ncol(COCIENTE)]
    CLASICO[[3]] <- FR[-nrow(FR), - ncol(FR)]
    CLASICO[[4]] <- PORCENTAJE[-nrow(PORCENTAJE), - ncol(PORCENTAJE)]
    
    
    TOTAL <- list()
    TOTAL[[1]] <- FA
    TOTAL[[2]] <- COCIENTE
    TOTAL[[3]] <- FR
    TOTAL[[4]] <- PORCENTAJE

    FILAS <- list()
    FILAS[[1]] <- FA[-nrow(FA),]
    FILAS[[2]] <- COCIENTE_F
    FILAS[[3]] <- FR_F
    FILAS[[4]] <- PORCENTAJE_F
    
    COLUMNAS <- list()
    COLUMNAS[[1]] <- FA[,-ncol(FA)]
    COLUMNAS[[2]] <- COCIENTE_C
    COLUMNAS[[3]] <- FR_C
    COLUMNAS[[4]] <- PORCENTAJE_C
    
    input_base <- MINI
    grupos_filas <- rownames(FA_interno)
    grupos_columnas <- colnames(FA_interno)
    cantidad_filas <- length(grupos_filas)*length(grupos_columnas)
    nombres_columnas <- c(colnames(input_base), "FA", "COCIENTE", "FR", "%")
    
    
    SIMPLE <- as.data.frame(matrix(NA, cantidad_filas, length(nombres_columnas)))
    colnames(SIMPLE) <- nombres_columnas
    
    contador_externo <- 0
    for (k in 1:length(grupos_filas)) {
      for (n in 1:length(grupos_columnas)) {
        contador_externo <- contador_externo + 1
        
        SIMPLE[contador_externo,1] <- grupos_filas[k]
        SIMPLE[contador_externo,2] <- grupos_columnas[n]        
        SIMPLE[contador_externo,3] <- as.character(TOTAL[[1]][k,n])        
        SIMPLE[contador_externo,4] <- as.character(TOTAL[[2]][k,n])       
        SIMPLE[contador_externo,5] <- TOTAL[[3]][k,n]       
        SIMPLE[contador_externo,6] <- as.character(TOTAL[[4]][k,n])       
        
        
      } # Fin for n
    } # Fin for k
    
    SIMPLE <- list(SIMPLE)  

  # Juntamos todo...
    RESULTADOS <- list(CLASICO, TOTAL, FILAS, COLUMNAS, SIMPLE)
    

   
   if (mentime == TRUE) {
     
     # Para total...
     for (j in 1:length(RESULTADOS)) {
     for (n in 1:length(RESULTADOS[[j]])) {
           
    
       dimensiones <- dim(RESULTADOS[[j]][[n]])
       CAMBIASO <- matrix("Sin pares de Datos en las columnas", dimensiones[1], dimensiones[2])
       colnames(CAMBIASO) <- rep("Sin Datos", dimensiones[2])
       rownames(CAMBIASO) <- rep("Sin Datos", dimensiones[1])
       RESULTADOS[[j]][[n]] <- CAMBIASO
       
} # Fin for n
} # Fin for j
     
   } #fin if mentime == TRUE
   
    
    
   return(RESULTADOS)
    

  
  
  
} # Fin function



if (2 == 1) {
  
  # # Ejemplo
  BASE <- mtcars[,c(2,8)]
  input_base <- BASE
  input_decimales <- 2
  input_marginal <- TRUE
  input_talk <- FALSE
  
AVER <-   DF02(BASE,2, FALSE, TRUE)
AVER$SIMPLE

AVER2 <-   DF02(BASE,2, TRUE, TRUE)
AVER2

}