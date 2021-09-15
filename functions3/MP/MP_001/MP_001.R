MP <- function(input_base=NULL, input_decimales= 2, input_impuesto="Numérica", input_talk=FALSE, input_save=FALSE) {
  

    

  nombres_elementos <- c("Variable", "Mín", "Media", "Mediana", "Máx", "n")
  
  RES1 <- as.data.frame(matrix(NA,1, length(nombres_elementos)))
  colnames(RES1) <- nombres_elementos
  
  Variables <- colnames(input_base)

  RES2 <- t(RES1)
  
  # Para cada columna del input... generamos una fila con estimaciones
  # de medidas de dispersion... y le agregamos en "n".

  
  mentime <- FALSE
  
  # Tomamos la columna... y le hacemos un na.omit()  
  Variable <- colnames(input_base)
  MINI <- na.omit(input_base)
  estas_dimensiones <- dim(MINI)

  if (input_save == TRUE)   MINI_SAVE <- MINI

  MINI <- as.vector(as.matrix(MINI))

  
  dt_verdad <- FALSE
  if (length(estas_dimensiones) == 2)     dt_verdad <- is.numeric(as.vector(as.matrix(MINI)))

  
  
  # Si no hay datos, le metemos unos datos de R...
  if (sum(estas_dimensiones) == 1) {
    
    mentime <- TRUE
    MINI <- TIJERA(mtcars, 2)
    dt_verdad <- TRUE
    if (input_save == TRUE)   {
      MINI_SAVE <- MINI
      colnames(MINI_SAVE) <- colnames(input_base)
      MINI_SAVE[,1] <- rep(NA, nrow(MINI))
    }
  } 
  

    # Si tiene al menos un dato... y es numerica
    if (mentime == FALSE) {
    
      if (dt_verdad == TRUE) {  
      # Minimo
      MIN <- min(MINI)
      MIN <- round2(MIN, input_decimales)
      
      # Media
      MEDIA <- mean(MINI)
      MEDIA <- round2(MEDIA, input_decimales)
      
      # Mediana
      MEDIANA <- median(MINI)
      MEDIANA <- round2(MEDIANA, input_decimales)
      
      
      # Maximo
      MAX <- max(MINI)
      MAX <- round2(MAX, input_decimales)
      
      # Cantidad de Datos
      N <- length(MINI)
      
      
      RES1[,1] <- Variables
      RES1[,2] <- MIN
      RES1[,3] <- MEDIA
      RES1[,4] <- MEDIANA
      RES1[,5] <- MAX
      RES1[,6] <- N
      
      
      # Resultado Tipo2
      RES2 <- data.frame(rbind(MIN, MEDIA, MEDIANA, MAX, N))
      rownames(RES2) <- nombres_elementos[c(2:length(nombres_elementos))]
      colnames(RES2) <- Variables
      
      
      } # Si es numerica la columna seleccionada
      
      if (dt_verdad == FALSE) {
        
        if (input_talk == TRUE) {
        cat("***Aviso: Variable ''", Variables, "'' NO es numerica***", "\n", sep="")
        } # Fin if (input_talk == TRUE)
      } # Fin !is.numeric()
      
      
    } # Fin mentime == FALSE

  
  

  if(mentime == TRUE | dt_verdad == FALSE) {
    
    texto_salida <- "Error"
    if (mentime == TRUE) texto_salida <- "Sin datos en la Columna" 
    if (mentime == FALSE && dt_verdad == FALSE) texto_salida <- "La variable NO es numérica. Realice CONTROL" 
    
        
    estas_dim <- dim(RES1)
    RES1_CAMBIO <- matrix(texto_salida, estas_dim[1], estas_dim[2])
    RES1_CAMBIO <- as.data.frame(RES1_CAMBIO)
    colnames(RES1_CAMBIO) <- colnames(RES1)
    RES1 <- RES1_CAMBIO
    
    estas_dim <- dim(RES2)
    RES2_CAMBIO <- matrix(texto_salida, estas_dim[1], estas_dim[2])
    RES2_CAMBIO <- as.data.frame(RES2_CAMBIO)
    colnames(RES2_CAMBIO) <- colnames(RES2)
    RES2 <- RES2_CAMBIO
  }
  
  RES1[,1] <- as.character(RES1[,1])
  
  if (input_save == FALSE) {
  RESULTADO <- list()
  RESULTADO[[1]] <- RES1
  RESULTADO[[2]] <- RES2
  }
  
  if (input_save == TRUE) {
    RESULTADO <- list()
    
 
    
    RESULTADO[[1]] <- RES1
    RESULTADO[[2]] <- RES2
    RESULTADO[[3]] <- MINI_SAVE
  }
  
    
  return(RESULTADO)
  
  
  
} # Fin function MP
##################################################################################################################





if (1 == 2) {
  
  BASE <- carga_xls("../005Control/CONTROL TOTAL NUMERICO.xlsx")
  input_base <- TIJERA(BASE, 8)
  # Ejemplo...
  SALE <-     MP(input_base, 4, TRUE, input_save = T)
  colnames(SALE[[3]])
  
  AVER
  
}


