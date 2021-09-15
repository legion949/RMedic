DF01 <- function(input_base=NULL, input_decimales= 2) {
  
  
  # input_base <- mtcars[2]
  # input_decimales <- 2
  
  
  # Tomamos la columna... y le hacemos un na.omit()  
  nombre_variable <- colnames(input_base)
  MINI <- input_base
  MINI <- na.omit(MINI)
  
  mentime <- FALSE
  
  # Si no hay datos, le metemos unos datos de R...
  if (sum(dim(MINI)) == 1) {
    
    mentime <- TRUE
    # MINI <- TIJERA(mtcars, 2)
    MINI <- mtcars[2]
  } 
  
  # Frecuencais Absolutas
  FA <- table(MINI)
  N_TOTAL <- sum(FA)
  GRUPOS <- as.character(names(FA))
  
  salida_N_TOTAL <- rep(N_TOTAL, length(FA))
  
  COCIENTE <- paste0(FA, "/", N_TOTAL)
  
  # Frecuencias Relativas
  FR <- round2((FA/N_TOTAL), input_decimales)
  
  # Porcentajes
  PORCENTAJE <- FR*100
  PORCENTAJE <- paste(PORCENTAJE, "%", sep="")
  
  # Frec. Abs. Acumuladas
  FAA <- cumsum(FA)
  
  # Frec. Rel. Acumuladas
  FRA <- cumsum(FR)
  
  # Porcentajes Acumulados
  PA <- FRA*100
  PA <- paste(PA, "%", sep="")
  
  # Tabla de Distribucion de Frecuencias
  TABLA <- cbind(GRUPOS, FA, salida_N_TOTAL, COCIENTE, FR,  PORCENTAJE, FAA, FRA, PA)
  TABLA <- as.data.frame(TABLA)
  nombres_tabla <- c(nombre_variable, "FA", "Total", "Cociente", "FR", "%", "FAA", "FRA", "%A")
  colnames(TABLA) <- nombres_tabla
  
  numericas <- c(5,8)
  for(n in 1:length(numericas)) TABLA[,numericas[n]] <- as.numeric(as.character(TABLA[,numericas[n]])) 
  
  # Fletamos las frecuencias acumuladas
  TABLA <- TABLA[,c(1:6)]
  
  if (mentime == TRUE) {
    
    estas_dimensiones <- dim(TABLA)
    TABLA2 <- as.data.frame(matrix("Sin datos en la Columna", estas_dimensiones[1], estas_dimensiones[2]))
    colnames(TABLA2) <- colnames(TABLA)
    TABLA <- TABLA2
    
  }
  
  ################################
  
  p <- FR
  q <- rep(NA, length(p))
  for (n in 1:length(q)) q[n] <- sum(p[-n])
  n <- N_TOTAL  
  
  Z <- qnorm(1-(0.05/2))
  Z <- round2(Z, input_decimales)
  
  desvio <- Z*sqrt((p*q)/n)
  desvio <- round2(desvio, input_decimales)
  
  LI <- p - desvio
  LS <- p + desvio
  
  dt_li <- LI < 0
  dt_ls <- LI > 1
  
  if (sum(dt_li) > 0) LI[dt_li] <- 0
  if (sum(dt_ls) > 0) LS[dt_ls] <- 1
  
    
  LI <- paste0((LI*100), "%")
  LS <- paste0((LS*100), "%")
  
  
  TABLA_02 <- cbind(GRUPOS, PORCENTAJE, LI, LS)
  colnames(TABLA_02) <- c(nombre_variable, "%", "LI95%", "LS95%")
  
  
  if (mentime == TRUE) {
    
    estas_dimensiones <- dim(TABLA_02)
    TABLA2 <- as.data.frame(matrix("Sin datos en la Columna", estas_dimensiones[1], estas_dimensiones[2]))
    colnames(TABLA2) <- colnames(TABLA_02)
    TABLA_02 <- TABLA2
    
  }
  
  
  #################################  
  
  
  salida <- list(TABLA, TABLA_02)
  
  return(salida)
  
  # Fin candado_DF01 == FALSE
  
  
  
  
} # Fin function
# 
if (1 == 2) {
  # Ejemplo
  input_base <- carga_xls("../005Control/CONTROL TOTAL CATEGORICO.xlsx")  
  input_decimales <- 2
  input_base <- TIJERA(input_base, 7)
  SALIDA <- DF01(input_base,3)
}

