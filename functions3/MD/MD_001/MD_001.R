MD <- function(input_base=NULL, input_decimales= 2, impuesto="Numérica", input_talk=FALSE) {
  

 
nombres_elementos <- c("Variable", "Varianza", "D.E.", "E.E.", "C.V.", "n")

RES1 <- as.data.frame(matrix(NA,1, length(nombres_elementos)))
colnames(RES1) <- nombres_elementos

Variables <- colnames(input_base)


RES2 <- t(RES1)


mentime <- FALSE

# Tomamos la columna... y le hacemos un na.omit()  
Variable <- colnames(input_base)
MINI <- na.omit(input_base)
estas_dimensiones <- dim(MINI)
MINI <- as.vector(as.matrix(MINI))


dt_verdad <- FALSE
if (length(estas_dimensiones) == 2)     dt_verdad <- is.numeric(as.vector(as.matrix(MINI)))



# Si no hay datos, le metemos unos datos de R...
if (sum(estas_dimensiones) == 1) {
  
  mentime <- TRUE
  MINI <- TIJERA(mtcars, 2)
  dt_verdad <- TRUE
} 


# Si tiene al menos un dato... y es numerica
if (mentime == FALSE) {
  
  if (dt_verdad == TRUE) {  
    
    VAR <- var(MINI)
    VAR <- round2(VAR, input_decimales)
    
    N <- length(MINI)
    # Primero sacamos el desvio...
    # Sin redondear el desvio, lo usamos para sacar el error estandard...
    # Y luego redondeamos los dos.
    # Esto es para sacar mejor al EE... por que sino sacas el DE... lo redondeas...
    # lo usas para sacar el EE y lo volves a redondear.
    DE <- sd(MINI)
    EE <- DE/sqrt(N)
    
    MEDIA <- mean(MINI)
    
    CV <- DE/MEDIA
    
    DE <- round2(DE, input_decimales)
    EE <- round2(EE, input_decimales)
    CV <- round2(CV, input_decimales)
    # RES1
    RES1[,1] <- Variables
    RES1[,2] <- VAR
    RES1[,3] <- DE
    RES1[,4] <- EE
    RES1[,5] <- CV
    RES1[,6] <- N
    
    
    
    # Resultado Tipo2
    RES2 <- data.frame(rbind(VAR, DE, EE, CV, N))
    rownames(RES2) <- nombres_elementos[c(2:length(nombres_elementos))]
    colnames(RES2) <- Variables
    
    
    } # Fin dt_verdad == TRUE
    
    if (dt_verdad == FALSE) {
      
      
      if(input_talk == TRUE) {
      cat("***Aviso: Variable ''", Variables, "'' NO es numerica***", "\n", sep="")
      } # Fin if input_talk == TRUE
    } # Fin dt_verdad == FALSE
  }


# Si es vacio o caracter
if(mentime == TRUE | dt_verdad == FALSE) {
  
  texto_salida <- "Error"
  if (mentime == TRUE) texto_salida <- "Sin datos en la Columna" 
  if (mentime == FALSE && dt_verdad == FALSE && impuesto == "Numérica") texto_salida <- "La variable NO es numérica. Realice CONTROL" 
  
  
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
} # Fin doble if

RESULTADO <- list()
RESULTADO[[1]] <- as.data.frame(RES1)
RESULTADO[[2]] <- as.data.frame(RES2)
    

RESULTADO[[1]][,1] <-as.character(RESULTADO[[1]][,1])


return(RESULTADO)

} # Fin function

if (1 == 2) {

BASE <- TIJERA(mtcars, 2)
# Ejemplo...
AVER <-     MD(BASE, 4, TRUE)

AVER

}