CHI_MASTER <- function(input_datos= NULL, input_decimales=2, input_alfa=0.05, input_sep="R") {
  
  
  
  # Inicio
  CHI <- list()
  
  # Ordenamiento General
  CHI$input <- list()           # Ingresan solo los elementos que son input de la funcion
  CHI$DETALLES <- list()        # Nombre de las variables...
  CHI$CONTROLES <- list()       # Detalle de los diferentes controles por los que paso la base de datos a trabajar...
  CHI$MINI <- list()            # Va la MINIBASE de trabajo, y quien es VR y quien es FACTOR
  CHI$ANALISIS <- list()        # Los analisis... un apartado sera "$ORIGINAL"... y otro será "$CONSULTORA"
  CHI$SCRIPT <- list()          # Un apartado sera el script para obtener exactamente lo mismo que la salida cruda de R.
  CHI$SALIDA_ARMADA <- list()   # La salida, que son objetos que ingresaran al Shiny...
  CHI$CONTROL_GENERAL <- list() # Guardamos... para saber... si se cumplio o no con todos los controles...
  
  
  
  
  # # # INPUT
  {
  
  # Esta informacion es inalterable... 
  # O sea... se pueda o no realizar el analisis... esta informacion debe estar...
  
  CHI$input$datos <- input_datos
  CHI$input$decimales <- input_decimales
  CHI$input$alfa <- input_alfa
  
  
  } # FIN INPUT
  ############################################################################
  
  
  
  # # # DETALLES
  {
    
    # Esta informacion es inalterable... 
    # O sea... se pueda o no realizar el analisis... esta informacion debe estar...
    
    CHI$DETALLES$nombre_VC1 <- colnames(input_datos)[1]
    CHI$DETALLES$nombre_VC2 <- colnames(input_datos)[2]

    CHI$DETALLES$aclaracion <- paste("En filas: ", colnames(input_datos)[1], "\n", 
                                     "En columnas: ", colnames(input_datos)[2], "\n", sep="")
    
    
  } # FIN DETALLES
  ############################################################################
  
  
  
  # # # CONTROLES
  {
    # Hay diferentes controles que hacer...
    # Cada control, afectara a las salidas estadisticas reemplazando los objetos
    # por carteles de aviso del error.
    
    # Se respetara el tipo de objeto...
    # Entonces... se esperaba obtener una tabla de datos... saldra una tabla con las mismas
    # dimensiones, pero con carteles de aviso de error.
    
    
    
    
    control_general <- TRUE
    
    control_OK <- list()
    
    frases_control <- list()
    frases_control[[1]] <- c("El objeto 'input_datos' debe ser un data.frame o una matriz. Ingrese un formato de datos correcto.")
    frases_control[[2]] <- c("El objeto 'input_datos' debe tener 2 columnas. Ingrese un formato de datos correcto.")
    frases_control[[3]] <- c("El objeto 'input_datos' debe tener al menos 2 filas. No puede realizarse el test Chi Cuadrado.")
    frases_control[[4]] <- c("Al realizar na.omit() sobre la base de datos, quedan menos de 2 filas con informacion. No puede realizarse el test Chi Cuadrado.")
    frases_control[[5]] <- c("La columna categórica1 ingresada debe tener al menos 2 niveles. No puede realizarse el test Chi Cuadrado.")
    frases_control[[6]] <- c("La columna categórica2 ingresada debe tener al menos 2 niveles. No puede realizarse el test Chi Cuadrado.")
    
    
    # Control 1...
    # El objeto input_datos debe ser un data.frame o una matrix.
    control_OK[[1]] <- FALSE
    names(control_OK[[1]]) <- c("data.frame o matrix")
    if (is.data.frame(input_datos) | is.matrix(input_datos)) control_OK[[1]] <- TRUE else control_general <- FALSE 
    
    
    # Control 2...
    # El objeto input_datos debe tener 2 columna.
    control_OK[[2]] <- FALSE
    names(control_OK[[2]]) <- c("Solo 2 columnas")
    if (control_general == TRUE) {
      if (ncol(input_datos) == 2) control_OK[[2]] <- TRUE else control_general <- FALSE
    }
    
    
    
    
    # Control 3...
    # El objeto input_datos debe tener al menos 2 fila.
    control_OK[[3]] <- FALSE
    names(control_OK[[3]]) <- c("Al menos 2 filas de input_datos")
    if (control_general == TRUE) {
      if (nrow(input_datos) >= 2) control_OK[[3]] <- TRUE else control_general <- FALSE 
    }
    
    
    
    # Control 4...
    # El objeto "MINI" (obtenido con na.omit(input_datos)) debe tener al menos dos filas
    control_OK[[4]] <- FALSE
    names(control_OK[[4]]) <- c("Al menos 2 filas de MINI")
    if (control_general == TRUE) {
      
      MINI_CONTROL <- input_datos
      MINI_CONTROL <- na.omit(MINI_CONTROL)
      
      if (nrow(MINI_CONTROL) >= 2) control_OK[[4]] <- TRUE else control_general <- FALSE 
    }
    
    
    
    # Control 5...
    # La columna 1, que sera VC, con dos niveles
    # Como podría ingresar una columna de "0" y "1"... no pedimos que sea un factor o una variable categorica...
    # Sino que la transformamos nosotros en factor, y vemos si tiene 2 niveles.
    control_OK[[5]] <- FALSE
    names(control_OK[[5]]) <- c("VC1 con al menos dos niveles")
    if (control_general == TRUE) {
      VC_CONTROL <- MINI_CONTROL[,1]
      VC_CONTROL <- as.factor(as.character(VC_CONTROL))
      
      niveles <- levels(VC_CONTROL)
      if (length(niveles) >= 2) control_OK[[5]] <- TRUE else control_general <- FALSE
    }
    
    
    
    
    # Control 6...
    # La columna 2, que sera VC2, con dos niveles
    # Como podría ingresar una columna de "0" y "1"... no pedimos que sea un factor o una variable categorica...
    # Sino que la transformamos nosotros en factor, y vemos si tiene 2 niveles.
    control_OK[[6]] <- FALSE
    names(control_OK[[6]]) <- c("VC2 con al menos dos niveles")
    if (control_general == TRUE) {
      VC2_CONTROL <- MINI_CONTROL[,2]
      VC2_CONTROL <- as.factor(as.character(VC2_CONTROL))
      
      niveles <- levels(VC2_CONTROL)
      if (length(niveles) >= 2) control_OK[[6]] <- TRUE else control_general <- FALSE
    }
    
    
    
    
    
    CHI$CONTROLES <- control_OK
    
    CHI$CONTROL_GENERAL <- control_general
    
  } # # # FIN CONTROLES
  ###############################################################################################################################
  
  
  
  # Si se cumplen todos los controles
  if (control_general == TRUE) {
    
    
    # MINI
    {
    MINI <- input_datos
    MINI <- na.omit(MINI)

    for (n in 1:ncol(MINI)) MINI[,n] <- as.factor(as.character(MINI[,n]))
    
    CHI$MINI <- MINI
    } # Fin MINI
    ######################################
    
    

# Por defecto el metodo es el clasico
metodo <- "Clásico"

# Tabla de Doble entrada

TABLAS_DF <- DF02(MINI, input_decimales=input_decimales, input_marginal = FALSE, input_talk = FALSE)
TABLA <- TABLAS_DF$FA


ANALISIS <- list()

# Analisis Original Chi Cuadrado
{
###

ORIGINAL <- list()

ORIGINAL$Test_Chi <- chisq.test(TABLA, simulate.p.value = FALSE)
ORIGINAL$metodo <- metodo

# Frase1 por defecto
frase1 <- c("Todos los valores esperados son mayores o iguala 5. \n Se realiza el test Chi Cuadrado Clásico.")
ORIGINAL$frase1_R <- frase1


# Analisis de los esperados
esperados <- c(ORIGINAL$Test_Chi$expected)
detec <- esperados >= 5
estos <- sum(as.numeric(detec))

if (estos < length(esperados)) {
  
  metodo <- "Montecarlo"
  ORIGINAL$Test_Chi <- chisq.test(TABLA, simulate.p.value = TRUE)
  ORIGINAL$metodo <- metodo
  
  frase1 <- c("Existen valores esperados menores a 5.", "\n", 
              "No se cumplen los requisitos para utilizar el test Chi Cuadrado Clásico.","\n",
              "Se genera el test Chi Cuadrado Exacto con la variante de Montecarlo.")

  
    
  ORIGINAL$frase1_R <- frase1

  
} # Fin if valores esperados
##################################################################

# Anclamos el analisis original
ANALISIS$ORIGINAL <- ORIGINAL

###
} # Fin Analisis Original
###########################################################################


# Tabla de Frecuencias de la funcion DF02()
ANALISIS$TABLAS_DF <- TABLAS_DF


# Analisis de la Consultora
{
  ###
  CONSULTORA <- list()  

  # Valor p interno
  valor_p_interno <- ANALISIS$ORIGINAL$Test_Chi$p.value
  valor_p_interno <- round(valor_p_interno, input_decimales)
  CONSULTORA$valor_p_interno <- valor_p_interno
  
  # Valor p externo
  if (valor_p_interno < 0.01) valor_p_externo <- c("<<0.01") else  valor_p_externo <- valor_p_interno
  CONSULTORA$valor_p_externo <- valor_p_externo
  
  # Desicion
  if (valor_p_interno < input_alfa) decision <- "Rechazo Ho" else  decision <- "NO Rechazo Ho"
  CONSULTORA$decision <- decision
  

# Resumen
{
nombres <- c("Filas", "Columnas", "Método", "Valor Chi", "G.L", "Valor p", "Alfa", "Decisión")  
RESUMEN <- matrix(NA, 1, length(nombres))
colnames(RESUMEN) <- nombres

RESUMEN[1,1] <- colnames(MINI)[1]
RESUMEN[1,2] <- colnames(MINI)[2]
RESUMEN[1,3] <- metodo
RESUMEN[1,4] <- round(ORIGINAL$Test_Chi$statistic, input_decimales)
if (metodo == "Clásico") RESUMEN[1,5] <- round(ORIGINAL$Test_Chi$parameter, input_decimales) else RESUMEN[1,5] <- "No Corresponde"
RESUMEN[1,6] <- valor_p_externo
RESUMEN[1,7] <- input_alfa
RESUMEN[1,8] <- decision

CONSULTORA$RESUMEN <- RESUMEN

} # Fin Resumen
##########################






# frase2_html: segun valor p
{
  
  
  
  frase2_v1 <- c("No pudo obtenerse un valor p.")
  
  frase2_v2 <- c("El valor p es mayor que el valor de alfa=", input_alfa, ".","<br/>",
                 "No se rechaza la Ho.", "<br/>",
                 "Las variables '", CHI$DETALLES$nombre_VC1 , "' y '", CHI$DETALLES$nombre_VC2, "' son estadísticamente independientes.","<br/>", 
                 "No existe una asociación estadísticamente significativa entre las variables.")
  
  
  frase2_v3 <- c("El valor p es igual que el valor de alfa=", input_alfa, ".","<br/>",
                 "No se rechaza la Ho.", "<br/>",
                 "Las variables '", CHI$DETALLES$nombre_VC1 , "' y '", CHI$DETALLES$nombre_VC2, "' son estadísticamente independientes.", "<br/>",
                 "No existe una asociación estadísticamente significativa entre las variables.")
  
  
  frase2_v4 <- paste("El valor p es menor que el valor de alfa=", input_alfa, ".", "<br/>", 
                     "Se rechaza la Ho.", "<br/>",
                     "Las variables '", CHI$DETALLES$nombre_VC1 , "' y '", CHI$DETALLES$nombre_VC2, "' están estadísticamente relacionadas.", "<br/>", 
                     "Existe una asociación estadísticamente significativa entre las variables.", sep="")
  
  if(is.na(valor_p_interno) | is.null(valor_p_interno)) frase_elegida_chi <- frase2_v1 else if (valor_p_interno > input_alfa) frase_elegida_chi <- frase2_v2 else if (valor_p_interno == input_alfa) frase_elegida_chi <- frase2_v3 else if (valor_p_interno < input_alfa) frase_elegida_chi <- frase2_v4
  
  CONSULTORA$FRASE2 <-  frase_elegida_chi
  
} # Fin Frase2_html
#######################################



# Tabla de Tendencias
{
###
  
  
  
  # Tendencias...
  valor_referencia <- qnorm(input_alfa/2, 0, 1)
  valor_referencia <- round(valor_referencia, input_decimales)
  valor_negativo <- valor_referencia
  valor_positivo <- -valor_negativo
  
  CONSULTORA$valores_t <- c(valor_negativo, valor_positivo)
  
  TENDENCIA <- ORIGINAL$Test_Chi$stdres
  
  for(n in 1:ncol(TENDENCIA)) for(k in 1:nrow(TENDENCIA)){
    
    celda <- ORIGINAL$Test_Chi$stdres[k,n]
    
    if (celda < valor_negativo) TENDENCIA[k,n] <- "NEGATIVA" else if (celda > valor_positivo) TENDENCIA[k,n] <- "POSITIVA" else TENDENCIA[k,n] <- "Sin Tendencia" 
  }
  
  
  
  
  estos <- c(TENDENCIA)
  
  estos_neg <- estos == "NEGATIVA"
  estos_neg <- sum(as.numeric(estos_neg))
  
  estos_pos <- estos == "POSITIVA"
  estos_pos <- sum(as.numeric(estos_pos))
  
  

  FRASE_POSITIVOS <- matrix(NA, estos_pos, 1)
  if (estos_pos == 0 ) {
    FRASE_POSITIVOS <- matrix(NA, 1, 1)
    FRASE_POSITIVOS[1,1] <- "Sin Tendencias Positivas entre las variables"
  } 
  
  FRASE_NEGATIVOS <- matrix(NA, estos_neg, 1)
  if (estos_neg == 0 ) {
    FRASE_NEGATIVOS <- matrix(NA, 1, 1)
    FRASE_NEGATIVOS[1,1] <- "Sin Tendencias Negativas entre las variables"
  } 
  

  contador_interno_neg <- 0
  contador_interno_pos <- 0
  
  
  for(n in 1:ncol(TENDENCIA)) for(k in 1:nrow(TENDENCIA)){
    
    
    celda <- TENDENCIA[k,n]
    
    if(estos_neg > 0)  if (celda == "NEGATIVA") if(estos_neg >= contador_interno_neg){
      contador_interno_neg <- contador_interno_neg + 1
      FRASE_NEGATIVOS[contador_interno_neg,1] <- paste("Tendencia Negativa entre '", rownames(TENDENCIA)[k], "' y '", colnames(TENDENCIA)[n], "'.", sep="") 
    } # Fin negativos
    
    if(estos_pos > 0)   if (celda == "POSITIVA") if(estos_neg >= contador_interno_pos){ 
      contador_interno_pos <- contador_interno_pos + 1
      FRASE_POSITIVOS[contador_interno_pos,1] <- paste("Tendencia Positiva entre '", rownames(TENDENCIA)[k], "' y '", colnames(TENDENCIA)[n], "'.", sep="")
    } # Fin positivos
  }
  
  colnames(FRASE_NEGATIVOS) <- c("Tendencias Negativas")
  CONSULTORA$FRASE_NEGATIVOS <- as.matrix(FRASE_NEGATIVOS)
  
  colnames(FRASE_POSITIVOS) <- c("Tendencias Positivos")
  CONSULTORA$FRASE_POSITIVOS <- as.matrix(FRASE_POSITIVOS)
  
  CONSULTORA$TENDENCIA <- TENDENCIA
  
  
###  
} # Fin Tabla de Tendencias
################################

ANALISIS$CONSULTORA <- CONSULTORA

###
} # Fin Analisis Consultora
##########################################


# Colocamos todos los analisis dentro del objeto "CHI"
CHI$ANALISIS <- ANALISIS



# SALIDA ARMADA
{
###

SALIDA_ARMADA <- list()


SALIDA_ARMADA[[1]]  <- c("Test Chi Cuadrado")
SALIDA_ARMADA[[2]]  <- CHI$DETALLES$aclaracion
SALIDA_ARMADA[[4]]  <- TABLA
SALIDA_ARMADA[[5]]  <- ORIGINAL$frase1_R
SALIDA_ARMADA[[6]]  <- RESUMEN
SALIDA_ARMADA[[7]]  <- TENDENCIA
SALIDA_ARMADA[[8]]  <- CONSULTORA$FRASE2
SALIDA_ARMADA[[9]]  <- CONSULTORA$FRASE_NEGATIVOS
SALIDA_ARMADA[[10]] <- CONSULTORA$FRASE_POSITIVOS


CHI$SALIDA_ARMADA <- SALIDA_ARMADA
###
} # SALIDA ARMADA
############################################################


  
  
  } # Fin control_OK == TRUE
####################################################################
  
  
  
  # Si no se cumplen todos los controles
  if (control_general == FALSE){
    
    
    # SALIDA ARMADA
    {
      ###
      
      SALIDA_ARMADA <- list()
      
      
      SALIDA_ARMADA[[1]]  <- c("Test Chi Cuadrado")
      SALIDA_ARMADA[[2]]  <- CHI$DETALLES$aclaracion
      SALIDA_ARMADA[[4]]  <- TABLA
      SALIDA_ARMADA[[5]]  <- frases_control[[contador_externo]]
      SALIDA_ARMADA[[6]]  <- matrix("No puede realizarse el test Chi Cuadrado.", 1, 8)
      SALIDA_ARMADA[[7]]  <- matrix("No puede realizarse el test Chi Cuadrado.", nrow(TABLA), ncol(TABLA))
      SALIDA_ARMADA[[8]]  <- frases_control[[contador_externo]]
      SALIDA_ARMADA[[9]]  <- matrix("No puede realizarse el test Chi Cuadrado.", 1, 2)
      SALIDA_ARMADA[[10]] <- matrix("No puede realizarse el test Chi Cuadrado.", 1, 2)
      
      
      CHI$SALIDA_ARMADA <- SALIDA_ARMADA
      ###
    } # SALIDA ARMADA
    ############################################################
    
    
  } # Fin control_general == FALSE
###########################################################################
  
  
  # Salida de todo...
  CHI
  
 # if( input_exit == "R")  (ANALISIS$SALIDA_ARMADA)
  
} # Fin function


if (1 == 2) {
 
 BASE <- mtcars[,c(9,8)]
 
 input_datos <- BASE
 input_alfa <- 0.05
 input_decimales <- 2
 
 
 
 JA <- CHI_MASTER(BASE, input_decimales=input_decimales, input_alfa=input_alfa)
 
 JA$ANALISIS$ORIGINAL$Test_Chi$observed
}