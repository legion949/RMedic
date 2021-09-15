

RLS_MASTER <- function(input_datos=NULL, input_decimales=2, input_alfa=0.05){
  
  
  
  # Inicio
  RLS <- list()
  
  # Ordenamiento General
  RLS$input <- list()           # Ingresan solo los elementos que son input de la funcion
  RLS$DETALLES <- list()        # Nombre de las variables...
  RLS$CONTROLES <- list()       # Detalle de los diferentes controles por los que paso la base de datos a trabajar...
  RLS$MINI <- list()            # Va la MINIBASE de trabajo, y quien es VR y quien es FACTOR
  RLS$ANALISIS <- list()        # Los analisis... un apartado sera "$ORIGINAL"... y otro será "$CONSULTORA"
  RLS$SCRIPT <- list()          # Un apartado sera el script para obtener exactamente lo mismo que la salida cruda de R.
  RLS$SALIDA_ARMADA <- list()   # La salida, que son objetos que ingresaran al Shiny...
  RLS$CONTROL_GENERAL <- list() # Guardamos... para saber... si se cumplio o no con todos los controles...
  
  
  # # # INPUT
  {
  
  # Esta informacion es inalterable... 
  # O sea... se pueda o no realizar el analisis... esta informacion debe estar...
  
  RLS$input$datos <- input_datos
  RLS$input$decimales <- input_decimales
  RLS$input$alfa <- input_alfa
  
  
  } # FIN INPUT
  ############################################################################
  
  
  # # # DETALLES
  {
    
    # Esta informacion es inalterable... 
    # O sea... se pueda o no realizar el analisis... esta informacion debe estar...
    
    RLS$DETALLES$nombre_X <- colnames(input_datos)[1]
    RLS$DETALLES$nombre_Y <- colnames(input_datos)[2]
    
    
  } # FIN DETALLES
  ############################################################################
  
  
  
  
  
  # # # CONTROLRES
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
    frases_control[[3]] <- c("El objeto 'input_datos' debe tener al menos 2 filas. No puede realizarse el test de Regresión Lineal Simple.")
    frases_control[[4]] <- c("Al realizar na.omit() sobre la base de datos, quedan menos de 2 filas con informacion. No puede realizarse el test de Regresión Lineal Simple.")
    frases_control[[5]] <- c("La Variable1 ingresada debe ser un objeto numérico. No puede realizarse el test de Regresión Lineal Simple.")
    frases_control[[6]] <- c("La Variable2 ingresada debe ser un objeto numérico. No puede realizarse el test de Regresión Lineal Simple.")
    frases_control[[7]] <- c("Los datos de 'X' y 'Y' son constantes. Presentan varianza cero. No puede realizarse el test de Regresión Lineal Simple.")
    frases_control[[8]] <- c("Los datos de 'X' son constantes. Presentan varianza cero. No puede realizarse el test de Regresión Lineal Simple.")
    
    
    # Control 1...
    # El objeto input_datos debe ser un data.frame o una matrix.
    control_OK[[1]] <- FALSE
    names(control_OK[[1]]) <- c("data.frame o matrix")
    if (is.data.frame(input_datos) | is.matrix(input_datos)) control_OK[[1]] <- TRUE else control_general <- FALSE 
    
    
    # Control 2...
    # El objeto input_datos debe tener 1 columna.
    control_OK[[2]] <- FALSE
    names(control_OK[[2]]) <- c("Solo 2 columnas")
    if (control_general == TRUE) {
      if (ncol(input_datos) == 2) control_OK[[2]] <- TRUE else control_general <- FALSE
    }
    
    
    
    
    # Control 3...
    # El objeto input_datos debe tener al menos 1 fila.
    control_OK[[3]] <- FALSE
    names(control_OK[[3]]) <- c("Al menos 2 filas de input_datos")
    if (control_general == TRUE) {
      if (nrow(input_datos) >= 2) control_OK[[3]] <- TRUE else control_general <- FALSE 
    }
    
    
    
    # Control 4...
    # El objeto "MINI" (obtenido con na.omit(input_datos)) debe tener al menos 2 filas
    control_OK[[4]] <- FALSE
    names(control_OK[[4]]) <- c("Al menos 2 filas de MINI")
    if (control_general == TRUE) {
      
      MINI_CONTROL <- input_datos
      MINI_CONTROL <- na.omit(MINI_CONTROL)
      
      if (nrow(MINI_CONTROL) >= 2) control_OK[[4]] <- TRUE else control_general <- FALSE 
    }
    
    
    
    # Control 5...
    # La columna 1, que sera X1, debe ser numérica.
    control_OK[[5]] <- FALSE
    names(control_OK[[5]]) <- c("X es numerica")
    if (control_general == TRUE) {
      X1_CONTROL <- MINI_CONTROL[,1]
      if (is.numeric(X1_CONTROL)) control_OK[[5]] <- TRUE else control_general <- FALSE
    }
    
    
    
    
    
    # Control 6...
    # La columna 2, que sera X2, debe ser numérica.
    control_OK[[6]] <- FALSE
    names(control_OK[[6]]) <- c("Y es numerica")
    if (control_general == TRUE) {
      X2_CONTROL <- MINI_CONTROL[,2]
      if (is.numeric(X2_CONTROL)) control_OK[[6]] <- TRUE else control_general <- FALSE
    }
    
    
    
    
    
    # Control 7...
    # Las variables no deben ser constantes...
    control_OK[[7]] <- FALSE
    names(control_OK[[7]]) <- c("Ambas variables son constantes.")
    if (control_general == TRUE) {
      
      varianza_control1 <- var(X1_CONTROL)
      varianza_control2 <- var(X2_CONTROL)
      
      var_control_total <- c(varianza_control1, varianza_control2)
      
      detec <- var_control_total == 0
      detec <- as.numeric(detec)
      
      if (sum(detec) != length(detec)) control_OK[[7]] <- TRUE else control_general <- FALSE
    }
    
    
    
    
    # Control 8...
    # Los datos de X no deben ser constantes... la varianza debe ser distinta de cero.
    control_OK[[8]] <- FALSE
    names(control_OK[[8]]) <- c("La varianza de X es distinta de cero.")
    if (control_general == TRUE) {
      
      varianza_control1 <- var(X1_CONTROL)
      
      
      if (varianza_control1 != 0) control_OK[[8]] <- TRUE else control_general <- FALSE
    }
    
    
    
    
    
    RLS$CONTROLES <- control_OK
    
    RLS$CONTROL_GENERAL <- control_general
    
  } # # # FIN CONTROLES
  ###############################################################################################################################
  
  
  
  
  # Parte 0: Input
  # Agregamos todos los input 
  
  if (control_general == TRUE) {
  
  
  MINI <- input_datos
  MINI <- na.omit(MINI)
  
  X <- MINI[,1]
  Y <- MINI[,2]
  
  
  # Regresion Lineal Simple (RLS)
  
  ORIGINAL <- list()
  
  # RLS
  {
  # Regresion Completa
  ORIGINAL <- lm (Y ~ X) 
  
 # RESIDUOS <- ORIGINAL$residuals
    
  # Pack Summary
  PACK_REGRESION <- summary(ORIGINAL)
  
  # Tabla de Regresion
  TABLA_REGRESION <- PACK_REGRESION[[4]]
  TABLA_REGRESION <- round(TABLA_REGRESION, input_decimales)
  rownames(TABLA_REGRESION) <- c("Ordenada", "Pendiente")
  colnames(TABLA_REGRESION) <- c("Estimados", "Error Estándard", "Valor t", "Valor p")
  
  # Valos de ordenada, pendiente y R2ajustado
  estimado_ordenada <- TABLA_REGRESION[1,1]
  estimado_pendiente <- TABLA_REGRESION[2,1]
  estimado_r2_ajustado <- PACK_REGRESION$adj.r.squared
  
  estimado_ordenada <- round(estimado_ordenada, input_decimales)
  estimado_pendiente <- round(estimado_pendiente, input_decimales)
  estimado_r2_ajustado <- round(estimado_r2_ajustado, input_decimales)
  
  
  # Valores p internos de ordenada, pendiente y R2ajustado
  valor_p_interno_ordenada <- TABLA_REGRESION[1,4]
  valor_p_interno_pendiente <- TABLA_REGRESION[2,4]
  
  valor_f <- PACK_REGRESION[[10]][1]
  valor_f <- round(valor_f, input_decimales)
  
  df_num <- PACK_REGRESION[[10]][2]
  df_den <- PACK_REGRESION[[10]][3]
  valor_p_interno_r2_ajustado <- pf(valor_f, df_num, df_den, lower.tail=F)
  valor_p_interno_r2_ajustado <- round(valor_p_interno_r2_ajustado, input_decimales)
  
  # Valor p externos de ordenada, pendiente y R2ajustado
  if (valor_p_interno_ordenada < 0.001) valor_p_externo_ordenada <- "<<0.001" else valor_p_externo_ordenada <- valor_p_interno_ordenada
  if (valor_p_interno_pendiente < 0.001) valor_p_externo_pendiente <- "<<0.001" else valor_p_externo_pendiente <- valor_p_interno_pendiente
  if (valor_p_interno_r2_ajustado < 0.001) valor_p_externo_r2_ajustado <- "<<0.001" else valor_p_externo_r2_ajustado <- valor_p_interno_r2_ajustado
  
  
  # Cambio de los valores de p de pendient y ordenada en la TABLA_REGRESION
  # por lo valos de "valor_p_interno_ordenada" y "valor_p_interno_pendiente"
  
  TABLA_REGRESION[1,4] <- valor_p_externo_ordenada
  TABLA_REGRESION[2,4] <- valor_p_externo_pendiente
  
  CONSULTORA <- list()
  CONSULTORA$PACK_REGRESION <- PACK_REGRESION
  CONSULTORA$TABLA_REGRESION <- TABLA_REGRESION
#  CONSULTORA$REGRESION <- REGRESION

  CONSULTORA$valor_p_externo_ordenada <- valor_p_externo_ordenada
  CONSULTORA$valor_p_externo_pendiente <- valor_p_externo_pendiente
  CONSULTORA$valor_p_externo_r2_ajustado <- valor_p_externo_r2_ajustado
  
  
  CONSULTORA$ordenada <- estimado_ordenada 
  CONSULTORA$pendiente <- estimado_pendiente
  CONSULTORA$r2_ajustado <- estimado_r2_ajustado
  
  
  # Desicion para la ordenada, pendiente y r2
  if (valor_p_interno_ordenada >= input_alfa) desicion_ordenada <- c("No rechazo Ho") else  desicion_ordenada <- c("Rechazo Ho")
  if (valor_p_interno_pendiente >= input_alfa) desicion_pendiente <- c("No rechazo Ho") else  desicion_pendiente <- c("Rechazo Ho")
  if (valor_p_interno_r2_ajustado >= input_alfa) desicion_r2_ajustado <- c("No rechazo Ho") else  desicion_r2_ajustado <- c("Rechazo Ho")
  
  
  
  # RESUMENES
  nombres_col <- c("Estimado", "Valor p", "Alfa", "Desición")
  nombres_fil <- c("Ordenada", "Pendiente", "R^2 Ajustado")
  RESUMEN <- as.data.frame(matrix(NA, 3, length(nombres_col)))
  colnames(RESUMEN) <- nombres_col
  rownames(RESUMEN) <- nombres_fil
  
  RESUMEN[1,] <- c(estimado_ordenada, valor_p_externo_ordenada, input_alfa, desicion_ordenada)
  RESUMEN[2,] <- c(estimado_pendiente, valor_p_externo_pendiente, input_alfa, desicion_pendiente)
  RESUMEN[3,] <- c(estimado_r2_ajustado, valor_p_externo_r2_ajustado, input_alfa, desicion_r2_ajustado)
  
  CONSULTORA$RESUMEN <- RESUMEN
  
  } # Fin RLS 
  ####################################
  
  
  
  
  # Frase de Ordenada
  # frase2_ordenada_html: segun valor p
  {
    
    frase2_v1 <- c("No pudo obtenerse un valor p.")
    
    frase2_v2 <- c("El valor estimado de ordenada es ", estimado_ordenada, "." ,"<br/>",
                   "El valor p de la ordenada es mayor que el valor de alfa=", input_alfa, ".","<br/>",
                   "No se rechaza la Ho.", "<br/>",
                   "La ordenada es estadísticamente igual a cero.","<br/>")
    
    
    frase2_v3 <- c("El valor estimado de ordenada es ", estimado_ordenada, "." ,"<br/>",
                   "El valor p de la ordenada es igual que el valor de alfa=", input_alfa, ".","<br/>",
                   "No se rechaza la Ho.", "<br/>",
                   "La ordenada es estadísticamente igual a cero.","<br/>")
    
    
    
    
    frase2_v4 <- c("El valor estimado de ordenada es ", estimado_ordenada, "." ,"<br/>",
                   "El valor p de la ordenada es menor que el valor de alfa=", input_alfa, ".","<br/>",
                   "Se rechaza la Ho.", "<br/>",
                   "La ordenada es estadísticamente distinta de cero.","<br/>")
    
    
    if(is.na(valor_p_interno_ordenada) | is.null(valor_p_interno_ordenada)) frase2_elegida_ordenada <- frase2_v1 else if (valor_p_interno_ordenada > input_alfa) frase2_elegida_ordenada <- frase2_v2 else if (valor_p_interno_ordenada == input_alfa) frase2_elegida_ordenada <- frase2_v3 else if (valor_p_interno_ordenada < input_alfa) frase2_elegida_ordenada <- frase2_v4

    CONSULTORA$FRASE2_ORDENADA <- frase2_elegida_ordenada
        
  } # Fin Frase2_html
  #######################################
  
  
  
  
  # Frase de Pendiente
  # frase2_ordenada_html: segun valor p
  {
    
    frase2_v1 <- c("No pudo obtenerse un valor p.")
    
    frase2_v2 <-  c("El valor estimado de pendiente es ", estimado_pendiente, "." ,"<br/>",
                    "El valor p de la pendiente es mayor que el valor de alfa=", input_alfa, ".","<br/>",
                    "No se rechaza la Ho.", "<br/>",
                    "La pendiente es estadísticamente igual a cero.","<br/>",
                    "Las variables '", RLS$DETALLES$nombre_X , "' y '", RLS$DETALLES$nombre_Y, "' no presentan una relación estadísticamente significativa.","<br/>",
                    "Las varaibles son estadísticamente independientes.")
    
    
    frase2_v3 <- c("El valor estimado de pendiente es ", estimado_pendiente, "." ,"<br/>",
                   "El valor p de la pendiente es igual que el valor de alfa=", input_alfa, ".","<br/>",
                   "La pendiente es estadísticamente igual a cero.","<br/>",
                   "Las variables '", RLS$DETALLES$nombre_X , "' y '", RLS$DETALLES$nombre_Y, "' no presentan una relación estadísticamente significativa.","<br/>",
                   "Las varaibles son estadísticamente independientes.")
    
    
    
    
    frase2_v4 <-  c("El valor estimado de pendiente es ", estimado_pendiente, "." ,"<br/>",
                    "El valor p de la pendiente es menor que el valor de alfa=", input_alfa, ".","<br/>",
                    "La pendiente es estadísticamente distinta a cero.","<br/>",
                    "Las variables '", RLS$DETALLES$nombre_X , "' y '", RLS$DETALLES$nombre_Y, "' tienen una relación estadísticamente signiticativa.","<br/>",
                    "Las varaibles no son estadísticamente independientes.")
    
    
    if(is.na(valor_p_interno_pendiente) | is.null(valor_p_interno_pendiente)) frase2_elegida_pendiente <- frase2_v1 else if (valor_p_interno_pendiente > input_alfa) frase2_elegida_pendiente <- frase2_v2 else if (valor_p_interno_pendiente == input_alfa) frase2_elegida_pendiente <- frase2_v3 else if (valor_p_interno_pendiente < input_alfa) frase2_elegida_pendiente <- frase2_v4
    
    CONSULTORA$FRASE2_PENDIENTE <- frase2_elegida_pendiente
    
  } # Fin Frase2_html
  #######################################
  
  
  
  
  # Frase de R2_ajustado
  # frase2_ordenada_html: segun valor p
  {
    
    frase2_v1 <- c("No pudo obtenerse un valor p.")
    
    frase2_v2 <- c("El valor estimado de r2ajustado es ", estimado_r2_ajustado, "." ,"<br/>",
                   "El valor p del r2 ajsutado es mayor que el valor de alfa=", input_alfa, ".", "<br/>",
                   "No se rechaza la Ho.", "<br/>",
                   "El valor de R2ajustado es estadísticamente igual a cero.","<br/>",
                   "Estadísticamente, la variable '", RLS$DETALLES$nombre_X , "' explica un ",  paste(estimado_r2_ajustado, "%", sep=""), " la variabilidad de '", RLS$DETALLES$nombre_Y, "'.","<br/>",
                   "El modelo no debiera ser utilizado para sacar conslusiones")
    
    
    frase2_v3 <- c("El valor estimado de r2ajustado es ", estimado_r2_ajustado, "." ,"<br/>",
                   "El valor p del r2 ajustado es igual que el valor de alfa=", input_alfa, ".", "<br/>",
                   "No se rechaza la Ho.", "<br/>",
                   "El valor de R2ajustado es estadísticamente igual a cero.","<br/>",
                   "Estadísticamente, la variable '", RLS$DETALLES$nombre_X , "' explica un ",  paste(estimado_r2_ajustado, "%", sep=""), " la variabilidad de '", RLS$DETALLES$nombre_Y, "'.","<br/>",
                   "El modelo no debiera ser utilizado para sacar conslusiones")
    
    
    
    frase2_v4 <- c("El valor estimado de r2ajustado es ", estimado_r2_ajustado, "." ,"<br/>",
                   "El valor p del r2 ajustado es menor que el valor de alfa=", input_alfa, ".", "<br/>",
                   "Se rechaza la Ho.", "<br/>",
                   "El valor de R2ajustado es estadísticamente distinto de cero.","<br/>",
                   "Estadísticamente, la variable '", RLS$DETALLES$nombre_X , "' explica un ",  paste(estimado_r2_ajustado*100, "%", sep=""), " la variabilidad de '", RLS$DETALLES$nombre_Y, "'.","<br/>",
                   "Debiera tenerse en consideración si este valor de ",paste(estimado_r2_ajustado*100, "%", sep="")  ," es suficientemente alto dentro de mi área de aplicación para sacar conclusiones con este modelo estadístico.")
    
    
    if(is.na(valor_p_interno_r2_ajustado) | is.null(valor_p_interno_r2_ajustado)) frase2_elegida_r2_ajustado <- frase2_v1 else if (valor_p_interno_r2_ajustado > input_alfa) frase2_elegida_r2_ajustado <- frase2_v2 else if (valor_p_interno_r2_ajustado == input_alfa) frase2_elegida_r2_ajustado <- frase2_v3 else if (valor_p_interno_r2_ajustado < input_alfa) frase2_elegida_r2_ajustado <- frase2_v4
    
    CONSULTORA$FRASE2_R2_AJUSTADO <- frase2_elegida_r2_ajustado
    
  } # Fin Frase2_html
  #######################################
  
  ANALISIS <- list()
  ANALISIS$ORIGINAL <- ORIGINAL
  ANALISIS$CONSULTORA <- CONSULTORA
  
  # Cargamos todo en RLS$
  RLS$ANALISIS <- ANALISIS
  # Salida Armanda
  
  SALIDA_ARMADA <- list()
  
  
  
  # Salida Armada
  {
  
  SALIDA_ARMADA$FRASE_INICIAL <- "Regresión Lineal Simple"
  
  SALIDA_ARMADA$TABLA_REGRESION <- CONSULTORA$TABLA_REGRESION
  
  SALIDA_ARMADA$RESUMEN <- CONSULTORA$RESUMEN
  
  SALIDA_ARMADA$FRASE_ORDENADA <- frase2_elegida_ordenada
  
  SALIDA_ARMADA$FRASE_PENDIENTE <- frase2_elegida_pendiente
  
  SALIDA_ARMADA$FRASE_R2A <- frase2_elegida_r2_ajustado
  
  }
  
  RLS$SALIDA_ARMADA <- SALIDA_ARMADA
  
  
  } # FIn if == TRUE
##################################################  
  
  
  
  if (control_general == FALSE) {
    
    candado_externo <- FALSE
    contador_externo <- 0
    while(candado_externo == FALSE) {
      
      contador_externo <- contador_externo + 1
      
      if (control_OK[[contador_externo]] == FALSE) {
        
        SALIDA_ARMADA <- list()
        ###
        
        ###
        SALIDA_ARMADA$FRASE_INICIAL <- "Regresión Lineal Simple"
        
        SALIDA_ARMADA$TABLA_REGRESION <- matrix("No puede realizarse el test de Regresión Lineal Simple", 1, 4)
        
        SALIDA_ARMADA$RESUMEN <- matrix("No puede realizarse el test de Regresión Lineal Simple", 1, 4)
        
        SALIDA_ARMADA$FRASE_ORDENADA <- frases_control[[contador_externo]]
        
        SALIDA_ARMADA$FRASE_PENDIENTE <- frases_control[[contador_externo]]
        
        SALIDA_ARMADA$FRASE_R2A <- frases_control[[contador_externo]]
        
       
        candado_externo <- TRUE 
        
      }
    }
  } # Fin if control_general == FALSE
  #######################################################################################
  
  
  
  RLS
  
} # Fin function RLS_MASTER()





if (1 == 2){
  
  BASE <- mtcars[,c(1,3)]
  input_alfa <- 0.05
  input_decimales <- 2
  
  
  
  AVER <-   RLS_MASTER(BASE)
  
} # Fin 1 == 2