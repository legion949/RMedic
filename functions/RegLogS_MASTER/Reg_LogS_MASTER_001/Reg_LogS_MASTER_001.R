

RegLogS_MASTER <- function(input_datos=NULL, input_decimales=2, input_alfa=0.05, input_contingencia=FALSE, input_cero_ref_y=NULL, input_cero_ref_x=NULL){
  
  # Lo volvemos character, por si ingreso un numero...
  
  # Si el sujeto no ingresa un valor para el Eje Y...
  # Por defecto... se va a tomar el 1er nivel de la variable X
  if(is.null(input_cero_ref_y)) input_cero_ref_y <- levels(as.factor(as.character(input_datos[,2])))[1]
  
  # Si la persona ingresa otro valor como referencia para el Eje Y... lo transforma en caracter...
  input_cero_ref_y <- as.character(input_cero_ref_y)
  
  # Si tiene que ser si o si de 2 por 2...
  # Le ponemos un cero por defecto al eje X...
  if(is.null(input_cero_ref_x)) input_cero_ref_x <- levels(as.factor(as.character(input_datos[,1])))[1]
  
  # Si no es nulo... pero lo ingreso aparte... que sea character
  if(!is.null(input_cero_ref_x)) input_cero_ref_x <- as.character(input_cero_ref_x)
  
  # Inicio
  RegLogS <- list()
  
  # Ordenamiento General
  RegLogS$input <- list()           # Ingresan solo los elementos que son input de la funcion
  RegLogS$DETALLES <- list()        # Nombre de las variables...
  RegLogS$CONTROLES <- list()       # Detalle de los diferentes controles por los que paso la base de datos a trabajar...
  RegLogS$MINI <- list()            # Va la MINIBASE de trabajo, y quien es VR y quien es FACTOR
  RegLogS$ANALISIS <- list()        # Los analisis... un apartado sera "$ORIGINAL"... y otro será "$CONSULTORA"
  RegLogS$SCRIPT <- list()          # Un apartado sera el script para obtener exactamente lo mismo que la salida cruda de R.
  RegLogS$SALIDA_ARMADA <- list()   # La salida, que son objetos que ingresaran al Shiny...
  RegLogS$CONTROL_GENERAL <- list() # Guardamos... para saber... si se cumplio o no con todos los controles...
  RegLogS$GRAFICO <- list()         # Guardo detalles para generar la curva de la regresion logistica
  
  # # # INPUT
  {
  
  # Esta informacion es inalterable... 
  # O sea... se pueda o no realizar el analisis... esta informacion debe estar...
  
  RegLogS$input$datos <- input_datos
  RegLogS$input$decimales <- input_decimales
  RegLogS$input$alfa <- input_alfa
  RegLogS$input$cero_ref_x <- input_cero_ref_x
  RegLogS$input$cero_ref_y <- input_cero_ref_y
  RegLogS$input$contingencia <- input_contingencia
  
  } # FIN INPUT
  ############################################################################
  
  
  # # # DETALLES
  {
    
    # Esta informacion es inalterable... 
    # O sea... se pueda o no realizar el analisis... esta informacion debe estar...
    
    RegLogS$DETALLES$nombre_X <- colnames(input_datos)[1]
    RegLogS$DETALLES$nombre_Y <- colnames(input_datos)[2]
    
    
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
  
    {
      
    frases_control <- list()
    frases_control[[1]] <- c("El objeto 'input_datos' debe ser un data.frame o una matriz. Ingrese un formato de datos correcto.")
    frases_control[[2]] <- c("El objeto 'input_datos' debe tener 2 columnas. Ingrese un formato de datos correcto.")
    frases_control[[3]] <- c("El objeto 'input_datos' debe tener al menos 2 filas. No puede realizarse el test de Regresión Logística Simple.")
    frases_control[[4]] <- c("Al realizar na.omit() sobre la base de datos, quedan menos de 2 filas con informacion. No puede realizarse el test de Regresión Logística Simple.")
    if (input_contingencia== FALSE)   frases_control[[5]] <- c("La Variable1(X) ingresada debe ser un objeto numérico o tener solo dos categorias. No puede realizarse el test de Regresión Logística Simple.")
    if (input_contingencia== TRUE)    frases_control[[5]] <- c("La Variable1(X) ingresada debe ser un objeto numérico con solo dos valores posibles o tener solo dos categorias. No puede realizarse el test de Regresión Logística Simple.")
    
    frases_control[[6]] <- c("La Variable2(Y) ingresada debe ser una variable categórica con solo dos niveles, o un objeto numérico con solo dos valores posibles. No puede realizarse el test de Regresión Logística Simple.")
    frases_control[[7]] <- c("Los datos de 'X' y 'Y' son constantes. Presentan varianza cero. No puede realizarse el test de Regresión Logística Simple.")
    frases_control[[8]] <- c("Los datos de 'X' son constantes. Presentan varianza cero. No puede realizarse el test de Regresión Logística Simple.")
    frases_control[[9]] <- c("Los datos de 'Y' son constantes. Presentan varianza cero. No puede realizarse el test de Regresión Logística Simple.")
    frases_control[[10]] <- c("El valor de 'input_cero_ref_y' no corresponde a una categoría de la Variable2(Y) ingresada. Modifica el valor del argumento 'input_cero_ref_y'. No puede realizarse el test de Regresión Logística Simple.")
    frases_control[[11]] <- c("El argumento 'input_cero_ref_x' está vacío o no corresponde a una categoría de la Variable1(X) ingresada. No puede realizarse el test de Regresión Logística Simple.")
    
    
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
      CAT_X1 <- as.factor(as.character(X1_CONTROL))
      
      
      if (input_contingencia== FALSE) if (is.numeric(X1_CONTROL) | length(levels(CAT_X1)) ==2) control_OK[[5]] <- TRUE else control_general <- FALSE
      if (input_contingencia== TRUE) if ( length(levels(CAT_X1)) ==2) control_OK[[5]] <- TRUE else control_general <- FALSE
      
          }
    
    
    
    
    # Control 6...
    # Los datos de X no deben ser constantes... la varianza debe ser distinta de cero.
    control_OK[[6]] <- FALSE
    names(control_OK[[6]]) <- c("La variable 'Y' debe ser un factor con solo 2 niveles o una numerico con solo dos valores posibles.")
    if (control_general == TRUE) {
      
      PA_CONTROL <- MINI_CONTROL[,2]
      PA_CONTROL <- as.factor(as.character(PA_CONTROL))
      PA_CONTROL <- as.numeric(PA_CONTROL)
      PA_CONTROL <- PA_CONTROL - 1
      PA_CONTROL <- as.factor(as.character(PA_CONTROL))
      niveles <- levels(PA_CONTROL)
      
      identidad <- c("0", "1")
      
      
      if (identical(niveles, identidad)) control_OK[[6]] <- TRUE else control_general <- FALSE
    }
    
    
    
    
    
    # Control 7...
    # Las variables no deben ser constantes...
    control_OK[[7]] <- FALSE
    names(control_OK[[7]]) <- c("Ambas variables son constantes.")
    if (control_general == TRUE) {
  
      PA_CONTROL <- MINI_CONTROL[,2]
      PA_CONTROL <- as.factor(as.character(PA_CONTROL))
      PA_CONTROL <- as.numeric(PA_CONTROL)
      
      varianza_control1 <- var(X1_CONTROL)
      varianza_control2 <- var(PA_CONTROL)
      
      var_control_total <- c(varianza_control1, varianza_control2)
      
      detec <- var_control_total == 0
      detec <- as.numeric(detec)
      
      if (sum(detec) != length(detec)) control_OK[[7]] <- TRUE else control_general <- FALSE
    }
    
    
    
    
    # Control 8...
    # Los datos de X no deben ser constantes... la varianza debe ser distinta de cero.
    control_OK[[8]] <- FALSE
    names(control_OK[[8]]) <- c("La varianza de X es igual a cero.")
    if (control_general == TRUE) {
      
      varianza_control1 <- var(X1_CONTROL)
      
      
      if (varianza_control1 != 0) control_OK[[8]] <- TRUE else control_general <- FALSE
    }
    
    
    
    
    
    # Control 9...
    # Los datos de X no deben ser constantes... la varianza debe ser distinta de cero.
    control_OK[[9]] <- FALSE
    names(control_OK[[9]]) <- c("La varianza de Y es igual a cero.")
    if (control_general == TRUE) {
      
      PA_CONTROL <- MINI_CONTROL[,2]
      PA_CONTROL <- as.factor(as.character(PA_CONTROL))
      PA_CONTROL <- as.numeric(PA_CONTROL)
      
      varianza_control2 <- var(PA_CONTROL)
      
      
      if (varianza_control2 != 0) control_OK[[9]] <- TRUE else control_general <- FALSE
    }
    
    
    
    
    
    
    # Control 10...
    # El valor de 'input_cero_ref_x' debe ser un valor dentro de la variable PA_CONTROL
    control_OK[[10]] <- FALSE
    names(control_OK[[10]]) <- c("El valor de 'input_cero_ref_y' no corresponde a la variable Y ingresada.")
    if (control_general == TRUE) {
      
      PA_CONTROL <- MINI_CONTROL[,2]
      PA_CONTROL <- as.factor(as.character(PA_CONTROL))
      niveles <- levels(PA_CONTROL)
      
      referencia <- input_cero_ref_y
      
      aver <- niveles == referencia
      aver <- as.numeric(aver)
      
      if (sum(aver) == 1) control_OK[[10]] <- TRUE else control_general <- FALSE
    }
    
    
    
    # Control 11...
    # El valor de 'input_cero_ref_x' debe ser un valor dentro de la variable PA_CONTROL
    control_OK[[11]] <- FALSE
    names(control_OK[[11]]) <- c("El valor de 'input_cero_ref_x' no corresponde a la variable Y ingresada.")
    if (control_general == TRUE) {
      
      X_CONTROL <- MINI_CONTROL[,1]
      X_PA_CONTROL <- as.factor(as.character(PA_CONTROL))
      niveles <- levels(X_PA_CONTROL)
      
      referencia <- input_cero_ref_y
      
      aver <- niveles == referencia
      aver <- as.numeric(aver)
      
      
      # Si tiene 2 niveles el eje X... y el input_cero_ref_x es uno de esos dos niveles... TODO OK
      if (control_general == TRUE) if(length(niveles) == 2) if (sum(aver) == 1) control_OK[[11]] <- TRUE else control_general <- FALSE
    
      # Si tiene mas de 2 niveles... pero X_CONTROL es numerico... le damos pase libre... todo OK
      if (control_general == TRUE) if(length(niveles) > 2) if (is.numeric(X_CONTROL)) control_OK[[11]] <- TRUE else control_general <- FALSE
      
      
      }

        
    
    
    
    
    } # if input_contingencia == FALSE
    ######################################################################################################################
    
    
    
    RegLogS$CONTROLES <- control_OK
    
    RegLogS$CONTROL_GENERAL <- control_general
    
  } # # # FIN CONTROLES
  ###############################################################################################################################
  
  
  
  
  # Parte 0: Input
  # Agregamos todos los input 
  
  if (control_general == TRUE) {
    
    
    MINI <- input_datos
    MINI <- na.omit(MINI)
    
    # Aca obtenemos, X... Y... PA2_X PA2_Y y PA_X y PA_Y.
    # PA es abreviacion de Presencia/Auseancia...
    # Si o si los datos de X deben ser transformados en PA_X...
    # Ahora... el eje Y, depende... de si tiene 2 niveles solamente, o si numerico.
    # Si tiene dos niveles... sea numerico o no... sera transforamdo en PA_X
    # Ahora... si es numerico, pero tiene más de dos niveles (por ejemplo, es continuo, tiene decimales...)
    # No va a pasar por esto de ser un objeto de presencia/ausencia... por que tiene que ingresar asi como esta...
    
    # El que ingresa a la regresion logistica es X_FINAL y el Y_FINAL
    # Y... queda como la informacion original de la base de datos ingresada... 
    # PA2 es un objeto intermedio... para obtener a PA.
    
    # Obtenemos los objetos iniciales, y ahi vamos transformando...
    X <- MINI[,1]
    Y <- MINI[,2]
    PA2_X <- MINI[,1]
    PA2_Y <- MINI[,2]
    
    # Transformacion para el Eje Y (Obligatorio)
    {
    
    # Tratamiento a PA_Y (Obligatorio)
    detec_0 <- PA2_Y == input_cero_ref_y
    detec_1 <- PA2_Y != input_cero_ref_y
    
    PA2_Y <- rep(NA, length(PA2_Y))
    PA2_Y[detec_0] <- 0
    PA2_Y[detec_1] <- 1
  
    
    # Esto es una tabla explicativa...
    # Si ya de por si "Y" tiene valores de "0" y "1"... la tabla siguiente es al vicio...
    # Pero... si tiene otras categorias... como "Enferno" y "Sano"... hace falta indicar que, por ejemplo "Enfermo" 
    # paso a ser "0" y "Sano" es "1".
    
    
    # Niveles originales
    niveles_Y <- levels(as.factor(as.character(Y)))
    
    
    # Ahora... lo que hay que hacer... es "alinear" el orden de los niveles de Y...
    # para que en la tabla aparezca 1ro el que va a ser codificado con "0".
    # Lo hago con el siguiente script...
     
    orden2 <- c(1,2)
    este <- orden2[niveles_Y == input_cero_ref_y]
    if (este == 1) orden2 <- c(1,2) else orden2 <- c(2,1)
    
    reordenamiento_Y <- niveles_Y[orden2]
    
    } # Fin Transformacion para el Eje Y (Oblogatorio)
    ###########################################################
    
    
    
    # Transformacion para el Eje X (Obligatorio/Optativo)
    {
      
      niveles_x <- levels(as.factor(as.character(PA2_X)))
      
      # Armamos el candado pero no lo cerramos...
      # O sea... tenemos que hacer la transformacion...
      # Pero...
      # Si PA2_X es numerica... y...
      # tiene mas de dos niveles... ponemos el candado...
      candadito_x <- FALSE
      if (is.numeric(PA2_X)) if (length(niveles_x) > 2) candadito_x <- TRUE
      
      if (candadito_x == FALSE) {
      # Tratamiento a PA_Y (Obligatorio)
      detec_0 <- PA2_X == input_cero_ref_x
      detec_1 <- PA2_X != input_cero_ref_x
      
      PA2_X <- rep(NA, length(PA2_X))
      PA2_X[detec_0] <- 0
      PA2_X[detec_1] <- 1
      
      
      # Esto es una tabla explicativa...
      # Si ya de por si "X" tiene valores de "0" y "1"... la tabla siguiente es al vicio...
      # Pero... si tiene otras categorias... como "Enferno" y "Sano"... hace falta indicar que, por ejemplo "Enfermo" 
      # paso a ser "0" y "Sano" es "1".
      
      
      # Niveles originales
      niveles_X <- levels(as.factor(as.character(X)))
      
      
      # Ahora... lo que hay que hacer... es "alinear" el orden de los niveles de X...
      # para que en la tabla aparezca 1ro el que va a ser codificado con "0".
      # Lo hago con el siguiente script...
      
     
      
      orden2 <- c(1,2)
      este <- orden2[niveles_X == input_cero_ref_x]
      if (este == 1) orden2 <- c(1,2) else orden2 <- c(2,1)
      
      reordenamiento_X <- niveles_X[orden2]
      }
      
    } # Fin Transformacion para el Eje Y (Oblogatorio)
    ###########################################################
    
    
    # Creacion del objeto X_FINAL e Y_FINAL
    
    X_FINAL <- PA2_X
    Y_FINAL <- PA2_Y
    
    # Tabla de Referencia
    {
    
    codigo <- c(0,1)
    
      # Creamos la Tabla de Referencia segun sea conveniente...
    
    if (candadito_x == FALSE) {
    nombres <- c(paste(colnames(MINI)[1], " (X)", sep=""), "Referencia (X e Y)", paste(colnames(MINI)[2], " (Y)", sep=""))
    TABLA_REFERENCIA <- matrix(NA, 2,length(nombres))
    colnames(TABLA_REFERENCIA) <- nombres
    
    TABLA_REFERENCIA[,1] <- reordenamiento_X
    TABLA_REFERENCIA[,2] <- codigo
    TABLA_REFERENCIA[,3] <- reordenamiento_Y
    
    }
    
    
    if (candadito_x == TRUE) {
      nombres <- c(paste(colnames(MINI)[1], " (X)", sep=""), "Referencia (Y)", paste(colnames(MINI)[2], " (Y)", sep=""))
      TABLA_REFERENCIA <- matrix(NA, 2,length(nombres))
      colnames(TABLA_REFERENCIA) <- nombres
      
      TABLA_REFERENCIA[,1] <- matrix("Variable X no se codifica." , 2, 1)
      TABLA_REFERENCIA[,2] <- codigo
      TABLA_REFERENCIA[,3] <- reordenamiento_Y
      
    }
    } # Fin Tabla de Referencia
    ##################################################################
      
    RegLogS$DETALLES$TABLA_REFERENCIA <- TABLA_REFERENCIA
    
      
    # Regresión Logística Simple (RegLogS)
    
    ORIGINAL <- list()
    GRAFICO <- list()
    
    # RegLogS
    {
    # Regresion Completa
    ORIGINAL <- glm(Y_FINAL ~ X_FINAL, family = binomial)
    
    # Detalles para el grafico
    GRAFICO$X_FINAL <- X_FINAL
    GRAFICO$Y_FINAL <- Y_FINAL
    
    MyData_X <- data.frame(X_FINAL = seq(min(X_FINAL), max(X_FINAL), by=0.01))
    PREDICCION_Y <- predict(ORIGINAL,  newdata=MyData_X, type = "response")
    
    MyData_X <- as.vector(as.matrix(MyData_X))
    PREDICCION_Y <- as.vector(as.matrix(PREDICCION_Y))
    
    GRAFICO$MyData_X <- MyData_X
    GRAFICO$PREDICCION_Y <- PREDICCION_Y
    
    # Pack Summary
    PACK_REGRESION <- summary(ORIGINAL)
    
    # Tabla de Regresion
    TABLA_REGRESION <- PACK_REGRESION$coefficients
    TABLA_REGRESION <- round(TABLA_REGRESION, input_decimales)
    rownames(TABLA_REGRESION) <- c("Ordenada", "Pendiente")
    colnames(TABLA_REGRESION) <- c("Estimados", "Error Estándard", "Valor t", "Valor p")

    
        
    # Valos de ordenada, pendiente y R2ajustado
    estimado_ordenada <- TABLA_REGRESION[1,1]
    estimado_pendiente <- TABLA_REGRESION[2,1]
    estimado_aic <- PACK_REGRESION$aic
    
    estimado_ordenada <- round(estimado_ordenada, input_decimales)
    estimado_pendiente <- round(estimado_pendiente, input_decimales)
    estimado_aic <- round(estimado_aic, input_decimales)
    
    
    # Valores p internos de ordenada, pendiente y R2ajustado
    valor_p_interno_ordenada <- TABLA_REGRESION[1,4]
    valor_p_interno_pendiente <- TABLA_REGRESION[2,4]

    
    desicion <- c("No Rechazo Ho", "No Rechazo Ho")
    if (valor_p_interno_ordenada < input_alfa) desicion[1] <- "Rechazo Ho"
    if (valor_p_interno_pendiente < input_alfa) desicion[2] <- "Rechazo Ho"

    TABLA_REGRESION <- cbind(TABLA_REGRESION, desicion)
    colnames(TABLA_REGRESION)[ncol(TABLA_REGRESION)] <- "Desición"
    
# Esto lo sacamos... pero hay que ver que hacmeos despues...
# El valor de AIC... no viene con un valor p...
# Esto era para el r2_ajustado...
    
#     valor_f <- PACK_REGRESION[[10]][1]
#     valor_f <- round(valor_f, input_decimales)
#     
#     df_num <- PACK_REGRESION[[10]][2]
#     df_den <- PACK_REGRESION[[10]][3]
#     valor_p_interno_aic <- pf(valor_f, df_num, df_den, lower.tail=F)
#     valor_p_interno_aic <- round(valor_p_interno_aic, input_decimales)
    
    # Valor p externos de ordenada, pendiente y R2ajustado
    if (valor_p_interno_ordenada < 0.01) valor_p_externo_ordenada <- "<<0.01" else valor_p_externo_ordenada <- valor_p_interno_ordenada
    if (valor_p_interno_pendiente < 0.01) valor_p_externo_pendiente <- "<<0.01" else valor_p_externo_pendiente <- valor_p_interno_pendiente
#    if (valor_p_interno_aic < 0.01) valor_p_externo_aic <- "<<0.01" else valor_p_externo_aic <- valor_p_interno_aic
    
    
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
    # CONSULTORA$valor_p_externo_aic <- valor_p_externo_aic
    
    
    CONSULTORA$ordenada <- estimado_ordenada 
    CONSULTORA$pendiente <- estimado_pendiente
    CONSULTORA$aic <- estimado_aic
    
    
    # Desicion para la ordenada, pendiente y r2
    if (valor_p_interno_ordenada >= input_alfa) desicion_ordenada <- c("No rechazo Ho") else  desicion_ordenada <- c("Rechazo Ho")
    if (valor_p_interno_pendiente >= input_alfa) desicion_pendiente <- c("No rechazo Ho") else  desicion_pendiente <- c("Rechazo Ho")
#    if (valor_p_interno_aic >= input_alfa) desicion_aic <- c("No rechazo Ho") else  desicion_aic <- c("Rechazo Ho")
    
    
    
    # RESUMENES
    nombres_col <- c("Estimado", "Valor p", "Alfa", "Desición")
    nombres_fil <- c("Ordenada", "Pendiente", "AIC")
    RESUMEN <- as.data.frame(matrix(NA, 3, length(nombres_col)))
    colnames(RESUMEN) <- nombres_col
    rownames(RESUMEN) <- nombres_fil
    
    RESUMEN[1,] <- c(estimado_ordenada, valor_p_externo_ordenada, input_alfa, desicion_ordenada)
    RESUMEN[2,] <- c(estimado_pendiente, valor_p_externo_pendiente, input_alfa, desicion_pendiente)
  #  RESUMEN[3,] <- c(estimado_aic, valor_p_externo_aic, input_alfa, desicion_aic)
    RESUMEN[3,] <- c(estimado_aic, "No Corresponde", "No Corresponde", "No Corresponde")
    
    
    CONSULTORA$RESUMEN <- RESUMEN
    
    } # Fin RegLogS 
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
                      "La variable '", RegLogS$DETALLES$nombre_Y , "' no depende de '", RegLogS$DETALLES$nombre_X, "'.")
      
      
      frase2_v3 <- c("El valor estimado de pendiente es ", estimado_pendiente, "." ,"<br/>",
                     "El valor p de la pendiente es igual que el valor de alfa=", input_alfa, ".","<br/>",
                     "La pendiente es estadísticamente igual a cero.","<br/>",
                     "La variable '", RegLogS$DETALLES$nombre_Y , "' no depende de '", RegLogS$DETALLES$nombre_X, "'.")
      
      
      
      
      frase2_v4 <-  c("El valor estimado de pendiente es ", estimado_pendiente, "." ,"<br/>",
                      "El valor p de la pendiente es menor que el valor de alfa=", input_alfa, ".","<br/>",
                      "La pendiente es estadísticamente distinta a cero.","<br/>",
                      "La variable '", RegLogS$DETALLES$nombre_Y , "' depende de '", RegLogS$DETALLES$nombre_X, "'. ")
      
      
      if(is.na(valor_p_interno_pendiente) | is.null(valor_p_interno_pendiente)) frase2_elegida_pendiente <- frase2_v1 else if (valor_p_interno_pendiente > input_alfa) frase2_elegida_pendiente <- frase2_v2 else if (valor_p_interno_pendiente == input_alfa) frase2_elegida_pendiente <- frase2_v3 else if (valor_p_interno_pendiente < input_alfa) frase2_elegida_pendiente <- frase2_v4
      
      CONSULTORA$FRASE2_PENDIENTE <- frase2_elegida_pendiente
      
    } # Fin Frase2_html
    #######################################
    
    
    # Le ponemos candado a esta parte... por que era la frase para el valor p del R2 ajustado...
    # Hay que ver que hacemos con esta parte...
    if (1 == 2) {
    # Frase de aic
    # frase2_ordenada_html: segun valor p
    {
      
      frase2_v1 <- c("No pudo obtenerse un valor p.")
      
      frase2_v2 <- c("El valor estimado de r2ajustado es ", estimado_aic, "." ,"<br/>",
                     "El valor p del r2 ajsutado es mayor que el valor de alfa=", input_alfa, ".", "<br/>",
                     "No se rechaza la Ho.", "<br/>",
                     "El valor de R2ajustado es estadísticamente igual a cero.","<br/>",
                     "Estadísticamente, la variable '", RegLogS$DETALLES$nombre_X , "' explica un ",  paste(estimado_aic, "%", sep=""), " la variabilidad de '", RegLogS$DETALLES$nombre_Y, "'.","<br/>",
                     "El modelo no debiera ser utilizado para sacar conslusiones")
      
      
      frase2_v3 <- c("El valor estimado de r2ajustado es ", estimado_aic, "." ,"<br/>",
                     "El valor p del r2 ajustado es igual que el valor de alfa=", input_alfa, ".", "<br/>",
                     "No se rechaza la Ho.", "<br/>",
                     "El valor de R2ajustado es estadísticamente igual a cero.","<br/>",
                     "Estadísticamente, la variable '", RegLogS$DETALLES$nombre_X , "' explica un ",  paste(estimado_aic, "%", sep=""), " la variabilidad de '", RegLogS$DETALLES$nombre_Y, "'.","<br/>",
                     "El modelo no debiera ser utilizado para sacar conslusiones")
      
      
      
      frase2_v4 <- c("El valor estimado de r2ajustado es ", estimado_aic, "." ,"<br/>",
                     "El valor p del r2 ajustado es menor que el valor de alfa=", input_alfa, ".", "<br/>",
                     "Se rechaza la Ho.", "<br/>",
                     "El valor de R2ajustado es estadísticamente distinto de cero.","<br/>",
                     "Estadísticamente, la variable '", RegLogS$DETALLES$nombre_X , "' explica un ",  paste(estimado_aic*100, "%", sep=""), " la variabilidad de '", RegLogS$DETALLES$nombre_Y, "'.","<br/>",
                     "Debiera tenerse en consideración si este valor de ",paste(estimado_aic*100, "%", sep="")  ," es suficientemente alto dentro de mi área de aplicación para sacar conclusiones con este modelo estadístico.")
      
      
      if(is.na(valor_p_interno_aic) | is.null(valor_p_interno_aic)) frase2_elegida_aic <- frase2_v1 else if (valor_p_interno_aic > input_alfa) frase2_elegida_aic <- frase2_v2 else if (valor_p_interno_aic == input_alfa) frase2_elegida_aic <- frase2_v3 else if (valor_p_interno_aic < input_alfa) frase2_elegida_aic <- frase2_v4
      
      CONSULTORA$FRASE2_aic <- frase2_elegida_aic
      
    } # Fin Frase2_html
    #######################################
    }
    ################################################################################################
    
    
    
    ANALISIS <- list()
    ANALISIS$ORIGINAL <- ORIGINAL
    ANALISIS$CONSULTORA <- CONSULTORA
    
    RegLogS$ANALISIS <- ANALISIS
    RegLogS$GRAFICO <- GRAFICO
    
    # Salida Armanda
    
    SALIDA_ARMADA <- list()
    
    
    
    # Salida Armada
    {
    
    SALIDA_ARMADA$FRASE_INICIAL <- "Regresión Logística Simple"
    
    SALIDA_ARMADA$TABLA_REFERENCIA <- TABLA_REFERENCIA
    
    SALIDA_ARMADA$TABLA_REGRESION <- CONSULTORA$TABLA_REGRESION
    
    SALIDA_ARMADA$RESUMEN <- CONSULTORA$RESUMEN
    
    SALIDA_ARMADA$FRASE_ORDENADA <- frase2_elegida_ordenada
    
    SALIDA_ARMADA$FRASE_PENDIENTE <- frase2_elegida_pendiente
    
  #  SALIDA_ARMADA$FRASE_R2A <- frase2_elegida_aic
    
    }
    
    RegLogS$SALIDA_ARMADA <- SALIDA_ARMADA
    
    
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
        SALIDA_ARMADA$FRASE_INICIAL <- "Regresión Logística Simple"
        
        
        SALIDA_ARMADA$TABLA_REFERENCIA <- matrix("No puede realizarse el test de Regresión Logística Simple", 1, 2)
        
        SALIDA_ARMADA$TABLA_REGRESION <- matrix("No puede realizarse el test de Regresión Logística Simple", 1, 4)
        
        SALIDA_ARMADA$RESUMEN <- matrix("No puede realizarse el test de Regresión Logística Simple", 1, 4)
        
        SALIDA_ARMADA$FRASE_ORDENADA <- frases_control[[contador_externo]]
        
        SALIDA_ARMADA$FRASE_PENDIENTE <- frases_control[[contador_externo]]
        
  #      SALIDA_ARMADA$FRASE_R2A <- frases_control[[contador_externo]]
        
        
        candado_externo <- TRUE 
        
      }
    }
    
    
    RegLogS$SALIDA_ARMADA <- SALIDA_ARMADA
    
  } # Fin if control_general == FALSE
  #######################################################################################
  
  
  
  RegLogS
  
} # Fin function RegLogS_MASTER()





if (1 == 2){
  
  BASE <- mtcars[,c(1,9)]
  
  input_datos <- BASE
  input_alfa <- 0.05
  input_decimales <- 2
  input_cero_ref_x <- 0
  input_cero_ref_y <- 0
  input_contingencia <- FALSE
  
  AVER <-   RegLogS_MASTER(BASE)
  
} # Fin 1 == 2