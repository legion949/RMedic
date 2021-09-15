

SHAPIRO_MASTER <- function(input_datos=NULL, input_decimales=2, input_alfa=0.05){
  
  
  # Detalle de los objetos de la salida...
  
  # Inicio
  SHAPIRO <- list()
  
  # Ordenamiento General
  SHAPIRO$input <- list()           # Ingresan solo los elementos que son input de la funcion
  SHAPIRO$DETALLES <- list()        # Nombre de las variables...
  SHAPIRO$CONTROLES <- list()       # Detalle de los diferentes controles por los que paso la base de datos a trabajar...
  SHAPIRO$MINI <- list()            # Va la MINIBASE de trabajo, y quien es VR y quien es FACTOR
  SHAPIRO$ANALISIS <- list()        # Los analisis... un apartado sera "$ORIGINAL"... y otro será "$CONSULTORA"
  SHAPIRO$SCRIPT <- list()          # Un apartado sera el script para obtener exactamente lo mismo que la salida cruda de R.
  SHAPIRO$SALIDA_ARMADA <- list()   # La salida, que son objetos que ingresaran al Shiny...
  SHAPIRO$CONTROL_GENERAL <- list() # Guardamos... para saber... si se cumplio o no con todos los controles...
  
  
  
  
  # # # INPUT
  {
  
  # Esta informacion es inalterable... 
  # O sea... se pueda o no realizar el analisis... esta informacion debe estar...
  
  SHAPIRO$input$datos <- input_datos
  SHAPIRO$input$decimales <- input_decimales
  SHAPIRO$input$alfa <- input_alfa
  
  
  } # FIN INPUT
  ############################################################################
  
  
  # # # DETALLES
  {
    
    # Esta informacion es inalterable... 
    # O sea... se pueda o no realizar el analisis... esta informacion debe estar...
    
    SHAPIRO$DETALLES$nombre_VR <- colnames(input_datos)[1]
    
    
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
    frases_control[[2]] <- c("El objeto 'input_datos' debe tener 1 columnas. Ingrese un formato de datos correcto.")
    frases_control[[3]] <- c("El objeto 'input_datos' debe tener al menos 2 filas. No puede realizarse el test de Normalidad de Shapiro-Wilks.")
    frases_control[[4]] <- c("Al realizar na.omit() sobre la base de datos, quedan 0 filas con informacion. No puede realizarse el test de Normalidad de Shapiro-Wilks.")
    frases_control[[5]] <- c("La columna ingresada debe ser un objeto numérico. No puede realizarse el test de Normalidad de Shapiro-Wilks.")
    frases_control[[6]] <- c("Los datos son constantes. Presentan varianza cero. No puede realizarse el test de Shapiro-Wilks.")
    
    
    # Control 1...
    # El objeto input_datos debe ser un data.frame o una matrix.
    control_OK[[1]] <- FALSE
    names(control_OK[[1]]) <- c("data.frame o matrix")
    if (is.data.frame(input_datos) | is.matrix(input_datos)) control_OK[[1]] <- TRUE else control_general <- FALSE 
    
    
    # Control 2...
    # El objeto input_datos debe tener 1 columna.
    control_OK[[2]] <- FALSE
    names(control_OK[[2]]) <- c("Solo 1 columnas")
    if (control_general == TRUE) {
      if (ncol(input_datos) == 1) control_OK[[2]] <- TRUE else control_general <- FALSE
    }
    
    
    
    
    # Control 3...
    # El objeto input_datos debe tener al menos 1 fila.
    control_OK[[3]] <- FALSE
    names(control_OK[[3]]) <- c("Al menos 2 filas de input_datos")
    if (control_general == TRUE) {
      if (nrow(input_datos) >= 2) control_OK[[3]] <- TRUE else control_general <- FALSE 
    }
    
    
    
    # Control 4...
    # El objeto "MINI" (obtenido con na.omit(input_datos)) debe tener al menos una fila
    control_OK[[4]] <- FALSE
    names(control_OK[[4]]) <- c("Al menos 2 filas de MINI")
    if (control_general == TRUE) {
      
      MINI_CONTROL <- input_datos
      MINI_CONTROL <- na.omit(MINI_CONTROL)
      
      if (nrow(MINI_CONTROL) >= 2) control_OK[[4]] <- TRUE else control_general <- FALSE 
    }
    
    
    
    # Control 5...
    # La columna 1, que sera VR, debe ser numérica.
    control_OK[[5]] <- FALSE
    names(control_OK[[5]]) <- c("VR es numerica")
    if (control_general == TRUE) {
      VR_CONTROL <- MINI_CONTROL[,1]
      if (is.numeric(VR_CONTROL)) control_OK[[5]] <- TRUE else control_general <- FALSE
    }
    
    
    
    
    
    # Control 6...
    # Los datos no deben ser constantes... la varianza debe ser distinta de cero.
    control_OK[[6]] <- FALSE
    names(control_OK[[6]]) <- c("Todas las varianzas distintas de cero")
    if (control_general == TRUE) {
      
      VR_CONTROL <- MINI_CONTROL[,1]
      VR_CONTROL <- as.vector(as.matrix(VR_CONTROL)) 
      varianza_control <- var(VR_CONTROL)
      
      
      if (varianza_control != 0) control_OK[[6]] <- TRUE else control_general <- FALSE
    }
    
    
    SHAPIRO$CONTROLES <- control_OK
    
    SHAPIRO$CONTROL_GENERAL <- control_general
    
  } # # # FIN CONTROLES
  ###############################################################################################################################
  
  
  
  # Si supero todos los controles... podemos realizar el analisis estadistico de ANOVA a 1 FACTOR
  if (control_general == TRUE) {
    
    # Creacion del objeto MINI
    MINI <- input_datos
    MINI <- na.omit(MINI)
    SHAPIRO$MINI <- MINI
    
    # Creacion de VR
    VR <- as.vector(as.matrix(MINI))
    
    
    # Analisis para Normalidad de Shapiro-Wilks
    {
    
    ANALISIS <- list()
    ANALISIS$ORIGINAL <- list()
    ANALISIS$CONSULTORA <- list()
    
    ANALISIS$ORIGINAL <- shapiro.test(VR)
    
    # # Redondeo de valores obtenidos de R
    # Estadistico
    estadistico <- round2(ANALISIS$ORIGINAL$statistic, input_decimales)
    ANALISIS$CONSULTORA$estadistico <- estadistico
    
    # Valor p interno
    valor_p_interno <- round2(ANALISIS$ORIGINAL$p.value, input_decimales)
    ANALISIS$CONSULTORA$valor_p_interno <- valor_p_interno
    
    # Valor p externo
    if (valor_p_interno < 0.001) valor_p_externo <- "<<0.001" else valor_p_externo <- valor_p_interno
    ANALISIS$CONSULTORA$valor_p_externo <- valor_p_externo
    
    # Decision
    if(valor_p_interno >= input_alfa) decision <- "No Rechazo Ho" else if(valor_p_interno < input_alfa) decision <- "Rechazo Ho"
    ANALISIS$CONSULTORA$decision <- decision
    
    # Miniresumen para otros analisis(Dice SI o NO a la normalidad)
    if(valor_p_interno >= input_alfa) normalidad <- "SI" else if(valor_p_interno < input_alfa) normalidad <- "NO"
    ANALISIS$CONSULTORA$normalidad <- normalidad
    
    SHAPIRO$ANALISIS <- ANALISIS
    
    }
    #################################################################################################################################  
    
    
    # Resumen
    {
      nombres <- c("Variable", "Estadístico (W)", "Valor p", "Alfa", "Decisión")
      RESUMEN <- as.data.frame(matrix(NA, 1, length(nombres)))
      colnames(RESUMEN) <- nombres
      
      RESUMEN[1,1] <- SHAPIRO$DETALLE$nombre_VR        # Nombre de Variable
      RESUMEN[1,2] <- estadistico    # Estadistico                         
      RESUMEN[1,3] <- valor_p_externo      # Valor p externo
      RESUMEN[1,4] <- input_alfa           # Alfa
      RESUMEN[1,5] <- decision             # Decision
      
      SHAPIRO$ANALISIS$CONSULTORA$RESUMEN <- RESUMEN
      
    }
    ##############################################################################################
    
    
    # frase2_html: segun valor p
    {
      
      
      
      frase2_v1 <- c("No pudo obtenerse un valor p.")
      
      frase2_v2 <- c("El valor p es mayor que el valor de alfa=", input_alfa, ".","<br/>",
                     "No se rechaza la Ho.", "<br/>",
                     "La variable '", SHAPIRO$DETALLES$nombre_VR , "' presenta una distribución estadísticamente normal.","<br/>")
      
      
      frase2_v3 <- c("El valor p es igual que el valor de alfa=", input_alfa, ".","<br/>",
                     "No se rechaza la Ho.", "<br/>",
                     "La variable '", SHAPIRO$DETALLES$nombre_VR , "' presenta una distribución estadísticamente normal.","<br/>")
      
      
      frase2_v4 <- c("El valor p es menor que el valor de alfa=", input_alfa, ".","<br/>",
                     "Se rechaza la Ho.", "<br/>",
                     "La variable '", SHAPIRO$DETALLES$nombre_VR , "' no presenta una distribución estadísticamente normal.","<br/>")
      
      if(is.na(valor_p_interno) | is.null(valor_p_interno)) frase_elegida_shapiro <- frase2_v1 else if (valor_p_interno > input_alfa) frase_elegida_shapiro <- frase2_v2 else if (valor_p_interno == input_alfa) frase_elegida_shapiro <- frase2_v3 else if (valor_p_interno < input_alfa) frase_elegida_shapiro <- frase2_v4
      
      
      SHAPIRO$ANALISIS$CONSULTORA$frase2_shapiro_html <- frase_elegida_shapiro
      
    } # Fin Frase2_html
    #######################################
    
    
    
    SALIDA_ARMADA <- list()
    
    SALIDA_ARMADA$FRASE1 <- paste("Test de Normalidad para la variable '", SHAPIRO$DETALLES$nombre_VR, "'.", sep="")
    
    SALIDA_ARMADA$RESUMEN <- RESUMEN
    
    SALIDA_ARMADA$FRASE2 <- frase_elegida_shapiro
    
  } # FIn control_OK == TRUE
  ######################################################################################################################
  
  
  
  if (control_general == FALSE) {
    
    candado_externo <- FALSE
    contador_externo <- 0
    while(candado_externo == FALSE) {
      
      contador_externo <- contador_externo + 1
      
      if (control_OK[[contador_externo]] == FALSE) {
        
        
        SALIDA_ARMADA <- list()
        
        SALIDA_ARMADA$FRASE1 <- paste("Test de Normalidad para la variable '", SHAPIRO$DETALLES$nombre_VR, "'.", sep="")
        
        SALIDA_ARMADA$RESUMEN <- matrix("No puede realizarse la prueba de Normalidad de Shapiro-Wilks", 1, 5)
        
        SALIDA_ARMADA$FRASE2 <- frases_control[[contador_externo]]
        
        
        candado_externo <- TRUE 
      } # Fin control_OK == FALSE
      
      
      
      
    } # FIN SALIDA ARMADA
    ###################################################
    
    
  } # Fin control_general == FALSE
  #######################################################3
  
  SHAPIRO$SALIDA_ARMADA <- list()
  SHAPIRO$SALIDA_ARMADA <- SALIDA_ARMADA
  
  SHAPIRO
  
  
  
} # Fin function CHI_MASTER()



if ( 1 == 2) {
  
  BASE <- TIJERA(mtcars, 1)
  
  SHAPIRO <- SHAPIRO_MASTER(BASE)
  
  SHAPIRO$RESUMEN
  
  SHAPIRO$SALIDA_ARMADA
  
  input_datos <- BASE
  input_decimales <- 2
  input_alfa <- 0.05
  
} # Fin if