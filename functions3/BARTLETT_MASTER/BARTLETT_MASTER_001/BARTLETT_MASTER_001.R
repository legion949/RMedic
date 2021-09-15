

BARTLETT_MASTER <- function(input_datos=NULL, input_decimales=2, input_alfa=0.05, input_ingreso="clasic"){
  
  
  
  
  # Inicio
  BARTLETT <- list()
  
  # Ordenamiento General
  BARTLETT$input <- list()           # Ingresan solo los elementos que son input de la funcion
  BARTLETT$DETALLES <- list()        # Nombre de las variables...
  BARTLETT$CONTROLES <- list()       # Detalle de los diferentes controles por los que paso la base de datos a trabajar...
  BARTLETT$MINI <- list()            # Va la MINIBASE de trabajo, y quien es VR y quien es FACTOR
  BARTLETT$ANALISIS <- list()        # Los analisis... un apartado sera "$ORIGINAL"... y otro será "$CONSULTORA"
  BARTLETT$SCRIPT <- list()          # Un apartado sera el script para obtener exactamente lo mismo que la salida cruda de R.
  BARTLETT$SALIDA_ARMADA <- list()   # La salida, que son objetos que ingresaran al Shiny...
  BARTLETT$CONTROL_GENERAL <- list() # Guardamos... para saber... si se cumplio o no con todos los controles...
  
  
  
  
  # # # INPUT
  {
  
  # Esta informacion es inalterable... 
  # O sea... se pueda o no realizar el analisis... esta informacion debe estar...
  
  BARTLETT$input$datos <- input_datos
  BARTLETT$input$decimales <- input_decimales
  BARTLETT$input$alfa <- input_alfa
  BARTLETT$input$input_ingreso <- input_ingreso
  
  } # FIN INPUT
  ############################################################################
  
  
  # Cosas de "clasic" pero sin analisis...
  if (input_ingreso == "clasic") {
    
    
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
      frases_control[[3]] <- c("El objeto 'input_datos' debe tener al menos 1 filas. No puede realizarse el test de Homogeneidad de Varianzas de Bartlett.")
      frases_control[[4]] <- c("La columna 1 debe ser un objeto numerico (X1). No puede realizarse el test de Homogeneidad de Varianzas de Bartlett.")
      frases_control[[5]] <- c("La columna 2 debe ser un objeto numerico (X2). No puede realizarse el test de Homogeneidad de Varianzas de Bartlett.") 
      frases_control[[6]] <- c("La columna 1 (X1) solo presenta celdas vacias. No puede realizarse el test de Homogeneidad de Varianzas de Bartlett.")
      frases_control[[7]] <- c("La columna 2 (X2) solo presenta celdas vacias. No puede realizarse el test de Homogeneidad de Varianzas de Bartlett.")
      frases_control[[8]] <- c("La columna 1 (X1) es constante. Presentan varianza cero. No puede realizarse el test de Homogeneidad de Varianzas de Bartlett.")
      frases_control[[9]] <- c("La columna 2 (X2) es constante. Presentan varianza cero. No puede realizarse el test de Homogeneidad de Varianzas de Bartlett.")
      
      
      
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
      names(control_OK[[3]]) <- c("Al menos 1 fila de input_datos")
      if (control_general == TRUE) {
        if (nrow(input_datos) >= 1) control_OK[[3]] <- TRUE else control_general <- FALSE 
      }
      
      
      
      
      
      # Control 4...
      # La columna 1, que sera X1, debe ser numérica.
      control_OK[[4]] <- FALSE
      names(control_OK[[4]]) <- c("X1 es numerica")
      if (control_general == TRUE) {
        X1_CONTROL <- input_datos[,1]
        if (is.numeric(X1_CONTROL)) control_OK[[4]] <- TRUE else control_general <- FALSE
      }
      
      
      
      
      # Control 5...
      # La columna 1, que sera X1, debe ser numérica.
      control_OK[[5]] <- FALSE
      names(control_OK[[5]]) <- c("X2 es numerica")
      if (control_general == TRUE) {
        X2_CONTROL <- input_datos[,2]
        if (is.numeric(X2_CONTROL)) control_OK[[5]] <- TRUE else control_general <- FALSE
      }
      
      
      
      
      
      # Control 6...
      # Los datos no deben ser constantes... la varianza debe ser distinta de cero.
      control_OK[[6]] <- FALSE
      names(control_OK[[6]]) <- c("Varianza de X1 distinta de cero")
      if (control_general == TRUE) {
        
        X1_CONTROL <- input_datos[,1]
        X1_CONTROL <- na.omit(X1_CONTROL)
        
        if (length(X1_CONTROL) > 0) control_OK[[6]] <- TRUE else control_general <- FALSE
      }
      
      
      
      # Control 7...
      # Los datos no deben ser constantes... la varianza debe ser distinta de cero.
      control_OK[[7]] <- FALSE
      names(control_OK[[7]]) <- c("Varianza de X1 distinta de cero")
      if (control_general == TRUE) {
        
        X2_CONTROL <- input_datos[,2]
        X2_CONTROL <- na.omit(X2_CONTROL)
        
        if (length(X2_CONTROL) > 0) control_OK[[7]] <- TRUE else control_general <- FALSE
      }
      
      
      # Control 8...
      # Los datos no deben ser constantes... la varianza debe ser distinta de cero.
      control_OK[[8]] <- FALSE
      names(control_OK[[8]]) <- c("Varianza de X1 distinta de cero")
      if (control_general == TRUE) {
        
        X1_CONTROL <- input_datos[,1]
        varianza_control <- var(X1_CONTROL)
        
        if (varianza_control != 0) control_OK[[8]] <- TRUE else control_general <- FALSE
      }
      
      
      
      
      # Control 9...
      # Los datos no deben ser constantes... la varianza debe ser distinta de cero.
      control_OK[[9]] <- FALSE
      names(control_OK[[9]]) <- c("Varianza de X2 distinta de cero")
      if (control_general == TRUE) {
        
        X2_CONTROL <- input_datos[,2]
        varianza_control <- var(X2_CONTROL)
        
        if (varianza_control != 0) control_OK[[9]] <- TRUE else control_general <- FALSE
      }
      
      
      
      BARTLETT$CONTROLES <- control_OK
      
      BARTLETT$CONTROL_GENERAL <- control_general
      
    } # # # FIN CONTROLES
    ###############################################################################################################################
    
    
    if (control_general == TRUE) {
      
      
      # # # DETALLES
      {
        
        # Esta informacion es inalterable... 
        # O sea... se pueda o no realizar el analisis... esta informacion debe estar...
        
        BARTLETT$DETALLES$nombre_X1 <- colnames(input_datos)[1]
        BARTLETT$DETALLES$nombre_X2 <- colnames(input_datos)[2]
        
        
      } # FIN DETALLES
      ############################################################################
      
      # MINI: Paso de forma "classic" a "anova"
      {
        
        X1 <- input_datos[,1]
        X2 <- input_datos[,2]
        
        X1 <- na.omit(X1)
        X2 <- na.omit(X2)
        
        VR <- c(X1, X2)
        FACTOR <- c(rep("X1", length(X1)), rep("X2", length(X2)))
        
        MINI <- cbind(VR, FACTOR)
        BARTLETT$MINI <- MINI
      } # Fin paso
      #######################################################
      
      
      
      
    } # Fin if control_general == TRUE
    ##############################################################################
    
    
    
  } # FIN Cosas de "clasic" pero sin analisis...
  #################################################################################################
  
  
  
  # Cosas de "anova" pero sin analisis...
  if (input_ingreso == "anova") {
    
    
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
      frases_control[[3]] <- c("El objeto 'input_datos' debe tener al menos 1 filas. No puede realizarse el test de Homogeneidad de Varianzas de Bartlett.")
      frases_control[[4]] <- c("Al aplicarse na.omit() sobre el objeto 'input_datos' se tienen 0 filas. No puede realizarse el test de Homogeneidad de Varianzas de Bartlett.")
      
      frases_control[[5]] <- c("La columna 1 debe ser un objeto numerico (VR). No puede realizarse el test de Homogeneidad de Varianzas de Bartlett.")
      frases_control[[6]] <- c("La columna 2 debe ser tener exactamente dos niveles de grupo o factor. No puede realizarse el test de Homogeneidad de Varianzas de Bartlett.") 
      frases_control[[7]] <- c("Se deben tener al menos 2 repeticiones de cada nivel de grupo o factor. No puede realizarse el test de Homogeneidad de Varianzas de Bartlett.")
      frases_control[[8]] <- c("Al menos un grupo presenta una varianza igual a cero. Presentan varianza cero. No puede realizarse el test de Homogeneidad de Varianzas de Bartlett.")
      
      
      
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
      names(control_OK[[3]]) <- c("Al menos 1 fila de input_datos")
      if (control_general == TRUE) {
        if (nrow(input_datos) >= 1) control_OK[[3]] <- TRUE else control_general <- FALSE 
      }
      
      
      
      
      
      # Control 4...
      # Leugo de aplicar na.omit() debe tener al menos 1 fila.
      control_OK[[4]] <- FALSE
      names(control_OK[[4]]) <- c("Al menos 1 fila de na.omit(input_datos)")
      if (control_general == TRUE) {
        
        MINI_CONTROL <- na.omit(input_datos)
        if (nrow(MINI_CONTROL) > 0) control_OK[[4]] <- TRUE else control_general <- FALSE 
      }
      
      
      
      # Control 5...
      # La columna 1, que sera X1, debe ser numerica.
      control_OK[[5]] <- FALSE
      names(control_OK[[5]]) <- c("VR es numerico.")
      if (control_general == TRUE) {
        
        VR_CONTROL <- MINI_CONTROL[,1]
        
        
        if (is.numeric(VR_CONTROL)) control_OK[[5]] <- TRUE else control_general <- FALSE
      }
      
      
      
      # Control 6...
      # El FACTOR debe tener exactamente 2 niveles
      control_OK[[6]] <- FALSE
      names(control_OK[[6]]) <- c("FACTOR con al menos 2 niveles")
      if (control_general == TRUE) {
        
        FACTOR_CONTROL <- as.factor(as.character(MINI_CONTROL[,2]))
        niveles_CONTROL <- levels(FACTOR_CONTROL)
        
        if (length(niveles_CONTROL) >= 2) control_OK[[6]] <- TRUE else control_general <- FALSE
      }
      
      
      
      
      
      # Control 7...
      # Ambos niveles de grupo o factor deben tener al menos 2 repeticiones.
      control_OK[[7]] <- FALSE
      names(control_OK[[7]]) <- c("Ambos niveles de grupo o factor deben tener al menos 2 repeticiones")
      if (control_general == TRUE) {
        
        
        FACTOR_CONTROL <- as.factor(as.character(MINI_CONTROL[,2]))
        repeticiones <- table(FACTOR_CONTROL)
        detec <- repeticiones >= 2
        suma <- sum(as.numeric(detec))
        
        if (suma == length(detec)) control_OK[[7]] <- TRUE else control_general <- FALSE
      }
      
      
      
      # Control 8...
      # Los datos no deben ser constantes... la varianza debe ser distinta de cero.
      control_OK[[8]] <- FALSE
      names(control_OK[[8]]) <- c("Varianzas distintas de cero")
      if (control_general == TRUE) {
        
        VR_CONTROL <- MINI_CONTROL[,1]
        FACTOR_CONTROL <- as.factor(as.character(MINI_CONTROL[,2]))
        
        varianzas_CONTROL <- tapply(VR_CONTROL, FACTOR_CONTROL, var)
        
        detec <- varianzas_CONTROL > 0
        suma <- sum(as.numeric(detec))
        
        if (suma == length(detec)) control_OK[[8]] <- TRUE else control_general <- FALSE
      }
      
      
      
      
      BARTLETT$CONTROLES <- control_OK
      
      BARTLETT$CONTROL_GENERAL <- control_general
      
    } # # # FIN CONTROLES
    ###############################################################################################################################
    
    
    if (control_general == TRUE) {
      
      
      # # # DETALLES
      {
        
        # Esta informacion es inalterable... 
        # O sea... se pueda o no realizar el analisis... esta informacion debe estar...
        
        BARTLETT$DETALLES$nombre_VR <- colnames(input_datos)[1]
        BARTLETT$DETALLES$nombre_FACTOR <- colnames(input_datos)[2]
        
        
      } # FIN DETALLES
      ############################################################################
      
      # MINI
      {
        
        MINI <- input_datos
        MINI <- na.omit(MINI)
        BARTLETT$MINI <- MINI
        
        
      } # Fin paso
      #######################################################
      
      
      
      
    } # Fin if control_general == TRUE
    ##############################################################################
    
  } # FIN Cosas de "clasic" pero sin analisis...
  #################################################################################################
  
  
  
  # Ahora... todo el resto de las cosas y los analisis...
  # Si es que corresponde...
  
  if (control_general == TRUE) {
    
    # Elementos VR y FACTOR
    {
      VR <- MINI[,1]
      FACTOR <- as.factor(as.character(MINI[,2]))
      
    } # Fin Elementos VR y FACTOR
    ########################################################
    
    
    
    # Test de Homogeneidad de Bartlett
    {
      ANALISIS <- list()
      ANALISIS$ORIGINAL <- list()
      ANALISIS$CONSULTORA <- list()
      
      
      ANALISIS$ORIGINAL <-   bartlett.test (VR, FACTOR)
      
      
      
      
      
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
      
      
      # Miniresumen para otros analisis(Dice SI o NO a la homogeneidad)
      if(valor_p_interno >= input_alfa) homogeneidad <- "SI" else if(valor_p_interno < input_alfa) homogeneidad <- "NO"
      ANALISIS$CONSULTORA$homogeneidad <- homogeneidad
      
      
      BARTLETT$ANALISIS <- ANALISIS
      
    } # FIN ANALISIS de Homogeneidad de Varianzas de Barlett
    ################################################################################################################################
    
    
    
    # Resumen
    {
      
      if (input_ingreso == "clasic") {  
        nombres <- c("X1", "X2", "Estadístico (K^2)", "Valor p", "Alfa", "Decisión")
        RESUMEN <- as.data.frame(matrix(NA, 1, length(nombres)))
        colnames(RESUMEN) <- nombres
        
        RESUMEN[1,1] <- BARTLETT$DETALLES$nombre_X1      # Nombre de Variable1
        RESUMEN[1,2] <- BARTLETT$DETALLES$nombre_X2      # Nombre de Variable2
        RESUMEN[1,3] <- BARTLETT$ANALISIS$CONSULTORA$estadistico            # Estadistico                         
        RESUMEN[1,4] <- BARTLETT$ANALISIS$CONSULTORA$valor_p_externo      # Valor p externo
        RESUMEN[1,5] <- input_alfa           # Alfa
        RESUMEN[1,6] <- BARTLETT$ANALISIS$CONSULTORA$decision             # Decision
        
        
      } # Fin Resumen para == "clasic
      ##################################################################################
      
      if (input_ingreso == "anova") {  
        nombres <- c("VR", "FACTOR", "Estadístico (K^2)", "Valor p", "Alfa", "Decisión")
        RESUMEN <- as.data.frame(matrix(NA, 1, length(nombres)))
        colnames(RESUMEN) <- nombres
        
        RESUMEN[1,1] <- BARTLETT$DETALLES$nombre_VR      # Nombre de Variable1
        RESUMEN[1,2] <- BARTLETT$DETALLES$nombre_FACTOR      # Nombre de Variable2
        RESUMEN[1,3] <- BARTLETT$ANALISIS$CONSULTORA$estadistico            # Estadistico                         
        RESUMEN[1,4] <- BARTLETT$ANALISIS$CONSULTORA$valor_p_externo      # Valor p externo
        RESUMEN[1,5] <- input_alfa           # Alfa
        RESUMEN[1,6] <- BARTLETT$ANALISIS$CONSULTORA$decision             # Decision
        
        
      } # Fin Resumen para == "clasic
      ##################################################################################
      
      
      BARTLETT$CONSULTORA$RESUMEN <- RESUMEN   
      
    } # FIN RESUMEN
    #################################################################
    
    
    
    
    # frase2_html: segun valor p
    {
      
      
      if (input_ingreso == "clasic") {  
        
        frase2_v1 <- c("No pudo obtenerse un valor p.")
        
        frase2_v2 <- c("El valor p es mayor que el valor de alfa=", input_alfa, ".","<br/>",
                       "No se rechaza la Ho.", "<br/>",
                       "Las variables '", BARTLETT$DETALLES$nombre_X1 , "' y '", BARTLETT$DETALLES$nombre_X2, "' presentan varianzas estadísticamente homogéneas.","<br/>")
        
        
        frase2_v3 <- c("El valor p es igual que el valor de alfa=", input_alfa, ".","<br/>",
                       "No se rechaza la Ho.", "<br/>",
                       "Las variables '", BARTLETT$DETALLES$nombre_X1 , "' y '", BARTLETT$DETALLES$nombre_X2, "' presentan varianzas estadísticamente homogéneas.","<br/>")
        
        
        
        frase2_v4 <- c("El valor p es menor que el valor de alfa=", input_alfa, ".","<br/>",
                       "Se rechaza la Ho.", "<br/>",
                       "Las variables '", BARTLETT$DETALLES$nombre_X1 , "' y '", BARTLETT$DETALLES$nombre_X2, "' no presentan varianzas estadísticamente homogéneas.","<br/>")
        
        
        if(is.na(valor_p_interno) | is.null(valor_p_interno)) frase_elegida_bartlett <- frase2_v1 else if (valor_p_interno > input_alfa) frase_elegida_bartlett <- frase2_v2 else if (valor_p_interno == input_alfa) frase_elegida_bartlett <- frase2_v3 else if (valor_p_interno < input_alfa) frase_elegida_bartlett <- frase2_v4
        
        
      }
      
      
      
      
      if (input_ingreso == "anova") {  
        
        frase2_v1 <- c("No pudo obtenerse un valor p.")
        
        frase2_v2 <- c("El valor p es mayor que el valor de alfa=", input_alfa, ".","<br/>",
                       "No se rechaza la Ho.", "<br/>",
                       "La variable '", BARTLETT$DETALLES$nombre_VR , "' presenta varianzas estadísticamente homogéneas para todos los niveles de '", BARTLETT$DETALLES$nombre_FACTOR, "'.","<br/>")
        
        
        frase2_v3 <- c("El valor p es igual que el valor de alfa=", input_alfa, ".","<br/>",
                       "No se rechaza la Ho.", "<br/>",
                       "La variables '", BARTLETT$DETALLES$nombre_VR , "' presenta al menos una varianza estadísticamente heterogénea para los niveles de '", BARTLETT$DETALLES$nombre_FACTOR, "'.","<br/>")
        
        
        frase2_v4 <- c("El valor p es menor que el valor de alfa=", input_alfa, ".","<br/>",
                       "No se rechaza la Ho.", "<br/>",
                       "La variable '", BARTLETT$DETALLES$nombre_VR , "' presenta al menos 1 varianza estadísticamente diferente dentro de los niveles de '", BARTLETT$DETALLES$nombre_FACTOR, "'.","<br/>")
        
        
        if(is.na(valor_p_interno) | is.null(valor_p_interno)) frase_elegida_bartlett <- frase2_v1 else if (valor_p_interno > input_alfa) frase_elegida_bartlett <- frase2_v2 else if (valor_p_interno == input_alfa) frase_elegida_bartlett <- frase2_v3 else if (valor_p_interno < input_alfa) frase_elegida_bartlett <- frase2_v4
        
        
      }
      
      
      
      BARTLETT$ANALISIS$CONSULTORA$frase2_bartlett_html <- frase_elegida_bartlett
      
    } # Fin Frase2_html
    #######################################
    
    
    
    SALIDA_ARMADA <- list()
    
    if (input_ingreso == "clasic")  SALIDA_ARMADA[[1]] <- paste("Test de Homogeneidad para las variables' ", BARTLETT$DETALLES$nombre_X1, "' y '", BARTLETT$DETALLES$nombre_X2, "'.", sep="")
    
    if (input_ingreso == "anova")  SALIDA_ARMADA[[1]] <- paste("Test de Homogeneidad para las variables' ", BARTLETT$DETALLES$nombre_VR, "' según los niveles de la variable categórica '",BARTLETT$DETALLES$nombre_FACTOR, "'.", sep="")
    
    SALIDA_ARMADA[[2]] <- RESUMEN
    
    SALIDA_ARMADA[[3]] <- frase_elegida_bartlett
    
    
    
    BARTLETT$SALIDA_ARMADA <- list()
    BARTLETT$SALIDA_ARMADA <- SALIDA_ARMADA
    
    
  }
  
  # Si no corresponde, modificamos la salida armada solamente...
  if (control_general == FALSE) { 
    
    candado_externo <- FALSE
    contador_externo <- 0
    while(candado_externo == FALSE) {
      
      contador_externo <- contador_externo + 1
      
      if (control_OK[[contador_externo]] == FALSE) {
        
        SALIDA_ARMADA <- list()
        
        if (input_ingreso == "clasic")  SALIDA_ARMADA[[1]] <- paste("Test de Homogeneidad para las variables' ", BARTLETT$DETALLES$nombre_X1, "' y '",BARTLETT$DETALLES$nombre_X2, "'.", sep="")
        
        if (input_ingreso == "anova")  SALIDA_ARMADA[[1]] <- paste("Test de Homogeneidad para las variables' ", BARTLETT$DETALLES$nombre_VR, "' y '",BARTLETT$DETALLES$nombre_FACTOR, "'.", sep="")
        
        SALIDA_ARMADA[[2]] <- matrix("No puede realizarse la prueba de Homogeneidad de Varianzas de Bartlett", 1, 5)
        
        SALIDA_ARMADA[[3]] <- frases_control[[contador_externo]]
        
        
        candado_externo <- TRUE 
        
      }
    }
    
    BARTLETT$SALIDA_ARMADA <- list()
    BARTLETT$SALIDA_ARMADA <- SALIDA_ARMADA
    
  } # Fin == FALSE
  ##########################################################################################
  
  
  
  BARTLETT
  
  
} # Fin function BARTLETT



if ( 1 == 2) {
  
  BASE <- mtcars[,c(3,8)]
  
  BARTLETT <- BARTLETT_MASTER(BASE, input_decimales=2, input_alfa=0.05, input_ingreso="clasic")
  
  BARTLETT$RESUMEN
  
  BARTLETT$SALIDA_ARMADA[[2]]
  
  
} # Fin if