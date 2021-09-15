

PROP02_MASTER <- function(input_datos=NULL, input_exito1=1, input_exito2=2, input_decimales=2, input_alfa=0.05){
  
  
  # Detalle de los objetos de la salida...
  
  # Inicio
  PROP02 <- list()
  
  # Ordenamiento General
  PROP02$input <- list()           # Ingresan solo los elementos que son input de la funcion
  PROP02$DETALLES <- list()        # Nombre de las variables...
  PROP02$CONTROLES <- list()       # Detalle de los diferentes controles por los que paso la base de datos a trabajar...
  PROP02$MINI <- list()            # Va la MINIBASE de trabajo, y quien es VR y quien es FACTOR
  PROP02$ANALISIS <- list()        # Los analisis... un apartado sera "$ORIGINAL"... y otro será "$CONSULTORA"
  PROP02$SCRIPT <- list()          # Un apartado sera el script para obtener exactamente lo mismo que la salida cruda de R.
  PROP02$SALIDA_ARMADA <- list()   # La salida, que son objetos que ingresaran al Shiny...
  PROP02$CONTROL_GENERAL <- list() # Guardamos... para saber... si se cumplio o no con todos los controles...
  
  
  
  
  # # # INPUT
  {
  
  # Esta informacion es inalterable... 
  # O sea... se pueda o no realizar el analisis... esta informacion debe estar...
  
  PROP02$input$datos <- input_datos
  PROP02$input$exito1 <- input_exito1
  PROP02$input$exito2 <- input_exito2

  PROP02$input$decimales <- input_decimales
  PROP02$input$alfa <- input_alfa
  
  
  } # FIN INPUT
  ############################################################################
  
  
  
  # # # DETALLES
  {
    
    # Esta informacion es inalterable... 
    # O sea... se pueda o no realizar el analisis... esta informacion debe estar...
    
    PROP02$DETALLES$nombre_VC1 <- colnames(input_datos)[1]
    PROP02$DETALLES$nombre_VC2 <- colnames(input_datos)[2]
    
    
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
    frases_control[[3]] <- c("El objeto 'input_datos' debe tener al menos 2 filas. No puede realizarse el test de Proporciones.")
    frases_control[[4]] <- c("Al realizar na.omit() sobre la base de datos, quedan menos de 2 filas con informacion. No puede realizarse el test de Proporciones.")
    frases_control[[5]] <- c("La columna categórica1 ingresada debe tener 2 niveles. No puede realizarse el test de Proporciones.")
    frases_control[[6]] <- c("La columna categórica2 ingresada debe tener 2 niveles. No puede realizarse el test de Proporciones.")
    
    
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
      
      if (nrow(MINI_CONTROL) >= 2) control_OK[[4]] <- TRUE else control_general <- FALSE 
    }
    
    
    
    # Control 5...
    # La columna 1, que sera VC, con dos niveles
    # Como podría ingresar una columna de "0" y "1"... no pedimos que sea un factor o una variable categorica...
    # Sino que la transformamos nosotros en factor, y vemos si tiene 2 niveles.
    control_OK[[5]] <- FALSE
    names(control_OK[[5]]) <- c("VC con dos niveles")
    if (control_general == TRUE) {
      VC_CONTROL <- MINI_CONTROL[,1]
      VC_CONTROL <- na.omit(VC_CONTROL)
      VC_CONTROL <- as.factor(as.character(VC_CONTROL))
      
      niveles <- levels(VC_CONTROL)
      if (length(niveles) == 2) control_OK[[5]] <- TRUE else control_general <- FALSE
    }
    
    
    
    
    # Control 6...
    # La columna 2, que sera VC2, con dos niveles
    # Como podría ingresar una columna de "0" y "1"... no pedimos que sea un factor o una variable categorica...
    # Sino que la transformamos nosotros en factor, y vemos si tiene 2 niveles.
    control_OK[[6]] <- FALSE
    names(control_OK[[6]]) <- c("VC2 con dos niveles")
    if (control_general == TRUE) {
      VC2_CONTROL <- MINI_CONTROL[,2]
      VC2_CONTROL <- na.omit(VC2_CONTROL)
      VC2_CONTROL <- as.factor(as.character(VC2_CONTROL))
      
      niveles <- levels(VC2_CONTROL)
      if (length(niveles) == 2) control_OK[[6]] <- TRUE else control_general <- FALSE
    }
    
    
    
    
    
    PROP02$CONTROLES <- control_OK
    
    PROP02$CONTROL_GENERAL <- control_general
    
  } # # # FIN CONTROLES
  ###############################################################################################################################
  
  
  
  # Si supero todos los controles... podemos realizar el analisis estadistico de ANOVA a 1 FACTOR
  if (control_general == TRUE) {
    
    # Creacion del objeto MINI
    MINI <- input_datos
    PROP02$MINI <- MINI
    
    TABLA <- table(MINI)
    # Analisis para Proporciones... Armado por ARN
    {
    
    ANALISIS <- list()
    ANALISIS$ORIGINAL <- list()
    ANALISIS$CONSULTORA$VC1 <- list()
    ANALISIS$CONSULTORA$VC2 <- list()
    # Tomamos VC...
    # Creamos una tabla de frecuencias...
    # Obtenemos el n total...
    # Obtenemos las precuencias relativas...
    # Tomamos la primera de las dos categorias...
    # Y con esa proporcion, de la 1era categoria alfabetica... hacemos el test.
    
    # VC1
    FA1 <- TABLA[1,]
    N1 <- sum(TABLA[1,])
    FR1 <- FA1/N1
    
    p1 <- FR1[input_exito2]
    q1 <- 1- p1
    varianza1 <- (p1*q1)/N1
    

    # VC2
    FA2 <- TABLA[2,]
    N2 <- sum(TABLA[2,])
    FR2 <- FA2/N2

    p2 <- FR2[input_exito2]
    q2 <- 1- p2
    varianza2 <- (p2*q2)/N2


    z_obs <- ((p1 - p2) - 0) / sqrt(varianza1 + varianza2)

    
    if (z_obs < 0) valor_p <- pnorm(z_obs, 0, 1, lower.tail = T) else valor_p <- pnorm(z_obs, 0, 1, lower.tail = F)
    
    
    ORIGINAL <- list()
    ORIGINAL$VC1 <- list()
    ORIGINAL$VC2 <- list()
    
    ORIGINAL$VC1$FA <- FA1
    ORIGINAL$VC1$FR <- FR1
    ORIGINAL$VC1$p <- p1
    ORIGINAL$VC1$q <- q1
    ORIGINAL$VC1$N <- N1
    ORIGINAL$VC1$varianza <- varianza1
    

    ORIGINAL$VC2$FA <- FA2
    ORIGINAL$VC2$FR <- FR2
    ORIGINAL$VC2$p <- p2
    ORIGINAL$VC2$q <- q2
    ORIGINAL$VC2$N <- N2
    ORIGINAL$VC2$varianza <- varianza2
    
    ORIGINAL$z_obs <- z_obs
    ORIGINAL$valor_p <- valor_p
    
    ANALISIS$ORIGINAL <- ORIGINAL
    
    # Consultora
    
    # Valor p interno 
    
    valor_p_interno <- round(valor_p, input_decimales)
    if (valor_p_interno < 0.01) valor_p_externo <- "<<0.01" else valor_p_externo <- valor_p_interno
    if (valor_p_interno < input_alfa) decision <- "Rechazo Ho" else if (valor_p_interno >= input_alfa) decision <- "No rechazo Ho"
    
  
    
    CONSULTORA <- list()
    CONSULTORA$VC1 <- list()
    CONSULTORA$VC2 <- list()
    
    CONSULTORA$VC1$FA <- FA1
    CONSULTORA$VC1$FR <- round(FR1, input_decimales)
    CONSULTORA$VC1$p <- round(p1, input_decimales)
    CONSULTORA$VC1$q <- round(q1, input_decimales)
    CONSULTORA$VC1$N <- round(N1, input_decimales)
    CONSULTORA$VC1$varianza <- round(varianza1, input_decimales)
    
    
    
    CONSULTORA$VC$FA <- FA2
    CONSULTORA$VC2$FR <- round(FR2, input_decimales)
    CONSULTORA$VC2$p <- round(p2, input_decimales)
    CONSULTORA$VC2$q <- round(q2, input_decimales)
    CONSULTORA$VC2$N <- round(N2, input_decimales)
    CONSULTORA$VC2$varianza <- round(varianza2, input_decimales)
    
    CONSULTORA$z_obs <- round(z_obs, input_decimales)
    
    
    CONSULTORA$valor_p_interno <- valor_p_interno
    CONSULTORA$valor_p_externo <- valor_p_externo
    CONSULTORA$decision <- decision
    
  
    
    # frase2_html: segun valor p
    {
    
    
    
    frase2_v1 <- c("No pudo obtenerse un valor p.")
    
    frase2_v2 <- c("El valor p es mayor que el valor de alfa=", input_alfa, ".","<br/>",
                   "No se rechaza la Ho.", "<br/>",
                   #"La variable '", SHAPIRO$DETALLES$nombre_VC , "' presenta una distribución estadísticamente normal.","<br/>"
                   "Las proporciones seleccionadas son estadísticamente iguales.", "<br/>")
    
    
    frase2_v3 <- c("El valor p es igual que el valor de alfa=", input_alfa, ".","<br/>",
                   "No se rechaza la Ho.", "<br/>",
                   #"La variable '", SHAPIRO$DETALLES$nombre_VR , "' presenta una distribución estadísticamente normal.","<br/>"
                   "Las proporciones seleccionadas son estadísticamente iguales.", "<br/>")
    
    frase2_v4 <- c("El valor p es menor que el valor de alfa=", input_alfa, ".","<br/>",
                   "Se rechaza la Ho.", "<br/>",
                   #"La variable '", SHAPIRO$DETALLES$nombre_VR , "' no presenta una distribución estadísticamente normal.","<br/>"
                   "Las proporciones seleccionadas son estadísticamente diferentes.", "<br/>")
    
    if(is.na(valor_p_interno) | is.null(valor_p_interno)) frase_elegida_prop <- frase2_v1 else if (valor_p_interno > input_alfa) frase_elegida_prop <- frase2_v2 else if (valor_p_interno == input_alfa) frase_elegida_prop <- frase2_v3 else if (valor_p_interno < input_alfa) frase_elegida_prop <- frase2_v4
    
    
    CONSULTORA$frase2 <- frase_elegida_prop
    
    } # Fin Frase2_html
    #######################################
    
      
    nombres <- c("Grupos", "Éxito", "Prop Obs", "Fracción", "Valor Z", "Valor p", "Alfa", "Decisión")
    RESUMEN <- matrix(NA, 2, length(nombres))
    colnames(RESUMEN) <- nombres
    
    RESUMEN[,1] <- levels(as.factor(MINI[,1]))
  #  RESUMEN[2,1] <- colnames(MINI)[2]
    
    RESUMEN[1,2] <- input_exito2
    RESUMEN[2,2] <- input_exito2
    
    RESUMEN[1,3] <- CONSULTORA$VC1$p
    RESUMEN[2,3] <- CONSULTORA$VC2$p
    
    
    RESUMEN[1,4] <- paste(FA1[input_exito2], "/", N1, sep="")
    RESUMEN[2,4] <- paste(FA2[input_exito2], "/", N2, sep="")
    
    
    RESUMEN[1,5] <- CONSULTORA$z_obs
    RESUMEN[1,6] <- CONSULTORA$valor_p_externo
    RESUMEN[1,7] <- input_alfa
    RESUMEN[1,8] <- decision
    
    RESUMEN[2,5] <- ""
    RESUMEN[2,6] <- ""
    RESUMEN[2,7] <- ""
    RESUMEN[2,8] <- ""
    
    
    CONSULTORA$RESUMEN <- RESUMEN
    
    
    PROP02$ANALISIS$ORIGINAL <- ORIGINAL
    PROP02$ANALISIS$CONSULTORA <- CONSULTORA
    
    
    }
    #################################################################################################################################  
    
    
    
   
    
    
    
    SALIDA_ARMADA <- list()
    
    SALIDA_ARMADA$FRASE1 <- "Test de Proporciones"
    
    SALIDA_ARMADA$RESUMEN <- RESUMEN
    
    SALIDA_ARMADA$FRASE2 <- frase_elegida_prop
    
  } # FIn control_OK == TRUE
  ######################################################################################################################
  
  
  
  if (control_general == FALSE) {
    
    candado_externo <- FALSE
    contador_externo <- 0
    while(candado_externo == FALSE) {
      
      contador_externo <- contador_externo + 1
      
      if (control_OK[[contador_externo]] == FALSE) {
        
        
        SALIDA_ARMADA <- list()
        
        SALIDA_ARMADA$FRASE1 <- paste("Test de Proporciones para la variable '", PROP02$DETALLES$nombre_VC, "'.", sep="")
        
        SALIDA_ARMADA$RESUMEN <- matrix("No puede realizarse el test de proporciones.", 1, 8)
        
        SALIDA_ARMADA$FRASE2 <- frases_control[[contador_externo]]
        
        
        candado_externo <- TRUE 
      } # Fin control_OK == FALSE
      
      
      
      
    } # FIN SALIDA ARMADA
    ###################################################
    
    
  } # Fin control_general == FALSE
  #######################################################3
  
  PROP02$SALIDA_ARMADA <- list()
  PROP02$SALIDA_ARMADA <- SALIDA_ARMADA
  
  PROP02
  
  
  
} # Fin function CHI_MASTER()



if ( 1 == 2) {
  
  BASE <- TIJERA(mtcars, 8)
  
  input_datos <- mtcars[,c(8,9)]
  input_exito1=1
  input_exito2=2
  input_decimales=2
  input_alfa=0.05
  
  
  PROP02 <- PROP02_MASTER(input_datos=input_datos)
  
  PROP02$RESUMEN
  
  PROP02$SALIDA_ARMADA
  
  
} # Fin if