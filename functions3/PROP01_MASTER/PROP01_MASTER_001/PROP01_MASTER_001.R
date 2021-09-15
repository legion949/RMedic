

PROP01_MASTER <- function(input_datos=NULL, input_exito=1,input_ref=0.5, input_decimales=2, input_alfa=0.05){
  
  
  # Detalle de los objetos de la salida...
  
  # Inicio
  PROP01 <- list()
  
  # Ordenamiento General
  PROP01$input <- list()           # Ingresan solo los elementos que son input de la funcion
  PROP01$DETALLES <- list()        # Nombre de las variables...
  PROP01$CONTROLES <- list()       # Detalle de los diferentes controles por los que paso la base de datos a trabajar...
  PROP01$MINI <- list()            # Va la MINIBASE de trabajo, y quien es VR y quien es FACTOR
  PROP01$ANALISIS <- list()        # Los analisis... un apartado sera "$ORIGINAL"... y otro será "$CONSULTORA"
  PROP01$SCRIPT <- list()          # Un apartado sera el script para obtener exactamente lo mismo que la salida cruda de R.
  PROP01$SALIDA_ARMADA <- list()   # La salida, que son objetos que ingresaran al Shiny...
  PROP01$CONTROL_GENERAL <- list() # Guardamos... para saber... si se cumplio o no con todos los controles...
  
  
  
  
  # # # INPUT
  {
  
  # Esta informacion es inalterable... 
  # O sea... se pueda o no realizar el analisis... esta informacion debe estar...
  
  PROP01$input$datos <- input_datos
  PROP01$input$exito <- input_exito
  PROP01$input$ref <- input_ref
  PROP01$input$decimales <- input_decimales
  PROP01$input$alfa <- input_alfa
  
  
  } # FIN INPUT
  ############################################################################
  
  
  
  # # # DETALLES
  {
    
    # Esta informacion es inalterable... 
    # O sea... se pueda o no realizar el analisis... esta informacion debe estar...
    
    PROP01$DETALLES$nombre_VC <- colnames(input_datos)[1]
    
    
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
    frases_control[[3]] <- c("El objeto 'input_datos' debe tener al menos 2 filas. No puede realizarse el test de Proporciones.")
    frases_control[[4]] <- c("Al realizar na.omit() sobre la base de datos, quedan menos de 2 filas con informacion. No puede realizarse el test de Proporciones.")
    frases_control[[5]] <- c("La columna categórica ingresada debe tener 2 niveles. No puede realizarse el test de Proporciones.")

    
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
    names(control_OK[[5]]) <- c("VC con dos niveles")
    if (control_general == TRUE) {
      VC_CONTROL <- MINI_CONTROL[,1]
      VC_CONTROL <- as.factor(as.character(VC_CONTROL))
      
      niveles <- levels(VC_CONTROL)
      if (length(niveles) == 2) control_OK[[5]] <- TRUE else control_general <- FALSE
    }
    
    
    
    
    
    
    PROP01$CONTROLES <- control_OK
    
    PROP01$CONTROL_GENERAL <- control_general
    
  } # # # FIN CONTROLES
  ###############################################################################################################################
  
  
  
  # Si supero todos los controles... podemos realizar el analisis estadistico de ANOVA a 1 FACTOR
  if (control_general == TRUE) {
    
    # Creacion del objeto MINI
    MINI <- input_datos
    MINI <- na.omit(MINI)
    PROP01$MINI <- MINI
    
    # Creacion de VC
    VC <- as.vector(as.matrix(MINI))
    
    
    # Analisis para Proporciones... Armado por ARN
    {
    
    ANALISIS <- list()
    ANALISIS$ORIGINAL <- list()
    ANALISIS$CONSULTORA <- list()
    
    # Tomamos VC...
    # Creamos una tabla de frecuencias...
    # Obtenemos el n total...
    # Obtenemos las precuencias relativas...
    # Tomamos la primera de las dos categorias...
    # Y con esa proporcion, de la 1era categoria alfabetica... hacemos el test.
    
    FA <- table(VC)
    N <- sum(FA)
    FR <- FA/N
    
    p <- FR[input_exito]
    q <- 1- p
    prop_poblacional <- input_ref
    
    varianza <- sqrt((p*q)/N)
    
    z_obs <- (p - prop_poblacional)/varianza
    
    if (z_obs < 0) valor_p <- pnorm(z_obs, 0, 1, lower.tail = T) else valor_p <- pnorm(z_obs, 0, 1, lower.tail = F)
    
    
    ORIGINAL <- list()
    ORIGINAL$FA <- FA
    ORIGINAL$FR <- FR
    ORIGINAL$p <- p
    ORIGINAL$N <- N
    ORIGINAL$prop_poblacional <- prop_poblacional
    ORIGINAL$varianza <- varianza
    ORIGINAL$z_obs <- z_obs
    ORIGINAL$valor_p <- valor_p
    
    ANALISIS$ORIGINAL <- ORIGINAL
    
    # Consultora
    
    # Valor p interno 
    
    valor_p_interno <- round2(valor_p, input_decimales)
    if (valor_p_interno < 0.01) valor_p_externo <- "<<0.01" else valor_p_externo <- valor_p_interno
    if (valor_p_interno < input_alfa) decision <- "Rechazo Ho" else if (valor_p_interno >= input_alfa) decision <- "No rechazo Ho"
    
  
    
    CONSULTORA <- list()
    CONSULTORA$FA <- FA
    CONSULTORA$FR <- round2(FR, input_decimales)
    CONSULTORA$p <- round2(p, input_decimales)
    CONSULTORA$N <- N
    CONSULTORA$prop_poblacional <- round2(prop_poblacional, input_decimales)
    CONSULTORA$varianza <- round2(varianza, input_decimales)
    CONSULTORA$z_obs <- round2(z_obs, input_decimales)
    CONSULTORA$valor_p_interno <- valor_p_interno
    CONSULTORA$valor_p_externo <- valor_p_externo
    CONSULTORA$decision <- decision
    
  
    
    # frase2_html: segun valor p
    {
    
    
    
    frase2_v1 <- c("No pudo obtenerse un valor p.")
    
    frase2_v2 <- c("El valor p es mayor que el valor de alfa=", input_alfa, ".","<br/>",
                   "No se rechaza la Ho.", "<br/>",
                   #"La variable '", SHAPIRO$DETALLES$nombre_VC , "' presenta una distribución estadísticamente normal.","<br/>"
                   "La proporción observada es estadísticamente igual al valor bajo hipótesis.", "<br/>")
    
    
    frase2_v3 <- c("El valor p es igual que el valor de alfa=", input_alfa, ".","<br/>",
                   "No se rechaza la Ho.", "<br/>",
                   #"La variable '", SHAPIRO$DETALLES$nombre_VR , "' presenta una distribución estadísticamente normal.","<br/>"
                   "La proporción observada es estadísticamente igual al valor bajo hipótesis.", "<br/>")
    
    frase2_v4 <- c("El valor p es menor que el valor de alfa=", input_alfa, ".","<br/>",
                   "Se rechaza la Ho.", "<br/>",
                   #"La variable '", SHAPIRO$DETALLES$nombre_VR , "' no presenta una distribución estadísticamente normal.","<br/>"
                   "La proporción observada es estadísticamente distinta del valor bajo hipótesis.", "<br/>")
    
    if(is.na(valor_p_interno) | is.null(valor_p_interno)) frase_elegida_prop <- frase2_v1 else if (valor_p_interno > input_alfa) frase_elegida_prop <- frase2_v2 else if (valor_p_interno == input_alfa) frase_elegida_prop <- frase2_v3 else if (valor_p_interno < input_alfa) frase_elegida_prop <- frase2_v4
    
    
    CONSULTORA$frase2 <- frase_elegida_prop
    
    } # Fin Frase2_html
    #######################################
    
      
    nombres <- c("Variable", "Éxito", "Prop Obs", "Valor bajo Hip", "Valor Z", "Valor p", "Alfa", "Decisión")
    RESUMEN <- matrix(NA, 1, length(nombres))
    colnames(RESUMEN) <- nombres
    
    RESUMEN[1,1] <- colnames(MINI)
    RESUMEN[1,2] <- input_exito
    RESUMEN[1,3] <- CONSULTORA$p
    RESUMEN[1,4] <- input_ref
    RESUMEN[1,5] <- CONSULTORA$z_obs
    RESUMEN[1,6] <- CONSULTORA$valor_p_externo
    RESUMEN[1,7] <- input_alfa
    RESUMEN[1,8] <- CONSULTORA$decision
    
    CONSULTORA$RESUMEN <- RESUMEN
    
    
    PROP01$ANALISIS$ORIGINAL <- ORIGINAL
    PROP01$ANALISIS$CONSULTORA <- CONSULTORA
    
    
    }
    #################################################################################################################################  
    
    
    
   
    
    
    
    SALIDA_ARMADA <- list()
    
    SALIDA_ARMADA$FRASE1 <- " "
    
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
        
        SALIDA_ARMADA$FRASE1 <- paste("Test de Proporciones para la variable '", PROP01$DETALLES$nombre_VC, "'.", sep="")
        
        SALIDA_ARMADA$RESUMEN <- matrix("No puede realizarse el test de proporciones.", 1, 8)
        
        SALIDA_ARMADA$FRASE2 <- frases_control[[contador_externo]]
        
        
        candado_externo <- TRUE 
      } # Fin control_OK == FALSE
      
      
      
      
    } # FIN SALIDA ARMADA
    ###################################################
    
    
  } # Fin control_general == FALSE
  #######################################################3
  
  PROP01$SALIDA_ARMADA <- list()
  PROP01$SALIDA_ARMADA <- SALIDA_ARMADA
  
  PROP01
  
  
  
} # Fin function CHI_MASTER()



if ( 1 == 2) {
  
  BASE <- TIJERA(mtcars, 8)
  
  input_datos <- BASE
  input_decimales <- 2
  input_alfa <- 0.05
  input_ref <- 0
  input_exito <- 1
  
  
  PROP01 <- PROP01_MASTER(input_datos=input_datos)
  
  PROP01$RESUMEN
  
  PROP01$SALIDA_ARMADA
  
  
} # Fin if