

COR_MASTER <- function(input_datos=NULL, input_decimales=2, input_metodo="all", input_alfa=0.05){
  
  
  
  # Detalle de los objetos de la salida...
  
  # Inicio
  COR <- list()
  
  # Ordenamiento General
  COR$input <- list()           # Ingresan solo los elementos que son input de la funcion
  COR$DETALLES <- list()        # Nombre de las variables...
  COR$CONTROLES <- list()       # Detalle de los diferentes controles por los que paso la base de datos a trabajar...
  COR$MINI <- list()            # Va la MINIBASE de trabajo, y quien es VR y quien es FACTOR
  COR$ANALISIS <- list()        # Los analisis... un apartado sera "$ORIGINAL"... y otro será "$CONSULTORA"
  COR$SCRIPT <- list()          # Un apartado sera el script para obtener exactamente lo mismo que la salida cruda de R.
  COR$SALIDA_ARMADA <- list()   # La salida, que son objetos que ingresaran al Shiny...
  COR$CONTROL_GENERAL <- list() # Guardamos... para saber... si se cumplio o no con todos los controles...

  
  # # # INPUT
  {
  
  # Esta informacion es inalterable... 
  # O sea... se pueda o no realizar el analisis... esta informacion debe estar...
  
  COR$input$datos <- input_datos
  COR$input$decimales <- input_decimales
  COR$input$alfa <- input_alfa
  
  
  } # FIN INPUT
  ############################################################################
  
  
  # # # DETALLES
  {
    
    # Esta informacion es inalterable... 
    # O sea... se pueda o no realizar el analisis... esta informacion debe estar...
    
    COR$DETALLES$nombre_X1 <- colnames(input_datos)[1]
    COR$DETALLES$nombre_X2 <- colnames(input_datos)[2]
    
    
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
    frases_control[[3]] <- c("El objeto 'input_datos' debe tener al menos 2 filas. No puede realizarse el test de Correlación.")
    frases_control[[4]] <- c("Al realizar na.omit() sobre la base de datos, quedan menos de 2 filas con informacion. No puede realizarse el test de Correlación.")
    frases_control[[5]] <- c("La Variable1 ingresada debe ser un objeto numérico. No puede realizarse el test de Correlación.")
    frases_control[[6]] <- c("La Variable2 ingresada debe ser un objeto numérico. No puede realizarse el test de Correlación.")
    frases_control[[7]] <- c("Los datos la Variable1 y Variable2 son constantes. Presentan varianza cero. No puede realizarse el test de Correlación.")
    frases_control[[8]] <- c("Los datos la Variable1 son constantes. Presentan varianza cero. No puede realizarse el test de Correlación.")
    frases_control[[9]] <- c("Los datos la Variable2 son constantes. Presentan varianza cero. No puede realizarse el test de Correlación.")
    
    
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
    names(control_OK[[5]]) <- c("X1 es numerica")
    if (control_general == TRUE) {
      X1_CONTROL <- MINI_CONTROL[,1]
      if (is.numeric(X1_CONTROL)) control_OK[[5]] <- TRUE else control_general <- FALSE
    }
    
    
    
    
    
    # Control 6...
    # La columna 2, que sera X2, debe ser numérica.
    control_OK[[6]] <- FALSE
    names(control_OK[[6]]) <- c("X2 es numerica")
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
    # Los datos de X1 no deben ser constantes... la varianza debe ser distinta de cero.
    control_OK[[8]] <- FALSE
    names(control_OK[[8]]) <- c("La varianza de X1 es distinta de cero.")
    if (control_general == TRUE) {
      
      varianza_control2 <- var(X2_CONTROL)
      
      
      if (varianza_control2 != 0) control_OK[[8]] <- TRUE else control_general <- FALSE
    }
    
    
    
    
    # Control 9...
    # Los datos de X2 no deben ser constantes... la varianza debe ser distinta de cero.
    control_OK[[9]] <- FALSE
    names(control_OK[[9]]) <- c("La varianza de X2 es distinta de cero.")
    if (control_general == TRUE) {
      
      varianza_control2 <- var(X2_CONTROL)
      
      
      if (varianza_control2 != 0) control_OK[[9]] <- TRUE else control_general <- FALSE
    }
    
    
    
    COR$CONTROLES <- control_OK
    
    COR$CONTROL_GENERAL <- control_general
    
  } # # # FIN CONTROLES
  ###############################################################################################################################
  
  
  
  if (control_general == TRUE) {
  
  # Parte 0: Input
  # Agregamos todos los input 
  
  #   input_datos <- mtcars[,c(1,3)]
  #   input_alfa <- 0.05
  #   input_decimales <- 2
  #   input_metodo <- "pearson"
  
  MINI <- input_datos
  MINI <- na.omit(MINI)
  
  COR$MINI <- MINI
  
  X1 <- MINI[1]
  X2 <- MINI[2]
  
  
  VR <- c(as.vector(X1), as.vector(X2))
  GRUPO <- c(rep("X1", nrow(MINI)), rep("X2", nrow(MINI)))
  
  # Base para el Test de Bartlett
  BASE2 <- cbind(VR, GRUPO)

  
  
  NORMALIDAD_X1 <- SHAPIRO_MASTER(X1, input_decimales=input_decimales, input_alfa=input_alfa)
  NORMALIDAD_X2 <- SHAPIRO_MASTER(X2, input_decimales=input_decimales, input_alfa=input_alfa)
  HOMOGENEIDAD <- BARTLETT_MASTER(MINI, input_decimales=input_decimales, input_alfa=input_alfa, input_ingreso="clasic")
  
  COR$ANALISIS$ORIGINAL <- list()
  COR$ANALISIS$ORIGINAL$NORMALIDAD_X1 <- NORMALIDAD_X1
  COR$ANALISIS$ORIGINAL$NORMALIDAD_X2 <- NORMALIDAD_X2
  COR$ANALISIS$ORIGINAL$HOMOGENEIDAD  <- HOMOGENEIDAD
  
  
  
  # Frase1: Normalidad y Homogeneidad... todo junto...
  {
  zeus_normalidad1  <- NORMALIDAD_X1$ANALISIS$CONSULTORA$normalidad
  zeus_normalidad2  <- NORMALIDAD_X2$ANALISIS$CONSULTORA$normalidad
  zeus_homogeneidad <- HOMOGENEIDAD$ANALISIS$CONSULTORA$homogeneidad
  
  nombres <- c("Normalidad X1", "Normalidad X2", "Homogeneidad de Varianzas")
  RESUMEN_NH <- as.data.frame(matrix(NA, 1, length(nombres)))
  colnames(RESUMEN_NH) <- nombres
  RESUMEN_NH[1,1] <- zeus_normalidad1
  RESUMEN_NH[1,2] <- zeus_normalidad2
  RESUMEN_NH[1,3] <- zeus_homogeneidad
  
  
  COR$ANALISIS$CONSULTORA <- list()
  COR$ANALISIS$CONSULTORA$RESUMEN_NH <- RESUMEN_NH
  
  
  frase1_nh <- c("Se cumplen los tres requisitos.", "<br/>", 
                 "Puede utilizarse la correlación de Pearson para sacar conclusiones.")
  
  frase2_nh <- c("No se cumplen los tres requisitos.", "<br/>", 
                 "No debe utilizarse la correlación de Pearson para sacar conclusiones", "<br/>",
                 "Debe realizar el test de correlación de Spearman para sacar conclusiones con estos datos.")
  
  
  if (input_metodo == "Spearman") {
    
    frase1_nh <- c("")
    frase2_nh <- c("")  
  }
  
  # CONTROL_nh
  if (sum(as.numeric(c(RESUMEN_NH == "SI"))) == ncol(RESUMEN_NH)) CONTROL_nh <- TRUE else CONTROL_nh <- FALSE  
  
  if (CONTROL_nh == TRUE) frase_nh <- frase1_nh else frase_nh <- frase2_nh  
  COR$ANALISIS$CONSULTORA$frase_nh <- frase_nh
  
  } # Fin Frase1
  ################################################################################################################################
  
  
  
  # COR de Pearson 
  {
    PEARSON <- cor.test(MINI[,1], MINI[,2], method="pearson", conf.level=(1-input_alfa))
    PEARSON$estimate <- round2(PEARSON$estimate, input_decimales)
    PEARSON$p.value <- round2(PEARSON$p.value, input_decimales)
    
    p_interno_pearson <- PEARSON$p.value
    if (p_interno_pearson < 0.001) p_externo_pearson <- "<<0.001" else p_externo_pearson <- p_interno_pearson
    PEARSON$p_externo <- p_externo_pearson
    
    if (p_interno_pearson >= input_alfa) decision_pearson <- "No rechazo Ho" else decision_pearson <- "Rechazo Ho"
    PEARSON$decision <- decision_pearson
  } # Fin COR de Pearson
  ###################################
  
  
  # COR de SPEARMAN
  {
    SPEARMAN <- cor.test(MINI[,1], MINI[,2], method="spearman", conf.level=(1-input_alfa)) 
    
    SPEARMAN$estimate <- round2(SPEARMAN$estimate, input_decimales)
    SPEARMAN$p.value <- round2(SPEARMAN$p.value, input_decimales)
    
    p_interno_spearman <- SPEARMAN$p.value
    if (p_interno_spearman < 0.001) p_externo_spearman <- "<<0.001" else p_externo_spearman <- p_interno_spearman
    SPEARMAN$p_externo <- p_externo_spearman
    
    if (p_interno_spearman >= input_alfa) decision_spearman <- "No rechazo Ho" else decision_spearman <- "Rechazo Ho"
    SPEARMAN$decision <- decision_spearman
    
  } # Fin COR de SPEARMAN
  ####################################
  
  
  # Agregamos los analisis al objeto general "COR"
  COR$ANALISIS$ORIGINAL$PEARSON <- PEARSON
  COR$ANALISIS$ORIGINAL$SPEARMAN <- SPEARMAN
  
  
  # Resumen Pearson
  {
  nombres <- c("X1", "X2", "Método", "Correlación", "G.L", "Valor p", "Alfa", "Decisión")
  RESUMEN_PEARSON <- as.data.frame(matrix(NA, 1, length(nombres)))
  colnames(RESUMEN_PEARSON) <- nombres
  
  RESUMEN_PEARSON[1,1] <- COR$DETALLES$nombre_X1
  RESUMEN_PEARSON[1,2] <- COR$DETALLES$nombre_X2
  RESUMEN_PEARSON[1,3] <- "Pearson"
  RESUMEN_PEARSON[1,4] <- PEARSON$estimate
  RESUMEN_PEARSON[1,5] <- PEARSON$parameter
  RESUMEN_PEARSON[1,6] <- PEARSON$p_externo
  RESUMEN_PEARSON[1,7] <- input_alfa
  RESUMEN_PEARSON[1,8] <- PEARSON$decision
  
  COR$ANALISIS$CONSULTORA$RESUMEN_PEARSON <- RESUMEN_PEARSON
  
  }
  ####################################
  
  
  # COR estimada (para meterla en las frases)
  cor_estimada_pearson <- RESUMEN_PEARSON[1,4]  
  
  
  
  
  
  # Frase de Pearson
  # frase2_pearson_html: segun valor p
  {
    
    frase2_v1 <- c("No pudo obtenerse un valor p.")
    
    frase2_v2 <- c("El valor estimado de correlación de Pearson es r=", cor_estimada_pearson ,".","<br/>",
                   "El valor p es mayor que el valor de alfa=", input_alfa, ".","<br/>",
                   "No se rechaza la Ho.", "<br/>",
                   "Las variables '", COR$DETALLES$nombre_X1 , "' y '", COR$DETALLES$nombre_X2, "' no presentan una correlación estadísticamente significativa.","<br/>",
                   "Las varaibles son estadísticamente independientes.")
    
    
    frase2_v3 <- c("El valor estimado de correlación de Pearson es r=", cor_estimada_pearson ,".","<br/>",
                   "El valor p es igual que el valor de alfa=", input_alfa, ".","<br/>",
                   "No se rechaza la Ho.", "<br/>",
                   "Las variables '", COR$DETALLES$nombre_X1 , "' y '", COR$DETALLES$nombre_X2, "' no presentan una correlación estadísticamente significativa.","<br/>",
                   "Las varaibles son estadísticamente independientes.")
    
    
    
    
    frase2_v4 <- c("El valor estimado de correlación de Pearson es r=", cor_estimada_pearson ,".","<br/>",
                   "El valor p es menor que el valor de alfa=", input_alfa, ".","<br/>",
                   "Se rechaza la Ho.", "<br/>",
                   "Las variables '", COR$DETALLES$nombre_X1 , "' y '", COR$DETALLES$nombre_X2, "' presentan una correlación estadísticamente distinta de cero.","<br/>",
                   "Existe una asociación estadísticamente significativa entre las variables.")
    
    
    if(is.na(PEARSON$p.value) | is.null(PEARSON$p.value)) frase_elegida_pearson  <- frase2_v1 else if (PEARSON$p.value > input_alfa)  frase_elegida_pearson <- frase2_v2 else if (PEARSON$p.value == input_alfa)  frase_elegida_pearson <- frase2_v3 else if (PEARSON$p.value < input_alfa)  frase_elegida_pearson <- frase2_v4
    
    COR$ANALISIS$CONSULTORA$frase2_pearson_html <- frase_elegida_pearson
    
  } # Fin Frase2_html
  #######################################
  
  
  
  # Resumen Spearman
  {
    nombres <- c("X1", "X2", "Método", "Correlación", "G.L", "Valor p", "Alfa", "Decision")
    RESUMEN_SPEARMAN <- as.data.frame(matrix(NA, 1, length(nombres)))
    colnames(RESUMEN_SPEARMAN) <- nombres
    
    RESUMEN_SPEARMAN[1,1] <- COR$DETALLES$nombre_X1
    RESUMEN_SPEARMAN[1,2] <- COR$DETALLES$nombre_X2
    RESUMEN_SPEARMAN[1,3] <- "Spearman"
    RESUMEN_SPEARMAN[1,4] <- SPEARMAN$estimate
    RESUMEN_SPEARMAN[1,5] <- "No posee"
    RESUMEN_SPEARMAN[1,6] <- SPEARMAN$p_externo
    RESUMEN_SPEARMAN[1,7] <- input_alfa
    RESUMEN_SPEARMAN[1,8] <- SPEARMAN$decision
    
    COR$ANALISIS$CONSULTORA$RESUMEN_SPEARMAN <- RESUMEN_SPEARMAN
    
  }
  ####################################
  
  
  # COR estimada (para meterla en las frases)
  cor_estimada_spearman <- RESUMEN_SPEARMAN[1,4] 
  
  
  # Frase de Spearman
  # frase2_spearman_html: segun valor p
  {
    
    frase2_v1 <- c("No pudo obtenerse un valor p.")
    
    frase2_v2 <- c("El valor estimado de correlación de Spearman es r=", cor_estimada_spearman ,".","<br/>",
                   "El valor p es mayor que el valor de alfa=", input_alfa, ".","<br/>",
                   "No se rechaza la Ho.", "<br/>",
                   "Las variables '", COR$DETALLES$nombre_X1 , "' y '", COR$DETALLES$nombre_X2, "' no presentan una correlación estadísticamente significativa.","<br/>",
                   "Las varaibles son estadísticamente independientes.")
    
    
    frase2_v3 <- c("El valor estimado de correlación de Spearman es r=", cor_estimada_spearman ,".","<br/>",
                   "El valor p es igual que el valor de alfa=", input_alfa, ".","<br/>",
                   "No se rechaza la Ho.", "<br/>",
                   "Las variables '", COR$DETALLES$nombre_X1 , "' y '", COR$DETALLES$nombre_X2, "' no presentan una correlación estadísticamente significativa.","<br/>",
                   "Las varaibles son estadísticamente independientes.")
    
    
    
    
    frase2_v4 <- c("El valor estimado de correlación de Spearman es r=", cor_estimada_spearman ,".","<br/>",
                   "El valor p es menor que el valor de alfa=", input_alfa, ".","<br/>",
                   "Se rechaza la Ho.", "<br/>",
                   "Las variables '", COR$DETALLES$nombre_X1 , "' y '", COR$DETALLES$nombre_X2, "' presentan una correlación estadísticamente distinta de cero.","<br/>",
                   "Existe una asociación estadísticamente significativa entre las variables.")
    
    if(is.na(SPEARMAN$p.value) | is.null(SPEARMAN$p.value)) frase_elegida_spearman <-  frase2_v1 else if (SPEARMAN$p.value > input_alfa) frase_elegida_spearman <- frase2_v2 else if (SPEARMAN$p.value == input_alfa) frase_elegida_spearman <- frase2_v3 else if (SPEARMAN$p.value < input_alfa) frase_elegida_spearman <- frase2_v4
    
    COR$ANALISIS$CONSULTORA$frase2_spearman_html <- frase_elegida_spearman
    
  } # Fin Frase2_html
  #######################################
  
  
  
  # Salida Armanda
  
  SALIDA_ARMADA <- list()
  
  # Si se puede Pearson
#  if (CONTROL_nh == TRUE | input_metodo == "Pearson")
  if (input_metodo == "Pearson" | (input_metodo == "all" && CONTROL_nh == TRUE)) {
    
    SALIDA_ARMADA$FRASE_INICIAL <- "Correlación de Pearson"
    
    SALIDA_ARMADA$RESUMEN_NH <- RESUMEN_NH
    
    SALIDA_ARMADA$FRASE_NH <-   frase_nh
    
    SALIDA_ARMADA$RESUMEN_COR <- RESUMEN_PEARSON
    
    SALIDA_ARMADA$FRASE <- frase_elegida_pearson
    
  } else   if (input_metodo == "Spearman" | (input_metodo == "all" && CONTROL_nh == FALSE)) {
    
    SALIDA_ARMADA$FRASE_INICIAL <- "Correlación de Spearman"
    
    SALIDA_ARMADA$RESUMEN_NH <- RESUMEN_NH
    
    SALIDA_ARMADA$FRASE_NH <-   frase_nh
    
    SALIDA_ARMADA$RESUMEN_COR <- RESUMEN_SPEARMAN
    
    SALIDA_ARMADA$FRASE <- frase_elegida_spearman
    
  }
  
  } # Fin if control_general == TRUE
  ###################################################################################
  
  
  if (control_general == FALSE) {
  
    candado_externo <- FALSE
    contador_externo <- 0
    while(candado_externo == FALSE) {
      
      contador_externo <- contador_externo + 1
      
      if (control_OK[[contador_externo]] == FALSE) {
        
        SALIDA_ARMADA <- list()
      ###
        SALIDA_ARMADA$FRASE_INICIAL <- paste("Test de Correlación entre '", COR$DETALLES$nombre_X1, "' y '",COR$DETALLES$nombre_X2, "'.", sep="")
        
        SALIDA_ARMADA$RESUMEN_NH <- matrix("No puede realizarse el test de Correlación", 1, 4)
        
        SALIDA_ARMADA$FRASE_NH <-   frases_control[[contador_externo]]
        
        SALIDA_ARMADA$RESUMEN_COR <- matrix("No puede realizarse el test de Correlación", 1, 6)
        
        SALIDA_ARMADA$FRASE <- frases_control[[contador_externo]]
        
        ###
       
        
        candado_externo <- TRUE 
        
      }
    }
  } # Fin if control_general == FALSE
  #######################################################################################
  
  
  COR$SALIDA_ARMADA <- SALIDA_ARMADA
  
  COR
  
} # Fin function COR_MASTER()





if (1 == 2){
  
  BASE <- mtcars[,c(1,3)]
  archivo <- c("/home/david/ARN/TUCUMAN/002_Tucuman Abril 2019/Ejercicio14.xlsx")
  BASE <- carga_xls(archivo)
  BASE <- BASE[,c(2,3)]
  
  input_base <- BASE
  input_alfa <- 0.05
  input_decimales <- 2
  input_datos <- BASE
  input_metodo="Spearman"
  
  AVER <-   COR_MASTER(BASE, input_metodo = "Spearman")
  
} # Fin 1 == 2
