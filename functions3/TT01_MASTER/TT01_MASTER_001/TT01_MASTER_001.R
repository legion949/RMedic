

TT01_MASTER <- function(input_datos=NULL, input_decimales=2, input_alfa=0.05){
  
  library(agricolae)
  
  # Parte 0: Input
  # Agregamos todos los input 
  #   
  #       input_datos <- mtcars[,c(1,2)]
  #       input_alfa <- 0.05
  #       input_decimales <- 2
  #       input_metodo <- "pearson"
  
 
  # En esta funcion TT01 es para el test de muestras independientes...
  # Y el formato de ingreso de los datos... es del tipo ANOVA
  # La 1er columna debe ser VR... por lo tanto numerica...
  # La 2da columna debe ser FACTOR... por lo tanto debe ser un factor... o categórica de ultima... pero no puede ser numerica...
  
  # La funcion TT01... no va a forzar a tomar como factor a la 2da columna...
  # Sino que le va a indicar al usuario que no ha ingresado un objeto en el formato correcto...
  
#   input_datos <- mtcars[,c(1,8)]
#   input_datos[,2] <- as.factor(as.character(input_datos[,2]))

  MINI <- input_datos
  MINI <- na.omit(MINI)
  
  # Separacion de VR y FACTOR
  VR <- MINI[,1]
  FACTOR <- MINI[,2]
  
  
  
  # Inicio
  TT01 <- list()
  
  # Ordenamiento General
  TT01$input <- list()
  TT01$DETALLES <- list()
  TT01$ANALISIS <- list()
  TT01$REQUERIMIENTOS <- list()
  TT01$SALIDA_ARMADA <- list()  
  
  # Input
  {
  TT01$input$datos <- input_datos
  TT01$input$decimales <- input_decimales
  TT01$input$alfa <- input_alfa
  }# Fin Input
  ################################################################
  
  
  
  
  
  
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
    frases_control[[3]] <- c("El objeto 'input_datos' debe tener al menos 4 filas. No puede realizarse el test t para muestras independientes.")
    frases_control[[4]] <- c("Al realizar na.omit() sobre la base de datos, quedan menos de 4 filas con informacion. No puede realizarse el test t para muestras independientes.")
    frases_control[[5]] <- c("La 1er columna ingresada debe ser un objeto numérico. No puede realizarse el test t para muestras independientes.")
    frases_control[[6]] <- c("La 2da columna ingresada debe ser un factor con 2 niveles. No puede realizarse el test t para muestras independientes.")
    frases_control[[7]] <- c("Al menos un nivel del factor presentan varianza cero. Los valores son constantes. No puede realizarse el test t para muestras independientes.")
    
    
    # Control 1...
    # El objeto input_datos debe ser un data.frame o una matrix.
    control_OK[[1]] <- FALSE
    names(control_OK[[1]]) <- c("data.frame o matrix")
    if (is.data.frame(input_datos) | is.matrix(input_datos)) control_OK[[1]] <- TRUE else control_general <- FALSE 
    
    
    # Control 2...
    # El objeto input_datos debe tener 2 columnas.
    control_OK[[2]] <- FALSE
    names(control_OK[[2]]) <- c("Solo 2 columnas")
    if (control_general == TRUE) {
      if (ncol(input_datos) == 2) control_OK[[2]] <- TRUE else control_general <- FALSE
    }
    
    
    
    
    # Control 3...
    # El objeto input_datos debe tener al menos 4 filas.
    control_OK[[3]] <- FALSE
    names(control_OK[[3]]) <- c("Al menos 4 filas de input_datos")
    if (control_general == TRUE) {
      if (nrow(input_datos) >= 4) control_OK[[3]] <- TRUE else control_general <- FALSE 
    }
    
    
    
    # Control 4...
    # El objeto "MINI" (obtenido con na.omit(input_datos)) debe tener al menos una fila
    control_OK[[4]] <- FALSE
    names(control_OK[[4]]) <- c("Al menos 4 filas de MINI")
    if (control_general == TRUE) {
      
      MINI_CONTROL <- input_datos
      MINI_CONTROL <- na.omit(MINI_CONTROL)
      
      if (nrow(MINI_CONTROL) >= 4) control_OK[[4]] <- TRUE else control_general <- FALSE 
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
    # La columna 2, que sera FACTOR, debe tener 2 niveles de factor.
    control_OK[[6]] <- FALSE
    names(control_OK[[6]]) <- c("FACTOR con 2 niveles es numerica")
    if (control_general == TRUE) {
      FACTOR_CONTROL <- MINI_CONTROL[,2]
      niveles <- levels(FACTOR_CONTROL)
      if (length(niveles) == 2) control_OK[[6]] <- TRUE else control_general <- FALSE
    }
    
      
    
    # Control 7...
    # Todos los niveles del factor, tienen varianzas distintas de cero.
    control_OK[[7]] <- FALSE
    names(control_OK[[7]]) <- c("Todas las varianzas distintas de cero")
    if (control_general == TRUE) {
      
      VR_CONTROL <- MINI_CONTROL[,1]
      FACTOR_CONTROL <- as.factor(as.character(MINI_CONTROL[,2]))
      
      varianzas_control <- tapply(VR_CONTROL, FACTOR_CONTROL, var)
      
      detec <- varianzas_control > 0
      suma <- sum(as.numeric(detec))
      
      if (suma == length(detec)) control_OK[[7]] <- TRUE else control_general <- FALSE
    }
    
    
    TT01$CONTROLES <- control_OK
    
    TT01$CONTROL_GENERAL <- control_general
    
  } # # # FIN CONTROLES
  ###############################################################################################################################
  
  
  # Si cumple con todos los controles previos
  if (control_general == TRUE) {
  
  TT01$REQUERIMIENTOS$HOMOGENEIDAD <- list()
  TT01$REQUERIMIENTOS$NORMALIDAD1 <- list()
  TT01$REQUERIMIENTOS$NORMALIDAD2 <- list()
  
  TT01$input$datos <- input_datos
  TT01$input$decimales <- input_decimales
  TT01$input$alfa <- input_alfa
  
  TT01$DETALLES$nombre_VR <- colnames(MINI)[1]
  TT01$DETALLES$nombre_FACTOR <- colnames(MINI)[2]
  
  
  # Requerimientos de Normalidad y Homogeneidad de Varianzas
  {
  
  # Homogeneidad de Varianzas...
  HOMOGENEIDAD <- BARTLETT_MASTER(MINI, input_ingreso="anova")
  
  # Normalidad grupo1
  G1 <- VR[FACTOR == levels(FACTOR)[1]]
  G1 <- data.frame(G1)
  colnames(G1) <- levels(FACTOR)[1]
  G1 <- na.omit(G1)
  NORMALIDAD1 <- SHAPIRO_MASTER(G1)
  
  # Normalidad grupo2
  G2 <- VR[FACTOR == levels(FACTOR)[2]]
  G2 <- data.frame(G2)
  colnames(G2) <- levels(FACTOR)[2]
  G2 <- na.omit(G2)
  NORMALIDAD2 <- SHAPIRO_MASTER(G2)
  
  
  # Resumen de Normalidad y Homogeneidad
  nombre_g1 <- paste("Normalidad ", levels(FACTOR)[1], sep="")
  nombre_g2 <- paste("Normalidad ", levels(FACTOR)[2], sep="")
  
  nombres <- c(nombre_g1, nombre_g2, "Homogeneidad de Varianzas", "Validez del Test t")
  RESUMEN_NH <- data.frame(matrix(NA, 1, length(nombres)))
  colnames(RESUMEN_NH) <- nombres

  normalidad1 <-  NORMALIDAD1$ANALISIS$CONSULTORA$normalidad
  normalidad2 <-  NORMALIDAD2$ANALISIS$CONSULTORA$normalidad
  homogeneidad <- HOMOGENEIDAD$ANALISIS$CONSULTORA$homogeneidad
  
  RESUMEN_NH[1,1] <- normalidad1
  RESUMEN_NH[1,2] <- normalidad2
  RESUMEN_NH[1,3] <- homogeneidad
  
  detec <- RESUMEN_NH[1,c(1:3)]
  detec <- detec == "SI"
  detec <- sum(as.numeric(detec))
  if (detec == 3) RESUMEN_NH[1,4] <- "Es válido el Test t" else RESUMEN_NH[1,4] <- "NO es válido el Test t"
    
  TT01$REQUERIMIENTOS$HOMOGENEIDAD <- HOMOGENEIDAD
  TT01$REQUERIMIENTOS$NORMALIDAD1 <-  NORMALIDAD1
  TT01$REQUERIMIENTOS$NORMALIDAD2 <-  NORMALIDAD2
  
  
  TT01$REQUERIMIENTOS$RESUMEN_NH <- RESUMEN_NH  
  
 
  # Frase de NH
  # frase3_test_t_html: segun valor "SI" o "NO" de normalidad y homogeneidad
  {
  
  frase3_v1 <- c("No se cumple con el requisito distribución normal de ambos grupos.", "<br/>",
                 "Todos los resultados estadísticos del test t son incorrectos, y no puede sacarse ningún tipo de conclusión sobre ellos,", "<br/>",
                 "Indistintamente del valor p obtenido en el test t, este análisis debe ser descartado.", "<br/>",
                 "Para analizar este pool de datos y poder sacar conclusiones correctas, debe utilizarse el test de Mann-Whitney.")
  
  frase3_v2 <- c("No se cumple con el requisito de homogeneidad de varianzas de ambos grupos.", "<br/>",
                 "Todos los resultados estadísticos del test t son incorrectos, y no puede sacarse ningún tipo de conclusión sobre ellos,", "<br/>",
                 "Indistintamente del valor p obtenido en el test t, este análisis debe ser descartado.", "<br/>",
                 "Para analizar este pool de datos y poder sacar conclusiones correctas, debe utilizarse el test de Mann-Whitney.")
  
  
  frase3_v3 <- c("No se cumplen los requisitos de distribución normal de ambos grupos y de homogeneidad de varianzas.", "<br/>",
                 "Todos los resultados estadísticos del Test t son incorrectos, y no puede sacarse ningún tipo de conclusión sobre ellos,", "<br/>",
                 "Indistintamente del valor p obtenido en el Test t, este análisis debe ser descartado.", "<br/>",
                 "Para analizar este pool de datos y poder sacar conclusiones correctas, debe utilizarse el test de Mann-Whitney.")
  
  
  
  
  frase3_v4 <- c("Se cumplen los requisitos necesarios de normaldiad y de homogeneidad de varianzas de los grupos." , "<br/>",
                 "Es válido sacar conclusiones del valor p obtenido para el Test t de muestras independientes.", "<br/>",
                 "Para este pool de datos, es mejor utilizar el test t en vez del test de Mann-Whitney.")
  
  
  if ((normalidad1 == "NO" | normalidad2 == "NO")  && homogeneidad == "SI") FRASE_NH <- frase3_v1 else if ((normalidad1 == "SI" && normalidad2 == "SI")  && homogeneidad == "NO") FRASE_NH <- frase3_v2 else if ((normalidad1 == "NO" | normalidad2 == "NO")  && homogeneidad == "NO") FRASE_NH <- frase3_v3 else if ((normalidad1 == "SI" && normalidad2 == "SI")  && homogeneidad == "SI") FRASE_NH <- frase3_v4
  
  TT01$REQUERIMIENTOS$FRASE_NH <- FRASE_NH
  
  } # Fin Frase3_html
  #######################################
  
   
  
  }
  #######################################################################
  
  
  
  
  # Test "T" y decision
  {
  
  # En esta oportunidad... ingreso al test t como VR y FACTOR...

  ANALISIS <- list()
  ANALISIS$ORIGINAL <- list()
  ANALISIS$CONSULTORA <- list()
  
  if (homogeneidad == "SI") arg_var.equal <- TRUE else arg_var.equal <- FALSE
  
  ANALISIS$ORIGINAL <-  t.test(VR ~ FACTOR, paired=FALSE, var.equal=arg_var.equal, conf.level= (1-input_alfa))
  
  valor_p_interno  <- round2(ANALISIS$ORIGINAL$p.value, input_decimales)
  if (valor_p_interno < 0.001) valor_p_externo <- "<<0.001" else valor_p_externo <- valor_p_interno
  ANALISIS$CONSULTORA$valor_p_interno <- valor_p_interno
  ANALISIS$CONSULTORA$valor_p_externo <- valor_p_externo
  
  if (valor_p_interno >= input_alfa) decision <- "No rechazo Ho" else decision <- "Rechazo Ho"
  ANALISIS$CONSULTORA$decision <- decision
  

  # Si una es VR y el otro es FACTOR

    
    nombres <- c("VR", "FACTOR", "Test Estadístico", "Valor t", "GL", "Valor p", "Decisión") 
    RESUMEN_TT <- data.frame(matrix(NA, 1, length(nombres)))
    colnames(RESUMEN_TT) <- nombres
    
    
    
    RESUMEN_TT[1,1] <- colnames(MINI)[1]
    RESUMEN_TT[1,2] <- colnames(MINI)[2]
    RESUMEN_TT[1,3] <- "Test t"
    RESUMEN_TT[1,4] <- round2(ANALISIS$ORIGINAL$statistic, input_decimales)
    RESUMEN_TT[1,5] <- round2(ANALISIS$ORIGINAL$parameter, input_decimales)
    RESUMEN_TT[1,6] <- valor_p_externo
    RESUMEN_TT[1,7] <- decision


    ANALISIS$CONSULTORA$RESUMEN <- RESUMEN_TT  
  
    } # Fin Test "T" y decision
  ###################################
  
  
  
  # Agregamos los analisis al objeto general "T"
  TT01$ANALISIS <- ANALISIS
  
  
  
  # frase2_test_t_html: segun valor p
  {
    
    frase2_v1 <- c("No pudo obtenerse un valor p.")
    
    frase2_v2 <- c("El valor p es mayor que el valor de alfa=", TT01$input$alfa, ".","<br/>",
                   "No se rechaza la Ho.", "<br/>",
                   "No existen diferencias estadísticamente significativas entre los grupos.","<br/>",
                   "Los niveles del factor '", TT01$DETALLES$nombre_FACTOR, "' son estadísticamente equivalentes entre si.", "<br/>",
                   "Los niveles del factor '", TT01$DETALLES$nombre_FACTOR, "' presentan medias estadísticamente iguales.", "<br/>")
    
    
    frase2_v3 <- c("El valor p es igual que el valor de alfa=", TT01$input$alfa, ".","<br/>",
                   "No se rechaza la Ho.", "<br/>",
                   "No existen diferencias estadísticamente significativas entre los grupos.","<br/>",
                   "Los niveles del factor '", TT01$DETALLES$nombre_FACTOR, "' son estadísticamente equivalentes entre si.", "<br/>",
                   "Los niveles del factor '", TT01$DETALLES$nombre_FACTOR, "' presentan medias estadísticamente iguales.", "<br/>")
    
    
    
    
    frase2_v4 <- c("El valor p es menor que el valor de alfa=", TT01$input$alfa, ".","<br/>",
                   "Se rechaza la Ho.", "<br/>",
                   "Existen diferencias estadísticamente significativas entre los grupos.","<br/>",
                   "Existen niveles del factor '", TT01$DETALLES$nombre_FACTOR, "' estadísticamente distintos.", "<br/>",
                   "Existen niveles del factor '", TT01$DETALLES$nombre_FACTOR, "' que presentan medias estadísticamente diferentes.", "<br/>")
    
    
    if(is.na(valor_p_interno) | is.null(valor_p_interno)) frase_elegida <- frase2_v1 else if (valor_p_interno > input_alfa) frase_elegida <- frase2_v2 else if (valor_p_interno == input_alfa) frase_elegida <- frase2_v3 else if (valor_p_interno < input_alfa) frase_elegida <- frase2_v4
    
    TT01$ANALISIS$CONSULTORA$frase2_test_t_html <- frase_elegida
    
  } # Fin Frase2_html
  #######################################
  
  
  
  
  # Salida Armanda
  
  SALIDA_ARMADA <- list()
  
  # Elementos de la Salida Armada
  {
  
  SALIDA_ARMADA$FRASE_INICIAL <- "Test t de comparaciones de muestras independientes"
  
  SALIDA_ARMADA$RESUMEN_TT <- TT01$ANALISIS$CONSULTORA$RESUMEN
  
  SALIDA_ARMADA$FRASE_TT <-    TT01$ANALISIS$CONSULTORA$frase2_test_t_html
  
  SALIDA_ARMADA$RESUMEN_NH <- TT01$REQUERIMIENTOS$RESUMEN_NH
  
  SALIDA_ARMADA$FRASE_NH <- TT01$REQUERIMIENTOS$FRASE_NH
  
  } # Fin Salida Armada
  ##########
  
  
  } # Fin if Si cumple los controles previos
  #################################################################################
  
  
  
  # Si no se cumplen los controles previos
  {
    if (control_general == FALSE) {
      
      candado_externo <- FALSE
      contador_externo <- 0
      while(candado_externo == FALSE) {
        
        contador_externo <- contador_externo + 1
        
        if (control_OK[[contador_externo]] == FALSE) {
          
          
          SALIDA_ARMADA <- list()
          
         
          
          SALIDA_ARMADA$FRASE_INICIAL <- "Test t de comparaciones de muestras independientes"
          
          SALIDA_ARMADA$RESUMEN_TT    <- matrix("No puede realizarse el Test t", 1, 7)
          
          SALIDA_ARMADA$FRASE_TT      <- frases_control[[contador_externo]]
          
          SALIDA_ARMADA$RESUMEN_NH    <- matrix("No puede realizarse el Test t", 1, 3)
          
          SALIDA_ARMADA$FRASE_NH      <- frases_control[[contador_externo]]
          
          
          candado_externo <- TRUE 
        } # Fin control_OK == FALSE
        
        
        
        
      } # FIN SALIDA ARMADA
      ###################################################
      
      
    } # Fin control_general == FALSE
    #######################################################3
    
      
      
    } # Fin Si no se cumplen los controles previos
  
  ###########################################################################################
  
  
  
  
  TT01$SALIDA_ARMADA <- SALIDA_ARMADA
  
  TT01
  
}  # Fin function KW_MASTER()





if (1 == 2){
  
  # Agregamos todos los input 
  #   
  BASE <- mtcars[,c(1,8)]
  BASE[,2] <- as.factor(as.character(BASE[,2]))
  input_datos <- BASE
  input_alfa <- 0.05
  input_decimales <- 2
  
  
  
  AVER <-   TT01_MASTER(BASE)
  
} # Fin 1 == 2