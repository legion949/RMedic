

ANOVA01_MASTER <- function(input_datos=NULL, input_decimales=2, input_alfa=0.05){
  
 
  
  # Librerias
  library(agricolae)
  
  
  # Detalle de los objetos de la salida...
  
  # Inicio
  ANOVA01 <- list()
  
  # Ordenamiento General
  ANOVA01$input <- list()           # Ingresan solo los elementos que son input de la funcion
  ANOVA01$DETALLES <- list()        # Nombre de las variables
  ANOVA01$CONTROLES <- list()       # Detalle de los diferentes controles por los que paso la base de datos a trabajar...
  ANOVA01$MINI <- list()            # Va la MINIBASE de trabajo, y quien es VR y quien es FACTOR
  ANOVA01$ANALISIS <- list()        # Los analisis... un apartado sera "$ORIGINAL"... y otro será "$CONSULTORA"
  ANOVA01$TUKEY <- list()           # El test de Tukey... un apartado sera "$ORIGINAL"... y otro será "$CONSULTORA"
  ANOVA01$REQUERIMIENTOS <- list()  # Un apartado sera "$NORMALIDAD_ERRORES" y otro "$HOMOGENEIDAD_ERRORES", y cada uno tendra "$ORIGINAL" y "$CONSULTORA"
  ANOVA01$SCRIPT <- list()          # Un apartado sera el script para obtener exactamente lo mismo que la salida cruda de R.
  ANOVA01$SALIDA_ARMADA <- list()   # La salida, que son objetos que ingresaran al Shiny...
  ANOVA01$CONTROL_GENERAL <- list() # Guardamos... para saber... si se cumplio o no con todos los controles...
  
  
  # # # INPUT
  {
  
  # Esta informacion es inalterable... 
  # O sea... se pueda o no realizar el analisis... esta informacion debe estar...
  
  ANOVA01$input$datos <- input_datos
  ANOVA01$input$decimales <- input_decimales
  ANOVA01$input$alfa <- input_alfa
  
  
  } # FIN INPUT
  ############################################################################
  
  
  # # # DETALLES
  {
    
    # Esta informacion es inalterable... 
    # O sea... se pueda o no realizar el analisis... esta informacion debe estar...
    
    ANOVA01$DETALLES$nombre_VR <- colnames(input_datos)[1]
    ANOVA01$DETALLES$nombre_FACTOR <- colnames(input_datos)[2]
    
    
  } # FIN DETALLES
  ############################################################################
  
  
  
  # # # CONTROLRES
  {
    # Hay diferentes controles que hacer...
    # Cada control, afectara a las salidas estadisticas reemplazando los objetos
    # por carteles de aviso del error.
    
    # Se respetara el tipo de objeto...
    # Entonces... se esperaba obtener una tabla de ANOVA... saldra una tabla con las mismas
    # dimensiones, pero con carteles de aviso de error.
    
    
    
    
    control_general <- TRUE
    
    control_OK <- list()
    
    frases_control <- list()
    frases_control[[1]] <- c("El objeto 'input_datos' debe ser un data.frame o una matriz. Ingrese un formato de datos correcto.")
    frases_control[[2]] <- c("El objeto 'input_datos' debe tener 2 columnas. Ingrese un formato de datos correcto.")
    frases_control[[3]] <- c("El objeto 'input_datos' debe tener al menos 1 fila.  No puede realizarse el test de ANOVA.")
    frases_control[[4]] <- c("Al realizar na.omit() sobre la base de datos, quedan 0 filas con informacion. No puede realizarse el test de ANOVA.")
    frases_control[[5]] <- c("La columna 1 debe ser un objeto numerico (VR). No puede realizarse el test de ANOVA.")
    frases_control[[6]] <- c("La columna 2 debe tener al menos 2 niveles como FACTOR. No puede realizarse el test de ANOVA.")
    frases_control[[7]] <- c("Los niveles del FACTOR (Columna 2) deben tener al menos 2 repeticiones cada nivel. No puede realizarse el test de ANOVA.")
    frases_control[[8]] <- c("Al menos un nivel del FACTOR (Columna 2) presenta varianza 0. No puede realizarse el test de ANOVA.")
    
    
    # Control 1...
    # El objeto input_datos debe ser un data.frame o una matrix.
    control_OK[[1]] <- FALSE
    names(control_OK[[1]]) <- c("data.frame o matrix")
    if (is.data.frame(input_datos) | is.matrix(input_datos)) control_OK[[1]] <- TRUE else control_general <- FALSE 
    
    
    # Control 2...
    # El objeto input_datos debe tener 2 columnas.
    control_OK[[2]] <- FALSE
    names(control_OK[[2]]) <- c("Solo dos columnas")
    if (control_general == TRUE) {
      if (ncol(input_datos) == 2) control_OK[[2]] <- TRUE else control_general <- FALSE
    }
    
    
    
    
    # Control 3...
    # El objeto input_datos debe tener al menos 1 fila.
    control_OK[[3]] <- FALSE
    names(control_OK[[3]]) <- c("Al menos 1 fila de input_datos")
    if (control_general == TRUE) {
      if (nrow(input_datos) > 0) control_OK[[3]] <- TRUE else control_general <- FALSE 
    }
    
    
    
    # Control 4...
    # El objeto "MINI" (obtenido con na.omit(input_datos)) debe tener al menos una fila
    control_OK[[4]] <- FALSE
    names(control_OK[[4]]) <- c("Al menos 1 fila de MINI")
    if (control_general == TRUE) {
      
      MINI_CONTROL <- input_datos
      MINI_CONTROL <- na.omit(MINI_CONTROL)
      
      if (nrow(MINI_CONTROL) > 0) control_OK[[4]] <- TRUE else control_general <- FALSE 
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
    # La columna 2... convertida en FACTOR... debe tener 
    # al menos 2 niveles...
    control_OK[[6]] <- FALSE
    names(control_OK[[6]]) <- c("FACTOR con al menos 2 niveles")
    if (control_general == TRUE) {
      FACTOR_CONTROL <- as.factor(as.character(MINI_CONTROL[,2]))
      niveles_control <- levels(FACTOR_CONTROL)
      
      if (length(niveles_control) > 1) control_OK[[6]] <- TRUE else control_general <- FALSE
    }
    
    
    
    
    # Control 7...
    # Todos los niveles del factor, tienen al menos 2 repeticiones
    control_OK[[7]] <- FALSE
    names(control_OK[[7]]) <- c("Cada nivel de Factor con al menos 2 repeticiones")
    if (control_general == TRUE) {
      FACTOR_CONTROL <- as.factor(as.character(MINI_CONTROL[,2]))
      tabla_control <- table(FACTOR_CONTROL)
      
      detec <- tabla_control >= 2
      suma <- sum(as.numeric(detec))
      
      if (suma == length(detec)) control_OK[[7]] <- TRUE else control_general <- FALSE
    }
    
    
    
    # Control 8...
    # Todos los niveles del factor, tienen varianzas distintas de cero.
    control_OK[[8]] <- FALSE
    names(control_OK[[8]]) <- c("Todas las varianzas distintas de cero")
    if (control_general == TRUE) {
      
      VR_CONTROL <- MINI_CONTROL[,1]
      FACTOR_CONTROL <- as.factor(as.character(MINI_CONTROL[,2]))
      
      varianzas_control <- tapply(VR_CONTROL, FACTOR_CONTROL, var)
      
      detec <- varianzas_control > 0
      suma <- sum(as.numeric(detec))
      
      if (suma == length(detec)) control_OK[[8]] <- TRUE else control_general <- FALSE
    }
    
    
    ANOVA01$CONTROLES <- control_OK
    
    ANOVA01$CONTROL_GENERAL <- control_general
    
  } # # # FIN CONTROLES
  ###############################################################################################################################
  
  
  
  # Si supero todos los controles... podemos realizar el analisis estadistico de ANOVA a 1 FACTOR
  if (control_general == TRUE) {
    
    # Creacion del objeto MINI
    MINI <- input_datos
    MINI <- na.omit(MINI)
    
    ANOVA01$MINI <- MINI
    
    # Creacion de VR y FACTOR
    VR <- MINI[,1]
    FACTOR <- as.factor(as.character(MINI[,2]))
    
    
    
    # Test de ANOVA, TABLA ANOVA y decision
    {
    
    # En esta oportunidad... ingreso al test de ANOVA como VR y FACTOR...
    
    # Generamos los objetos donde guardaremos los resultados...
    ANALISIS <- list()
    ANALISIS$ORIGINAL <- list()
    ANALISIS$CONSULTORA <- list()
    
    # ANOVA ORIGINAL
    ANALISIS$ORIGINAL <-  aov(VR ~ FACTOR)
    
    # Tabla de ANOVA para CONSULTORA
    TABLA_ANOVA <- summary(ANALISIS$ORIGINAL)[[1]]
    colnames(TABLA_ANOVA) <- c("G.L.", "Sumas de Cuadrados", "Cuadrados Medios", "Valor F", "Valor p")
    TABLA_ANOVA <- as.data.frame(TABLA_ANOVA)
    for (fila in 1:nrow(TABLA_ANOVA)) for (columna in 1:ncol(TABLA_ANOVA)) TABLA_ANOVA[fila, columna] <- round(TABLA_ANOVA[fila, columna], input_decimales)
    
    
    
    p_interno_anova <- TABLA_ANOVA[1,5]
    p_interno_anova <- round(p_interno_anova, input_decimales)
    if (p_interno_anova < 0.001) p_externo_anova <- "<<0.001" else p_externo_anova <- p_interno_anova
    ANALISIS$CONSULTORA$p_interno <- p_interno_anova
    ANALISIS$CONSULTORA$p_externo <- p_externo_anova
    
    TABLA_ANOVA[1,5] <- p_externo_anova 
    TABLA_ANOVA[2,5] <- ""
    TABLA_ANOVA[2,4] <- ""
    
    TABLA_ANOVA[,1] <- as.character(TABLA_ANOVA[,1])
    
    
    rownames(TABLA_ANOVA)[1] <- paste("Factor: ", colnames(MINI)[2], sep="")
    rownames(TABLA_ANOVA)[2] <- "Residuos"
    ANALISIS$CONSULTORA$TABLA_ANOVA <- TABLA_ANOVA
    
    
    if (p_interno_anova >= input_alfa) decision_anova <- "No rechazo Ho" else decision_anova <- "Rechazo Ho"
    ANALISIS$CONSULTORA$decision <- decision_anova
    
    
    ANOVA01$ANALISIS <- ANALISIS
    
    } # Fin ANOVA, TABLA ANOVA y decision
    ###################################
    
    
    
    # Test de Tukey
    {
      # Objetos para tener en cuenta...
      ANOVA01$TUKEY$ORIGINAL <- list()
      ANOVA01$TUKEY$CONSULTORA <- list()
      
      # Tukey "Original"
      TUKEY_COMPLETO <- HSD.test(ANOVA01$ANALISIS$ORIGINAL, "FACTOR")
      ANOVA01$TUKEY$ORIGINAL <- TUKEY_COMPLETO
      
      # Tukey para la CONSULTORA
      TABLA_TUKEY <- TUKEY_COMPLETO$groups
      TABLA_TUKEY <- cbind(rownames(TABLA_TUKEY), TABLA_TUKEY)
      colnames(TABLA_TUKEY) <- c("Niveles del Factor", "Medias", "Grupos Estadísticos")
      
      FRASE_TUKEY <- c("El Test de Tukey solo tiene sentido si se rechaza la Ho de ANOVA.","<br/>",
                       "Letras iguales de 'Grupos Estadísticos' corresponden a niveles del factor estadísticamente iguales." ,"<br/>",
                       "Letras diferentes de 'Grupos Estadísticos' corresponden a niveles del factor estadísticamente distintos.")
      
      
      
      ANOVA01$TUKEY$CONSULTORA$TABLA_TUKEY <- TABLA_TUKEY
      
      ANOVA01$TUKEY$CONSULTORA$FRASE_TUKEY <- FRASE_TUKEY
      
    }
    #######################################
    
    
    
    # Analisis de los ERRORES
    {
      
      # Aislamos los ERRORES
      {
        ERROR <- ANOVA01$ANALISIS$ORIGINAL$residuals
        dim(ERROR) <- c(length(ERROR), 1)
        ERROR <- as.data.frame(ERROR)
        colnames(ERROR) <- "Residuos"
      }
      ##############################################
      
      
      # Normalidad de los Errores
      NORMALIDAD <- SHAPIRO_MASTER(ERROR, input_decimales, input_alfa)
      ANOVA01$REQUERIMIENTOS$NORMALIDAD <- NORMALIDAD
      
      
      
      # Homogeneidad de Varianzas de los ERRORES
      BASE_H <- cbind(ERROR, FACTOR)
      HOMOGENEIDAD <- BARTLETT_MASTER(BASE_H, input_decimales, input_alfa, input_ingreso="anova")
      ANOVA01$REQUERIMIENTOS$HOMOGENEIDAD <- HOMOGENEIDAD
      
      
      # Creacion de un RESUMEN_NH
      {
      nombres <- c("Normalidad", "Homogeneidad", "Utilización de ANOVA")
      RESUMEN_NH <- as.data.frame(matrix(NA, 1, length(nombres)))
      colnames(RESUMEN_NH) <- nombres
      
      normalidad <-   NORMALIDAD$ANALISIS$CONSULTORA$normalidad
      homogeneidad <- HOMOGENEIDAD$ANALISIS$CONSULTORA$homogeneidad
      
      RESUMEN_NH[1,1] <- normalidad
      RESUMEN_NH[1,2] <- homogeneidad
      if (normalidad == "SI" && homogeneidad == "SI") RESUMEN_NH[1,3] <- "Validada" else RESUMEN_NH[1,3] <- "Descartada"
      
      ANOVA01$REQUERIMIENTOS$RESUMEN_NH <-  RESUMEN_NH 
      }
      ###################################################
      
      
      
      # Frase de NH
      # frase3_anova_html: segun valor "SI" o "NO" de normalidad y homogeneidad
      {
        
        frase3_v1 <- c("No se cumple con el requisito distribución normal de los Residuos/Errores.", "<br/>",
                       "Todos los resultados estadísticos del test de ANOVA a 1 FACTOR son incorrectos, y no puede sacarse ningún tipo de conclusión sobre ellos.", "<br/>",
                       "Indistintamente del valor p obtenido en el ANOVA, este análisis debe ser descartado.", "<br/>",
                       "Para analizar este pool de datos y poder sacar conclusiones correctas, debe utilizarse el test de Kruskal-Wallis.")
        
        frase3_v2 <- c("No se cumple con el requisito de homogeneidad de varianzas de los Residuos/Errores.", "<br/>",
                       "Todos los resultados estadísticos del test de ANOVA a 1 FACTOR son incorrectos, y no puede sacarse ningún tipo de conclusión sobre ellos,", "<br/>",
                       "Indistintamente del valor p obtenido en el ANOVA, este análisis debe ser descartado.", "<br/>",
                       "Para analizar este pool de datos y poder sacar conclusiones correctas, debe utilizarse el test de Kruskal-Wallis.")
        
        
        frase3_v3 <- c("No se cumplen los requisitos de distribución normal de los Residuos/Errores, ni de homogeneidad de varianzas de los mismos.", "<br/>",
                       "Todos los resultados estadísticos del test de ANOVA a 1 FACTOR son incorrectos, y no puede sacarse ningún tipo de conclusión sobre ellos,", "<br/>",
                       "Indistintamente del valor p obtenido en el ANOVA, este análisis debe ser descartado.", "<br/>",
                       "Para analizar este pool de datos y poder sacar conclusiones correctas, debe utilizarse el test de Kruskal-Wallis.")
        
        
        
        
        frase3_v4 <- c("Se cumplen los requisitos necesarios de Normaldiad de los errores y de homogeneidad de varianzas de los errores." , "<br/>",
                       "Es válido sacar conclusiones de la tabla de ANOVA", "<br/>",
                       "Para este pool de datos, es mejor utilizar el test de ANOVA que el test de Kruskal-Wallis.")
        
        
        if (normalidad == "NO" && homogeneidad == "SI") FRASE_NH <- frase3_v1 else if (normalidad == "SI" && homogeneidad == "NO") FRASE_NH <- frase3_v2 else if (normalidad == "NO" && homogeneidad == "NO") FRASE_NH <- frase3_v3 else if (normalidad == "SI" && homogeneidad == "SI") FRASE_NH <- frase3_v4
        
        ANOVA01$REQUERIMIENTOS$FRASE_NH <- FRASE_NH
        
      } # Fin Frase3_html
      #######################################
      
      
      
      
    } # Fin ANALISIS DE LOS ERRORES
    ##############################################
    
    
    # Resumen ANOVA
    {  #          1        2        3         4                  5         6       7
      nombres <- c("VR", "FACTOR", "Test", "Estadístico F",  "Valor p", "Alfa", "Decisión")
      RESUMEN_ANOVA <- as.data.frame(matrix(NA, 1, length(nombres)))
      colnames(RESUMEN_ANOVA) <- nombres
      
      RESUMEN_ANOVA[1,1] <- ANOVA01$DETALLES$nombre_VR
      RESUMEN_ANOVA[1,2] <- ANOVA01$DETALLES$nombre_FACTOR
      RESUMEN_ANOVA[1,3] <- "ANOVA 1 Factor"
      RESUMEN_ANOVA[1,4] <- ANOVA01$ANALISIS$CONSULTORA$TABLA_ANOVA[1,4]
      RESUMEN_ANOVA[1,5] <- ANOVA01$ANALISIS$CONSULTORA$p_externo
      RESUMEN_ANOVA[1,6] <- ANOVA01$input$alfa
      RESUMEN_ANOVA[1,7] <- ANOVA01$ANALISIS$CONSULTORA$decision
      
      ANOVA01$ANALISIS$CONSULTORA$RESUMEN_ANOVA <- RESUMEN_ANOVA
      
    }
    ####################################
    
    
    # Frase de ANOVA
    # frase2_anova_html: segun valor p
    {
      
      frase2_v1 <- c("No pudo obtenerse un valor p.")
      
      frase2_v2 <- c("El valor p es mayor que el valor de alfa=", ANOVA01$input$alfa, ".","<br/>",
                     "No se rechaza la Ho.", "<br/>",
                     "Todos los niveles del factor '", ANOVA01$DETALLES$nombre_FACTOR, "' son estadísticamente equivalentes entre si.", "<br/>",
                     "Todos los niveles del factor '", ANOVA01$DETALLES$nombre_FACTOR, "' presentan medias estadísticamente iguales.", "<br/>")
      
      
      frase2_v3 <- c("El valor p es igual que el valor de alfa=", ANOVA01$input$alfa, ".","<br/>",
                     "No se rechaza la Ho.", "<br/>",
                     "Todos los niveles del factor '", ANOVA01$DETALLES$nombre_FACTOR, "' son estadísticamente equivalentes entre si.", "<br/>",
                     "Todos los niveles del factor '", ANOVA01$DETALLES$nombre_FACTOR, "' presentan medias estadísticamente iguales.", "<br/>")
      
      
      
      
      frase2_v4 <- c("El valor p es menor que el valor de alfa=", ANOVA01$input$alfa, ".","<br/>",
                     "Se rechaza la Ho.", "<br/>",
                     "Existen niveles del factor '", ANOVA01$DETALLES$nombre_FACTOR, "' estadísticamente distintos.", "<br/>",
                     "Existen niveles del factor '", ANOVA01$DETALLES$nombre_FACTOR, "' que presentan medias estadísticamente diferentes.", "<br/>")
      
      
      if(is.na(p_interno_anova) | is.null(p_interno_anova)) frase_elegida_anova <- frase2_v1 else if (p_interno_anova > input_alfa) frase_elegida_anova <- frase2_v2 else if (p_interno_anova == input_alfa) frase_elegida_anova <- frase2_v3 else if (p_interno_anova < input_alfa) frase_elegida_anova <- frase2_v4
      
      ANOVA01$ANALISIS$CONSULTORA$frase2_anova_html <- frase_elegida_anova
      
    } # Fin Frase2_html
    #######################################
    
    
    # SALIDA ARMADA
    {    
      SALIDA_ARMADA <- list()
      
      # Elementos de la Salida Armada
      {
      
      SALIDA_ARMADA$FRASE_INICIAL <- "Test de ANOVA a 1 FACTOR"
      
      SALIDA_ARMADA$TABLA_ANOVA   <-   ANOVA01$ANALISIS$CONSULTORA$TABLA_ANOVA
      
      SALIDA_ARMADA$RESUMEN_ANOVA <-   ANOVA01$ANALISIS$CONSULTORA$RESUMEN_ANOVA
      
      SALIDA_ARMADA$FRASE_ANOVA   <-   ANOVA01$ANALISIS$CONSULTORA$frase2_anova_html
      
      SALIDA_ARMADA$TABLA_TUKEY   <-   ANOVA01$TUKEY$CONSULTORA$TABLA_TUKEY
      
      SALIDA_ARMADA$FRASE_TUKEY   <-   ANOVA01$TUKEY$CONSULTORA$FRASE_TUKEY
      
      SALIDA_ARMADA$RESUMEN_NH    <-   ANOVA01$REQUERIMIENTOS$RESUMEN_NH
      
      SALIDA_ARMADA$FRASE_NH      <-   ANOVA01$REQUERIMIENTOS$FRASE_NH
      
      } # Fin Salida Armada
      ##########
      
      
      ANOVA01$SALIDA_ARMADA <- SALIDA_ARMADA   
      
    } # FIN SALIDA ARMADA
    ###################################################
    
  } # Fin if control_general == TRUE
  #############################################################################################################################
  
  
  
  
  # Si no supero todos los controles...
  if (control_general == FALSE) {
    
    candado_externo <- FALSE
    contador_externo <- 0
    while(candado_externo == FALSE) {
      
      contador_externo <- contador_externo + 1
      
      if (control_OK[[contador_externo]] == FALSE) {
        
        # SALIDA ARMADA
        {    
          SALIDA_ARMADA <- list()
          
          # Elementos de la Salida Armada
          {
          
          SALIDA_ARMADA$FRASE_INICIAL <-   "Test de ANOVA a 1 FACTOR"
          
          SALIDA_ARMADA$TABLA_ANOVA   <-   matrix("No puede realizarse el test de ANOVA", 1, 5)
          
          SALIDA_ARMADA$RESUMEN_ANOVA <-   matrix("No puede realizarse el test de ANOVA", 1, 5)
          
          SALIDA_ARMADA$FRASE_ANOVA   <-   frases_control[[contador_externo]]
          
          SALIDA_ARMADA$TABLA_TUKEY   <-   matrix("No puede realizarse el test de ANOVA", 1, 5)
          
          SALIDA_ARMADA$FRASE_TUKEY   <-   frases_control[[contador_externo]]
          
          SALIDA_ARMADA$RESUMEN_NH    <-   frases_control[[contador_externo]]
          
          SALIDA_ARMADA$FRASE_NH      <-   frases_control[[contador_externo]]
          
          } # Fin Salida Armada
          ##########
          
          
          ANOVA01$SALIDA_ARMADA <- SALIDA_ARMADA   
          
        } # Fin bucle while
        
        candado_externo <- TRUE 
      } # Fin control_OK == FALSE
    } # FIN SALIDA ARMADA
    ###################################################
    
    
    
  } # Fin control_general == FALSE
  
  
  ANOVA01
  
} # Fin function ANOVA01_MASTER()





if (1 == 2){
  
  # Agregamos todos los input 
  #   
  BASE <- mtcars[,c(1,2)]
  input_alfa <- 0.05
  input_decimales <- 2
  input_datos <- BASE
  
  
  AVER <-   ANOVA01_MASTER(BASE)
  
} # Fin 1 == 2
