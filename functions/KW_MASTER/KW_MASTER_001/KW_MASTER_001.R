

KW_MASTER <- function(input_datos=NULL, input_decimales=2, input_alfa=0.05){
  
  library(agricolae)
  
  # Parte 0: Input
  # Agregamos todos los input 
#   
    # input_datos <- mtcars[,c(1,2)]
    # input_alfa <- 0.05
    # input_decimales <- 2
    # input_metodo <- "pearson"
  
  MINI <- input_datos
  MINI <- na.omit(MINI)
  
#   VR <- TIJERA(MINI, 1)
#   FACTOR <- TIJERA(MINI, 2)
#   
  
  VR <- MINI[,1]
  FACTOR <- MINI[,2]
  
 
  
  # Inicio
  KW <- list()
  
  # Ordenamiento General
  KW$input <- list()
  KW$DETALLES <- list()
  KW$ANALISIS <- list()
  KW$SALIDA_ARMADA <- list()
  
  
  KW$input$datos <- input_datos
  KW$input$decimales <- input_decimales
  KW$input$alfa <- input_alfa
  
  KW$DETALLES$nombre_VR <- colnames(MINI)[1]
  KW$DETALLES$nombre_FACTOR <- colnames(MINI)[2]
  
  
  

  # Test de KW y decision
  {
  
  # En esta oportunidad... ingreso al test de kw como VR y FACTOR...
  # Pero puede armarse para ingresar con el nombre de las variables, y así las salidas tienen el nombre de las variables.
  
    ANALISIS <- kruskal(VR, FACTOR, input_alfa)
    ANALISIS$statistic <- round2(ANALISIS$statistic, input_decimales)
    ANALISIS$p.value <- ANALISIS$statistic[,3]
    
    p_interno_kw <- ANALISIS$p.value
    if (p_interno_kw < 0.001) p_externo_kw <- "<<0.001" else p_externo_kw <- p_interno_kw
    ANALISIS$p_externo <- p_externo_kw
    
    if (p_interno_kw >= input_alfa) decision_kw <- "No rechazo Ho" else decision_kw <- "Rechazo Ho"
    ANALISIS$decision <- decision_kw
  } # Fin KW de Pearson
  ###################################
  
 
  
  # Agregamos los analisis al objeto general "KW"
  KW$ANALISIS <- ANALISIS
   
  # Tabla de Grupos KW
  {
    TABLA_GRUPOS <- ANALISIS$groups 
    TABLA_GRUPOS <- cbind(rownames(TABLA_GRUPOS), TABLA_GRUPOS) 
    TABLA_GRUPOS <- TABLA_GRUPOS[,c(1,2,2,3)]
    
    colnames(TABLA_GRUPOS) <- c("Nivel del Factor", "n", "Mediana", "Grupo Estadístico")
    for (heaven in 1:nrow(TABLA_GRUPOS)) {
      
      este_nivel <- TABLA_GRUPOS[heaven,1]
      detec <- FACTOR == este_nivel
      mini_VR <- VR[detec]
      
      esta_rep <- length(mini_VR)
      
      esta_mediana <- median(mini_VR)
      esta_mediana <- round2(esta_mediana, input_decimales)
    
      TABLA_GRUPOS[heaven,2] <- as.character(esta_rep)
      TABLA_GRUPOS[heaven,3] <- esta_mediana
      
      
    } # Fin for heaven
    
    KW$ANALISIS$TABLA_GRUPOS <- TABLA_GRUPOS
    
    KW$ANALISIS$FRASE_GRUPOS <- c("Letras de 'Grupo Estadístico' iguales, indica niveles del Factor estadísticamente iguales.","<br/>",
                                  "Letras de 'Grupo Estadístico' distintas, indica niveles del Factor estadísticamente diferentes.")
    
  }
  #######################################
  
  
 

  
  
  # Resumen KW
  {  #          1        2        3         4                  5         6       7
  nombres <- c("VR", "FACTOR", "Test", "Estadístico Chi",  "Valor p", "Alfa", "Decisión")
  RESUMEN_KW <- as.data.frame(matrix(NA, 1, length(nombres)))
  colnames(RESUMEN_KW) <- nombres
  
  RESUMEN_KW[1,1] <- KW$DETALLES$nombre_VR
  RESUMEN_KW[1,2] <- KW$DETALLES$nombre_FACTOR
  RESUMEN_KW[1,3] <- "Kruskal-Wallis"
  RESUMEN_KW[1,4] <- KW$ANALISIS$statistic[1]
  RESUMEN_KW[1,5] <- KW$ANALISIS$p_externo
  RESUMEN_KW[1,6] <- KW$input$alfa
  RESUMEN_KW[1,7] <- KW$ANALISIS$decision
  
  KW$ANALISIS$RESUMEN_KW <- RESUMEN_KW
  
  }
  ####################################
  

  
  
  
  
  # Frase de Pearson
  # frase2_kw_html: segun valor p
  {
    
    frase2_v1 <- c("No pudo obtenerse un valor p.")
    
    frase2_v2 <- c("El valor p es mayor que el valor de alfa=", KW$input$alfa, ".","<br/>",
                   "No se rechaza la Ho.", "<br/>",
                   "Todos los niveles del factor '", KW$DETALLES$nombre_FACTOR, "' son estadísticamente equivalentes entre si.", "<br/>",
                   "Todos los niveles del factor '", KW$DETALLES$nombre_FACTOR, "' presentan medianas estadísticamente iguales.", "<br/>")
    
    
    frase2_v3 <- c("El valor p es igual que el valor de alfa=", KW$input$alfa, ".","<br/>",
                   "No se rechaza la Ho.", "<br/>",
                   "Todos los niveles del factor '", KW$DETALLES$nombre_FACTOR, "' son estadísticamente equivalentes entre si.", "<br/>",
                   "Todos los niveles del factor '", KW$DETALLES$nombre_FACTOR, "' presentan medianas estadísticamente iguales.", "<br/>")
    
    
    
    
    frase2_v4 <- c("El valor p es menor que el valor de alfa=", KW$input$alfa, ".","<br/>",
                   "Se rechaza la Ho.", "<br/>",
                   "Existen niveles del factor '", KW$DETALLES$nombre_FACTOR, "' estadísticamente distintos.", "<br/>",
                   "Existen niveles del factor '", KW$DETALLES$nombre_FACTOR, "' que presentan medianas estadísticamente diferentes.", "<br/>")
    
    
    if(is.na(KW$ANALISIS$p.value) | is.null(KW$ANALISIS$p.value)) KW$ANALISIS$frase2_kw_html <- frase2_v1 else if (KW$ANALISIS$p.value > KW$input$alfa) KW$ANALISIS$frase2_kw_html <- frase2_v2 else if (KW$ANALISIS$p.value == KW$input$alfa) KW$ANALISIS$frase2_kw_html <- frase2_v3 else if (KW$ANALISIS$p.value < KW$input$alfa) KW$ANALISIS$frase2_kw_html <- frase2_v4
    
  } # Fin Frase2_html
  #######################################
  
  
  
  
  
  # Salida Armanda
  
  SALIDA_ARMADA <- list()
  
  # Si se puede Pearson
  {
    
    SALIDA_ARMADA$FRASE_INICIAL <- "Test de Kruskal-Wallis"
    
    SALIDA_ARMADA$RESUMEN_KW <- KW$ANALISIS$RESUMEN_KW
    
    SALIDA_ARMADA$FRASE_KW <-   KW$ANALISIS$frase2_kw_html
    
    SALIDA_ARMADA$TABLA_GRUPOS <- KW$ANALISIS$TABLA_GRUPOS
    
    SALIDA_ARMADA$FRASE_GRUPOS <- KW$ANALISIS$FRASE_GRUPOS
    
  }
  
  
  
  KW$SALIDA_ARMADA <- SALIDA_ARMADA
  
  KW
  
} # Fin function KW_MASTER()





if (1 == 2){
  
  # Agregamos todos los input 
  #   
      BASE <- mtcars[,c(1,2)]
      input_alfa <- 0.05
      input_decimales <- 2
      input_metodo <- "pearson"
  
  
  AVER <-   KW_MASTER(BASE)
  
} # Fin 1 == 2
