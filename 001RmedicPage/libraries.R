# libraries.R

# Librerias de la página inicial
require(shiny)
require(rCharts)
require(datasets)
# require(clusterProfiler)
library(plotly)
library(shiny)
library(colourpicker)
library(readxl)
library(DT)
library(datasets)
library(Hmisc)

# Librerias del RMedic
library(shiny)
library(RCurl)
library(XML)
library(gdata)
library(XLConnect)
library(ggplot2)
library(shinyjs)
library(ggvis)
library(sfsmisc) # errbar()
library(gplots)
library(agricolae)
library(coin) # Mann-Whitney para Wilcoxon test


# Las nuevas para mi escript de subida de archivos...
library(shiny)
library(RCurl)
library(XML)
library(gdata)
library(XLConnect)
library(ggplot2)



#### NUEVO

AVER <- data.frame(matrix(NA, 4, 3))

AVER[,1] <- c(1:nrow(AVER))
AVER[,2] <- c(1.2, "A", "1..4", "1,3")
AVER[,3] <- c("A b", "Z Z", " ", "  ")







num_control <- function(BASE=NULL, cols=c(1:ncol(BASE))){
  
  # stopifnot(!is.null(BASE))
  # stopifnot(!is.null(cols))
  
  aviso1 <- list()
  aviso2 <- list()
  aviso3 <- list()
  
  nombres <- c("Celdas Vacías", "Vacío", "Letras Minúsculas", "Letras Mayúsculas", "Espacios", "Símbolos", "Comas", "No Punto", "Solo 1 Punto", "Dos o mas puntos", "Números", "OK")
  ORDEN <- list()
  FRASE <- list()
  
  
  if (length(cols) == 1) {
    
    B2 <- data.frame(BASE)
    colnames(B2) <- names(BASE)
    BASE <- B2
    
  }
  
  numeros <- c(0:9)
  punto <- c(".")
  letras <- letters
  LETRAS <- LETTERS
  espacio <- c(" ")
  coma <- c(",")
  orden <- c(1:nrow(BASE))
  vacio <- c("")
  vacio2 <- NA
  
  elementos      <- list(numeros,   punto,   letras,   LETRAS,   espacio,   coma)
  names(elementos) <- c("numeros", "punto", "letras", "LETRAS", "espacio", "coma")
  # col_eleccion <- c(1,3:7,10)
  
  col_eleccion <- c(1:length(nombres))
  
  
  
  for (n in 1:length(cols)) {
    
    
    
    
    cat(n, "\n")  
    ORDEN[[n]] <- list()
    FRASE[[n]] <- list()
    
    for (z in 1:length(col_eleccion)) {
      
      ORDEN[[n]][[z]] <- list()
      FRASE[[n]][[z]] <- list()
    }
    
    names(ORDEN[[n]]) <- nombres[col_eleccion]
    
    
    MINI <- BASE[,cols[n]]
    
    dt <- is.numeric(MINI)
    
    aviso1[[n]] <- list()
    if(dt == TRUE) {
      
      aviso1[[n]] <- c("Es numérica")
    } 
    
    if(dt == FALSE) {
      
      
      # No es numerica...    
      
      aviso1[[n]] <- c("NO es numérica")
    }
    
    #   orden <- rep(NA, length(MINI))
    
    # Entonces... nos fijamos en cada dato...
    #  1) Si es NA...
    #  2) Si es vacio...
    #  3) Presenta letras minusculas
    #  4) Presenta letras mayusculas
    #  5) Presenta espacios
    #  6) Presenta simbolos  (cualquier cosa que
    #     no es un número, ni una letra, ni la coma, ni punto, ni un espacio)
    #  7) Presenta comas
    #  8) No presenta punto  
    #  9) Presenta solo un punto  
    # 10) Presenta dos o más punto
    # 11) Presenta numeros
    # 12) Todo OK
    
    
    cantidad <- length(MINI)
    
    
    #    nombres <- c("Is NA", "vacio", "letras minúsculas", "letras mayúsculas", "espacios", "simbolos", "comas", "No Punto", "Solo 1 punto", "dos o mas puntos", "Numeros", "OK")
    DT <- matrix(FALSE, cantidad, length(nombres))
    colnames(DT) <- nombres
    
    
    # Genera la matriz de deteccion para cada elemento...
    for( h in 1:cantidad) {
      #     cat(h, "\n")
      este <- MINI[h]
      
      d1 <-  is.na(este)
      if ( sum(d1)  > 0)  DT[h, 1] <- TRUE   # Is NA
      
      if ( sum(d1)  == 0) {
        d2 <-  este == vacio 
        if ( sum(d2)  > 0)  DT[h, 2] <- TRUE   # Vacio
        
        # Si no es NA y no es vacio...
        
        if (sum(d1,d2) == 0) {            
          
          # Partimos a "este"
          este <- as.character(este)
          metralla <- strsplit(este, "")[[1]]
          digitos <- length(metralla)
          
          metralla2 <- metralla
          metralla <- unique(metralla)
          
          
          todo <- c(letras, LETRAS, punto, coma, numeros, espacio, vacio)
          
          
          
          d3 <- duplicated(c(metralla, letras))
          d4 <- duplicated(c(metralla, LETRAS))
          d5 <- duplicated(c(metralla, espacio))
          
          d6 <- FALSE
          for (k in 1:length(metralla)) {
            if (d6 == FALSE) {
              
              esta_metralla <- metralla[k]
              dtm <- esta_metralla == todo
              if (sum(dtm) == 0) d6 <- TRUE
            }
          }
          
          
          
          d7 <- duplicated(c(metralla, coma))
          d8 <- metralla2 == punto
          d9 <- duplicated(c(metralla, numeros))
          
          if ( sum(d3)  > 0)  DT[h,  3] <- TRUE   # Letras minusculas
          if ( sum(d4)  > 0)  DT[h,  4] <- TRUE   # Letras Mayusculas
          if ( sum(d5)  > 0)  DT[h,  5] <- TRUE   # Espacios
          if ( sum(d6) == 1)  DT[h,  6] <- TRUE   # Simbolos
          if ( sum(d7)  > 0)  DT[h,  7] <- TRUE   # Hay comas
          if ( sum(d8) == 0)  DT[h,  8] <- TRUE   # No hay punto
          if ( sum(d8) == 1)  DT[h,  9] <- TRUE   # Solo 1 punto
          if ( sum(d8)  > 1)  DT[h, 10] <- TRUE   # Dos o mas puntos
          if ( sum(d9)  > 0)  DT[h, 11] <- TRUE   # Hay numeros
          if ((sum(d3, d4, d5, d6, d7) == 0) && sum(d8) < 2 && (sum(d8, d9) == digitos))  DT[h,12] <- TRUE   # Todo OK
          
        }
      }
    }
    
    DT2 <- DT[,col_eleccion]        
    # Generamos la frase para cada celda 
    
    
    for (z in 1:ncol(DT2)) {
      
      #      ORDEN[[n]][[z]] <- list()
      #names(ORDEN[[n]][[z]]) <- colnames(DT2)
      
      ORDEN[[n]][[z]] <- orden[DT2[,z]]
      #      FRASE[[n]][[z]] <- list()
    }
    
    #   names(ORDEN[[n]]) <- colnames(DT2)
    
    
    
    
    
    #                       1            2               3                   4          
    #   nombres <- c("Celdas Vacías", "Vacio", "Letras Minúsculas", "Letras Mayúsculas",
    #                       5            6          7         8           9         
    #                  "Espacios", "Simbolos", "Comas", "No Punto", "Solo 1 Punto",
    #                      10                11      12         
    #                 "Dos o mas puntos", "Números", "OK")
    
    # Frases por Defecto...
    FRASE[[n]][[1]] <- c("Las siguientes celdas no poseen datos.")
    FRASE[[n]][[2]] <- c("Las siguientes celdas son vacías.")
    FRASE[[n]][[3]] <- c("Las siguientes celdas poseen letras minúsculas. La información debe ser corregida o dejar las celdas vacías.")
    FRASE[[n]][[4]] <- c("Las siguientes celdas poseen letras mayúsculas. La información debe ser corregida o dejar las celdas vacías.")
    FRASE[[n]][[5]] <- c("Las siguientes celdas presentan espacios. Deben eliminarse los espacios de las celdas.")
    FRASE[[n]][[6]] <- c("Las siguientes celdas presentan símbolos. Deben ser corregidos a valores numéricos o dejar las celdas vacías.")
    FRASE[[n]][[7]] <- c("Las siguientes celdas presentan comas. Deben ser eliminadas o cambiados por puntos, según corresponda.")
    FRASE[[n]][[8]] <- c("Las siguientes celdas no presentan punto. Esto puede o no ser un error.")
    FRASE[[n]][[9]] <- c("Las siguientes celdas presentan un solo punto. Esto puede o no ser un error.")
    FRASE[[n]][[10]] <- c("Las siguientes celdas presentan más de un separador decimal (punto). Las celdas deben contener solo un separador decimal o quedar vacías, según corresponda.")
    FRASE[[n]][[11]] <- c("Las siguientes celdas presentan valores numéricos. Esto en puede o no ser un error.")
    FRASE[[n]][[12]] <- c("Las siguientes celdas presentan el formato correcto: solo números y pueden contener un punto como separador decimal.")
    names(FRASE[[n]]) <- nombres[col_eleccion]
    
    
    
    # Cambios en frases de cada uno...
    
    # 1) Celdas NA
    if (length(orden[DT2[,1]]) == 0)  FRASE[[n]][[1]] <- c("No se presentan celdas vacías.")
    if (length(orden[DT2[,1]]) == nrow(DT2))  FRASE[[n]][[1]] <- c("Todas las celdas están vacías. No se presenta información en la columna seleccionada.")
    
    # 2) Celdas Vacias
    if (length(orden[DT2[,2]]) == 0)  FRASE[[n]][[2]] <- c("No se presentan celdas vacías.")
    if (length(orden[DT2[,2]]) == nrow(DT2))  FRASE[[n]][[2]] <- c("Todas las celdas están vacías. No se presenta información en la columna seleccionada.")
    
    # 3) Letras Minusculas
    if (length(orden[DT2[,3]]) == 0)  FRASE[[n]][[3]] <- c("No se presentan celdas con letras minúsculas.")
    
    # 4) Letras Minusculas
    if (length(orden[DT2[,4]]) == 0)  FRASE[[n]][[4]] <- c("No se presentan celdas con letras mayúsculas.")
    
    # 5) Espacios
    if (length(orden[DT2[,5]]) == 0)  FRASE[[n]][[5]] <- c("No se presentan espacios.")
    
    # 6) Simbolos
    if (length(orden[DT2[,6]]) == 0)  FRASE[[n]][[6]] <- c("No se presentan símbolos.")
    
    # 7) Comas
    if (length(orden[DT2[,7]]) == 0)  FRASE[[n]][[7]] <- c("No se presentan comas.")
    
    # 8) Sin Puntos
    if (length(orden[DT2[,8]]) == nrow(DT2))  FRASE[[n]][[8]] <- c("Ninguna celda presenta puntos.")
    if (length(orden[DT2[,8]]) == 0)  FRASE[[n]][[8]] <- c("Todas las celda presentan al menos 1 punto.")
    
    # 9) Solo un punto
    if (length(orden[DT2[,9]]) == nrow(DT2))  FRASE[[n]][[9]] <- c("Todas las celdas presentan solo 1 punto.")
    if (length(orden[DT2[,9]]) == 0)  FRASE[[n]][[9]] <- c("Ninguna celda presenta solo un punto.")
    
    # 10) Dos o mas puntos
    if (length(orden[DT2[,10]]) == nrow(DT2))  FRASE[[n]][[10]] <- c("Todas las celdas presentan 2 o más puntos. Todas las celdas deben ser corregidas.")
    if (length(orden[DT2[,10]]) == 0)  FRASE[[n]][[10]] <- c("Ninguna celda presenta dos o más puntos.")
    
    
    # 11) Numeros
    if (length(orden[DT2[,11]]) == nrow(DT2))  FRASE[[n]][[11]] <- c("Todas las celdas presentan números.")
    if (length(orden[DT2[,11]]) == 0)  FRASE[[n]][[11]] <- c("Ninguna celda presenta números. Todas las celdas deben ser corregidas, o tal vez se equivoco en la elección de la columna.")
    
    # 12) OK
    if (length(orden[DT2[,12]]) == nrow(DT2))  FRASE[[n]][[12]] <- c("Todas las celdas tienen el formato correcto. Recomendamos verificar el valor mínimo y máximo de la variable.")
    if (length(orden[DT2[,12]]) == 0)  FRASE[[n]][[12]] <- c("Ninguna celda presenta el formato correcto. Todas las celdas deben ser corregidas, o tal vez se equivoco en la elección de la columna.")
    
    
    
    #      ORDEN[[n]][[z]] <- list()
    #names(ORDEN[[n]][[z]]) <- colnames(DT2)
    
    ORDEN[[n]][[z]] <- orden[DT2[,z]]
    #      FRASE[[n]][[z]] <- list()
    
    
    
  }
  
  CONTROL <- list()
  CONTROL[[1]] <- list()
  CONTROL[[1]] <- ORDEN
  
  CONTROL[[2]] <- list()
  CONTROL[[2]] <- FRASE
  
  names(CONTROL) <- c("Orden", "Frase")
  
  
  
  ############
  # RECORTE!!!
  # Vamos a quitar los elementos
  # 2 - Vacios
  # 8 - No Puntos
  # 9 - Solo un punto
  
  
  
  if (1 == 1) {
    original <- c(1:length(CONTROL[[1]][[1]]))
    retiro <- c(2, 8, 9)
    tf <- rep(TRUE, length(original))
    tf[retiro] <- FALSE
    nuevo <- original[tf]
    
    BETA_CONTROL  <- list()
    
    BETA_CONTROL[[1]] <- list()
    BETA_CONTROL[[1]][[1]] <- list()
    BETA_CONTROL[[2]] <- list()
    BETA_CONTROL[[2]][[1]] <- list()
    
    for (p in 1:length(nuevo)) {
      BETA_CONTROL[[1]][[1]][[p]] <- list()
      BETA_CONTROL[[2]][[1]][[p]] <- list()
    }
    
    names(BETA_CONTROL) <- names(CONTROL)
    names(BETA_CONTROL[[1]][[1]]) <- names(CONTROL[[1]][[1]])[nuevo]
    names(BETA_CONTROL[[2]][[1]]) <- names(CONTROL[[2]][[1]])[nuevo]
    
    
    for (p in 1:length(nuevo)) {
      BETA_CONTROL[[1]][[1]][[p]] <- CONTROL[[1]][[1]][[nuevo[p]]]
      BETA_CONTROL[[2]][[1]][[p]] <- CONTROL[[2]][[1]][[nuevo[p]]]
    }
    
    
    # remove(CONTROL_NUMERICO)
    CONTROL <- BETA_CONTROL
    
  } # 1 == 2
  
  
  
  return(CONTROL)
}







cat_control <- function(BASE=NULL, cols=c(1:ncol(BASE))){
  
  
  aviso1 <- list()
  aviso2 <- list()
  aviso3 <- list()
  
  
  if (length(cols) == 1) {
    
    B2 <- data.frame(BASE)
    colnames(B2) <- names(BASE)
    BASE <- B2
    
  }
  
  names_var <- colnames(BASE)
  orden <- c(1:nrow(BASE))
  ORDEN <- list()
  FRASE <- list()
  
  
  
  
  # Para cada variable...
  for (n in 1:length(cols)) {
    
    # Solo los datos de la columna...
    MINI <- BASE[,cols[n]]
    MINI <- as.character(MINI)
    
    # Tabla de Frecuencias
    FA <- table(MINI)
    lvls <- names(FA)
    
    
    mo <- c(1:length(lvls))
    tabs_names <- paste0("(", mo, ") ", lvls)
    tabs_name2 <- c("Celdas Vacías", tabs_names)
    
    
    cat(n, "\n")  
    ORDEN[[n]] <- list()
    FRASE[[n]] <- list()
    # Para cada pestania que habra...
    for (z in 1:length(tabs_name2)) {
      
      ORDEN[[n]][[z]] <- list()
      FRASE[[n]][[z]] <- list()
    }
    
    names(ORDEN[[n]]) <- tabs_name2
    
    
    
    
    
    
    # No es numerica...    
    aviso1[[n]] <- c("Variable Cualitativa")
    
    
    # Entonces... nos fijamos en cada dato...
    # Y lo que hace es, identificar en que filas hay:
    # 1) Celdas NA
    # 2) Celdas Vacías
    # 3) Cada nivel de la variable categorica...
    
    
    cantidad <- length(MINI)
    
    
    #      nombres <- c("Is NA", "Vacio", "letras minúsculas", "letras mayúsculas", "espacios", "simbolos", "comas", "No Punto", "Solo 1 punto", "dos o mas puntos", "Numeros", "OK")
    DT <- matrix(FALSE, cantidad, length(tabs_name2))
    colnames(DT) <- tabs_name2
    vacio <- c("")
    
    # Genera la matriz de deteccion para cada elemento...
    for(h in 1:cantidad) {
      
      # Este dato en particular...
      este <- MINI[h]
      
      # Nos fijamos si es NA
      d1 <-  is.na(este)
      if ( sum(d1)  > 0)  DT[h, 1] <- TRUE   # Is NA
      
      # Si no es NA...
      if ( sum(d1)  == 0) {
        
        
        
        
        for (g in 1:length(lvls)) {
          
          este_lvl <- lvls[g]
          
          if (este == este_lvl) {
            
            DT[h, (g + 1)] <- TRUE
            
          }
          
        }
        
        
        
        
      }
    }
    
    DT2 <- DT
    # DT2 <- DT[,col_eleccion]        
    # Generamos la frase para cada celda 
    for (z in 1:ncol(DT2)) {
      
      
      ORDEN[[n]][[z]] <- orden[DT2[,z]]
    }
    
    #   names(ORDEN[[n]]) <- colnames(DT2)
    
    
    # # Frases por Defecto...
    # 1) Celdas vacias (NA)
    FRASE[[n]][[1]] <- c("Las siguientes celdas no presenta información:")
    
    
    # 2) Todas las otras categorias 
    for (z in 2:ncol(DT2)) {
      
      categoria <- NA
      categoria <- lvls[z-1]
      FRASE[[n]][[z]] <- paste0("La categoría '", categoria , "' se encuentra en las siguientes filas:")
    }
    
    
    
    # # Frases Especiales para Celdas vacias
    if (length(orden[DT2[,1]]) == 0) FRASE[[n]][[1]] <- "Todas las celdas contienen información."
    if (length(orden[DT2[,1]]) == nrow(DT2)) FRASE[[n]][[1]] <- "La variable NO contiene información. Todas las celdas son vacías."
    
    
  }
  
  CONTROL <- list()
  CONTROL[[1]] <- list()
  CONTROL[[1]] <- ORDEN
  
  CONTROL[[2]] <- list()
  CONTROL[[2]] <- FRASE
  
  
  return(CONTROL)
}
