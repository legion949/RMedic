

#### libraries.R

# listado_inicial <- c("datasets",  "utils",     "grDevices", "graphics",  "stats",     "methods"  )
# listado_agregado <- c("shiny", "xtable", "rCharts", "plotly", "colourpicker", "readxl", "Hmisc", "fmsb", "RCurl", "XML", "gdata", "XLConnect", "ggplot2", "shinyjs", "ggvis", "sfsmisc", "gplots", "agricolae", "coin")
# options(defaultPackages = c(listado_inicial, listado_agregado))



# Funciones
source("../load_functions/load_functions.R")



# Cargamos todas las otras funciones
EVA01("../functions")

# Librerias de la página inicial
require(shiny)
library(xtable)
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
library(fmsb)

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

#####

brm1 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
brm2 <- c("M", "M", "M", "M", "F", "F", "M", "F", "F", "F", "M", "F", "M", "F", "F", "M", "M", "M", "M", "M")
brm3 <- c(1, 1, 2, 2, 2, 1, 2, 3, 2, 2, 1, 2, 2, 3, 2, 3, 3, 2, 1, 2)
brm4 <- c(145, 144, 149, 169, 141, 138, 155, 156, 139, 137, 152, 142, 158, 175, 140, 190, 180, 162, 142, 168)
brm <- data.frame(brm1,brm2,brm3,brm4)
colnames(brm) <- c("Orden", "Sexo", "Cons. Alcoh.", "TAS")

remove(brm1, brm2, brm3, brm4)





engrampar_letras <- function(nombre_columnas) {
  
  if (!is.null(nombre_columnas)) if (length(nombre_columnas) > 0) {
  nombres2 <- nombre_columnas
  letras_elegidas <- paste0("(", num2let(c(1:length(nombres2))), ")")
  nombres2 <- paste0(letras_elegidas, " - ", nombres2)
  
  } else nombres2 <- NULL
  return(nombres2)
  
  
}



prep_base <- function(BASE_SALIDA) {
  
  # A cada columna tipo FACTOR la vuelve tipo CATEGORICA
  # Para RMedic no me hace falta columnas tipo factor, por que no hay una
  # precarga de niveles del factor.
  
  # Por cada columna
  for (n in 1:ncol(BASE_SALIDA)) {
    
    mini <- BASE_SALIDA[,n]
    
    if (!is.numeric(mini)) mini <- as.character(mini)
    
    for (k in 1:length(mini)) {
      
      este <- mini[k]
      if (!is.na(este)) if (este == "") mini[k] <- NA
      
    } # Fin for k
    
    BASE_SALIDA[,n] <- mini
    
  } # Fin for n 
  
  return(BASE_SALIDA)
  
}



# Funcion para la tabla salida del Control
tabla_control <- function(BASE_CONTROL=NULL, DETALLE=NULL, MAX=NULL) {
  

  # carga <- carga_xls("/home/david/Descargas/Cursos Oncologos/Semana 02 - Base de Datos/Bases/Base - Ejercicio 01 - Corregida.xlsx")[[2]]
  # BASE_CONTROL <- carga[4]
  # DETALLE <- "Numérica"
  # MAX <- 10
  
  
  # Para ver que es realmente la variable
  dt_verdad <- is.numeric(as.vector(as.matrix(BASE_CONTROL)))


  # Objeto para hacer modificaciones
  minibase <- as.character(BASE_CONTROL[,1])
  
  # Objeto que solo tiene datos y el numero de orden correspondiente
  minibase2 <- cbind(c(1:length(minibase)), minibase)
  dt_algo <- !is.na(minibase)
  minibase2 <- minibase2[dt_algo,]
  
  # Solo los datos
  minibase3 <- minibase2[,2]
  
  if(DETALLE == "Categórica") {
    

    # Celdas Vacias    
    dt_vacia <- is.na(BASE_CONTROL)
    orden_vacia <- c(1:length(BASE_CONTROL))[dt_vacia]
    pos_vacia <- paste0(orden_vacia, collapse = ",")
    if(length(orden_vacia) == 0) pos_vacia <- "(Sin Detalle)"

    if (!is.null(MAX)) {
        if(length(orden_vacia) > MAX) pos_vacia <- paste0(paste0(orden_vacia[c(1:MAX)], collapse = ","), "... (Solo primeros ", MAX, " registros)")
      
    } # Fin if MAX
    detalle_vacia <- c("Celdas Vacías", "----", sum(dt_vacia), pos_vacia)

    # Si hay al menos un dato que no es vacio...
    if (length(orden_vacia) < nrow(BASE_CONTROL)) {
      
    # Celdas con Espacios
    metralla <- strsplit(minibase, "")
    dt_espacio <- rep(FALSE, length(metralla))
 
    # Detectamos a las celdas que solo tienen espacios...
    # No tienen ningun otro caracter... 
    for (n in 1:length(metralla)) {
    # 
   esquirla <- metralla[[n]]
   if (!is.na(esquirla[1])) if (length(esquirla) > 0)  if (sum(esquirla == " ") == length(esquirla)) dt_espacio[n] <- TRUE
    # 
 } # Fin for n

    # Detectamos y damos el orden de las filas que estas rellenas solo de espacios
    orden_espacio <- c(1:length(metralla))[dt_espacio]
    pos_espacio <- paste0(orden_espacio, collapse = ",")

    # Las celdas con espacios, seran reemplazados por "NA",
    # para excluirlas del resto de las detecciones
    if (length(orden_espacio) > 0) minibase[orden_espacio] <- NA
    
    # Armamos una tabla de frecuencias con lo que efectivamente tiene datos
    TABLA <- table(na.omit(minibase))
    nombres_originales <- names(TABLA)

    # Le damos otro formato a la TABLA
    # Lo convertimos en un data frame de una columna
    # y le vamos agregando columnas
    orden_col <- c(1:length(TABLA))
    dim(orden_col) <- c(length(orden_col), 1)
    TABLA <- as.data.frame(TABLA)
    relleno <- rep(NA, length(orden_col))
    
    TABLA <- cbind(orden_col, TABLA, relleno)
    colnames(TABLA) <- c("Orden del Nivel", paste0("Niveles de '",colnames(BASE_CONTROL), "'"),  "Frecuencia", "Número de Orden")
    
    # Que todo sea tipo caracter...
    TABLA[,1] <- as.character(TABLA[,1])
    TABLA[,2] <- as.character(TABLA[,2])
    TABLA[,3] <- as.character(TABLA[,3])
    TABLA[,4] <- as.character(TABLA[,4])
    
    # Vamos a ir ahora por las categorias que tienen espacios...
    # Recordemos que "nombres_originales" ya no tiene las celdas con solo espacios...
    rambo <- strsplit(nombres_originales, "")
    dt_rambo <- rep(FALSE, length(rambo))

    # Detallamos la posicion de orden de los datos de cada nivel
    for (n in 1:nrow(TABLA)) {
      
      este_nivel <- TABLA[n,2]
      dt_este_nivel <- minibase3 == este_nivel
      orden_este_nivel <- minibase2[dt_este_nivel,1]
      pos_este_nivel <- paste0(orden_este_nivel, collapse = ",")
      
      if (!is.null(MAX)) {
        if (length(orden_este_nivel) > MAX)        pos_este_nivel <- paste0(paste0(orden_este_nivel[c(1:MAX)], collapse = ","), "... (Solo los ", MAX, " primeros registros)")
        
        TABLA[n,4] <- pos_este_nivel
      } 
    } # Fin for n
  
    # Reordenamiento...
    # Primero lo que empieza con letra...
    # Despues, por defecto ya, quedan lo que empiezan por numeros
    # Y al final... los "infinite_incantatem"
    if (1 == 1) {
    nombres1 <- TABLA[,2]
    partido <- strsplit(nombres1, "")
    dt_primero_letra <- rep(FALSE, length(partido))
    dt_alguna_letra <- dt_primero_letra
    
    malandra <- c("", " ", "'", '"', "_", "-")
    mis_letras <- c(LETTERS, letters)
    
    
    for (n in 1:length(dt_primero_letra)) {
      
      sujeto <-   unique(partido[[n]])
      
      # Que no tenga a "malandra".
      grupo1 <- c(sujeto, malandra)
      mi_tabla1 <- table(grupo1)
      dt_tabla1 <- mi_tabla1 >= 2
      if (sum(dt_tabla1) == 0) puro <- TRUE else puro <- FALSE
      
      if (puro == TRUE) {
        # Si su primer elemento es una letra
        mini1_dt <- partido[[n]][1] == mis_letras
        if (sum(mini1_dt) == 1) dt_primero_letra[n] <- TRUE  
        
        # Si hay alguna letra en algun lado
        grupo2 <- c(sujeto, mis_letras)
        mi_tabla2 <- table(grupo2)
        dt_tabla2 <- mi_tabla2 >= 2
        if (sum(dt_tabla2) == 1) dt_alguna_letra[n] <- TRUE  
        
        # Correccion para que "alguna letra" no incluya a los que son 
        # de primero letra
        if (dt_primero_letra[n]) dt_alguna_letra[n] <- FALSE 
      } # Fin if puro
    } # Fin for n
    
    mi_rejunte <- sum(dt_primero_letra, dt_alguna_letra)    
    if (mi_rejunte > 0) {
      
      orden_tabla1 <- c(1:nrow(TABLA))
      ord1 <- orden_tabla1[dt_primero_letra]
      ord2 <- orden_tabla1[dt_alguna_letra]
      cambios1 <- c(ord1, ord2)
      TABLA <- rbind(TABLA[cambios1, ], TABLA[-cambios1, ])
      TABLA[,1] <- as.character(orden_tabla1)
    } # Fin if > 0
    } # Fin reordenamiento
    
    
    if (!is.null(MAX)) {
      if(length(orden_vacia) > MAX) pos_vacia <- paste0(paste0(orden_vacia[c(1:MAX)], collapse = ","), "... (Solo primeros ", MAX, " registros)")
      if(length(orden_espacio) > MAX) pos_espacio <- paste0(paste0(orden_espacio[c(1:MAX)], collapse = ","), "... (Solo primeros ", MAX, " registros)")
      
    } # Fin if MAX
    
    detalle_vacia <- c("Celdas Vacías", "----", sum(dt_vacia), pos_vacia)
    detalle_espacio <- c("Celdas con solo Espacios", "----", sum(dt_espacio), pos_espacio)
    
  
  
    
    if (length(orden_espacio) > 0) TABLA <- rbind(detalle_vacia, detalle_espacio, TABLA) else TABLA <- rbind(detalle_vacia, TABLA)
    
    } else {
      
      TABLA <- detalle_vacia
      dim(TABLA) <- c(1, length(TABLA))
      colnames(TABLA) <- c("Orden del Nivel", paste0("Niveles de '",colnames(BASE_CONTROL), "'"),  "Frecuencia", "Número de Orden")
      
      
    }
    
  } else   if(DETALLE == "Numérica") {
    
    # Si de verdad es numerica
    if (dt_verdad == TRUE) {
      

      detalle <- c("Detalle", "Valor de la Variable", "Frecuencia", "Número de Orden")
      TABLA <- matrix(NA, 2, length(detalle))
      colnames(TABLA) <- detalle
      TABLA[,1] <- c("Mínimo", "Máximo")
      TABLA[,2] <- c(min(BASE_CONTROL), max(BASE_CONTROL))
      
      dt_min <- min(BASE_CONTROL) == BASE_CONTROL
      orden_min <- c(1:nrow(BASE_CONTROL))[dt_min]
      pos_min <- paste0(orden_min, collapse = ",")
      
      dt_max <- max(BASE_CONTROL) == BASE_CONTROL
      orden_max <- c(1:nrow(BASE_CONTROL))[dt_max]
      pos_max <- paste0(orden_max, collapse = ",")

      dt_vacias <- is.na(BASE_CONTROL)
      orden_vacias <- c(1:nrow(BASE_CONTROL))[dt_vacias]
      if (length(orden_vacias) == 0) pos_vacias <- "(Sin Detalle)" else pos_vacias <- paste0(orden_vacias, collapse = ",") 

      
      # Vemos... dentro de los errores, quienes son solo espacios
      metralla <- strsplit(as.character(BASE_CONTROL[,1]), "")
      dt_espacio <- rep(FALSE, length(metralla))
      
      for (n in 1:length(metralla)) {
        
        esquirla <- metralla[[n]]
        if (!is.na(esquirla) && length(esquirla) > 0)  if (sum(esquirla == " ") == length(esquirla)) dt_espacio[n] <- TRUE
        
      } # Fin for n
      
      # Detalle de celdas con espacios
      orden_espacio <- c(1:length(BASE_CONTROL))[dt_espacio]
      if (length(orden_espacio) == 0) pos_espacio <- "(Sin Detalle)" else   pos_espacio <- paste0(orden_espacio, collapse = ",")

            
      if (!is.null(MAX)) {
        if(length(orden_min) > MAX) pos_min <- paste0(paste0(orden_min[c(1:MAX)], collapse = ","), "... (Solo primeros ", MAX, " regtistros)")
        if(length(orden_max) > MAX) pos_max <- paste0(paste0(orden_max[c(1:MAX)], collapse = ","), "... (Solo primeros ", MAX, " regtistros)")
        if(length(orden_vacias) > MAX) pos_vacias <- paste0(paste0(orden_vacias[c(1:MAX)], collapse = ","), "... (Solo primeros ", MAX, " regtistros)")
        if(length(orden_espacio) > MAX) pos_espacio <- paste0(paste0(orden_espacio[c(1:MAX)], collapse = ","), "... (Solo primeros ", MAX, " regtistros)")
        
      } # Fin if MAX
      
      detalle_vacio <- c("Celdas Vacías", "(Sin Detalle)", sum(dt_vacias), pos_vacias)
      detalle_espacio <- c("Celdas con Espacios", "(Sin Detalle)", sum(dt_espacio), pos_espacio)

      TABLA[,3] <- c(sum(dt_min), sum(dt_max))
      TABLA[,4] <- c(pos_min, pos_max)
      
      TABLA <- rbind(detalle_vacio, TABLA)
      
      if (length(orden_espacio) > 0) TABLA <- rbind(detalle_vacio, detalle_espacio, TABLA)
    } else {

      # Lo primero que va a hacer... es ver si hay al menos un dato no vacio
      

      # Lo 2do que vamos a hacer...
      # es ver si, en la computadora del sujeto...
      # el archivo que creo en los datos tiene al punto
      # como separador decimal, pero en su computadora esta
      # establecido la coma.
      # En este caso... el tipo carga los datos... y se ven bien, pero
      # el sistema no los detecta como numericos.
      
      # Si obviando las celdas vacias...
      # Tenemos una cantidad "x" de datos... y "x" > 0
      # Si aplicamos as.numeric() y tenemos la misma cantidad "x"
      # es que los datos tienen el formato correcto para R, o sea,
      # tienen el punto como separador decimal, pero en su computadora
      # esta establecida la coma como separador decimal.
      # Se le debe indicar eso... que en su computadora el separador decimal
      # es la coma, y en el archivo son puntos... que debe cambiar los puntos por comas.
            

            
      # Celdas correctas
      #  minibase <- c("", NA, 1, 1.3, "2,6", "JA", " ", "   ")
      orden_datos <- c(1:length(minibase))
      
      # Vemos quien es vacio del original
      paso1 <- is.na(minibase)

      # Si al menos un dato no es vacio
      if (sum(paso1) < nrow(BASE_CONTROL)) {
            
      # Forzamos conversion a numerico
      paso2 <- suppressWarnings(as.numeric(as.vector(as.matrix(BASE_CONTROL))))
      
      # Vemos quien es numerico realmente
      paso3 <- !is.na(paso2)
      
      # Vemos quien NO es vacio y NO es numerico
      # O sea... todos los errores
      armado <- as.numeric(paso1) + as.numeric(paso3)
      paso4 <- rep(NA, length(paso3))
      paso4[armado == 0] <- TRUE
      paso4[armado != 0] <- FALSE
      
      # Detalle de celdas vacias
      orden_vacia <- c(1:nrow(BASE_CONTROL))[paso1]
      pos_vacia <- paste0(orden_vacia, collapse = ",")
      if(length(orden_vacia) == 0) pos_vacia <- "(Sin Detalle)"
      
      # Detalle de todos los errores
      orden_error <- c(1:length(BASE_CONTROL))[paso4]
      pos_error <- paste0(orden_error, collapse = ",")
      
      # dim(detalle_error) <- c(1, length(detalle_error))
      # detalle_error <- as.data.frame(detalle_error)
      
      # Vemos... dentro de los errores, quienes son solo espacios
      metralla <- strsplit(as.vector(as.matrix(BASE_CONTROL))[paso4], "")
      paso5 <- rep(FALSE, length(paso4))
      for (n in 1:length(metralla)) {
        
        esquirla <- metralla[[n]]
        if (length(esquirla) > 0)  if (sum(esquirla == " ") == length(esquirla)) paso5[orden_error[n]] <- TRUE
        
      } # Fin for n
      
      # Detalle de celdas con espacios
      orden_espacio <- c(1:length(BASE_CONTROL))[paso5]
      pos_espacio <- paste0(orden_espacio, collapse = ",")
      
      
      # Los errores que no son espacios
      armado2 <- as.numeric(paso4) + as.numeric(paso5)
      paso6 <- rep(NA, length(paso5))
      paso6[armado2 == 1] <- TRUE
      paso6[armado2 != 1] <- FALSE
      orden_error2 <- c(1:length(BASE_CONTROL))[paso6]
      pos_error2 <- paste0(orden_error2, collapse = ",")
      
      
      
      if (!is.null(MAX)) {
        if(length(orden_vacia) > MAX) pos_vacia <- paste0(paste0(orden_vacia[c(1:MAX)], collapse = ","), "... (Solo primeros ", MAX, " regtistros)")
        if(length(orden_error) > MAX) pos_error <- paste0(paste0(orden_error[c(1:MAX)], collapse = ","), "... (Solo primeros ", MAX, " regtistros)")
        if(length(orden_espacio) > MAX) pos_espacio <- paste0(paste0(orden_espacio[c(1:MAX)], collapse = ","), "... (Solo primeros ", MAX, " regtistros)")
        if(length(orden_error2) > MAX) pos_error2 <- paste0(paste0(orden_error2[c(1:MAX)], collapse = ","), "... (Solo primeros ", MAX, " regtistros)")
      } # Fin if MAX

      
      detalle_vacia <- c("Celdas Vacías", sum(paso1), pos_vacia)
      detalle_error <- c("Celdas con Errores", sum(paso4), pos_error)
      detalle_espacio <- c("Celdas con solo Espacios", sum(paso5), pos_espacio)
      detalle_error2 <- c("Celdas con Error Diferenciado", sum(paso6), pos_error2)
      
      
      # A la tabla le sum
      TABLA <- rbind(detalle_vacia, detalle_error)
      
      if (length(orden_espacio) > 0)   TABLA <- rbind(detalle_vacia, detalle_espacio, detalle_error2)
      
      colnames(TABLA) <- c("Detalle", "Frecuencia", "Número de Orden")
      
      } else {
        
        # Detalle de celdas vacias
        orden_vacia <- c(1:length(BASE_CONTROL))[paso1]
        pos_vacia <- paste0(orden_vacia, collapse = ",")
        if(length(orden_vacia) == 0) pos_vacia <- "(Sin Detalle)"
        
        if (!is.null(MAX)) {
          if(length(orden_vacia) > MAX) pos_vacia <- paste0(paste0(orden_vacia[c(1:MAX)], collapse = ","), "... (Solo primeros ", MAX, " regtistros)")
        
        } # Fin if MAX

        detalle_vacia <- c("Celdas Vacías", sum(paso1), pos_vacia)
        TABLA <- detalle_vacia
        dim(TABLA) <- c(1, length(TABLA))
        colnames(TABLA) <- c("Detalle", "Frecuencia", "Número de Orden")
        
               
      }
      
    }
    
  
  
}

  return(TABLA)
}



paso_BASE <- function(BASE) {
  
  
  if (!is.null(BASE)) {
      if (ncol(BASE) > 0) {
  
        paso <- TRUE
        
    } else paso <- FALSE
  } else paso <- FALSE
}

paso_detalle <- function(detalle) {
  
  
  if (!is.null(detalle)) {
    if (detalle != "") {
     
        
        paso <- TRUE
        
   
    } else paso <- FALSE
  } else paso <- FALSE
}



finite_incantatem <- function(input_vector=NULL) {
  
  nombres_originales <- as.character(as.vector(as.matrix(input_vector)))
  #nombres_originales <- c("", " ", NA, "F", "F  ", "  F", "   F    ")
    
  # Vamos a ir ahora por las categorias con espacios
  rambo <- list()
  for (n in 1:length(nombres_originales)) {
    este_nombre <- nombres_originales[n]
    rambo[[n]] <- list()
    if (!is.na(este_nombre)) rambo[[n]] <- strsplit(este_nombre , "")[[1]] else rambo[[n]] <- este_nombre
  }

  
  dt_rambo <- rep(FALSE, length(rambo))
  
  # Vamos armando una modificacion en la presentacion de los niveles
  nombres_modificados <- nombres_originales
  
  # Detectamos si los nombres de los niveles 
  # tiene espacios en el nombre a izquierda o derecha
  for (n in 1:length(rambo)) {
    este <- rambo[[n]]
    
    if (nombres_originales[n] != "" && !is.na(este)) {
      
    # Por izquierda
    if (este[1] == " ") dt_rambo[n] <- TRUE
    
    # Por derecha
    if (este[length(este)] == " ") dt_rambo[n] <- TRUE
    
    }
    if (dt_rambo[n] == TRUE) {
      
      este2 <- este
      aqui_espacios <- este == " "
      este2[aqui_espacios] <- "_"
      armado2 <- paste0(este2, collapse = "")
      armado2 <- paste0("'", armado2,"'", collapse="")
      nombres_modificados[n] <- armado2
    }
    
  } # Fin for n
  
  return(nombres_modificados)
  
  
}

# BASE <- BASE1

finite_base <- function(BASE=NULL) {
  
  # Esta es una funcion que le saca los espacios a la derecha o izquierda de cada celda 
  # a las variables categoricas.
  
  for (n in 1:ncol(BASE)) {
    
    if(!is.numeric(BASE[,n]) && length(na.omit(BASE[,n])) > 0) {
      
      cambio <- finite_incantatem(BASE[,n])
      BASE[,n] <- cambio
    }
    
   # return(BASE)
  }
  
  return(BASE)
}


id_var <- function(input_cat=NULL, input_colnames=NULL) {
  
  input_cat <- as.vector(as.matrix(input_cat))
  input_colnames <- as.vector(as.matrix(input_colnames))
  
  dt_pos <- input_cat == input_colnames
  orden <- c(1:length(input_colnames))
  este_orden <- orden[dt_pos]
  esta_letra <- num2let(c(este_orden))
  
  armado <- paste0(input_cat, " - (", esta_letra, ")")
  
  return(armado)
}

graf_barras <- function(input_tabla = NULL, input_y=NULL, input_color=NULL) {
  
  # input_tabla <- df01(mtcars[2])$df01$DF2
  # input_y = "FA"
  # input_color = NULL
  
  
  interno <- c(2,5,6)
  dim(interno) <- c(1, length(interno))
  interno <- as.data.frame(interno)
  colnames(interno) <- c("FA", "FR", "%")
  
  TIPO_GRAF <- input_y
  if (input_y == "%")  TIPO_GRAF <-   "FR"
  
  esta_columna <- interno[,TIPO_GRAF]
  MINI_TABLA1 <- input_tabla[,esta_columna]
  MINI_TABLA2 <- suppressWarnings(as.numeric(as.character(MINI_TABLA1)))
  dt_ok <- sum(is.na(MINI_TABLA2)) == 0
  
  if (dt_ok == TRUE) {
  

  MINI_TABLA2 <- as.vector(MINI_TABLA2)
  names(MINI_TABLA2) <- input_tabla[,1]
  
  
  if (input_y == "%")  MINI_TABLA2 <- MINI_TABLA2*100 

  
  barplot(MINI_TABLA2, col=input_color, 
          xlab=colnames(input_tabla)[1], ylab=input_y)
  
  } else {
    plot(0:30, col="white", axes = F, xlab="", ylab="")
    text(15, 15,input_tabla[1,1], col=input_color, cex=2)
  }
  
  
  
  
  
  
}


graf_tortas <- function(input_tabla = NULL, input_y = "FA", input_color = NULL) {
    
  # input_tabla <- df01(mtcars[2])$df01$DF2
  # input_y = "FA"
  # input_color = NULL
  
  
  interno <- c(2,5,6)
  dim(interno) <- c(1, length(interno))
  interno <- as.data.frame(interno)
  colnames(interno) <- c("FA", "FR", "%")
  
  TIPO_GRAF <- input_y
  if (input_y == "%")  TIPO_GRAF <-   "FR"
  
  esta_columna <- interno[,TIPO_GRAF]
  MINI_TABLA1 <- input_tabla[,esta_columna]
  MINI_TABLA2 <- suppressWarnings(as.numeric(as.character(MINI_TABLA1)))
  dt_ok <- sum(is.na(MINI_TABLA2)) == 0
  
  if (dt_ok == TRUE) {
    
    
    MINI_TABLA2 <- as.vector(MINI_TABLA2)
    names(MINI_TABLA2) <- input_tabla[,1]
    
    
    if (input_y == "%")  MINI_TABLA2 <- MINI_TABLA2*100 
    
   pie(MINI_TABLA2, xlab=colnames(input_tabla)[1], ylab=" ", col=input_color)
    
  } else {
    plot(0:30, col="white", axes = F, xlab="", ylab="")
    text(15, 15,input_tabla[1,1], col=input_color, cex=2)
  }
  
}

graficar_1c <- function(input_base=NULL, input_graf="boxplot", input_color="red", input_tipograf=NULL, input_cerrado = NULL, input_save=NULL) {
  

  dt_ok1 <- is.numeric(as.vector(as.matrix(input_base)))
  dt_ok2 <- nrow(na.omit(input_base)) == 0

  if (dt_ok1 == TRUE) {

    # Grafico Histograma
    if(input_graf == "hist") {
      VR <- as.vector(as.matrix(input_base))
      if(is.null(input_tipograf)) tipo_graf <- "FA" else tipo_graf <- input_tipograf
      if (tipo_graf == "FA") detalle_interno <- T else detalle_interno <- F
      if(is.null(input_cerrado))  cerrado <- T else cerrado <- input_cerrado
      if(is.null(input_save)) guardar <- F else guardar <- input_save
      
      if(guardar == F) {
      hist(VR, col=input_color, 
           xlab=colnames(input_base),
           ylab= tipo_graf,
           main=" ",
           freq=detalle_interno,
           right = cerrado
           )
      }
      
      if(guardar == T) {
      GRAFICO <-  hist(VR, 
               # col=input_color, 
               # xlab=colnames(input_base),
               # ylab= tipo_graf,
               # main=" ",
               # freq=detalle_interno,
                right = cerrado,
                plot=F)
      CORTES <- cut(VR, GRAFICO$breaks, right = cerrado, include.lowest = T)
      CORTES <- as.data.frame(CORTES)
      colnames(CORTES) <- paste0("CAT ", colnames(input_base))
      return(CORTES)
      }
      
    }
    
  # Grafico Boxplot
  if(input_graf == "boxplot")    boxplot(input_base, col=input_color, ylab=colnames(input_base))

    # Grafico de Dispersion
    if(input_graf == "dispersion") {
      orden <- rep(1, nrow(input_base))
      cambio_de_base <- cbind(orden, input_base)
      plot(cambio_de_base, col=input_color, ylab=colnames(cambio_de_base)[2], xlab="", axes=F) 
      axis(2)
    } 
    

    # Grafico de Puntos
    if(input_graf == "puntos") {

      VR <- as.vector(as.matrix(input_base))
      base_ordenada <- input_base[order(VR),]
      TABLA <- table(as.character(VR))
      apilado <- sequence(TABLA)
      tabla_puntos <- cbind(base_ordenada, apilado)

      plot(tabla_puntos, col=input_color, ylab="Frecuencia", 
           xlab=colnames(input_base)[1], axes=F,
           ylim=c(0,max(TABLA))) 
      axis(2)
      axis(1)
    } 
    
    
    # Grafico de Dispersion
    if(input_graf == "mdes") {
      library(gplots)
      VR <- as.vector(as.matrix(input_base))
      FACTOR <- as.factor(as.character(rep("",length(VR))))  
      
      MEDIA <- mean(VR)
      SD <- sd(VR)
      
      plotmeans(VR ~ FACTOR, 
                p = 0.86,
                col = input_color, 
                barcol= "blue", 
                xlab = "", 
                ylab=colnames(input_base),
                connect = FALSE, n.label = FALSE,
                ylim=c(MEDIA-3*SD, MEDIA+3*SD),
                barwidth = 1.5, xaxt= "n", ci.label = F )
      axis(2)
      # plot(cambio_de_base, col=input_color, ylab=colnames(cambio_de_base)[2], xlab="", axes=F) 
      # axis(2)
    } 
    
    if(input_graf == "mye") {
      library(Hmisc)
      
      VR <- as.vector(as.matrix(input_base))
      N <- length(VR)
      SD <- sd(VR)
      EE <- SD/sqrt(N)
      MEDIA <- mean(VR)
      MIN <- min(VR)
      MAX <- max(VR)
      plot(1,1, col= "white", axes = F, ylab=colnames(input_base), xlab="")
      errbar(1, MEDIA, MEDIA+EE, MEDIA-EE,
             #   main=input$columna_graf_torta,
             col= input_color,
             #   errbar.col = color2,
             ylab=  colnames(input_base),
             xlab="",
             ylim=c(MEDIA-3*SD, MEDIA+3*SD),
             lwd = 1.5,  axes = F)
      axis(2)
      
    }
        
    
  } else {
    plot(0:30, col="white", axes = F, xlab="", ylab="")
    if (dt_ok1 == FALSE && dt_ok2 == FALSE) text(15, 15, "La variable NO es numérica.\n Realice CONTROL", col=input_color, cex=2)
    if (dt_ok2 == TRUE)  text(15, 15, "Columna Sin Datos", col=input_color, cex=2)
  }

}

graficar_2c <- function(input_base=NULL, input_graf="xy", input_color="red", input_tipograf=NULL, input_cerrado = NULL, input_save=NULL) {
  
  
  
  dt_ok1 <- c(is.numeric(as.vector(as.matrix(input_base[,1]))), is.numeric(as.vector(as.matrix(input_base[,2]))))
  dt_ok2 <- nrow(na.omit(input_base)) == 0
  dt_ok3 <- sum(dt_ok1) == 2
  
  if (dt_ok3 == TRUE) {
    # Grafico Histograma
    if(input_graf == "xy") {
      plot(input_base, col=input_color)
    } 
  } else {
    plot(0:30, col="white", axes = F, xlab="", ylab="")
    if (dt_ok3 == FALSE && dt_ok2 == FALSE) text(15, 15, "Las variables NO son numéricas.\n Realice CONTROL", col=input_color, cex=1.7)
    if (dt_ok2 == TRUE)  text(15, 15, "Columna Sin Datos", col=input_color, cex=2)
    
  }
  
}


graficar_qc <- function(input_base=NULL, input_graf="boxplot", input_color="red", input_tipograf=NULL, input_cerrado = NULL, input_save=NULL) {
  
  
  dt_ok1 <- is.numeric(as.vector(as.matrix(input_base[,1])))
  dt_ok2 <- nrow(na.omit(input_base)) == 0
  
  if (dt_ok1 == TRUE) {
    
    VR <- input_base[,1]
    FACTOR <- as.factor(as.character(input_base[,2]))
    
    # Grafico Boxplot
    if(input_graf == "boxplot")    boxplot(VR ~ FACTOR, col=input_color, ylab=colnames(input_base)[1], xlab=colnames(input_base)[2])
    
    
    # Grafico de Dispersion
    if(input_graf == "mdes") {
      library(gplots)
   
      MEDIA <- mean(VR)
      SD <- sd(VR)
      plotmeans(VR ~ FACTOR, 
                col = input_color, 
                ylab =colnames(input_base)[1], xlab=colnames(input_base)[2],
                connect = FALSE, n.label = F,
                barwidth= 1.5)
    
      # plotmeans(VR ~ FACTOR, 
      #           p = 0.86,
      #           col = input_color, 
      #           barcol= "blue", 
      #           xlab = colnames(input_base)[2], 
      #           ylab=colnames(input_base)[1],
      #           connect = FALSE, n.label = FALSE,
      #           ylim=c(MEDIA-3*SD, MEDIA+3*SD),
      #           barwidth = 1.5, xaxt= "n", ci.label = F )
      # axis(2)
      # plot(cambio_de_base, col=input_color, ylab=colnames(cambio_de_base)[2], xlab="", axes=F) 
      # axis(2)
    } 
    
    if(input_graf == "mye") {
      library(Hmisc)
      
    
      N <- tapply(VR, FACTOR, length)
      SD <- tapply(VR, FACTOR, sd)
      EE <- SD/sqrt(N)
      MEDIA <- tapply(VR, FACTOR, mean)
      MIN <- tapply(VR, FACTOR, min)
      MAX <- tapply(VR, FACTOR, max)
  #    plot(1,1, col= "white", axes = F, ylab=colnames(input_base)[1], xlab=colnames(input_base)[2])
      
   
      
      errbar(c(1:length(levels(FACTOR))), MEDIA, MEDIA+EE, MEDIA-EE,
             #   main=input$columna_graf_torta,
             col= input_color,
             #   errbar.col = color2,
             ylab=  colnames(input_base)[1],
             xlab=colnames(input_base)[2],
             #     ylim=c(MEDIA-3*SD, MEDIA+3*SD),
             lwd = 1.5,  axes = F)
      axis(2)
      
      axis(1, at=c(1:length(levels(FACTOR))), labels=levels(FACTOR), tick=T, cex.axis=0.8, las=1)      
    }
    
    
  } else {
    plot(0:30, col="white", axes = F, xlab="", ylab="")
    if (dt_ok1 == FALSE && dt_ok2 == FALSE) text(15, 15, "La variable NO es numérica.\n Realice CONTROL", col=input_color, cex=2)
    if (dt_ok2 == TRUE)  text(15, 15, "Columna Sin Datos", col=input_color, cex=2)
  }
  
}



# graficar_2c(mtcars[,c(2,7)], input_color = "red", input_graf = "xy")