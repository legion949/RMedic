
num2let <- function(n, lets = LETTERS) {
  base <- length(lets)
  if (length(n) > 1) return(sapply(n, num2let, lets = lets))
  stopifnot(n > 0)
  out <- ""
  repeat {
    if (n > base) {
      rem <- (n-1) %% base
      n <- (n-1) %/% base
      out <- paste0(lets[rem+1], out)
    } else return( paste0(lets[n], out) )
  }
}

let2num <- function(x, lets = LETTERS) {
  base <- length(lets)
  s <- strsplit(x, "")
  sapply(s, function(x) sum((match(x, lets)) * base ^ seq(length(x) - 1, 0)))
}


#helper function (convert vector to named list)
namel<-function (vec){
  tmp<-as.list(vec)
  names(tmp)<-as.character(unlist(vec))
  tmp
}

OpcionesDeColumnas <- function(my_names = ""){
  
  # Letras
  letras_elegidas <- paste0("(", num2let(c(1:length(my_names))), ")")
  
  # Visual del usuario
  visual_usuario <- paste0(letras_elegidas, " - ", my_names)
  
  
  # Armamos el vector de salida
  vector_salida <- my_names
  names(vector_salida) <- visual_usuario
  
  return(vector_salida)
}



MyLetter <- function(Base = NULL, the_col = NULL) {
  
  
  
  if(is.null(Base)) return(NULL)
  if(is.null(the_col)) return(NULL)
  if(the_col == "") return(NULL) 
  if(sum(colnames(Base) == the_col) == 0) return(NULL)
  
  dt_col <- colnames(Base) == the_col
  pos_col <- c(1:length(dt_col))
  the_col <- pos_col[dt_col]
  my_letter <- num2let(the_col)
  
  return(my_letter)
}



################


RMedic_1q_tablas <- function(input_base = NULL, input_decimales = NULL, input_cadena = NULL) {
  
  # Default values
  if (is.null(input_decimales)) input_decimales <- 2
  if (is.null(input_cadena)) input_cadena <- T
  
  # # # Control 1 - input_base
  {
    ###
    # 1- Es nulo
    # 2- No es un data.frame
    # 3- Tiene cero columnas
    # 4- Tienes más de 1 columna
    # 5- No tiene filas (nrow(input_base))
    
    veredicto1 <- control_1q(input_base = input_base, input_cadena = input_cadena)
    output_cadena <- veredicto1
    
    ###  
  } # Fin Control 1 - input_base
  #################################################
  
  
  
  # # # minibase y Control 2 -  base "NO DATA"
  {
    ###
    
    # Si todo va OK...
    if (output_cadena) {
      
      # Creamos "mini"  
      minibase <- na.omit(input_base)
      
      # Vemos que mini tenga filas
      if (nrow(minibase) == 0) {
        cat("Error df01: 'input_base' posee solo celdas vacias.", "\n")
        output_cadena <- FALSE
      }
      
      
      
    } # Fin if Si todo va OK...
    ###################################################
    
    # Si hay algun problema...
    if (output_cadena == FALSE){
      minibase <- as.data.frame(mtcars[2])
      if(!is.factor(minibase[1])) minibase[1] <- as.factor(as.character(minibase[,1]))
      colnames(minibase) <- "No Data"
      cat("Error df01: 'minibase' externo agregado (NO DATA)", "\n")
    } # Fin if si hay problemas
    ##########################################################
    
    
    ### 
  } # Fin Control 2 -  base "NO DATA"
  ################################################################
  
  
  # # # SandBox en caso de no ser valido
  {
    ###
    
    # Si hay algun problema...
    if (output_cadena == FALSE){
      minibase <- as.data.frame(mtcars[2])
      if(!is.factor(minibase[1])) minibase[1] <- as.factor(as.character(minibase[,1]))
      
      colnames(minibase) <- "No Data"
      cat("Error df01: 'minibase' externo agregado (NO DATA)", "\n")
    } # Fin if si hay problemas
    ##########################################################
    
    ###  
  }
  ############################################################################
  
  
  # # # Cohersion como as.factor()
  {
    ###
    
    # Si todo va OK...
    if (output_cadena) {
      
      # Si cumple todo... Falta que sea de tipo factor.
      if(!is.factor(minibase[,1])) minibase[,1] <- as.factor(as.character(minibase[,1]))
      
      lvl1 <- sort(levels(input_base[,1]))
      lvl2 <- sort(levels(minibase[,1]))
      
      if(identical(lvl1, lvl2)) minibase[,1] <- factor(minibase[,1], levels = levels(input_base[,1]))
      
      
    } # Fin if Si todo va OK...
    ###################################################
    
    
    
    
    ### 
  } # Fin Cohersion como as.factor()
  ################################################################
  
  
  
  
  
  
  
  # Tabla 1: Distribucion de Frecuencias
  {
    ###
    
    # Frecuencais Absolutas
    fa <- table(minibase)
    
    # Total Absoluto
    n_total <- sum(fa)
    grupos <- as.character(names(fa))
    salida_n_total <- rep(n_total, length(fa))
    
    # Cociente
    cociente <- paste0(fa, "/", n_total)
    
    # Frecuencias Relativas
    fr <- fa/n_total
    #    fr <- round2((fa/n_total), input_decimales)
    
    # Porcentajes
    porcentaje  <- fr*100
    
    
    # Redondeos y otros detalles (Ahora si!)
    fr <- round2(fr, input_decimales)
    porcentaje <- round2(porcentaje, input_decimales)
    porcentaje  <- paste(porcentaje, "%", sep="")
    
    # Fusion
    fusion <- paste0(fa, " (", porcentaje, ")")
    
    # Tabla de Distribucion de Frecuencias
    tabla01 <- cbind(grupos, fa, salida_n_total, cociente, fr,  
                     porcentaje, fusion)
    
    rotulo <- paste0("Variable: ", colnames(minibase))
    tabla01 <- as.data.frame(tabla01)
    nombres_tabla01 <- c(rotulo, "Frecuancia Absoluta", "Total", "Cociente", "Frecuencia Relativa", "Porcentaje", "FA (%)")
    colnames(tabla01) <- nombres_tabla01
    
    numericas <- c(2, 3, 5)
    for(n in 1:length(numericas)) tabla01[,numericas[n]] <- as.numeric(as.character(tabla01[,numericas[n]])) 
    
    
    
    ###
  } # Fin Tabla 1
  ##################################
  
  
  # Tabla 2: Intervalos de Confianza
  {
    ###
    
    p <- fr
    q <- rep(NA, length(p))
    for (n in 1:length(q)) q[n] <- sum(p[-n])
    n <- n_total  
    
    alfa <- c(0.10, 0.05, 0.01)
    
    
    tablas2_ic <- list()
    
    for (k in 1:length(alfa)) {
      
      este_alfa <- alfa[k]
      este_alfa_2 <- este_alfa/2
      confianza <- paste0((1 - este_alfa)*100, "%")
      
      Z <- qnorm(1-este_alfa_2)
      Z <- round2(Z, input_decimales)
      
      desvio <- Z*sqrt((p*q)/n)
      # desvio <- round2(desvio, input_decimales)
      
      li_prop <- p - desvio
      ls_prop <- p + desvio
      
      dt_li <- li_prop < 0
      dt_ls <- ls_prop > 1
      
      if (sum(dt_li) > 0) li_prop[dt_li] <- 0
      if (sum(dt_ls) > 0) ls_prop[dt_ls] <- 1
      
      
      li_porcentaje <- li_prop*100
      ls_porcentaje <- ls_prop*100
      
      li_porcentaje <- round2(li_porcentaje, input_decimales)
      ls_porcentaje <- round2(ls_porcentaje, input_decimales)
      
      li_porcentaje <- paste0(li_porcentaje, "%")
      ls_porcentaje <- paste0(ls_porcentaje, "%")
      
      tabla02 <- cbind(grupos, porcentaje, li_porcentaje, ls_porcentaje)
      colnames(tabla02) <- c(rotulo, "Porcentaje", paste0("Límite Inferior ", confianza), paste0("Límite Supeior ", confianza))
      tabla02 <- as.data.frame(tabla02)
      rownames(tabla02) <- rownames(tabla01)
      
      
      if (output_cadena == FALSE) {
        
        estas_dimensiones <- dim(tabla02)
        tabla_no_data02 <- as.data.frame(matrix("Sin datos en la Columna", estas_dimensiones[1], estas_dimensiones[2]))
        colnames(tabla_no_data02) <- colnames(tabla02)
        tabla02 <- tabla_no_data02
        
      }
      
      tablas2_ic[[k]] <- tabla02
      
    }
    
    
    
    ###
  } # Fin Tabla 2
  ##################################
  
  
  
  # Mis Tablas
  {
    ###
    mis_tablas <- list(tabla01, tablas2_ic[[1]], tablas2_ic[[2]], tablas2_ic[[3]])
    names(mis_tablas) <- c("Distribución de Frecuencias", 
                           "Intervalos de Confianza del 90% para el Porcentaje",
                           "Intervalos de Confianza del 95% para el Porcentaje", 
                           "Intervalos de Confianza del 99% para el Porcentaje")
    
    ###    
  }
  ##########################################################################
  
  
  
  
  
  
  # # # Cambios "NO DATA" o "Errores"
  {
    ###
    
    
    # Si hay errores... Y estamos en "NO DATA"
    if (output_cadena == FALSE) {
      
      
      # Cambiamos los valores por avisos
      for (n in 1:length(mis_tablas)) {
        
        esta_tabla <- mis_tablas[[n]]
        estas_dimensiones <- dim(esta_tabla)
        tabla_no_data <- as.data.frame(matrix("Sin datos", estas_dimensiones[1], estas_dimensiones[2]))
        colnames(tabla_no_data) <- colnames(esta_tabla)
        esta_tabla <- tabla_no_data
        
        mis_tablas[[n]]  <- esta_tabla
        
      } # Fin for n
      
    } # Fin if si hay errores...
    #############################################################
    
    
    ###   
  } # Fin Cambios "NO DATA" o "Errores"
  ##################################################
  
  
  
  # # # Salida
  {
    ###
    
    return(mis_tablas)
    
    
    ###  
  } # Fin Salida
  ##################
  
  
  
}

########################




control_1q <- function(input_base = NULL, input_cadena = NULL){
  
  # # # Explicacion
  {
    ###
    # La funcion "control_1q()" es una funcion de control sobre input_base.
    # Verifica que input_base sea un dataframe de solo una columna, con al menos una fila.
    # Otorgara avisos si el objeto input_base:
    # 1- Es nulo
    # 2- No es un data.frame
    # 3- Tiene cero columnas
    # 4- Tienes más de 1 columna
    # 5- No tiene filas (nrow(input_base))
    
    # Aclaramos que el punto 5 es ver las filas que tiene input_base, y no 
    # ver si las filas ingresadas son vacias o no.
    
    
    # Si se cumple todo, la funcion devuelve un "TRUE". 
    # Si al menos un detalle falta, devuelve un "FALSE".
    # El control se realiza si el objeto input_cadena es TRUE.
    # Si input_cadena es FALSE quiere decir que ya hay un paso previo de una funcion
    # externa que no se cumple, y por lo tanto carece de sentido realizar las tareas, devolviendo
    # directamente un FALSE control_1q() como resultado.
    
    ###    
  } # Fin Explicacion
  ############################################################
  
  
  # # # Funcionamiento por defecto
  {
    ###
    
    if (is.null(input_cadena)) input_cadena <- TRUE
    
    
    
    
    # Armamos el siguiente eslabon
    output_cadena <- input_cadena
    
    ###
  } # Fin Funcionamiento
  ################################################
  
  
  # # # Controles 1
  {
    ###
    
    # Verificar que input_base no sea nulo
    if(output_cadena && is.null(input_base)) {
      cat("Error control_1q: input_base no debe ser nulo", "\n")
      output_cadena <- FALSE
    }  
    
    # Verificar si es un data frame
    if(output_cadena && !is.data.frame(input_base)) {
      cat("Error control_1q: input_base debe ser un data.frame", "\n")
      output_cadena <- FALSE
    }
    
    # Verificar si tiene al menos una columna
    if(output_cadena && ncol(input_base) == 0) {
      cat("Error control_1q: input_base no tiene columnas.")
      output_cadena <- FALSE
    }
    
    # Verificar si tiene mas de 1 columna
    if(output_cadena && ncol(input_base) > 1) {
      cat("Error control_1q: input_base debe ser solo una columna")
      output_cadena <- FALSE
    }
    
    # Verificar si es no tiene datos
    if(output_cadena && nrow(input_base) == 0) {
      stop("Error control_1q: input_base no presenta filas")
      output_cadena <- FALSE
    }
    
    
    
    ###      
  } # Fin Controles 1
  ###################################################################
  
  # # # Salida
  {
    ###
    
    salida <- output_cadena
    
    names(salida) <- c("output_cadena")
    
    return(salida)
    
    
    ###  
  } # Fin Salida
  ##################
  
  
}


################



round2 <- function(x, n) { 
  posneg <- sign(x) 
  
  z <- abs(x)*10^n 
  z <- z + 0.5 
  z <- trunc(z) 
  z <- z/10^n 
  z*posneg 
} 
#############

CifrasPerfectas <- function(cifras = NULL, digitos = 2){
  
  cifras_perfectas <- rep(NA, length(cifras))
  
  for (k in 1:length(cifras)){
    
    cantidad_cifras <- nchar(cifras[k])
    
    cifras_extra <- digitos - cantidad_cifras
    
    el_extra <- rep(0, cifras_extra)
    
    cifras_perfectas[k] <- paste0(el_extra, cifras[k])
  }
  
  return(cifras_perfectas)
}

#######################


RMedic_1c_tablas <- function(input_base = NULL, input_decimales = NULL, input_cadena = NULL,
                             input_min = NULL, input_max = NULL, input_breaks = NULL,
                             input_side = NULL) {
  
  # Default values
  if (is.null(input_decimales)) input_decimales <- 2
  if (is.null(input_cadena)) input_cadena <- T
  
  
  
  # # # Control 1 - input_base
  {
    ###
    # 1- Es nulo
    # 2- No es un data.frame
    # 3- Tiene cero columnas
    # 4- Tienes más de 1 columna
    # 5- No tiene filas (nrow(input_base))
    
    veredicto1 <- control_1c(input_base = input_base, input_cadena = input_cadena)
    output_cadena <- veredicto1
    
    ###  
  } # Fin Control 1 - input_base
  #################################################
  
  
  
  # # # minibase y Control 2 -  base "NO DATA"
  {
    ###
    
    # Si todo va OK...
    if (output_cadena) {
      
      # Creamos "minibase"  
      minibase <- na.omit(input_base)
      mini_vector <- minibase[,1]
      
      # Vemos que minibase tenga filas
      if (nrow(minibase) == 0) {
        cat("Error df01: 'input_base' posee solo celdas vacias.", "\n")
        output_cadena <- FALSE
      }
      
      # minibase sea numerica
      if (!is.numeric(minibase[,1])) {
        cat("Error df01: 'input_base' debe ser numerico.", "\n")
        output_cadena <- FALSE
      }
      
      
    } # Fin if Si todo va OK...
    ###################################################
    
    # Si hay algun problema...
    if (output_cadena == FALSE){
      minibase <- as.data.frame(mtcars[1])
      colnames(minibase) <- "No Data"
      cat("Error df01: 'minibase' externo agregado (NO DATA)", "\n")
    } # Fin if si hay problemas
    ##########################################################
    
    
    ### 
  } # Fin Control 2 -  base "NO DATA"
  ################################################################
  
  
  # # # SandBox en caso de no ser valido
  {
    ###
    
    # Si hay algun problema...
    if (output_cadena == FALSE){
      minibase <- as.data.frame(mtcars[2])
      mini_vector <- minibase[,1]
      minibase[1] <- as.factor(as.character(minibase[,1]))
      colnames(minibase) <- "No Data"
      cat("Error df01: 'minibase' externo agregado (NO DATA)", "\n")
    } # Fin if si hay problemas
    ##########################################################
    
    ###  
  }
  ############################################################################
  
  
  # More default values
  {
    ###
    
    if(is.null(input_min))  input_min <- min(mini_vector)
    if(is.null(input_max))  input_max <- max(mini_vector)
    if(is.null(input_breaks))  input_breaks <- nclass.Sturges(mini_vector)
    if(is.null(input_side))  input_side <- T
    # 
    ###
  } # Fin More default values
  #################################################
  
  
  # Tabla 1 - Medidas Resumen
  {
    ###
    
    nombres_elementos <- c("Variable", "Media", "Desvío Estándard", "n")
    tabla1 <- as.data.frame(matrix(NA,1, length(nombres_elementos)))
    colnames(tabla1) <- nombres_elementos
    
    
    # Media
    media <- mean(mini_vector)
    media <- round2(media, input_decimales)
    
    # Maximo
    desvio <- sd(mini_vector)
    desvio <- round2(desvio, input_decimales)
    
    # Cantidad de Datos
    n_muestra <- length(mini_vector)
    
    
    tabla1[1,1] <- colnames(minibase)
    tabla1[1,2] <- media
    tabla1[1,3] <- desvio
    tabla1[1,4] <- n_muestra
    
    ###
  } # Fin Tabla 1 - Medidas Resumen
  ############################################################
  
  
  # Tabla 2 - Medidas Posicion
  {
    ###
    
    
    nombres_elementos <- c("Variable", "Mínimo", "Media", "Mediana", "Máximo", "n")
    tabla2 <- as.data.frame(matrix(NA,1, length(nombres_elementos)))
    colnames(tabla2) <- nombres_elementos
    
    
    
    minimo <- min(mini_vector)
    minimo <- round2(minimo, input_decimales)
    
    # Media
    media <- mean(mini_vector)
    media <- round2(media, input_decimales)
    
    # Mediana
    mediana <- median(mini_vector)
    mediana <- round2(mediana, input_decimales)
    
    
    # Maximo
    maximo <- max(mini_vector)
    maximo <- round2(maximo, input_decimales)
    
    # Cantidad de Datos
    n_muestra <- length(mini_vector)
    
    
    tabla2[1,1] <- colnames(minibase)
    tabla2[1,2] <- minimo
    tabla2[1,3] <- media
    tabla2[1,4] <- mediana
    tabla2[1,5] <- maximo
    tabla2[1,6] <- n_muestra
    
    ###
  } # Fin Tabla 2 - Medidas Posicion
  ############################################################
  
  
  # Tabla 3 - Cuartiles
  {
    
    input_busqueda = c(25, 50, 75)
    nombres_columnas <- c("Variable", paste0(c("Q1", "Q2", "Q3"), 
                                             paste0("(", input_busqueda, "%", ")")), "n")
    
    tabla3 <- as.data.frame(matrix(NA, 1, length(nombres_columnas)))
    colnames(tabla3) <- nombres_columnas
    
    percentiles <- quantile(mini_vector,  probs = input_busqueda/100)
    
    tabla3[1,1] <- colnames(minibase)
    tabla3[1, c(2:(ncol(tabla3)-1))] <- percentiles
    tabla3[1,ncol(tabla3)] <- nrow(minibase)
    
  } # End Tabla 3 - Cuartiles
  ###########################################################
  
  
  # Tabla 4 - Deciles
  {
    
    input_busqueda <- c(10, 20, 30, 40, 50, 60, 70, 80, 90)
    nombres_columnas <- c("Variable", paste0( "D",input_busqueda, " (", input_busqueda, "%)"), "n")
    
    tabla4 <- as.data.frame(matrix(NA, 1, length(nombres_columnas)))
    colnames(tabla4) <- nombres_columnas
    
    percentiles <- quantile(mini_vector,  probs = input_busqueda/100)
    
    tabla4[1,1] <- colnames(minibase)
    tabla4[1, c(2:(ncol(tabla4)-1))] <- percentiles
    tabla4[1,ncol(tabla4)] <- nrow(minibase)
    
  } # End Tabla 4 - Deciles
  ###########################################################
  
  
  # Tabla 5 - Percentiles
  {
    
    input_busqueda <- c(1, 5, 10, 25, 50, 75, 90, 95, 99)
    nombres_columnas <- c("Variable", paste0( "P",input_busqueda, " (", input_busqueda, "%)"), "n")
    
    tabla5 <- as.data.frame(matrix(NA, 1, length(nombres_columnas)))
    colnames(tabla5) <- nombres_columnas
    
    percentiles <- quantile(mini_vector,  probs = input_busqueda/100)
    
    tabla5[1,1] <- colnames(minibase)
    tabla5[1, c(2:(ncol(tabla5)-1))] <- percentiles
    tabla5[1,ncol(tabla5)] <- nrow(minibase)
    
  } # End Tabla 5 - Percentiles
  ###########################################################
  
  
  
  # Tabla 6 - Medidas Dispersion 
  {
    ###
    
    
    nombres_elementos <- c("Variable", "Rango", "Varianza", "Desvío Estándard", 
                           "Error Estándard", "Coeficiente de Variación",
                           "Coeficiente de Variación Porcentual", "n")
    
    tabla6 <- as.data.frame(matrix(NA,1, length(nombres_elementos)))
    colnames(tabla6) <- nombres_elementos
    
    # rango
    rango <- abs(max(mini_vector) - min(mini_vector))
    rango <- round2(rango, input_decimales)
    
    # Varianza
    varianza <- var(mini_vector)
    varianza <- round2(varianza, input_decimales)
    
    # Desvio
    desvio <- sd(mini_vector)
    
    # Tamanio muestral
    n_muestra <- length(mini_vector)
    
    # Error estandard
    ee <- desvio/sqrt(n_muestra)
    
    # Media
    media <- mean(mini_vector)
    
    # Coeficiente de Variacion
    cv <- desvio/media
    
    cv_porcentual <- cv*100
    
    # Primero sacamos el desvio...
    # Sin redondear el desvio, lo usamos para sacar el error estandard...
    # Y luego redondeamos los dos.
    # Esto es para sacar mejor al EE... por que sino sacas el DE... lo redondeas...
    # lo usas para sacar el EE y lo volves a redondear.
    # Lo mismo con el CV.
    
    desvio <- round2(desvio, input_decimales)
    ee <- round2(ee, input_decimales)
    cv <- round2(cv, input_decimales)
    cv_porcentual <- paste0(round2(cv_porcentual, input_decimales), "%")
    
    
    tabla6[,1] <- colnames(minibase)
    tabla6[,2] <- rango
    tabla6[,3] <- varianza
    tabla6[,4] <- desvio
    tabla6[,5] <- ee
    tabla6[,6] <- cv
    tabla6[,7] <- cv_porcentual
    tabla6[,8] <- n_muestra
    
    ###
  } # Fin Tabla 6 - Medidas Dispersion 
  ############################################################
  
  
  # Tabla 7 - Desviaciones
  {
    ###
    
    
    nombres_elementos <- c("Variable", "Rango Intercuartílico", "Desviación Intercuartílica", "n")
    
    tabla7 <- as.data.frame(matrix(NA,1, length(nombres_elementos)))
    colnames(tabla7) <- nombres_elementos
    
    
    Q1 <- quantile(mini_vector,  probs = 25/100)
    Q3 <- quantile(mini_vector,  probs = 75/100)
    
    RI <- Q3 - Q1
    DI <- RI/2
    
    # Redondeos
    RI <- round2(RI, input_decimales) 
    DI <- round2(DI, input_decimales) 
    
    
    # Tamanio muestral
    n_muestra <- length(mini_vector)
    
    
    
    
    tabla7[,1] <- colnames(minibase)
    tabla7[,2] <- RI
    tabla7[,3] <- DI
    tabla7[,4] <- n_muestra
    
    
    ###
  } # Fin Tabla 7 - Desviaciones
  ############################################################
  
  
  
  
  
  # Tabla 8: Intervalos de Confianza
  {
    ###
    
    
    alfa <- c(0.10, 0.05, 0.01)
    nombres_columnas <- c("Variable", "Media", "Confianza", "Límite Inferior IC", "Límite Superior IC", "n")
    tabla8 <- as.data.frame(matrix(NA, length(alfa), length(nombres_columnas)))
    colnames(tabla8) <- nombres_columnas
    
    for (n in 1:nrow(tabla8)) {
      
      este_alfa <- alfa[n]
      este_alfa_2 <- este_alfa/2
      gl <- n_muestra - 1
      
      
      desvio <- sd(mini_vector)
      desvio <- round2(desvio, input_decimales)
      
      t_li <- qt(este_alfa_2, df = gl, lower.tail = TRUE)
      t_ld <- qt((1-este_alfa_2), df = gl, lower.tail = TRUE)
      
      brazo <- t_ld*(desvio/sqrt(n_muestra))
      brazo <- round2(brazo, input_decimales)
      
      media_li <- media - brazo
      media_ld <- media + brazo
      
      tabla8[n, 1] <- colnames(input_base)
      tabla8[n, 2] <- media
      tabla8[n, 3] <- paste0((1-este_alfa)*100, "%")
      tabla8[n, 4] <- media_li
      tabla8[n, 5] <- media_ld
      tabla8[n, 6] <- n_muestra
      
    } # Fin for n
    
    
    
    
    
    
    
    ###  
  } # Fin Tabla 8
  #############################################################
  
  
  
  
  # Tabla 9: Tabla de Frecuencias
  {
    ###
    
    diferencia <- input_max - input_min
    rango <- diferencia/input_breaks
    
    cortes <- input_min + c(0:input_breaks)*rango
    
    # VerificacioN!
    # Pasa que si la variable es constante, hay que hacer
    # una sola categoria con todo. La funcion de R aunque
    # la variable sea constante, te tira mas de 1 intervalo.
    # Eso esta mal.
    tabla <- table(mini_vector)
    cantidad_categorias <- length(names(tabla))
    cantidad_cortes <- length(cortes)
    if(cantidad_categorias < cantidad_cortes) cortes <- cantidad_categorias
    
    if(cantidad_categorias > 1) {
      info <- cut(mini_vector, breaks = cortes , right = input_side,
                  include.lowest = T)
      
    } else info <- mini_vector
    
    dim(info) <- c(length(info), 1)
    info <- as.data.frame(info)
    colnames(info) <- colnames(input_base)
    
    #  tabla9 <- RMedic_1q_tablas(input_base = info, input_decimales = input_decimales)[[1]]
    
    # Pasamos la ultima fila como primera
    #  if(input_side) tabla9 <- tabla9[c(nrow(tabla9), (1:(nrow(tabla9)-1))), ]
    
    # Frecuencais Absolutas
    fa <- table(info)
    
    # Total Absoluto
    n_total <- sum(fa)
    grupos <- as.character(names(fa))
    salida_n_total <- rep(n_total, length(fa))
    
    # Cociente
    cociente <- paste0(fa, "/", n_total)
    
    # Frecuencias Relativas
    fr <- (fa/n_total)
    fr_guardado <- fr
    fr <- round2(fr, input_decimales)
    
    
    
    # Porcentajes
    porcentaje <- fr*100
    porcentaje <- round2(porcentaje, input_decimales)
    porcentaje  <- paste(porcentaje, "%", sep="")
    
    # Fusion
    fusion <- paste0(fa, " (", porcentaje, ")")
    
    # Tabla de Distribucion de Frecuencias
    tabla9 <- cbind(grupos, fa, salida_n_total, cociente, fr,
                    porcentaje, fusion)
    
    rotulo <- paste0("Variable: ", colnames(info))
    tabla9 <- as.data.frame(tabla9)
    nombres_tabla9 <- c(rotulo, "Frecuancia Absoluta", "Total", "Cociente", "Frecuencia Relativa", "Porcentaje", "FA (%)")
    colnames(tabla9) <- nombres_tabla9
    
    numericas <- c(2, 3, 5)
    for(n in 1:length(numericas)) tabla9[,numericas[n]] <- as.numeric(as.character(tabla9[,numericas[n]]))
    
    ###
  } # Fin Tabla 9 Tabla de Frecuencias
  ######################################################
  
  
  # Mis Tablas
  {
    ###
    mis_tablas <- list(tabla1, tabla2, tabla3, tabla4, tabla5, tabla6, tabla7, 
                       tabla8, tabla9)
    
    # ,  tabla8)
    
    names(mis_tablas) <- c("Medidas Resumen", 
                           "Medidas de Posición",
                           "Cuartiles",
                           "Deciles",
                           "Percentiles",
                           "Medidas de Dispersión", 
                           "Desviaciones",
                           "Intervalo de Confianza (IC) para la media",
                           "Distribución de Frecuencias")
    
    ###    
  }
  ##########################################################################
  
  
  
  
  
  
  # # # Cambios "NO DATA" o "Errores"
  {
    ###
    
    
    # Si hay errores... Y estamos en "NO DATA"
    if (output_cadena == FALSE) {
      
      
      # Cambiamos los valores por avisos
      for (n in 1:length(mis_tablas)) {
        
        esta_tabla <- mis_tablas[[n]]
        estas_dimensiones <- dim(esta_tabla)
        tabla_no_data <- as.data.frame(matrix("Sin datos", estas_dimensiones[1], estas_dimensiones[2]))
        colnames(tabla_no_data) <- colnames(esta_tabla)
        esta_tabla <- tabla_no_data
        
        mis_tablas[[n]]  <- esta_tabla
        
      } # Fin for n
      
    } # Fin if si hay errores...
    #############################################################
    
    
    ###   
  } # Fin Cambios "NO DATA" o "Errores"
  ##################################################
  
  
  
  # # # Salida
  {
    ###
    
    return(mis_tablas)
    
    
    ###  
  } # Fin Salida
  ##################
  
  
  
}


############


control_1c <- function(input_base = NULL, input_cadena = NULL){
  
  # # # Funcionamiento por defecto
  {
    ###
    
    if (is.null(input_cadena)) input_cadena <- TRUE
    
    
    
    
    # Armamos el siguiente eslabon
    output_cadena <- input_cadena
    
    ###
  } # Fin Funcionamiento
  ################################################
  
  
  # # # Controles 1
  {
    ###
    
    # Verificar que input_base no sea nulo
    if(output_cadena && is.null(input_base)) {
      cat("\n", "Error control_1c: input_base no debe ser nulo", "\n")
      output_cadena <- FALSE
    }  
    
    # Verificar si es un data frame
    if(output_cadena && !is.data.frame(input_base)) {
      cat("\n", "Error control_1c: input_base debe ser un data.frame", "\n")
      output_cadena <- FALSE
    }
    
    # Verificar si tiene al menos una columna
    if(output_cadena && ncol(input_base) == 0) {
      cat("\n", "Error control_1c: input_base no tiene columnas")
      output_cadena <- FALSE
    }
    
    # Verificar si tiene mas de 1 columna
    if(output_cadena && ncol(input_base) > 1) {
      cat("\n", "Error control_1c: input_base debe ser solo una columna")
      output_cadena <- FALSE
    }
    
    # Verificar si es no tiene datos
    if(output_cadena && nrow(input_base) == 0) {
      cat("\n", "Error control_1c: input_base no presenta filas")
      output_cadena <- FALSE
    }
    
    # Verificar que sea numerica
    if(output_cadena && !is.numeric(input_base[,1])) {
      cat("\n", "Error control_1c: input_base debe ser numérica.", "\n", "Utilice la solapa 'Control' sobre esta variable.", "\n")
      output_cadena <- FALSE
    }
    
    
    ###      
  } # Fin Controles 1
  ###################################################################
  
  # # # Salida
  {
    ###
    
    salida <- output_cadena
    
    names(salida) <- c("output_cadena")
    
    return(salida)
    
    
    ###  
  } # Fin Salida
  ##################
  
  
}


##############






RMedic_2q_tablas <- function(input_base = NULL, input_decimales = NULL, input_cadena = NULL) {
  
  
  # Input originales 
  {
    ###
    
    input_originales <- list(input_decimales, input_cadena)
    names(input_originales) <- c("input_decimales", "input_cadena")
    
    ###    
  } # Fin Argumentos originales
  ##############################################################################
  
  
  # # # Funcionamiento por defecto 
  {
    ###
    
    # Funcionamiento por defecto
    if (is.null(input_cadena)) input_cadena <- TRUE
    
    # Decimales por defecto
    if (is.null(input_decimales)) input_decimales <- 2
    
    
    # Armamos el siguiente eslabon
    output_cadena <- input_cadena
    
    ###
  } # Fin Funcionamiento
  ##########################################################
  
  
  # # # Control 1 - input_base
  {
    ###
    veredicto1 <- control_2q(input_base = input_base, input_cadena = output_cadena)
    output_cadena <- veredicto1
    
    ###  
  } # Fin Control 1 - input_base
  #################################################
  
  
  # # # Control 2 - input_decimales
  {
    ###
    
    # Hacemos el control de decimales
    veredicto2 <- control_decimales(input_decimales = input_decimales, input_cadena = output_cadena)
    
    # Si no pasa el control numerico, asignamos un nuevo valor para input_decimales
    # pero guardamos el original por si hace falta
    if (veredicto2 == FALSE){
      input_decimales_original <- input_decimales
      input_decimales <- 2
    }
    
    # Si paso el control anterior... guardamos esta nueva opinion
    if (veredicto1) output_cadena <- veredicto2
    
    ###  
  } # Fin Control 2 - input_decimales
  ############################################################################
  
  
  # # # Modificaciones, Controles 2, y base "NO DATA"
  {
    ###
    
    # Si todo va OK...
    if (output_cadena) {
      
      # Si cumple todo... Falta cada columna sea de tipo factor.
      if (is.factor(input_base[,1]) == FALSE) input_base[,1] <- as.factor(as.character(input_base[,1]))
      if (is.factor(input_base[,2]) == FALSE) input_base[,2] <- as.factor(as.character(input_base[,2]))
      
      # Creamos "mini"  
      mini <- na.omit(input_base)
      
      # Vemos que mini tenga filas
      if (nrow(mini) == 0) {
        cat("Error df02: 'mini' sin filas", "\n")
        output_cadena <- FALSE
      }
      
    } # Fin if Si todo va OK...
    ###########################
    
    # Si no hay datos o tienen errores...
    if (output_cadena == FALSE){
      mini <- as.data.frame(mtcars[c(8,2)])
      mini[,1] <- as.factor(as.character(mini[,1]))
      mini[,2] <- as.factor(as.character(mini[,2]))
      colnames(mini) <- "No Data"
      cat("Error df02: 'mini' externo agregado (NO DATA)", "\n")
    }
    ### 
  } # Fin Modificaciones
  ################################################################    
  
  
  # # # Resolucion
  {
    ###
    
    # 1) Marginales al total
    {
      ###
      
      
      
      # Detalles Iniciales y fa
      fa <- table(mini)
      n_total <- sum(fa)
      fa_interno <- fa
      m_col <- colSums(fa)
      m_row <- rowSums(fa)
      
      # fa marginal
      fa_marginal <- cbind(fa, m_row)
      fa_marginal <- rbind(fa_marginal, c(m_col, n_total))
      colnames(fa_marginal)[ncol(fa_marginal)] <- c("Total por Filas")
      rownames(fa_marginal)[nrow(fa_marginal)] <- c("Total por Columnas")
      
      # Cociente al Total con marginales
      cociente_marginal <- fa_marginal
      for(n in 1:ncol(cociente_marginal)) cociente_marginal[,n] <- paste0(cociente_marginal[,n], "/", n_total)
      
      
      # Frecuencias Relativas al Total con Marginales
      fr_marginal <- fa_marginal/n_total
      fr_guardado <- fr_marginal
      fr_marginal <- round2(fr_marginal, input_decimales)
      
      # Si es "NaN" le ponemos un cero... Hay que hacerlo dos veces
      for (n in 1:ncol(fr_marginal)) if (is.nan(fr_marginal[n])) fr_marginal[n,] <- rep(0, nrow(fr_marginal))
      for (k in 1:nrow(fr_marginal)) if (is.nan(fr_marginal[k])) fr_marginal[,k] <- rep(0, ncol(fr_marginal))
      
      m_col2 <- fr_marginal[nrow(fr_marginal),-ncol(fr_marginal)]
      m_row2 <- fr_marginal[-nrow(fr_marginal),ncol(fr_marginal)]
      totales_raros <- c(sum(m_col2), sum(m_row2))
      diferencia <- abs(totales_raros - 1)
      dt_diferencia <- max(diferencia) == diferencia
      orden_diferencia <- c(1,2)[dt_diferencia]
      if (length(orden_diferencia) == 2) orden_diferencia <- orden_diferencia[1]
      
      
      # En "fr"... los marginales al total por filas y columnas deben sumar 1 en cada caso.
      # Por temas de redondeo... puede que uno sea igual a 1 y el otro no... o que ambos sean diferentes de 1.
      # En ese caso deberiamos avisarle que las sumas marginales no estan dando como debieran... y que
      # debe cambiar los decimales para traajar...
      
      # # Deteccion de redondeo incorrecto
      fr_interno <- fr_marginal
      # if (input_aviso == TRUE) if (totales_raros[orden_diferencia] != 1) fr_marginal[nrow(fr_marginal), ncol(fr_marginal)] <- paste0(fr_marginal[nrow(fr_marginal), ncol(fr_marginal)], "(Redondeo Incorrecto)")  
      
      # Porcentajes al Total con marginales
      # # porcentaje_marginal <- fr_interno
      porcentaje_marginal <- fr_guardado
      porcentaje_marginal <- porcentaje_marginal*100
      porcentaje_marginal <- round2(porcentaje_marginal, input_decimales)
      #dim(PORCENTAJE) <- dim(FR_interno)
      
      porcentaje2_marginal <- porcentaje_marginal
      for(n in 1:ncol(porcentaje2_marginal)) for (k in 1:nrow(porcentaje2_marginal)) {
        porcentaje2_marginal[k,n] <- paste(porcentaje2_marginal[k,n], "%", sep="")
      }
      
      # Guardamos unos porcentajes internos...
      porcentaje_interno <- porcentaje_marginal
      
      # # Si tuvo problemas con las "fr" dejamos constancia tambien en la tabla de porcentajes
      # if (input_aviso == TRUE) if (totales_raros[orden_diferencia] != 1) porcentaje_marginal[nrow(porcentaje_marginal), ncol(porcentaje_marginal)] <- paste0(porcentaje_marginal[nrow(porcentaje_marginal), ncol(porcentaje_marginal)], "(Redondeo Incorrecto)")  
      
      # Fusion marginal
      fusion_marginal <- matrix(NA, nrow(fa_marginal), ncol(fa_marginal))
      colnames(fusion_marginal) <- colnames(fa_marginal)
      rownames(fusion_marginal) <- rownames(fa_marginal)
      for (k in 1:nrow(fusion_marginal)) fusion_marginal[k,] <- paste0(fa_marginal[k,], " (", porcentaje2_marginal[k,], ")")
      
      
      ###
    } # Fin Marginales al total
    ###################################################################################
    
    
    # 2) Todo por filas
    {
      
      # FA por filas
      fa_filas <- fa
      total_filas <- rowSums(fa)
      fa_filas <- cbind(fa_filas, total_filas)
      colnames(fa_filas)[ncol(fa_filas)] <- "Total por Filas"
      
      # Cociente por filas
      cociente_filas <- fa_filas
      for (n in 1:nrow(cociente_filas)) cociente_filas[n,] <- paste0(cociente_filas[n,], "/", total_filas[n]) 
      
      # "fr" por filas
      fr_filas <- fa_filas
      fr_filas_interno <- fr_filas
      
      for (n in 1:nrow(cociente_filas)) {
        
        fr_filas[n, ] <- fr_filas[n, ]/total_filas[n]
        
        if (total_filas[n] == 0) fr_filas[n, ] <- rep(0, length(fr_filas[n, ]))
        
        fr_filas_interno[n, ] <- fr_filas[n, ]
        fr_filas[n, ] <- round2(fr_filas[n, ], input_decimales)
      }
      
      
      totales_fr_filas <- fr_filas[,ncol(fr_filas)]
      dt_fr_filas <- totales_fr_filas != 1
      #    if (input_aviso == TRUE) fr_filas[dt_fr_filas] <- paste0(fr_filas[dt_fr_filas], "(Redondeo Incorrecto)")
      
      
      
      # "porcentaje" por filas
      porcentaje_filas <- fr_filas_interno*100
      porcentaje_filas <- round2(porcentaje_filas, input_decimales)
      
      porcentaje2_filas <-  porcentaje_filas
      for (n in 1:nrow(porcentaje2_filas)) porcentaje2_filas[n,] <- paste0(porcentaje2_filas[n,], "%")
      #     porcentaje_filas[dt_fr_filas] <- paste0(porcentaje_filas[dt_fr_filas], "(Redondeo Incorrecto)")
      
      # Fusion por filas
      fusion_filas <- matrix(NA, nrow(fa_filas), ncol(fa_filas))
      colnames(fusion_filas) <- colnames(fa_filas)
      rownames(fusion_filas) <- rownames(fa_filas)
      for (k in 1:nrow(fusion_filas)) fusion_filas[k,] <- paste0(fa_filas[k,], " (", porcentaje2_filas[k,], ")")
      
      
      ###
    } # Todo por filas
    ###################################################################
    
    
    # 3) Todo por columnas
    {
      ###
      
      
      # FA por columnas
      fa_columnas <- fa
      total_columnas <- colSums(fa)
      fa_columnas <- rbind(fa_columnas, total_columnas)
      rownames(fa_columnas)[nrow(fa_columnas)] <- "Total por columnas"
      
      # Cociente por columnas
      cociente_columnas <- fa_columnas
      for (n in 1:ncol(cociente_columnas)) cociente_columnas[,n] <- paste0(cociente_columnas[,n], "/", total_columnas[n]) 
      
      # "fr" por columnas
      fr_columnas <- fa_columnas
      fr_columnas_interno <- fr_columnas
      
      for (n in 1:ncol(cociente_columnas)) {
        
        fr_columnas[,n ] <- fr_columnas[,n ]/total_columnas[n]
        if (total_columnas[n] == 0) fr_columnas[,n ] <- rep(0, length(fr_columnas[,n ]))
        fr_columnas_interno[,n ] <- fr_columnas[,n ]
        fr_columnas[,n ] <- round2(fr_columnas[,n ], input_decimales)
      }
      
      
      totales_fr_columnas <- fr_columnas[nrow(fr_columnas),]
      dt_fr_columnas <- totales_fr_columnas != 1
      #    if (input_aviso == TRUE) fr_columnas[dt_fr_columnas] <- paste0(fr_columnas[dt_fr_columnas], "(Redondeo Incorrecto)")
      
      
      
      # "porcentaje" por columnas
      porcentaje_columnas <- fr_columnas_interno*100
      porcentaje_columnas <- round2(porcentaje_columnas, input_decimales)
      
      porcentaje2_columnas <- porcentaje_columnas
      for (n in 1:ncol(porcentaje2_columnas)) porcentaje2_columnas[,n] <- paste0(porcentaje2_columnas[,n], "%")
      #    if (input_aviso == TRUE)  porcentaje_columnas[dt_fr_columnas] <- paste0(porcentaje_columnas[dt_fr_columnas], "(Redondeo Incorrecto)")
      
      
      # Fusion por columnas
      fusion_columnas <- matrix(NA, nrow(fa_columnas), ncol(fa_columnas))
      colnames(fusion_columnas) <- colnames(fa_columnas)
      rownames(fusion_columnas) <- rownames(fa_columnas)
      for (k in 1:nrow(fusion_columnas)) fusion_columnas[k,] <- paste0(fa_columnas[k,], " (", porcentaje2_columnas[k,], ")")
      
      
      ###
    } # Todo por columnas
    #################################################
    
    
    # 4) Clasico
    {
      ###
      
      # FA Clasico  
      fa_clasico <- fa_marginal[-nrow( fa_marginal), - ncol( fa_marginal)]
      if(is.null(dim(fa_clasico))) {
        
        dim(fa_clasico) <- c((nrow(fa_marginal)-1), (ncol(fa_marginal)-1)) 
        fa_clasico <- as.data.frame(fa_clasico)
        rownames(fa_clasico) <- rownames(fa_marginal)[-nrow(fa_marginal)]
        colnames(fa_clasico) <- colnames(fa_marginal)[-ncol(fa_marginal)]
      }    
      
      # Cociente Clasico
      cociente_clasico <- cociente_marginal[-nrow( fa_marginal), - ncol( fa_marginal)]    
      if(is.null(dim(cociente_clasico))) {
        
        dim(cociente_clasico) <- c((nrow(fa_marginal)-1), (ncol(fa_marginal)-1)) 
        cociente_clasico <- as.data.frame(cociente_clasico)
        rownames(cociente_clasico) <- rownames(fa_marginal)[-nrow(fa_marginal)]
        colnames(cociente_clasico) <- colnames(fa_marginal)[-ncol(fa_marginal)]
      }   
      
      # FR Clasico
      fr_clasico <- fr_marginal[-nrow( fa_marginal), - ncol( fa_marginal)] 
      if(is.null(dim(fr_clasico))) {
        
        dim(fr_clasico) <- c((nrow(fa_marginal)-1), (ncol(fa_marginal)-1)) 
        fr_clasico <- as.data.frame(fr_clasico)
        rownames(fr_clasico) <- rownames(fa_marginal)[-nrow(fa_marginal)]
        colnames(fr_clasico) <- colnames(fa_marginal)[-ncol(fa_marginal)]
      }  
      
      # %
      porcentaje_clasico <- porcentaje_marginal[-nrow( fa_marginal), - ncol( fa_marginal)] 
      if(is.null(dim(porcentaje_clasico))) {
        
        dim(porcentaje_clasico) <- c((nrow(fa_marginal)-1), (ncol(fa_marginal)-1)) 
        porcentaje_clasico <- as.data.frame(porcentaje_clasico)
        rownames(porcentaje_clasico) <- rownames(fa_marginal)[-nrow(fa_marginal)]
        colnames(porcentaje_clasico) <- colnames(fa_marginal)[-ncol(fa_marginal)]
      }  
      
      # %%
      porcentaje2_clasico <- porcentaje2_marginal[-nrow( fa_marginal), - ncol( fa_marginal)] 
      if(is.null(dim(porcentaje2_clasico))) {
        
        dim(porcentaje2_clasico) <- c((nrow(fa_marginal)-1), (ncol(fa_marginal)-1)) 
        porcentaje2_clasico <- as.data.frame(porcentaje2_clasico)
        rownames(porcentaje_clasico) <- rownames(fa_marginal)[-nrow(fa_marginal)]
        colnames(porcentaje_clasico) <- colnames(fa_marginal)[-ncol(fa_marginal)]
      }  
      
      # Fusion
      fusion_clasico <- matrix(NA, nrow(fa_clasico), ncol(fa_clasico))
      colnames(fusion_clasico) <- colnames(fa_clasico)
      rownames(fusion_clasico) <- rownames(fa_clasico)
      for (k in 1:nrow(fusion_clasico)) fusion_clasico[k,] <- paste0(fa_clasico[k,], " (", porcentaje2_clasico[k,], ")")
      ###  
    } # Clasico
    ##################################
    
    
    ###
  } # Fin Resolucion
  ##################################################################
  
  
  # # # Objetos Intermedios
  {
    ###  
    
    #   input_columnas <- c(2,5)  
    
    # Una funcion propia...
    # Para convertir en "character" algunas columnas de varias tablas en una lista
    char_machine <- function(input_lista = NULL, input_columnas = NULL) {
      
      for (k in 1:length(input_columnas)) {
        for (n in 1:ncol(input_lista[[input_columnas[k]]])) {
          
          input_lista[[input_columnas[k]]][,n] <- as.character(input_lista[[input_columnas[k]]][,n])
          
        } # Fin for n
      } # Fin for k
      
      
      return(input_lista)
    } 
    
    
    
    
    
    # Todo lo CLASICO 
    {
      ###
      
      CLASICO <- list()
      CLASICO[[1]] <- as.data.frame(fa_clasico)
      CLASICO[[2]] <- as.data.frame(cociente_clasico)
      CLASICO[[3]] <- as.data.frame(fr_clasico)
      # ELIMINAR! # CLASICO[[4]] <- as.data.frame(porcentaje_clasico) 
      CLASICO[[4]] <- as.data.frame(porcentaje2_clasico)
      CLASICO[[5]] <- fusion_clasico
      names(CLASICO) <- c("Frecuencias Absolutas",
                          "Cociente al total", 
                          "Frecuencias Relativas al total",
                          "Porcentajes al total",
                          "Fusión (Frecuencia Absoluta y Porcentaje)")
      #   CLASICO <- char_machine(CLASICO, input_columnas)
      
      ###
    } # Fin Todo lo CLASICO
    ############################################################################
    
    
    # Todo al TOTAL
    {
      ###
      
      TOTAL <- list()
      TOTAL[[1]] <- as.data.frame(fa_marginal)
      TOTAL[[2]] <- as.data.frame(cociente_marginal)
      TOTAL[[3]] <- as.data.frame(fr_marginal)
      # ELIMINAR! #TOTAL[[4]] <- as.data.frame(porcentaje_marginal)
      TOTAL[[4]] <- as.data.frame(porcentaje2_marginal)
      TOTAL[[5]] <- fusion_marginal
      names(TOTAL) <- c("Frecuencias Absolutas",
                        "Cociente al total", 
                        "Frecuencias Relativas al total",
                        "Porcentajes al total",
                        "Fusión (Frecuencia Absoluta y Porcentaje)")
      #  TOTAL <- char_machine(TOTAL, input_columnas)
      
      ###
    } # Todo al TOTAL
    ############################################################################
    
    
    
    # Todo por FILAS
    {
      ###
      
      
      FILAS <- list()
      FILAS[[1]] <- as.data.frame(fa_filas)
      FILAS[[2]] <- as.data.frame(cociente_filas)
      FILAS[[3]] <- as.data.frame(fr_filas)
      # ELIMINAR! # FILAS[[4]] <- as.data.frame(porcentaje_filas)
      FILAS[[4]] <- as.data.frame(porcentaje2_filas)
      FILAS[[5]] <- fusion_filas
      names(FILAS) <- c("Frecuencias Absolutas",
                        "Cociente por filas", 
                        "Frecuencias Relativas por filas",
                        "Porcentajes por filas",
                        "Fusión por filas (Frecuencia Absoluta y Porcentaje)")
      #  FILAS <- char_machine(FILAS, input_columnas)
      
      ###
    } # Todo por filas
    ############################################################################
    
    
    
    # Todo por COLUMNAS
    {
      ###
      
      COLUMNAS <- list()
      COLUMNAS[[1]] <- as.data.frame(fa_columnas)
      COLUMNAS[[2]] <- as.data.frame(cociente_columnas)
      COLUMNAS[[3]] <- as.data.frame(fr_columnas)
      # ELIMAR! # COLUMNAS[[4]] <- as.data.frame(porcentaje_columnas)
      COLUMNAS[[4]] <- as.data.frame(porcentaje2_columnas)
      COLUMNAS[[5]] <- fusion_columnas
      names(COLUMNAS) <- c("Frecuencias Absolutas",
                           "Cociente por columnas", 
                           "Frecuencias Relativas por columnas",
                           "Porcentajes por columnas",
                           "Fusión por columnas (Frecuencia Absoluta y Porcentaje)")   
      # COLUMNAS <- char_machine(COLUMNAS, input_columnas)
      
      ###
    } # Fin Todo por COLUMNAS
    ############################################################################
    
    
    # SIMPLE
    {
      ###
      SIMPLE <- list()
      
      grupos_filas <- rownames(fa)
      grupos_columnas <- colnames(fa)
      cantidad_filas <- length(grupos_filas)*length(grupos_columnas)
      nombres_columnas <- c(colnames(input_base), "Frecuencia Absoluta", 
                            "Total", "Cociente al Total", "Frecuencia Relativa al Total",
                            "Porcentaje al Total", "FA (%)")
      
      
      tabla_simple <- as.data.frame(matrix(NA, cantidad_filas, length(nombres_columnas)))
      colnames(tabla_simple) <- nombres_columnas
      
      contador_externo <- 0
      for (k in 1:length(grupos_filas)) {
        for (n in 1:length(grupos_columnas)) {
          contador_externo <- contador_externo + 1
          
          tabla_simple[contador_externo,1] <- grupos_filas[k]
          tabla_simple[contador_externo,2] <- grupos_columnas[n]        
          tabla_simple[contador_externo,3] <- TOTAL[[1]][k,n]    
          tabla_simple[contador_externo,4] <- sum(TOTAL[[1]]) 
          tabla_simple[contador_externo,5] <- TOTAL[[2]][k,n]       
          tabla_simple[contador_externo,6] <- TOTAL[[3]][k,n]       
          tabla_simple[contador_externo,7] <- TOTAL[[4]][k,n]  
          tabla_simple[contador_externo,8] <- TOTAL[[5]][k,n]
          # ELIMINAR! SIMPLE[contador_externo,8] <- TOTAL[[5]][k,n] 
          
        } # Fin for n
      } # Fin for k
      
      SIMPLE[[1]] <- tabla_simple
      # 
      # names(SIMPLE) <- "Simple Entrada"
      names(SIMPLE) <- "Simple Entrada"
      
      ###
    } # Fin SIMPLE
    ############################################################################
    
    
    
    
    
    # ARMADO ESPECIAL
    {
      
      # Hay demasiadas tablas... Estas son las que va a mirar
      # el usuario por defecto.
      ARMADO_ESPECIAL <- list()
      ARMADO_ESPECIAL[[1]] <- TOTAL[[1]]
      ARMADO_ESPECIAL[[2]] <- FILAS[[4]]
      ARMADO_ESPECIAL[[3]] <- FILAS[[5]]
      names(ARMADO_ESPECIAL) <- c("Frecuencias Absolutas y Marginales",
                                  "Porcentajes por filas",
                                  "Fusión por filas (Frecuencia Absoluta y Porcentaje)")
    }
    ########################################
    
    
    ###
  } # Fin Objetos Intermedios
  ##############################################################################
  
  
  # # # Mis Tablas y cambios "NO DATA" y "Errores"
  {
    ###
    
    mis_tablas <- list(ARMADO_ESPECIAL, CLASICO, TOTAL, FILAS, COLUMNAS, SIMPLE)
    names(mis_tablas) <- c("Resumen", "Clasico", "Al total", "Por filas", "Por columnas", "Simple Entrada")  
    
    
    
    
    ###  
  } # Fin Mis Tablas
  ########################################################
  
  
  # # # Cambios "NO DATA" o "Errores"
  {
    ###
    
    # Si no es valido trabajar... Y estamos en "NO DATA"
    if (output_cadena == FALSE) {
      
      cambio1 <- "Sin datos en la Columna"
      cambio2 <- "Modificar input_decimales"
      
      # Damos aviso si es algo de los datos o los decimales
      cambio_aplicado <- cambio1
      if (veredicto1 == TRUE && veredicto2 == FALSE) cambio_aplicado <- cambio2
      
      
      
      # Cambiamos los valores por avisos
      for (n in 1:length(mis_tablas)) for (k in 1:length(mis_tablas[[n]])) {
        
        esta_tabla <- mis_tablas[[n]][[k]]
        
        estas_dimensiones <- dim(esta_tabla)
        tabla_no_data <- as.data.frame(matrix(cambio_aplicado, estas_dimensiones[1], estas_dimensiones[2]))
        colnames(tabla_no_data) <- colnames(esta_tabla)
        esta_tabla <- tabla_no_data
        
        mis_tablas[[n]][[k]]  <- esta_tabla
        
      } # Fin for n
      
    } # Fin si no es valido trabajar
    
    ###   
  } # Fin Cambios "NO DATA" o "Errores"
  ##################################################
  
  
  # # # Objetos Finales
  {
    ###
    
    referencias <- list()
    referencias[[1]] <- paste0("En filas: ", colnames(mini)[1])
    referencias[[2]] <- paste0("En columnas: ", colnames(mini)[2])
    
    ###  
  } # Fin Objetos Finales
  ###############################
  
  
  # # # Salida
  {
    ###
    
    salida <- list(mis_tablas, referencias, output_cadena, input_originales)
    names(salida) <- c("df02", "referencias", "output_cadena", "input_originales")
    
    return(salida)
    
    
    ###  
  } # Fin Salida
  ##################
  
  
  
  
  
  
} # Fin function

###########################


control_2q <- function(input_base = NULL, input_cadena = NULL){
  
  # # # Funcionamiento por defecto
  {
    ###
    
    if (is.null(input_cadena)) input_cadena <- TRUE
    
    
    
    
    # Armamos el siguiente eslabon
    output_cadena <- input_cadena
    
    ###
  } # Fin Funcionamiento
  ################################################
  
  
  # # # Controles 1
  {
    ###
    
    # Verificar que input_base no sea nulo
    if(output_cadena && is.null(input_base)) {
      cat("Error control_2q: input_base no debe ser nulo", "\n")
      output_cadena <- FALSE
    }  
    
    # Verificar si es un data frame
    if(output_cadena && !is.data.frame(input_base)) {
      cat("Error control_2q: input_base debe ser un data.frame", "\n")
      output_cadena <- FALSE
    }
    
    # Verificar si tiene al menos una columna
    if(output_cadena && ncol(input_base) == 0) {
      cat("Error control_2q: input_base no tiene columnas")
      output_cadena <- FALSE
    }
    
    # Verificar si tiene mas de 2 columna
    if(output_cadena && ncol(input_base) != 2) {
      cat("Error control_2q: input_base debe ser dos columnas")
      output_cadena <- FALSE
    }
    
    # Verificar si es no tiene datos
    if(output_cadena && nrow(input_base) == 0) {
      stop("Error control_2q: input_base no presenta filas")
      output_cadena <- FALSE
    }
    
    
    
    ###      
  } # Fin Controles 1
  ###################################################################
  
  # # # Salida
  {
    ###
    
    salida <- output_cadena
    
    names(salida) <- c("output_cadena")
    
    return(salida)
    
    
    ###  
  } # Fin Salida
  ##################
  
  
}


#######################


control_decimales <- function(input_decimales = NULL, input_cadena = NULL){
  
  # # # Funcionamiento por defecto
  {
    ###
    
    if (is.null(input_cadena)) input_cadena <- TRUE
    
    
    
    
    # Armamos el siguiente eslabon
    output_cadena <- input_cadena
    
    ###
  } # Fin Funcionamiento
  ################################################
  
  
  # # # Controles  - input_decimales
  {
    ###
    
    # Verificar que input_decimales es un vector
    if(output_cadena && !is.vector(input_decimales)) {
      cat("Error input_decimales: input_decimales debe ser un vector", "\n")
      output_cadena <- FALSE
    }
    
    # Verificar que input_decimales es un solo elemento
    if(output_cadena && length(input_decimales) > 1) {
      cat("Error input_decimales: input_decimales debe ser un solo número", "\n")
      output_cadena <- FALSE
    }
    
    # Verificar que input_decimales es un numero
    if(output_cadena && is.numeric(input_decimales) == FALSE) {
      cat("Error input_decimales: input_decimales debe ser un número", "\n")
      output_cadena <- FALSE
    }
    
    # Verificar que input_decimales no es "NA"
    if(output_cadena && is.na(input_decimales)) {
      cat("Error input_decimales: input_decimales no debe ser 'NA'", "\n")
      output_cadena <- FALSE
    }
    
    
    # Verificar que input_decimales no es "NaN"
    if(output_cadena && is.nan(input_decimales)) {
      cat("Error input_decimales: input_decimales no debe ser 'NaN'", "\n")
      output_cadena <- FALSE
    }
    
    ###      
  } # Fin Controles 
  ###################################################################
  
  
  # # # Salida
  {
    ###
    
    salida <- output_cadena
    
    names(salida) <- c("output_cadena")
    
    return(salida)
    
    
    ###  
  } # Fin Salida
  ##################
  
  
}

#########################

control_2c <- function(input_base = NULL, input_cadena = NULL){
  
  # # # Funcionamiento por defecto
  {
    ###
    
    if (is.null(input_cadena)) input_cadena <- TRUE
    
    
    
    
    # Armamos el siguiente eslabon
    output_cadena <- input_cadena
    
    ###
  } # Fin Funcionamiento
  ################################################
  
  
  # # # Controles 1
  {
    ###
    
    # Verificar que input_base no sea nulo
    if(output_cadena && is.null(input_base)) {
      cat("\n", "Error control_2c: input_base no debe ser nulo", "\n")
      output_cadena <- FALSE
    }  
    
    # Verificar si es un data frame
    if(output_cadena && !is.data.frame(input_base)) {
      cat("\n", "Error control_2c: input_base debe ser un data.frame", "\n")
      output_cadena <- FALSE
    }
    
    # Verificar si tiene al menos una columna
    if(output_cadena && ncol(input_base) == 0) {
      cat("\n", "Error control_1c: input_base no tiene columnas")
      output_cadena <- FALSE
    }
    
    # Verificar si tiene solo 1 columna
    if(output_cadena && ncol(input_base) == 1) {
      cat("\n", "Error control_2c: input_base debe ser dos columnas.")
      output_cadena <- FALSE
    }
    
    # Verificar si tiene mas de 2 columna
    if(output_cadena && ncol(input_base) > 2) {
      cat("\n", "Error control_2c: input_base debe ser solo dos columnas.")
      output_cadena <- FALSE
    }
    
    # Verificar si es no tiene datos
    if(output_cadena && nrow(input_base) == 0) {
      cat("\n", "Error control_2c: input_base no presenta filas")
      output_cadena <- FALSE
    }
    
    # Verificar que sea numerica la columna 1
    if(output_cadena && !is.numeric(input_base[,1])) {
      cat("\n", "Error control_2c: la columna 1 de input_base debe ser numérica.", "\n", "Utilice la solapa 'Control' sobre esta variable.", "\n")
      output_cadena <- FALSE
    }
    
    # Verificar que sea numerica la columna 2
    if(output_cadena && !is.numeric(input_base[,2])) {
      cat("\n", "Error control_2c: la columna 2 de input_base debe ser numérica.", "\n", "Utilice la solapa 'Control' sobre esta variable.", "\n")
      output_cadena <- FALSE
    }
    
    ###      
  } # Fin Controles 1
  ###################################################################
  
  # # # Salida
  {
    ###
    
    salida <- output_cadena
    
    names(salida) <- c("output_cadena")
    
    return(salida)
    
    
    ###  
  } # Fin Salida
  ##################
  
  
}

##########################

RMedic_2c_tablas <- function(input_base = NULL, input_decimales = NULL, input_cadena = NULL,
                             input_min1 = NULL, input_max1 = NULL, input_breaks1 = NULL,
                             input_side1 = NULL,
                             input_min2 = NULL, input_max2 = NULL, input_breaks2 = NULL,
                             input_side2 = NULL) {
  
  
  
  
  # Medidas de posicion y dispersion simultaneas
  # Default values
  if (is.null(input_decimales)) input_decimales <- 2
  if (is.null(input_cadena)) input_cadena <- T
  
  
  
  
  # # # Control 1 - input_base
  {
    ###
    # 1- Es nulo
    # 2- No es un data.frame
    # 3- Tiene cero columnas
    # 4- Tiene 0 columnas
    # 5- Tiene 1 columna
    # 6- Tiene mas de 2 columnas
    # 7- No tiene filas (nrow(input_base))
    
    veredicto1 <- control_2c(input_base = input_base, input_cadena = input_cadena)
    output_cadena <- veredicto1
    
    ###  
  } # Fin Control 1 - input_base
  #################################################
  
  
  
  # # # minibase y Control 2 -  base "NO DATA"
  {
    ###
    
    # Si todo va OK...
    if (output_cadena) {
      
      # Creamos "minibase"  
      minibase_general <- na.omit(input_base)
      minibase1 <- minibase_general[1]
      minibase2 <- minibase_general[2]
      mini_vector1 <- minibase_general[,1]
      mini_vector2 <- minibase_general[,2]
      
      # Vemos que minibase tenga filas
      if (nrow(minibase_general) == 0) {
        cat("Error en RMedic_2c_tablas(): 'input_base' posee solo celdas vacias.", "\n")
        output_cadena <- FALSE
      }
      
      # minibase sea numerica
      if (!is.numeric(mini_vector1)) {
        cat("Error RMedic_2c_tablas(): La columna 1 de 'input_base' debe ser numerica.", "\n")
        output_cadena <- FALSE
      }
      
      # minibase sea numerica
      if (!is.numeric(mini_vector2)) {
        cat("Error RMedic_2c_tablas(): La columna 2 de 'input_base' debe ser numerica.", "\n")
        output_cadena <- FALSE
      }
      
    } # Fin if Si todo va OK...
    ###################################################
    
    # Si hay algun problema...
    if (output_cadena == FALSE){
      minibase_general <- mtcars[c(1,3)]
      minibase1 <- minibase_general[1]
      minibase2 <- minibase_general[2]
      mini_vector1 <- minibase_general[,1]
      mini_vector2 <- minibase_general[,2]
      colnames(minibase_general) <- c("No Data1", "No Data2")
      cat("Error en RMedic_2c_tablas(): 'minibase' externo agregado (NO DATA)", "\n")
    } # Fin if si hay problemas
    ##########################################################
    
    
    ### 
  } # Fin Control 2 -  base "NO DATA"
  ################################################################
  
  
  # # # SandBox en caso de no ser valido
  {
    ###
    
    # Si hay algun problema...
    if (output_cadena == FALSE){
      minibase_general <- mtcars[c(1,3)]
      minibase1 <- minibase_general[1]
      minibase2 <- minibase_general[2]
      mini_vector1 <- minibase_general[,1]
      mini_vector2 <- minibase_general[,2]
      colnames(minibase) <- c("No Data1", "No Data2")
      cat("Error en RMedic_2c_tablas(): 'minibase' externo agregado (NO DATA)", "\n")
    } # Fin if si hay problemas
    ##########################################################
    
    ###  
  }
  ############################################################################
  
  
  # More default values
  {
    ###
    
    # Todo lo de la variable 1
    if(is.null(input_min1))  input_min1 <- min(mini_vector1)
    if(is.null(input_max1))  input_max1 <- max(mini_vector1)
    if(is.null(input_breaks1))  input_breaks1 <- nclass.Sturges(mini_vector1)
    if(is.null(input_side1))  input_side1 <- T
    
    # Todo lo de la variable 2
    if(is.null(input_min2))  input_min2 <- min(mini_vector2)
    if(is.null(input_max2))  input_max2 <- max(mini_vector2)
    if(is.null(input_breaks2))  input_breaks2 <- nclass.Sturges(mini_vector2)
    if(is.null(input_side2))  input_side2 <- T
    # 
    ###
  } # Fin More default values
  #################################################
  
  
  
  # Tablas para Variable 1
  tablas <- list()
  
  tablas[[1]] <- RMedic_1c_tablas(input_base = minibase1,
                                  input_decimales = input_decimales,
                                  input_cadena = input_cadena,
                                  input_min = input_min1, 
                                  input_max = input_max1,
                                  input_breaks = input_breaks1,
                                  input_side = input_side1)
  
  
  tablas[[2]] <- RMedic_1c_tablas(input_base = minibase2,
                                  input_decimales = input_decimales,
                                  input_cadena = input_cadena,
                                  input_min = input_min2, 
                                  input_max = input_max2,
                                  input_breaks = input_breaks2,
                                  input_side = input_side2)
  
  
  armado <- list()
  
  directo1 <- c(1:7)
  
  for(k in directo1){
    # Medidas Resumen
    armado[[k]] <- rbind(tablas[[1]][[k]], tablas[[2]][[k]])
    rownames(armado[[k]]) <- c(1:nrow(armado[[k]]))
    names(armado)[k] <- names(tablas[[1]])[k]
  }
  
  directo2 <- c(8,9,10)
  conteo_interno <- 0
  for(k in directo2){
    
    conteo_interno <- conteo_interno + 1
    # Medidas Resumen
    armado[[k]] <- rbind(tablas[[1]][[8]][conteo_interno, ], tablas[[2]][[8]][conteo_interno, ])
    rownames(armado[[k]]) <- c(1:nrow(armado[[k]]))
    names(armado)[k] <- names(tablas[[1]])[8]
  }
  
  # Distribucion de Frecuencias - Var1
  armado[[11]] <- tablas[[1]][[9]]
  names(armado)[11] <- paste0(names(tablas[[1]])[9], " - Variable 1")
  
  # Distribucion de Frecuencias - Var2
  armado[[12]] <- tablas[[2]][[9]]
  names(armado)[12] <- paste0(names(tablas[[1]])[9], " - Variable 2")
  
  
  return(armado)
  
} # Fin function