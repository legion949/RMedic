
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
  
  
  dt_ok <- FALSE
  
  if(!is.null(Base))
    if(!is.null(the_col))
      if(the_col != "")
        if(sum(colnames(Base) == the_col) > 0)
          dt_col <- colnames(Base) == the_col
  pos_col <- c(1:length(dt_col))
  the_col <- pos_col[dt_col]
  my_letter <- num2let(the_col)
  
  my_letter
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
    fusion <- paste0(fa, "(", porcentaje, ")")
    
    # Tabla de Distribucion de Frecuencias
    tabla01 <- cbind(grupos, fa, salida_n_total, cociente, fr,  
                     porcentaje, fusion)
    
    rotulo <- paste0("Variable: ", colnames(minibase))
    tabla01 <- as.data.frame(tabla01)
    nombres_tabla01 <- c(rotulo, "Frecuancia Absoluta", "Total", "Cociente", "Frecuencia Relativa", "Porcentaje", "FA(%)")
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
    
    
    nombres_elementos <- c("Variable", "Varianza", "Desvío Estándard", "Error Estándard", "Coeficiente de Variación", "n")
    
    tabla6 <- as.data.frame(matrix(NA,1, length(nombres_elementos)))
    colnames(tabla6) <- nombres_elementos
    
    
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
    
    
    # Primero sacamos el desvio...
    # Sin redondear el desvio, lo usamos para sacar el error estandard...
    # Y luego redondeamos los dos.
    # Esto es para sacar mejor al EE... por que sino sacas el DE... lo redondeas...
    # lo usas para sacar el EE y lo volves a redondear.
    # Lo mismo con el CV.
    
    desvio <- round2(desvio, input_decimales)
    ee <- round2(ee, input_decimales)
    cv <- round2(cv, input_decimales)
    
    
    
    tabla6[,1] <- colnames(minibase)
    tabla6[,2] <- varianza
    tabla6[,3] <- desvio
    tabla6[,4] <- ee
    tabla6[,5] <- cv
    tabla6[,6] <- n_muestra
    
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
    
    # Si hay al menos 2 cortes para hacer...
    if(cortes >= 2) {
    info <- cut(mini_vector, breaks = cortes , right = input_side,
                include.lowest = T)
    }
    
    # Si la variable es constante, no hay nada para cortar!
    if (cortes == 1) info <- mini_vector
    
    
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
    fr <- round2((fa/n_total), input_decimales)
    
    
    
    # Porcentajes
    porcentaje  <- paste(fr*100, "%", sep="")
    
    # Fusion
    fusion <- paste0(fa, "(", porcentaje, ")")
    
    # Tabla de Distribucion de Frecuencias
    tabla9 <- cbind(grupos, fa, salida_n_total, cociente, fr,
                    porcentaje, fusion)
    
    rotulo <- paste0("Variable: ", colnames(info))
    tabla9 <- as.data.frame(tabla9)
    nombres_tabla9 <- c(rotulo, "Frecuancia Absoluta", "Total", "Cociente", "Frecuencia Relativa", "Porcentaje", "FA(%)")
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