
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
    fr <- round2((fa/n_total), input_decimales)
    
    
    
    # Porcentajes
    porcentaje  <- paste(fr*100, "%", sep="")
    
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
      desvio <- round2(desvio, input_decimales)
      
      li_prop <- p - desvio
      ls_prop <- p + desvio
      
      dt_li <- li_prop < 0
      dt_ls <- ls_prop > 1
      
      if (sum(dt_li) > 0) li_prop[dt_li] <- 0
      if (sum(dt_ls) > 0) ls_prop[dt_ls] <- 1
      
      
      li_porcentaje <- paste0((li_prop*100), "%")
      ls_porcentaje <- paste0((ls_prop*100), "%")
      
      
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