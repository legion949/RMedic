

# Funcion para cargar archivos xls, como yo quiero...
carga_xls <- function(input_archivo = NULL, input_hoja = 1) {
  
  # Libreria
  library(readxl)
  
  # Mi archivo  
  metralla <- strsplit(input_archivo, "/")[[1]]
  output_archivo <- metralla[length(metralla)]
  
  # Cargamos la base completa
  # Le quitamos los avisos
#  suppressWarnings({
    BASE_XLS <- as.data.frame(read_excel(input_archivo, col_names= TRUE, sheet = input_hoja, trim_ws = FALSE))
 # })
  
  # Cargamos solo el encabezado
  BASE_ENCABEZADO <- as.data.frame(read_excel(input_archivo, col_names= FALSE, sheet = input_hoja, n_max = 1, trim_ws = FALSE))
  
  # Vemos cuales son los encabezados vacios  
  salida_columna <- rep(TRUE, ncol(BASE_XLS))
  nombre_vacio <- is.na(BASE_ENCABEZADO[1,])
  
  # Puede pasar que haya diferencias entre BASE_XLS
  # y BASE_ENCABEZADO en la cantidad de columnas.
  # Puede que BASE_ENCABEZADO tenga menos cantidad de columnas
  
  
  # Nos fijamos si hay diferencias
  # Y si las hay, agregamos algunos espacios de nombres de columnas
  diferencia <- length(salida_columna) - length(nombre_vacio) 
  if (diferencia > 0) nombre_vacio <- c(nombre_vacio, rep(TRUE, diferencia))
  
  
  # Borramos la base encabezado
  # remove(BASE_ENCABEZADO)
  
  
  # Ahora... vamos a ver si sobran filas y columnas al final de la base de datos
  # que estan totalmente vacias, pero R las detecto, por ejemplo, por que 
  # estaban coloreadas o por que tuvieron datos en algun momento y ahora no...
  
  # Recortamos las columnas extras que aparecen a la derecha.
  # Son columnas sin nombre de columna y totalmente vacias
  # Creamos una columna vacia de referencia
  # Comenzaremos desde la ultima columna e iremos viendo 
  # si son iguales a una columna vacia
  
  # referencia_col <- rep(NA, nrow(BASE_XLS))

  # Le colocamos un candado, por que el recorte debe hacerse hasta que
  # deje de encontrar columnas vacias
  # Ademas "estado_columna" es un vector que dice quien se queda y quien se va de las columnas...
  
  estado_columna <- rep(TRUE, ncol(BASE_XLS))
  candado_columna <- FALSE
  iniciador_columna <- ncol(BASE_XLS) 
  # Dt de las columnas extra

  # Eliminacion de columnas agregadas vacias a la derecha  
  for (n in iniciador_columna:1) {
    
    # No hay candado para la eliminacion de columnas...
    if (candado_columna == FALSE) {
      
    # Si la columna elegida no tiene nombre...
    if (nombre_vacio[n] == TRUE) {
      
      # Y toda la columna esta vacia...
      if (length(na.omit(as.vector(as.matrix(BASE_XLS[,n])))) == 0) {
        
        # Debe desaparecer
        estado_columna[n] <- FALSE        
        
        
      } else candado_columna <- TRUE
      
    } else candado_columna <- TRUE
    
  } else break
}  # Fin for n
  
  # Recorte de las columnas extra
  BASE_XLS <- BASE_XLS[,estado_columna]
  
  
  # Hacemos lo mismo con las filas...
  # Creamos una referencia de filas vacias

  candado_fila <- FALSE 
  estado_fila <- rep(TRUE, nrow(BASE_XLS))
  iniciador_fila <- nrow(BASE_XLS) 
  
  
  
  # Eliminacion de filas agregadas vacias al final de la base de datos  
  for (n in iniciador_fila:1) {
    
    # No hay candado para la eliminacion de filas...
    if (candado_fila == FALSE) {
      
        # Y toda la fila esta vacia...
        if (length(na.omit(as.vector(as.matrix(BASE_XLS[n,])))) == 0) {
          
          # Debe desaparecer
          estado_fila[n] <- FALSE        
          
          
        } else candado_fila <- TRUE
        
      
      
    } else break
  }  # Fin for n
  
  
  # Recorte de las filas extra
  BASE_XLS <- BASE_XLS[estado_fila,]
  

  # # # Correccion de Errores en la carga
  # A veces pasa que una columna es completamente numerica y no la detecta como tal.
  # Puede ser por el separador decimal o por el formato de celda... O anda a saber por que.

  if (1 == 1) {

    # Vemos cuales son las columnas numericas    
    dt_numeric1 <- rep(FALSE, ncol(BASE_XLS))
    dt_numeric2 <- dt_numeric1
    for (n in 1:ncol(BASE_XLS)) dt_numeric1[n] <- is.numeric(BASE_XLS[,n])

    
    # Si hay alguna columna NO numerica
    if (sum(dt_numeric1) > length(dt_numeric1)) {
    
      
      # Aislamos el numero de posicion de las columnas no numericas
      columnas_no_numericas <- c(1:ncol(BASE_XLS))[!dt_numericas1]
      
      # Ahora... Vamos a tomar las que NO son numericas
      # y vamos a ver si al someterlas a estress se vuelven numericas o no
      for (n in 1:length(columnas_no_numericas)){
        
        este_orden1 <- columnas_no_numericas[n]
        
        if (dt_numeric1[este_orden1] == FALSE){
          
          # Vemos los datos originales sin las celdas vacias
          esta_columna1 <- na.omit(BASE_XLS[,este_orden1])
          
          # Forzamos eso a numerico...
          esta_columna2 <- suppressWarnings(as.numeric(as.character(esta_columna1)))
  
          # Si quedo algo de datos originales
          if (length(esta_columna1) > 0) {
            
            # Si quedo algo de datos forzados...
            if (length(esta_columna2) > 0) {
              
              # Si la cantidad de datos originales es igual a la forzada
              if (length(esta_columna1) == length(esta_columna2)) {
                
                # Si lo que quedo forzado es numerico
                if (is.numeric(esta_columna2)) {
                  
                  dt_numeric2[este_orden1] <- TRUE
                  
                }
              }
            }
          }
          
        }   # Fin if == FALSE 
        
      } # Fin for n
      
      # Si hay alguna transformacion para hacer...
      if (sum(dt_numeric2) > 0) {
      
        # Aislamos el numero de posicion de las columnas no numericas
        columnas_mod <- c(1:ncol(BASE_XLS))[dt_numericas2]
      
        # Modificamos las columnas que deben ser cohercionadas
        for (n in 1:length(columnas_mod)) {
          
          este_orden2 <- columna_mod[n]
          
          #suppressWarnings()
          BASE_XLS[, este_orden2] <- as.numeric(as.character(BASE_XLS[, este_orden2]))
          
        } # Fin for n
        
      } # Fin if... Si hay alguna transformacion que salio bien
      
    } # Fin if... Si hay alguna columna numerica
    
  

    
  }

  aviso <- list()
  aviso[[1]] <- c("Columnas sin cambios")
  aviso[[2]] <- c("Filas sin cambios")
  aviso[[3]] <- c("Columnas NO cohercionadas")
  names(aviso) <- c("Columnas Fantasma", "Filas Fantasma", "Columnas Cohercionadas")
  
  recorte1 <- length(estado_columna) - sum(estado_columna)
  recorte2 <- length(estado_fila) - sum(estado_fila)
  cambio <- sum(dt_numeric2)
  
  orden_cambio <- c(1:ncol(BASE_XLS))[dt_numeric2]
  
  if (recorte1 > 0) aviso[[1]] <- paste0("Columnas recortadas: ", recorte1, " - Orden: ")
  if (recorte2 > 0) aviso[[2]] <- paste0("Filas recortadas: ", recorte2)
  if (cambio > 0) aviso[[3]] <- paste0("Columnas recortadas: ", cambio, " - Columnas: ", paste0(orden_cambio, collapse=","))
  
  
  
    
  SALIDA <- list(output_archivo, BASE_XLS, aviso)
  names(SALIDA) <- c("archivo", "base", "aviso")
  return(SALIDA)
  
}



if (1 == 2) {
  
  
  
  input_archivo <- "BASE.xlsx"
  input_hoja <- 1
  BASE <- carga_xls(input_archivo = input_archivo, input_hoja = input_hoja)
  
  
}

