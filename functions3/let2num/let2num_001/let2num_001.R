
let2num <- function(input_letras=NULL) {  
  ###
  
  # Lo que hace es...
  # Tomar un vector con letras...  y devolver un vector con números
  # El número que devuelve para cada elemento del vector es el número de orden
  # que le corresponde a cada letra o conjunto de letras si utilizaramos
  # las letras para contar.
  # "A" seria 1
  # "B" seria 2
  # "Z" seria 26
  # "AA" seria 27...
  # "FG" es 163
  # "goku" es 203679
  
  if (!is.null(input_letras)) {
    
  # Letras ingresadas
  #input_letras <- c("AMJ")
  
  # Cantidad de letras
  cantidad_letras <- length(letters)
  
  # Letras minusculas de R
  let_min_R <- letters[1:length(letters)]
  orden_min_R <- c(1:length(let_min_R))
  
  # Letras mayusculas de R
  let_may_R <- LETTERS[1:length(letters)]
  orden_may_R <- c(1:length(let_min_R))
  
  # Numeros de salida
  exit_numeros <- rep(NA, length(input_letras))
  
  # Traduccion de codificacion a numeros
  for (n in 1:length(input_letras)) {
    
    # Cada codificacion en particular..
    codificacion_seleccionada <- input_letras[n]
    
    # El particionado de esa codificacion en particular...
    letritas <- strsplit(codificacion_seleccionada, "")
    letritas <- letritas[[1]]
    
    
    
    # Numeritos de cada letrita...
    numeritos <- rep(NA, length(letritas))
    
    # Para cada letrita... detectamos que numero le corresponde...
    for (k in 1:length(letritas)) {
      
      
      # Puede ser mayuscula o minuscula
      detec_may <- let_may_R == letritas[k]
      cant_may <- sum(as.numeric(detec_may))
      
      detec_min <- let_min_R == letritas[k]
      cant_min <- sum(as.numeric(detec_min))
      
      
      # Si es mayuscula
      if (cant_may > 0) numeritos[k] <- orden_may_R[detec_may]
      
      # Si es minuscula
      if (cant_min > 0) numeritos[k] <- orden_min_R[detec_min]
      
      
      
      ###  
    } # Fin for k letritas
    ###############################################################
    
    
    # Ahora... 
    # Cada codificacion... tiene un objeto "numeritos".
    # "numeritos" tiene un numero por cada letra de la codificacion
    
    # Ahora... 
    # La codificacion "Z" es la 26... el "numeritos" que le corresponde es el "26"
    # La codificacion "AA" es la 27... el numeritos que le corresponde es el "1,1"
    # Esto es por que... al tener dos letras... el 1er "1" implica que ya se dio toda 
    # una vuelta al abecedario... 
    # Entonces... la 1er posicion debe ser multiplicada por "26"... para tener "26,1"
    # y entonces al sumarlos otorgue un 26+1 = 27
    
    # Si bien EXCEL solo trabajo con hasta 2 letras para la codificacion...
    # Generamos un script que permite traduccir cualquier cantidad de letras...
    
    # Entonces... 
    # "numeritos" debe ser modificado de la siguiente manera...
    # Su primer casilla debe ser... exactamente el numero que tiene
    # S 2da casilla... 27 elevado a la 1 + el numero que tiene la celda 2
    # Su 3er casilla... 27 elevado a la 2 + el numero que tiene en la celda 3..
    # Y así...
 
    potencias <- c((length(numeritos)-1):0)
    
    numeritos_transformados <- rep(NA, length(numeritos))
    
    numeritos_transformados <- numeritos*(cantidad_letras^potencias)
    
   
    
    exit_numeros[n] <- sum(numeritos_transformados)
    
    
    ###
  } # Fin for n
  ############################################################
  
  
  return(exit_numeros)
  
  } else return(NULL)
  ###
} # Fin funcion let2num
#############################




if (1 == 2) {
  # # # MUESTRA
  codigo <- c("A", "Z", "AA","AX", "HH", "AAA", "ZZ")
AVER <-   let2num(codigo)
AVER

# AMJ es 124
let2num("AMJ")

let2num("goku")

num2let(133479)

} 


