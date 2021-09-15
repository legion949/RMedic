
num2let <- function(input_numeros=NULL)
{  
  ###
  
  # Idea General...
  # Toma un numero, y devuelve una letra o conjunto de letras.
  # Es una asociacion entre los numeros y orden de las letras.
  
  # input_numeros <- c(1024, 0)
  # Intro...
  # Lo que queremos hacer es que... A partir de un numero que ingresamos, nos de la 
  # codificacion en letras como tiene el EXCEL.
  # Entonces... los numeros que ingresamos estan en base 10...
  # Podemos decir que el alfabeto romano... que tiene 26 letras... es una base 26...
  # Pero... a parte... a esto ultimo... hay que hacerle una correccion...
  # Normalmente una base 26... seria del 0 al 25... pero aca queremos que sea del 1 al 26.
  
  # Entonces... vamos a pasar de base 10 a base 26 clasica, y luego de base 26 clasica a base 26 romana.
  # Y luego... ahi si... de base 26 romana al alfabeto

  v1 <- !is.null(input_numeros)
  v2 <- is.numeric(input_numeros)
  v3 <- (sum(input_numeros == 0) == 0)
  
  if (v1 && v2 && v3) { 
    
  # Parte 0: Elementos basicos 
  {
  # Cantidad de elementos del vector
  cantidad_numeros <- length(input_numeros)
  
  # Letras minusculas de R
  let_min_R <- letters[1:length(letters)]

  # Letras mayusculas de R
  let_may_R <- LETTERS[1:length(letters)]

  # Codificacion del vector de salida
  exit_letras <- rep(NA, length(input_numeros))

  # Cantidad de letras de nuestro abecedario  
  cantidad_letras <- length(letters)
  
  ###
  } # Fin Parte 0
  ########################################################
  
  
  # Traduccion de numeros a letras
  for (n in 1:length(input_numeros)) {
    
  # Cada numero en particular..
  numero_seleccionado <- input_numeros[n]
    

  # Parte1...
  # Detectamos cuantas potencias de 26 soporta como maximo el "numero_seleccionado
  {
  
    divisor <- 26
    dividendo <- numero_seleccionado
    candado <- FALSE
    contador_pasos <- 1
    
    # El objetivo de este bucle while...
    # Es saber hasta que potencia es divisible de manera entera el numero a convertir...
    # Y cuantas letras tendra entonces la salida
    
    # Con esto sabemos la potencia maxima que puede dividir al numero
      
      maxima_potencia <- log(dividendo)%/%log(divisor)
      
      potencia_paso <- c(maxima_potencia:1)
      dividendo_paso <- c()
      cociente_paso <- c()
      resto_paso <- c()
      
      for (k in 1:length(potencia_paso)) {
        
        if (k == 1)   {
          dividendo_paso[1] <- dividendo
          cociente_paso[1] <- dividendo_paso[1]%/%(divisor^potencia_paso[1])
          resto_paso[1] <- dividendo - cociente_paso[1]*(divisor^potencia_paso[1])
        } # Fin if ==
        
        
        if (k > 1) {
        dividendo_paso[k] <- resto_paso[k-1]  
        cociente_paso[k] <- dividendo_paso[k]%/%(divisor^potencia_paso[k])
        resto_paso[k] <- resto_paso[k-1] - cociente_paso[k]*(divisor^potencia_paso[k])
        } # Fin if >
      } # Fin for k
      
      ultimo_resto <- resto_paso[length(resto_paso)]
      
      # Determinamos cuantos digitos tiene
      if (ultimo_resto == 0) digitos <- cociente_paso else digitos <- c(cociente_paso, ultimo_resto)
      
      digitos
      
        
 
     
  } # Fin for n
  
  exit_letras[n] <- paste0(LETTERS[digitos], collapse="")
  }
    
  return(exit_letras)
  
  } else {
   
    if(v3 == FALSE) cat("num2let: Valores numericos igual a cero NO son permitidos \n ") 
  return(NULL)
   
   
    
  }
    
###
} # Fin funcion num2let
#################################################################



if (1 == 2) {
# # # MUESTRA

 AVER <-  num2let(c(1024))
 AVER
 
 num2let(133479)
 
 
 num2let(0)
# 
# # codigo2 <- c(2, 4, 16, 12, 44, 76, 77)
# # AVER <- num2let(codigo2)
# 
# 
# # codigo2 <- c(2, 4, 16, 12, 44, 76, 77)
# # num2let(codigo2)
  
}