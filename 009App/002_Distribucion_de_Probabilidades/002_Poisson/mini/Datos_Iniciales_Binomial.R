
# Color inicial de la aplicacion
# No me gusta que empiece con negro... a si que empieza con alguno de estos colores
colores <- c("red", "orange", "blue", "skyblue")

color_inicial <- sample(c(1:length(colores)),1)
color_inicial <- colores[color_inicial]


# El N total... sería entre 10 y 20...
total <- sample(c(10:20), 1)


# # # Para el inicio de la aplicacion... 
# # # tenemos cuatro posibilidades, para el valor de x...
# 1) Es un solo valor
# 2) Tres valores con coma
# 3) Dos valores con los 2 puntos
# 4) Cuatro valores, con coma, dos puntos y coma

esta_pos <- sample(c(1:4), 1)

  # Si sale sorteado el 1...
if (esta_pos == 1) casos <- sample(c(1:total), 1)
############################################################################

  # Si sale sorteado el 2...
if (esta_pos == 2) {
   casos <- sample(c(1:total), 3)
   casos <- casos[order(casos)]
   casos <- paste(casos[1], ",", casos[2], ",", casos[3], sep="")
   } # Fin == 2
##################################################################################

  # Si sale sorteado el 3...
if (esta_pos == 3) {
casos <- sample(c(1:total), 2)
casos <- casos[order(casos)]
casos <- paste(casos[1], ":", casos[2], sep="")
} # Fin == 3
##################################################################################

  # Si sale sorteado el 4...
if (esta_pos == 4) {
lala <- FALSE
while(lala == FALSE) {
  casos <- sample(c(1:total), 4)
  if ( (casos[2] - casos[3]) > 1) lala <- TRUE
} # Fin while lala == FALSE

  casos <- casos[order(casos)]
casos <- paste(casos[1], ",", casos[2], ":", casos[3], ",", casos[4], sep="")
} # Fin == 4
############################################################################################


# El valor de probabildiad... 
# Tiene un cero, como toda probabilidad... 
# Luego será un 2 o un 3... y luego cualquier numero entre 0 y 1
# Se aprecia mejor la binomial con un valor de probaiblidad entre 0.2 y 0.3
num_dec1 <- sample(c(2:3),1)
num_dec2 <- sample(c(0:9),1)

# Unimos los valores sorteados...
valor_prob <- as.numeric(paste("0.", num_dec1, num_dec2, sep=""))

# La cantidad de decimales... en principio sera 4
decimales <- 4


# Unimos todos los elementos iniciales en una listsa
inicio <- list()

  inicio$color_inicial <- color_inicial  # Color
  inicio$total <- total                  # Total... en N
  inicio$casos <- casos                  # valores de x
  inicio$valor_prob <- valor_prob        # probabilidad
  inicio$decimales <- decimales          # decimales 
  

  
  
# # # Listado de Errores
  #  1) Valor p con elementos no numéricos
  #  2) Valor p con varios decimales
  #  3) Valor p menor a 0
  #  4) Valor p mayor a 1
  #  5) Valores de x no numericos
  #  6) Valores de x con decimales
  #  7) Valores de x negativos
  #  8) Valores de n no numericos
  #  9) Valores de n con decimales
  # 10) Valores de n negativos
  # 11) Valos de x mayores a n
  # 12) Valores de redondeo no numericos
  # 13) Valores de redondeo negativos
  
  
  
  frase_error_binomial <- list()
  
  # 1) Valor p con elementos no numericos
  frase_error_binomial[[1]] <- c("El valor p debe ser un número.")
  
  # 2) Valor p con vraios decimales
  
  
  # 2) Valor p menor a 0
  frase_error_binomial[[2]] <- c("El valor p debe ser cero o positivo.")
  
  # 3) Valor p mayaor a 1
  frase_error_binomial[[3]] <- c("El valor p debe ser cero o positivo.")
  
  # 4) Valores de x no numericos
  frase_error_binomial[[4]] <- c("Los valores de x deben ser números.")
  
  # 5) Valores de n no numericos
  frase_error_binomial[[5]] <- c("El valor de Tamaño Muestral (n) debe ser un número.")
  
  # 6) Valos de x mayores a n
  frase_error_binomial[[6]] <- c("Es incorrecto ingresar valores de \"x\" mayores que \"n\".")
  
  # 7) Valores de redondeo negativos
  frase_error_binomial[[7]] <- c("El redondeo debe ser un valor 0 o positivo.")
  
  # 8) Valores de redondeo no numericos
  frase_error_binomial[[8]] <- c("El valor p debe ser cero o positivo.")

  
  