
# Color inicial de la aplicacion
# No me gusta que empiece con negro... a si que empieza con alguno de estos colores
colores <- c("red", "orange", "blue", "skyblue")

color_inicial <- sample(c(1:length(colores)),1)
color_inicial <- colores[color_inicial]


# El N total... sería entre 10 y 20...
total <- sample(c(10:20), 1)

# Valor de lambda inicial...
# Sera un valor entre 2 y 6... con dos decimales
V1 <- sample(c(2:6), 1)
V2 <- sample(c(0:9), 1)
V3 <- sample(c(0:9), 1)

valor_lambda <- paste(V1, ".", V2, V3, sep="")
valor_lambda <- as.numeric(as.character(valor_lambda))



# # # Para el inicio de la aplicacion... 
# # # tenemos cuatro posibilidades, para el valor de x...
# 1) Es un solo valor
# 2) Tres valores con coma
# 3) Dos valores con los 2 puntos (:)
# 4) Cuatro valores, con coma, dos puntos y coma

# Hay un rango de eleccion de valores...
estaca <- round(valor_lambda)
rango <- (estaca-2):(estaca+4)

esta_pos <- sample(c(1:4), 1)

# Si sale sorteado el 1...
if (esta_pos == 1) casos <- rango[sample(c(1:length(rango)), 1)]
############################################################################

# Si sale sorteado el 2...
if (esta_pos == 2) {
  casos <- rango[sample(c(1:length(rango)), 3, replace=FALSE)]
  casos <- casos[order(casos)]
  casos <- paste(casos[1], ",", casos[2], ",", casos[3], sep="")
} # Fin == 2
##################################################################################

# Si sale sorteado el 3...
if (esta_pos == 3) {
  casos <- rango[sample(c(1:length(rango)), 2, replace=FALSE)]
  casos <- casos[order(casos)]
  casos <- paste(casos[1], ":", casos[2], sep="")
} # Fin == 3
##################################################################################

# Si sale sorteado el 4...
if (esta_pos == 4) {
  lala <- FALSE
  while(lala == FALSE) {
    casos <- rango[sample(c(1:length(rango)), 4, replace=FALSE)]
    if ( (casos[2] - casos[3]) > 1) lala <- TRUE
  } # Fin while lala == FALSE
  
  casos <- casos[order(casos)]
  casos <- paste(casos[1], ",", casos[2], ":", casos[3], ",", casos[4], sep="")
} # Fin == 4
############################################################################################


# La cantidad de decimales... en principio sera 4
decimales <- 4


# Unimos todos los elementos iniciales en una listsa
inicio <- list()

inicio$color_inicial <- color_inicial  # Color
inicio$lambda <- valor_lambda
inicio$total <- total                  # Total... en N
inicio$casos <- casos                  # valores de x
inicio$decimales <- decimales          # decimales 


