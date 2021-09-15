# 
# if (1 == 2) {
# ODD_RATIO_MASTER <- function(n00, n01, n10, n11, alpha = 0.05){
# 
#   # Inicio
#   OD <- list()
#   
#   # Ordenamiento General
#   OD$input <- list()           # Ingresan solo los elementos que son input de la funcion
#   OD$DETALLES <- list()        # Nombre de las variables...
#   OD$CONTROLES <- list()       # Detalle de los diferentes controles por los que paso la base de datos a trabajar...
#   OD$MINI <- list()            # Va la MINIBASE de trabajo, y quien es VR y quien es FACTOR
#   OD$ANALISIS <- list()        # Los analisis... un apartado sera "$ORIGINAL"... y otro será "$CONSULTORA"
#   OD$SCRIPT <- list()          # Un apartado sera el script para obtener exactamente lo mismo que la salida cruda de R.
#   OD$SALIDA_ARMADA <- list()   # La salida, que son objetos que ingresaran al Shiny...
#   OD$CONTROL_GENERAL <- list() # Guardamos... para saber... si se cumplio o no con todos los controles...
#   
#   
#   
#   #
#   #  Compute the odds ratio between two binary variables, x and y,
#   #  as defined by the four numbers nij:
#   #
#   #    n00 = number of cases where x = 0 and y = 0
#   #    n01 = number of cases where x = 0 and y = 1
#   #    n10 = number of cases where x = 1 and y = 0
#   #    n11 = number of cases where x = 1 and y = 1
#   #
#   OR <- (n00 * n11)/(n01 * n10)
#   #
#   #  Compute the Wald confidence intervals:
#   #
#   siglog <- sqrt((1/n00) + (1/n01) + (1/n10) + (1/n11))
#   zalph <- qnorm(1 – alpha/2)
#   logOR <- log(OR)
#   loglo <- logOR – zalph * siglog
#   loghi <- logOR + zalph * siglog
#   #
#   ORlo <- exp(loglo)
#   ORhi <- exp(loghi)
#   #
#   oframe <- data.frame(LowerCI = ORlo, OR = OR, UpperCI = ORhi, alpha = alpha)
#   oframe
# }
# 
# }