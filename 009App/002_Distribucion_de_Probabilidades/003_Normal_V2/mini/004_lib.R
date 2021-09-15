
# Librerias
{
  library(shiny)
  library(shinyjs)
} # Fin Librerias
#######################################################

### Funcion para pintar!
FillCurve <- function (x1= -4 , x2= -3.999, col= "orange", Nmin= -4, muestreo= 400, Nmax= 4, mean=0 , sd= 1, media2 = 0, desvio2= 1,z_ext=c(-4,4)) {
  x <- seq(Nmin, Nmax, length= muestreo)
  curve(dnorm(x, mean, sd),xlim= c(Nmin, Nmax), xlab="Valores de Z", ylab="Frecuencias Relativas", main="Distribución Normal Estándard", axes=F)
  axis(2,  las=1)
  axis(1, c(z_ext[1]:z_ext[2]), labels= round(((c(z_ext[1]:z_ext[2])*desvio2) + media2),2) ,las=1)
  
  s <- seq(from= x1, to= x2, by= 0.001)
  for(i in 1:length(s)){
    segments(x0= s[i], y0= 0, x1= s[i], y1= dnorm(s[i]), col= col)
    # curve(dnorm(x, mean, sd),xlim= c(Nmin, Nmax), xlab="Valores de Z", ylab="Frecuencia Relativa", main="Distribución Normal Estándard", add=T)
    
  } # Fin for i
} # Fin function
### END funcion para pintar 