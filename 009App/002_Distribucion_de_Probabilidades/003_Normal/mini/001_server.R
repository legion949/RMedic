# Expression that generates a histogram. The expression is
# wrapped in a call to renderPlot to indicate that:
#
#  1) It is "reactive" and therefore should be automatically
#     re-executed when inputs change
#  2) Its output type is a plot

ALFA <- 0.05
N <- 10

withMathJax()

# Marco para todos los graficos...
valores_z_extremos <- c(-5,5)
cantidad <- 400
####################################



# Gráfico 1
output$distPlot1 <- renderPlot({
  
  # Detalles varios
  varianza <- input$sigma_cuad
  desvio <- sqrt(varianza)
#  cantidad <- 400
  media <- input$mu
  
  # El gráfico tendra hasta 5 desvios a la derecha
  # y 5 desvios a la izquierda de la media
  valores_extremos <- c(media-5*desvio, media+5*desvio)
  
  
  #  la <- sqrt(input$sigma_cuad)
  
  
  # x <- seq(-4, 4, length=cantidad)
  
  # Genero los datos del eje "X" y del "Y"
  # Apartir de los valores extremos, para una normal standard
  x <- seq(valores_z_extremos[1], valores_z_extremos[2], length=cantidad)
  y <- dnorm(x,mean=0, sd=1)
  
  # Los datos estan tomados de una distribución normal stándard...
  # Los valores de x... tiene media 0
  # yo quiero que tengan la media que # Ahora los corrijo
  # Los valosres de "X" son valores estandarizados..:
  # Lo que hago ahora... es desestandarizarlos...
  x_mod <- x*(desvio)+media
  
  

  zi <- qnorm(ALFA/2)
  zd <- qnorm(1-ALFA/2)
  
  la <- sqrt(input$sigma_cuad)
  
  limites <- c(NA, NA)
  limites[1] <- zi*(sqrt(input$sigma_cuad)/sqrt(N))+input$mu
  limites[2] <- zd*(sqrt(input$sigma_cuad)/sqrt(N))+input$mu
  
  # dif <- input$mu - limites[1]
  dif <- desvio
  detalle <- c(valores_z_extremos[1]:valores_z_extremos[2])
  valores_x <- detalle*dif
  valores_x <- valores_x + media
  valores_x <- round(valores_x, 2)
  
  valores_x_mod <- valores_x
  valores_x_mod[1] <- "-Inf"
  valores_x_mod[length(valores_x)] <- "+Inf"
#  valores_x <- c(zi, zd)
  
  #  FillCurve(limite_inferior, limite_superior, mean=input$mu, sd=input$sigma_cuad )
  
  plot(x_mod, y, xlab="Valores de la Variable", 
       ylab="Frecuencias Relativas", 
       main="Distribución Normal", 
       axes=F, col="white")
  
#  s <- seq(from= x1, to= x2, by= 0.001)
  for(i in 1:(length(x_mod)-1)){
    segments(x0= x_mod[i], y0= y[i], x1= x_mod[i+1], y1= y[i+1], col=input$color)
    # curve(dnorm(x, mean, sd),xlim= c(Nmin, Nmax), xlab="Valores de Z", ylab="Frecuencia Relativa", main="Distribución Normal Estándard", add=T)
    
  } # Fin for i
  axis(2,  las=1)
  axis(1, c(valores_x), labels=valores_x_mod ,las=1)
  
#  for (jose in 1:length(limites)) segments(x0= limites[jose], y0= 0, x1= limites[jose], y1= dnorm(valores_z[jose]), col=input$variable,lwd=2)
  
  # # # curve(dnorm(x_mod, mean=input$mu, sd=la))
  
  
})




# Tabla de Valores z y probabilidad

sliderValues1 <- reactive({
  
  media <- input$mu
  varianza <- input$sigma_cuad
  desvio <- sqrt(varianza)
  unidad <- paste0("$$", input$unidades, "$$")
  unidad2 <- paste0("$$", input$unidades, "^{2}$$")
  decimales <- input$decimales
    
  nombre_columnas <- c("Media", "Varianza", "Desvío")
  nombre_columnas <- c("$$Media  (\\mu)$$", "$$Varianza  (\\sigma^{2})$$", "$$Desvío  (\\sigma)$$")
  nombre_columnas <- c("$$(\\mu)$$Media", "$$(\\sigma^{2})$$Varianza", "$$(\\sigma)$$Desvío")
  
  nombre_filas <- c("Valores", "Unidades")
  
  resumen <- matrix(NA, 2, length(nombre_columnas))
  colnames(resumen) <- nombre_columnas
  rownames(resumen) <- nombre_filas
  
  resumen[1, ] <- round(c(media, varianza, desvio), decimales)
  
  resumen[2, ] <- c(unidad, unidad2, unidad)
  
  

  
  
  resumen
  
  #######################################################
}) 

decimales_internos <- reactive({
  
if (!is.null(input$decimales)) {
  aver <- input$decimales
  aver 
}
}) 

observe(
output$values1 <- renderTable(align="c", 
                              digits= decimales_internos(),
                              rownames = T, {
  sliderValues1()
}))

# Fin Tabla
##################################################  



# Limite Inferior y Superior de Z y de la Variable Original

sliderValues2 <- reactive({
  
  zi <- qnorm(ALFA/2)
  zd <- qnorm(1-ALFA/2)
  zi <- round(zi, 2)
  zd <- round(zd, 2)
  
  la <- sqrt(input$sigma_cuad)
  
  limites_VO <- c(NA, NA)
  limites_VO[1] <- zi*(sqrt(input$sigma_cuad)/sqrt(N))+input$mu
  limites_VO[2] <- zd*(sqrt(input$sigma_cuad)/sqrt(N))+input$mu
  limites_VO <- round(limites_VO, 4)
  # Compose data frame
  data.frame(
    Izq. = c(paste("",zi, sep="")),
    Der. =c(paste("",zd, sep="")),
    row.names=c("Intervalo de Z"), 
    stringsAsFactors=FALSE)
  #######################################################
}) 




output$values2 <- renderTable({
  sliderValues2()
})
# Fin Tabla
##################################################  


# Limite Inferior y Superior de Z y de la Variable Original

sliderValues3 <- reactive({
  
  zi <- qnorm(ALFA/2)
  zd <- qnorm(1-ALFA/2)
  
  la <- sqrt(input$sigma_cuad)
  
  limites_VO <- c(NA, NA)
  limites_VO[1] <- zi*(sqrt(input$sigma_cuad)/sqrt(N))+input$mu
  limites_VO[2] <- zd*(sqrt(input$sigma_cuad)/sqrt(N))+input$mu
  limites_VO <- round(limites_VO, 4)
  # Compose data frame
  data.frame(
    Inferior = c(limites_VO[1]),
    Superior =c(limites_VO[2]),
    row.names=c("Int. de Confianza"), 
    stringsAsFactors=FALSE)
  #######################################################
}) 




output$values3 <- renderTable({
  sliderValues3()
})
# Fin Tabla
##################################################  









# Texto 1  - Valor Alfa
output$text1 <- renderText({ 
  
  # Si el valor izquierdo es menor que el derecho... CORRECTO
  paste("El valor alfa seleccionado es alfa=", ALFA, "." ,sep="")
  
})
# Fin Texto 1
########################

# Texto 2 - Valor de Mu
output$text2 <- renderText({ 
  
  # Si el valor izquierdo es menor que el derecho... CORRECTO
  paste("La media poblacional es mu=", input$mu, "." ,sep="")
  
})
# Fin Texto 2
########################


# Texto 3 -  Valor de Varianza Poblacional
output$text3 <- renderText({ 
  
  # Si el valor izquierdo es menor que el derecho... CORRECTO
  paste("El valor de Varianza Poblacional es Sigma_Cuad=", input$sigma_cuad, "." ,sep="")
  
})
# Fin Texto 3
###########################################


# Texto 4 -  Tamaño Muestral
output$text4 <- renderText({ 
  
  # Si el valor izquierdo es menor que el derecho... CORRECTO
  paste("El tamaño muestral es n=", N,"." ,sep="")
  
})
# Fin Texto 4
#############################


# Texto 5 -  Intervalo de confianza de la Media Poblacional
output$text5 <- renderText({ 
  # Si el valor izquierdo es menor que el derecho... CORRECTO
  zi <- qnorm(ALFA/2)
  zd <- qnorm(1-ALFA/2)
  
  la <- sqrt(input$sigma_cuad)
  
  limites <- c(NA, NA)
  limites[1] <- zi*(sqrt(input$sigma_cuad)/sqrt(N))+input$mu
  limites[2] <- zd*(sqrt(input$sigma_cuad)/sqrt(N))+input$mu
  limites <- round(limites, 4)
  
  paste("El intervalo de confianza para la media poblacional está comprendido entre ", limites[1], " y ", limites[2],  "." ,sep="")
  
  
  
})
# Fin Texto 5
############################################################


# Texto 6 - Intervalos de los valores Z
output$text6 <- renderText({ 
  
  zi <- qnorm(ALFA/2)
  zi <- round(zi, 4)
  zd <- qnorm(1-ALFA/2)
  zd <- round(zd, 4)
  
  paste ("Los valores Z correspondientes son ", zi, " y ", zd, ".", sep="")
})
# Fin Texto 6
##############
#   



# Texto 7 - Intervalos de los valores Z
output$text7 <- renderText({ 
  
  
  paste("Los valores Z van de - a + infinito. El infinito en si mismo no es un número, sino una tendencia.
        La integral de la distribución normal estándard de menos a más infinito es igual a 1.
        Ahora bien, no existen valores infinitos de diámetro a la altura del pecho del paraíso, 
        ni puede ser infitino el diámetro craneal... No hay magnitudes biológicas infinitas, 
        sino que las variables tienen un rango en el que es posible encontrarlas.
        Hay que tener en cuenta que los valores de la variable biológica pueden o no ser negativos 
        (por ejemplo diámetro a la altura del pecho no puede ser un valor negativo jamás), 
        pero los valores estandarizados si pueden serlo.
        Los valores estandarizados muestran la posición relativa de un valor respecto a la media (por ejemplo si un valor
        z es negativo, indica que el valor de la variable original asociado es menor que la media).
        Ese rango de la variable biológica lleva aparejado un rango en la variable estandarizada Z,
        entonces podemos ir desde los valores biológicos a los valores Z (ESTANDARIZACION)
        o podemos ir desde los valores Z a los valores biológicos (DESDESTANDARIZACION).
        Al determinar un rango en la variable Z, la integral entre dichos valores es menor a 1.
        La probabilidad que queda fuera del rango es el valor de alfa (Error de Tipo 1) y el valor de
        probabilidad que queda dentro del rango de valores z es 1-alfa (Confianza).
        Un par de valores de la variable original definidos como límites superiores e inferiores se corresponden con
        un único par de valores z, que determinan a su vez un valor de Confianza, y entonces un valor de alfa
        (Proceso de Estandarización).
        Un valor de alfa implica un único valor de Confianza, que se asocia a un único par de valores z y estos se corresponden
        con un único par de valores de la variable (Proceso de Desestandarización).")
  
})
# Fin Texto 7
##############
#   




# Texto 8 - Intervalos de los valores Z
output$text8 <- renderText({ 
  
  
  paste ("Si una variable posee distribución Normal dicha distribución es NO STANDARD. 
         Esto implica que esa distribución no cumple con los 3 axiomas de probabilidad.
         El valor de probabilidad de un rango de la variable NO es obtenido en la distribución de la variable sino
         de la distribución que surge de estandarizar los valores de la variable original a valores de una nueva variable: valores Z.
         La distribución de los valores Z es la Distribución Normal Estándard. Esta distribución si cumple con los axiomas de probabilidad.
         A cada valor de la variable le corresponde uno y solo un valor Z. Esto implica que hay una correspondencia entre la variable original y la variable Z. 
         El valor de probabilidad de un rango de valores de la variable original con ***Distribución Normal No Estándard***
         se asocia a la probabilidad obtenida de los valores Z correspondientes en la ***Distribución Normal ESTANDARD***.")
  
})
# Fin Texto 8
##############
#   


# Texto 9 - Conceptos Claves
output$text9 <- renderText({ 
  
  
  paste ("Distribución Normal ***NO ESTANDARD***, Distribución Normal Estándard, Valores Z, Probabilidad en funciones continuas, mu, sigma")
  
})
# Fin Texto 9
##############
#   


# Texto Resol - Conceptos Claves
output$Resol <- renderText({ 
  
  zi <- qnorm(ALFA/2)
  zi <- round(zi, 4)
  zd <- qnorm(1-ALFA/2)
  zd <- round(zd, 4)
  
  limites <- c(NA, NA)
  limites[1] <- zi*(sqrt(input$sigma_cuad)/sqrt(N))+input$mu
  limites[2] <- zd*(sqrt(input$sigma_cuad)/sqrt(N))+input$mu
  limites <- round(limites, 4)
  
  
  paste ("A partir del valor alfa=",ALFA," determinamos el par de valores z.
         El valor zi es el valor de z que acumula una probabilidad de alfa/2=", round(ALFA/2, 4), ".", 
         "Para encontrar el valor de zi buscamos el valor de probabilidad ", round(ALFA/2, 4), " en el cuerpo de la tabla
         y tomamos nota de los valores de los márgenes para determinar el valor z, que en este caso es",
         zi, ". El valor zd es el valor de z que acumula un aprobabilidad de 1-alfa/2=",round((1-ALFA/2), 4),
         " que implica el valor zd=", zd, ". Por ser los valores zi y zd equidistantes a valor z=0, es que zd=-zi.",
         " Ahora, a partir de la fórmula de estandarización despejamos los valores de la variable original que
         se asocian a los valores z obtenidos. Estos valores son el límite inferior (LI) y superior (LS) de la variable original.")
  
})
# Fin Texto Resol
##################
#   


# Texto Mu - Conceptos Claves
output$mu <-  renderText({ 
  
  
  paste (input$mu)
  
})
# Fin Texto 9
##############


# Texto Sigma Cuad - Conceptos Claves
output$sigma_cuad <-  renderText({ 
  
  
  paste (input$sigma_cuad)
  
})
# Fin Texto 9
##############


# Texto Sigma Cuad - Conceptos Claves
output$Libro<-  renderText({ 
  
  paste("En el libro de Bioestadística 1, en el capítulo 7 llamado \"Distribuciones en el Muestreo\" 
        página 94 en adelante se desarrolla el tema del intervalo de confianza para 
        la media poblacional con varianza poblacional conocida.", sep="")
  
  
})
# Fin Libro
##############



# Texto 10
output$ex6 <- renderUI({
  #  if (input$decimales1 < input$decimales2) {
  # invalidateLater(5000, session)
  
  zi <- qnorm(ALFA/2)
  zi <- round(zi, 4)
  zd <- qnorm(1-ALFA/2)
  zd <- round(zd, 4)
  
  
  limites <- c(NA, NA)
  limites[1] <- zi*(sqrt(input$sigma_cuad)/sqrt(N))+input$mu
  limites[2] <- zd*(sqrt(input$sigma_cuad)/sqrt(N))+input$mu
  limites <- round(limites, 4)
  
  withMathJax(sprintf(" $$LI = \\mu + zi*\\frac{\\sigma}{\\sqrt{n}} = %.02f + (%.02f)*\\frac{%.02f}{\\sqrt{%.00f}} = %.02f$$", input$mu,zi,input$sigma_cuad,N, limites[1]))
  # %.04f - %.04f = %.04f$$", 1,2, 3, 4, 5
  #  LI= zi*(sigma/sqrt(n))+mu
  
  #   } # Fin if
  #  else paste("", sep="")
})
# Fin Texto 10
########################



# Texto 11
output$ex7 <- renderUI({
  #  if (input$decimales1 < input$decimales2) {
  # invalidateLater(5000, session)
  
  
  zi <- qnorm(ALFA/2)
  zi <- round(zi, 4)
  zd <- qnorm(1-ALFA/2)
  zd <- round(zd, 4)
  
  
  limites <- c(NA, NA)
  limites[1] <- zi*(sqrt(input$sigma_cuad)/sqrt(N))+input$mu
  limites[2] <- zd*(sqrt(input$sigma_cuad)/sqrt(N))+input$mu
  limites <- round(limites, 4)
  
  withMathJax(sprintf(" $$LS = \\mu + zd*\\frac{\\sigma}{\\sqrt{n}} = %.02f + %.02f *\\frac{%.02f}{\\sqrt{%.00f}} = %.02f$$", input$mu,zd,input$sigma_cuad,N, limites[2]))
  # %.04f - %.04f = %.04f$$", 1,2, 3, 4, 5
  #  LI= zi*(sigma/sqrt(n))+mu
  
  #   } # Fin if
  #  else paste("", sep="")
})
# Fin Texto 10
########################