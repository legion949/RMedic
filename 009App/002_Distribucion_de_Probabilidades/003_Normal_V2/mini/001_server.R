# Expression that generates a histogram. The expression is
# wrapped in a call to renderPlot to indicate that:
#
#  1) It is "reactive" and therefore should be automatically
#     re-executed when inputs change
#  2) Its output type is a plot


output$gmenu3 <- renderUI({
  
  if (!is.null(input$mu)) if (!is.null(input$forma)) {
    conditionalPanel("1 == 1",
                     conditionalPanel('input.forma == "menor"',
                                      numericInput("var1", "Valor de la Variable Original:", min=-1e20,  max=1e20, step= 0.01, value= 90)),
                     
                     conditionalPanel('input.forma == "mayor"',
                                      numericInput("var2", "Valor de la Variable Original:", min=-1e20,  max=1e20, step= 0.01, value= 110)),
                     
                     conditionalPanel('input.forma == "entre"',
                                      numericInput("var3", "Valor Izquierdo de la Variable Original:", min=-1e20,  max=1e20, step= 0.01, value= 90),
                                      numericInput("var4", "Valor Derecho de la Variable Original:", min=-1e20,  max=1e20, step= 0.01, value= 110))  
    )
  } else return(NULL)
})

ALFA <- 0.05
# N <- 10

# Marco para todos los graficos...
valores_z_extremos <- c(-4,4)
cantidad <- 400
cantidad_desvio <- max(valores_z_extremos)
####################################



# Gráfico 1
output$distPlot1 <- renderPlot({
  
  if (!is.null(input$sigma_cuad)) {
    if (!is.null(input$mu)){
      if (!is.null(input$forma)){

  # Detalles varios
  varianza <- input$sigma_cuad
  desvio <- sqrt(varianza)
  media <- input$mu

  
  # El gráfico tendra hasta 5 desvios a la derecha
  # y 5 desvios a la izquierda de la media
  valores_extremos <- c(media-cantidad_desvio*desvio, media+cantidad_desvio*desvio)
  
  
  
  # Genero los valores x e y de los puntos para graficar una distribucion normal standard
  x <- seq(valores_z_extremos[1], valores_z_extremos[2], length=cantidad)
  y <- dnorm(x,mean=0, sd=1)
  
  
  # Los datos estan tomados de una distribución normal stándard...
  # Los valores de x... tiene media 0
  # yo quiero que tengan la media que 
  # Ahora los desestandarizo
  # Los valosres de "X" son valores estandarizados..:
  # Lo que hago ahora... es desestandarizarlos...
  x_mod <- x*(desvio)+media
  
  # Determino los valores que quiero que salgan en el eje X de la normal estandard
  x_standard <- valores_z_extremos[1]:valores_z_extremos[2]

  # Determino los valores que van a salir de la variable original
  x_real <- x_standard*desvio + media
  x_real <- round(x_real, 2)
  
  # Generamos la grafica de la distribucion normal estandard
  curve(dnorm(x, 0, 1),xlim= valores_z_extremos, xlab="Variable Original", ylab="Frecuencia Relativa",
        main="Distribución Normal", ylim=c(0,0.5), axes= F)
  axis(2,  las=1)
  
  # Cambio el detalle del eje x...
  # En la posiciones de los valores z, Coloco valores de la variable real
  axis(1, c(x_standard), labels=x_real ,las=1)
  
  zi <- -10
  zd <- 10
  
  li <-  zi
  ld <-  zd
  
 
  
  candado_interno <- T
  if (input$forma == "menor") {
    if (!is.null(input$var1)) {
      
      ld <- (input$var1-media)/desvio
      candado_interno <- F
    }
  } 
  
  if (input$forma == "mayor") {
    if (!is.null(input$var2)) {
      
    li <- (input$var2-media)/desvio
    candado_interno <- F
    }
  }
  
  if (input$forma == "entre") {
    if (!is.null(input$var3)) { if (!is.null(input$var4)) {
    li <- (input$var3-media)/desvio
    ld <- (input$var4-media)/desvio
    
    candado_interno <- F
    }
    }
  }
  
if (candado_interno == FALSE) {
  cat(li, "\n", ld, "\n")
  x_pintada <- seq(li, ld, by= 0.001)
#  dt_x <- sum(as.numeric(x_pintada >= valores_z_extremos[1]) + as.numeric(x_pintada <= valores_z_extremos[2])) == 2 
#  x_pintada <- x_pintada[dt_x]
  y_pintada <- dnorm(x_pintada)
  
  segments(x0= x_pintada, y0= 0, x1= x_pintada, y1= y_pintada, col= input$variable)
  
  
   # Volvemos a pintar la gráfica de la normal... porque se le pinto en el borde el naranja.
   # ACLARACION: podria primero colorear y despues hacer la grafica... pero por las dudas
   # se muestre el paso a paso, prefiero que primero muestra una grafica y luego como se colorea.
  
    curve(dnorm(x, 0, 1),xlim= valores_z_extremos, xlab="Variable Original", ylab="Frecuencia Relativa",
        main="Distribución Normal", ylim=c(0,0.5), add= T)

    
}
      
      }}}  
  
})


output$distPlot2 <- renderPlot({
  
  if (!is.null(input$sigma_cuad)) {
    if (!is.null(input$mu)){
      if (!is.null(input$forma)){
        
        # Detalles varios
        varianza <- input$sigma_cuad
        desvio <- sqrt(varianza)
        media <- input$mu
        
        
        # El gráfico tendra hasta 5 desvios a la derecha
        # y 5 desvios a la izquierda de la media
        valores_extremos <- c(media-cantidad_desvio*desvio, media+cantidad_desvio*desvio)
        
        
        
        # Genero los valores x e y de los puntos para graficar una distribucion normal standard
        x <- seq(valores_z_extremos[1], valores_z_extremos[2], length=cantidad)
        y <- dnorm(x,mean=0, sd=1)
        
        
        # Los datos estan tomados de una distribución normal stándard...
        # Los valores de x... tiene media 0
        # yo quiero que tengan la media que 
        # Ahora los desestandarizo
        # Los valosres de "X" son valores estandarizados..:
        # Lo que hago ahora... es desestandarizarlos...
        x_mod <- x*(desvio)+media
        
        # Determino los valores que quiero que salgan en el eje X de la normal estandard
        x_standard <- valores_z_extremos[1]:valores_z_extremos[2]
        
        
        
        # Generamos la grafica de la distribucion normal
        curve(dnorm(x, 0, 1),xlim= valores_z_extremos, xlab="Valores de Z", ylab="Frecuencia Relativa",
              main="Distribución Normal Estándard", ylim=c(0,0.5), axes= F)
        axis(2,  las=1)
        axis(1, c(x_standard), labels=x_standard ,las=1)
        
        zi <- -10
        zd <- 10
        
        li <-  zi
        ld <-  zd
        
        
        
        candado_interno <- T
        if (input$forma == "menor") {
          if (!is.null(input$var1)) {
            
            ld <- (input$var1-media)/desvio
            candado_interno <- F
          }
        } 
        
        if (input$forma == "mayor") {
          if (!is.null(input$var2)) {
            
            li <- (input$var2-media)/desvio
            candado_interno <- F
          }
        }
        
        if (input$forma == "entre") {
          if (!is.null(input$var3)) { if (!is.null(input$var4)) {
            li <- (input$var3-media)/desvio
            ld <- (input$var4-media)/desvio
            
            candado_interno <- F
          }
          }
        }
        
        if (candado_interno == FALSE) {
          cat(li, "\n", ld, "\n")
          x_pintada <- seq(li, ld, by= 0.001)
    #      dt_x <- sum(as.numeric(x_pintada >= valores_z_extremos[1]) + as.numeric(x_pintada <= valores_z_extremos[2])) == 2 
    #      x_pintada <- x_pintada[dt_x]
          y_pintada <- dnorm(x_pintada)
          
          segments(x0= x_pintada, y0= 0, x1= x_pintada, y1= y_pintada, col= input$variable)
          
          
          # Volvemos a pintar la gráfica de la normal... porque se le pinto en el borde el naranja.
          # ACLARACION: podria primero colorear y despues hacer la grafica... pero por las dudas
          # se muestre el paso a paso, prefiero que primero muestra una grafica y luego como se colorea.
          
          curve(dnorm(x, 0, 1),xlim= valores_z_extremos, xlab="Valores de Z", ylab="Frecuencia Relativa",
                main="Distribución Normal Estándard", ylim=c(0,0.5),  add= T)
          
          
        }
        
      }}}  
  
})

# Tabla de Valores z y probabilidad

sliderValues1 <- reactive({
  
  aver <- c(input$mu, sqrt(input$sigma_cuad), input$sigma_cuad)
  aver <- round(aver, 2)
  
  dim(aver) <- c(1, length(aver))
  colnames(aver) <- c("Mu", "Desvío", "Varianza")
  
  
  aver
  
  #######################################################
}) 

output$values111 <- renderTable({
  sliderValues1()
})
# Fin Tabla
##################################################  


output$text <- renderText({
  
  
  if (!is.null(input$forma)) if (!is.null(input$sigma_cuad)) if (!is.null(input$mu)){
    
    
    # Detalles varios
    varianza <- input$sigma_cuad
    desvio <- sqrt(varianza)
    #  cantidad <- 400
    media <- input$mu
    
    
    li <- -10000000
    ld <- 10000000
    
    if (input$forma == "menor") ld <- input$var1
    
    if (input$forma == "mayor") li <- input$var2
    
    if (input$forma == "entre") {
      li <- input$var3
      ld <- input$var4
    }
    
    li <- as.numeric(as.character(li))
    ld <- as.numeric(as.character(ld))
    
    prob2 <- pnorm(ld, media, desvio)
    prob1 <- pnorm(li, media, desvio)
    prob <- prob2 - prob1
    prob <- round(prob, 2)
    
    if (input$forma == "menor") frase <- paste0("La probabilidad de pacientes con valores menores a ", input$var1, " es de <b>", prob, "</b>.")
    
    if (input$forma == "mayor") frase <- paste0("La probabilidad de pacientes con valores mayores a ", input$var2, " es de <b>", prob, "</b>.")
  
    if (input$forma == "entre") frase <- paste0("La probabilidad de pacientes con valores entre ", input$var3, " y ", input$var4, " es de <b>", prob, "</b>.")
    
    frase  
      
  } else return(NULL)
  
})



# Fin Texto 10
########################
