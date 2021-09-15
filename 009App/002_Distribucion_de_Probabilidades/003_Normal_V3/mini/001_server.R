# Expression that generates a histogram. The expression is
# wrapped in a call to renderPlot to indicate that:
#
#  1) It is "reactive" and therefore should be automatically
#     re-executed when inputs change
#  2) Its output type is a plot


output$gmenu3 <- renderUI({
  
  if (!is.null(input$mu)) if (!is.null(input$forma)) {
    conditionalPanel("1 == 1",
                     conditionalPanel('input.opciones == "opc1"',
                     conditionalPanel('input.forma == "menor"',
                                      numericInput("var1", "Valor de la Variable Original:", min=-1e20,  max=1e20, step= 0.01, value= 90)),
                     
                     conditionalPanel('input.forma == "mayor"',
                                      numericInput("var2", "Valor de la Variable Original:", min=-1e20,  max=1e20, step= 0.01, value= 110)),
                     
                     conditionalPanel('input.forma == "entre"',
                                      numericInput("var3", "Valor Izquierdo de la Variable Original:", min=-1e20,  max=1e20, step= 0.01, value= 90),
                                      numericInput("var4", "Valor Derecho de la Variable Original:", min=-1e20,  max=1e20, step= 0.01, value= 110))),  
    
                     conditionalPanel('input.opciones == "opc2"',
                                      conditionalPanel('input.forma == "menor"',
                                                       numericInput("z_var1", "Valor Z:", min=-1e20,  max=1e20, step= 0.01, value= -1.96)),
                                      
                                      conditionalPanel('input.forma == "mayor"',
                                                       numericInput("z_var2", "Valor Z:", min=-1e20,  max=1e20, step= 0.01, value= 1)),
                     
                                      conditionalPanel('input.forma == "entre"',
                                                       numericInput("z_var3", "Valor Z Izquierdo :", min=-1e20,  max=1e20, step= 0.01, value= -1.96),
                                                       numericInput("z_var4", "Valor Z Derecho:", min=-1e20,  max=1e20, step= 0.01, value= 1))  
                     ),
                     conditionalPanel('input.opciones == "opc3"',
                                                       numericInput("prob_var1", "Probabilidad (Entre 0 y 1):", min=0,  max=1, step= 0.01, value= 0.25))
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
      if (!is.null(input$forma))  if (!is.null(input$opciones)){

  mis_valores <- sliderValues4()
        
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
  
 
  
  candado_interno <- F
  
  # opc 1... variable original
 
  if (input$forma == "menor") {
    if (!is.null(input$var1)) {
      
      ld <- mis_valores[1,2]
      candado_interno <- F
    }
  } 
  
  if (input$forma == "mayor") {
    if (!is.null(input$var2)) {
      
    li <- mis_valores[1,2]
    candado_interno <- F
    }
  }
  
  if (input$forma == "entre") {
    if (!is.null(input$var3)) { if (!is.null(input$var4)) {
    li <- mis_valores[1,3]
    ld <- mis_valores[1,4]
    
    candado_interno <- F
    }
    }
  }
 
  
  
if (candado_interno == FALSE) {
#  cat(li, "\n", ld, "\n")
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
      if (!is.null(input$forma)) if (!is.null(input$opciones)){
        
        # Detalles varios
        varianza <- input$sigma_cuad
        desvio <- sqrt(varianza)
        media <- input$mu
        
        mis_valores <- sliderValues4()
        
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
            
            ld <- mis_valores[1,2]
            candado_interno <- F
          }
        } 
        
        if (input$forma == "mayor") {
          if (!is.null(input$var2)) {
            
            li <- mis_valores[1,2]
            candado_interno <- F
          }
        }
        
        if (input$forma == "entre") {
          if (!is.null(input$var3)) { if (!is.null(input$var4)) {
            li <- mis_valores[1,3]
            ld <- mis_valores[1,4]
            
            candado_interno <- F
          }
          }
        }
     
        
           
        if (candado_interno == FALSE) {
 #         cat(li, "\n", ld, "\n")
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



sliderValues2 <- reactive({
  
  if (!is.null(input$mu)) if (!is.null(input$sigma_cuad)) if (!is.null(input$opciones)) if (!is.null(input$forma)) {
  
  media <- input$mu
  varianza <- input$sigma_cuad
  desvio <- sqrt(varianza)
  
  vi <- NA
  vd <- NA
  zi <- NA 
  zd <- NA
  prob <- NA
  porc <- NA
  
  
  if (input$opciones == "opc1") {
    if (input$forma == "menor") {
  vi <- input$var1
  zi <- (vi - media)/desvio
  prob <- pnorm(zi, 0, 1)
    }
    
    if (input$forma == "mayor") {
      vd <- input$var2
      zd <- (vd - media)/desvio
      prob <- pnorm(zd, 0, 1, lower.tail = FALSE)
    }
    
    if (input$forma == "entre") {

      
      vi <- input$var1
      vd <- input$var2
      zi <- (vi - media)/desvio 
      zd <- (vd - media)/desvio
      prob2 <- pnorm(zd, 0, 1)
      prob1 <- pnorm(zi, 0, 1)
      prob <- prob2 - prob1
    }
  } # Fin opc1
  
  if (input$opciones == "opc2") {
    if (input$forma == "menor") {
      zi <- input$z_var1
      vi <- zi*desvio+media
      prob <- pnorm(zi, 0, 1)
    }
    
    if (input$forma == "mayor") {
      zd <- input$z_var2
      vd <- zd*desvio+media
      prob <- pnorm(zd, 0, 1)
    }
    
    if (input$forma == "entre") {
      
      zi <- input$z_var3
      zd <- input$z_var4
      vi <- zi*desvio+media
      vd <- zd*desvio+media
      prob2 <- pnorm(zd, 0, 1)
      prob1 <- pnorm(zi, 0, 1)
      prob <- prob2 - prob1
    }
  } # Fin opc2
  
  if (input$opciones == "opc3") {
    if (input$forma == "menor") {
      prob <- input$prob_var1
      zi <- qnorm(prob)
      vi <- zi*desvio+media
    }
    
    if (input$forma == "mayor") {
      prob <- input$prob_var1
      zd <- qnorm(prob, lower.tail = FALSE)
      vd <- zd*desvio+media
    }
    
    if (input$forma == "entre") {
      
      prob <- input$prob_var1
      prob_mod <- 0.5 - (prob/2)
      zi <- qnorm(prob_mod)
      zd <- qnorm(prob_mod, lower.tail = FALSE)
      vi <- zi*desvio+media
      vd <- zd*desvio+media
    }
  } # Fin opc3
  
  prob <- round(prob, 2)
  porc <- prob*100
  
  valores <- c(media, desvio, varianza, vi, vd, zi, zd, prob, porc)
  valores <- round(valores, 2)
 # valores <- as.character(valores)
  dim(valores) <- c(1,length(valores))
  valores <- as.data.frame(valores)
#  colnames(valores) <- c("Media", "Desvio", "Varianza", "Vi", "Vd", "zi", "zd", "Probabilidad", "Porcentaje")
  valores
  
  
} })


sliderValues3 <- reactive({
  
  if (!is.null(input$mu)) if (!is.null(input$sigma_cuad)) if (!is.null(input$opciones)) if (!is.null(input$forma)) {
    
  tabla <- sliderValues2()[,c(1:3)]
  colnames(tabla) <- c("Media", "Desvío", "Varianza")
  tabla

  }
})

sliderValues4 <- reactive({
  
   if (!is.null(input$mu)) if (!is.null(input$sigma_cuad)) if (!is.null(input$opciones)) if (!is.null(input$forma)) {
    
  valores <- sliderValues2()
  valores <- t(valores)
  valores <- valores[(4:length(valores)),]
  valores <- na.omit(valores)
  valores <- t(valores)
  
  tabla <- valores
  
  # tabla <- as.data.frame(valores)
  # tabla <- t(tabla)
  # tabla <- na.omit(tabla)
  # tabla <- t(tabla)
  # tabla <- as.data.frame(tabla)
  
  if (1 == 1) {
  if (input$forma == "menor") {
    
    nombres <- c("Valor de la Variable", "Valor z", "P(Z<z)", "Porcentaje")
    
    
  }
  
  if (input$forma == "mayor") {
    nombres <- c("Valor de la Variable", "Valor z", "P(Z>z)", "Porcentaje")
    
  }
  
  if (input$forma == "entre") {
    
    nombres <- c("Valor Izquierdo", "Valor Derecho", "Zi", "Zd", "P(Z>z)", "Porcentaje")
    
  }
 
  if(!is.null(tabla)) if (ncol(tabla) == length(nombres))  colnames(tabla) <- nombres 
  }
 
  tabla  
  
  }
})



output$values111 <- renderTable({
  if (!is.null(input$mu)) if (!is.null(input$sigma_cuad)) if (!is.null(input$opciones)) if (!is.null(input$forma)) {
    
  aver <-  sliderValues4()
  aver
  }
})
# Fin Tabla
##################################################  



output$values222 <- renderTable({
  sliderValues3()
})
# Fin Tabla
##################################################  


output$text <- renderText({
  
  
  if (!is.null(input$forma)) if (!is.null(input$sigma_cuad)) if (!is.null(input$mu)) if (!is.null(input$opciones)){
    
    
    mis_valores <- sliderValues4()
    
    # Detalles varios
    varianza <- input$sigma_cuad
    desvio <- sqrt(varianza)
    #  cantidad <- 400
    media <- input$mu
    
    
    li <- -10000000
    ld <- 10000000
    
    if (input$forma == "menor") ld <- mis_valores[1,1]
    
    if (input$forma == "mayor") li <- mis_valores[1,1]
    
    if (input$forma == "entre") {
      li <- mis_valores[1,1]
      ld <- mis_valores[1,2]
    }
    
    li <- as.numeric(as.character(li))
    ld <- as.numeric(as.character(ld))
    
    prob <- mis_valores[1,length(mis_valores)-1]
    
    if (input$forma == "menor") frase <- paste0("La probabilidad de pacientes con valores menores a ", ld, " es de <b>", prob, "</b>.")
    
    if (input$forma == "mayor") frase <- paste0("La probabilidad de pacientes con valores mayores a ", li, " es de <b>", prob, "</b>.")
  
    if (input$forma == "entre") frase <- paste0("La probabilidad de pacientes con valores entre ", li, " y ", ld, " es de <b>", prob, "</b>.")
    
    frase  
      
  } else return(NULL)
  
})



# Fin Texto 10
########################
