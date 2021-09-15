



# # # # #
# 2C - 12 - Regresion Lineal Simple

output$MODorden_12 <- renderUI({
  
  selectInput(inputId = "orden_12", 
              label="Eje X", 
              choices = colnames(BASE_goku())
              )
  
})


orden_12 <- reactive({
  
  vector_12 <- c(2,1)
  if (!is.null(input$orden_12) && (!is.null(BASE_goku()) && ncol(BASE_goku()) == 2)){
    
   if (colnames(BASE_goku())[1] == input$orden_12) vector_12 <- c(1,2) else vector_12 <- c(2,1)
  }
  vector_12
  
})



# Test de Regresion Lineal Simple
Test12_2C_RLS <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2 && sum(tipo_var_goku() == "Numérica") == 2) {
    if(!is.null(decimales_goku())) {
      if(!is.null(input$alfa_ho)) {
              
              
              DATOS <- BASE_goku()[,orden_12()]
              DECIMALES <- decimales_goku()
              ALFA <- input$alfa_ho
              
              
              
              ANALISIS <-  RLS_MASTER( input_datos = DATOS,
                                       input_decimales = DECIMALES,
                                       input_alfa = ALFA)
              
              
              ANALISIS
              
   
      } else return(NULL)
    } else return(NULL)
  } else return(NULL)
  
})
#######################################################



# Tabla 1 de Regresion
observe( output$tabla01_ho_2c_12 <- renderTable(rownames = T, digits=decimales_goku(), {
  
  if (!is.null(Test12_2C_RLS())){
    Test12_2C_RLS()$SALIDA_ARMADA$RESUMEN
  } else return(NULL)
}))



# Tabla 2 de Regresion
observe( output$tabla02_ho_2c_12 <- renderTable(rownames = T, digits=decimales_goku(), {
  
  if (!is.null(Test12_2C_RLS())){
    Test12_2C_RLS()$SALIDA_ARMADA$TABLA_REGRESION
  } else return(NULL)
}))


# Frase 1 RLS R2A
output$frase01_ho_2c_12 <- renderUI({
  if (!is.null(Test12_2C_RLS())){
    HTML(Test12_2C_RLS()$SALIDA_ARMADA$FRASE_R2A)
  } else return(NULL)
})

# Frase 1 RLS Pendiente
output$frase02_ho_2c_12 <- renderUI({
  if (!is.null(Test12_2C_RLS())){
    HTML(Test12_2C_RLS()$SALIDA_ARMADA$FRASE_PENDIENTE)
  } else return(NULL)
})


# Frase 1 RLS Pendiente
output$frase03_ho_2c_12 <- renderUI({
  if (!is.null(Test12_2C_RLS())){
    HTML(Test12_2C_RLS()$SALIDA_ARMADA$FRASE_ORDENADA)
  } else return(NULL)
})



# Grafico XY
output$plot_ho_2c_12 <- renderPlot({ 
  
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2 && sum(tipo_var_goku() == "Numérica") == 2) {
    if(!is.null(decimales_goku())) {
      if(!is.null(input$alfa_ho)) {
              
              DATOS <- BASE_goku()[,orden_12()]
              
              
              
              X <- DATOS[,1]
              Y <- DATOS[,2]
              
              pendiente <- Test12_2C_RLS()$ANALISIS$CONSULTORA$pendiente
              ordenada <-  Test12_2C_RLS()$ANALISIS$CONSULTORA$ordenada
              
              plot(X, Y,
                   #main=input$columna_graf_torta,
                   col= "red",
                   xlab= colnames(DATOS)[1],   
                   ylab= colnames(DATOS)[2]
              )
              abline(ordenada, pendiente)
              
      } else return(NULL)          
    } else return(NULL)
  } else return(NULL)
})




# Pestania 1
output$p1_ho_2c_12 <- renderUI({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2) {
    
    
    div(
    # uiOutput("MODorden_12"),
      h3("Test de Regresión Lineal Simple"),
      h3("Tabla de Regresion"),
      tableOutput("tabla02_ho_2c_12"), br(),
      
      h3("Gráfico XY"), br(),
      plotOutput("plot_ho_2c_12", width = "400px", height = "400px"), br(),
      
      h3("Resumen de Estadísticos de Regresión Lineal Simple"),
      tableOutput("tabla01_ho_2c_12"), br(),
      
      
      h3("R2 Ajustado"),
      uiOutput("frase01_ho_2c_12"),
      h3("Pendiente"),
      uiOutput("frase02_ho_2c_12"),
      h3("Ordenada"),
      uiOutput("frase03_ho_2c_12")
      
    )
    
  } else return(NULL)
  
})


# Pestania 2: Requisitos

# Grafico de Residuos vs. Predichos
output$graf_res_pred <- renderPlot({
  
  residuos <- Test12_2C_RLS()$ANALISIS$ORIGINAL$residual
  predichos <- Test12_2C_RLS()$ANALISIS$ORIGINAL$fitted.values
  
  plot(predichos, residuos, col="red")
  
})

#####################################################

# Normalidad Errores
Normalidad_Errores <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2 && sum(tipo_var_goku() == "Numérica") == 2) {
    if(!is.null(decimales_goku())) {
      if(!is.null(input$alfa_ho)) {
          
          
       residuos <- Test12_2C_RLS()$ANALISIS$ORIGINAL$residual
       dim(residuos) <- c(length(residuos), 1)
       colnames(residuos) <- "Residuos"
       
         
          SHAPIRO_MASTER( input_datos = residuos, 
                          input_decimales = decimales_goku(),
                          input_alfa = input$alfa_ho)
          
          
          

      } else return(NULL)
    } else return(NULL)
  } else return(NULL)
  
  
  
  
})




# Salida de valores resumen 
observe( output$tabla_ne <- renderTable(rownames = FALSE, digits=decimales_goku(), {
  #  if(!is.null(Test02_1C_Normalidad())) {
  Normalidad_Errores()$SALIDA_ARMADA$RESUMEN
  # } else return(NULL)
}))

# Frase 1
observe(output$frase01_ne<- renderUI({
  if(!is.null(Normalidad_Errores())) {
    HTML(Normalidad_Errores()$SALIDA_ARMADA$FRASE1)
  } else return(NULL)
}))

# Frase 2
observe(output$frase02_ne <- renderUI({
  if(!is.null(Normalidad_Errores())) {
    HTML(Normalidad_Errores()$SALIDA_ARMADA$FRASE2)
  } else return(NULL)
}))



########################################################


# Armado Parte 2
output$p2_ho_2c_12 <- renderUI({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2) {
    
    
    div(
      "El test de Regresión Lineal Simple tiene requisitos de:",br(),
      "1) Normalidad de los Errores (Prueba formal: Test de Normalidad de Shapiro-Wilk)", br(),
      "2) Homogeneidad de Varianzas de los Errores (Prueba informal: gráfico de residuos vs. predichos)", br(),
      "Ambos requisitos deben cumplirse. Caso contrario las conclusiones del Análisis RLS son inválidas, indisticamente de los resultados obtenidos.",
      
      h3("1) Normalidad de los Errores"),
      tableOutput("tabla_ne"),
      uiOutput("frase01_ne"),
      uiOutput("frase02_ne"),
      br(),
      h3("2) Homogeneidad de Varianzas de los Errores"),
      plotOutput("graf_res_pred", width = "400px", height = "400px"), br(), br()
      
      
      
    )
    
  } else return(NULL)
  
})


# ARmado general
Opc_ho_2c_12 <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2) {
    
    div(
      uiOutput("MODorden_12"),
    tabsetPanel(
      tabPanel("Requisitos RLS", uiOutput("p2_ho_2c_12")),
      tabPanel("Análisis RLS", uiOutput("p1_ho_2c_12"))
    ),
    br(), br(), br(), br()
    )
   
     
  } else return(NULL)
  
})





