
# # # # #
# 2C - 10 - Correlacion de Pearson



# Test de Diferencia de Proporciones
Test10_2C_Pearson <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2 && sum(tipo_var_goku() == "Numérica") == 2) {
    if(!is.null(decimales_goku())) {
      if(!is.null(input$alfa_ho)) {
        
              
              
              DATOS <- BASE_goku()
              
              
              DECIMALES <- decimales_goku()
              ALFA <- input$alfa_ho
              
              
              
              ANALISIS <-  suppressWarnings(
                           COR_MASTER( input_datos = DATOS,
                                       input_decimales = DECIMALES,
                                       input_metodo="Pearson",
                                       input_alfa = ALFA)
                            )
              
              
              ANALISIS
              
      
      } else return(NULL)
    } else return(NULL)
  } else return(NULL)
  
})
#######################################################


# Tabla de Requisitos
observe( output$tabla01_ho_2c_10 <- renderTable(rownames = FALSE, digits=decimales_goku(), {
  
  if (!is.null(Test10_2C_Pearson())){
    Test10_2C_Pearson()$SALIDA_ARMADA$RESUMEN_NH
  } else return(NULL)
}))


# Tabla Resumen de Correlacion
observe( output$tabla02_ho_2c_10 <- renderTable(rownames = FALSE, digits=decimales_goku(), {
  
  if (!is.null(Test10_2C_Pearson())){
    Test10_2C_Pearson()$SALIDA_ARMADA$RESUMEN_COR
  } else return(NULL)
}))



# Frase de los requisitos
output$frase01_ho_2c_10 <- renderUI({
  if (!is.null(Test10_2C_Pearson())){
    HTML(Test10_2C_Pearson()$SALIDA_ARMADA$FRASE_NH)
  } else return(NULL)
})


# Frase del test de Correlacion
output$frase02_ho_2c_10 <- renderUI({
  if (!is.null(Test10_2C_Pearson())){
    HTML(Test10_2C_Pearson()$SALIDA_ARMADA$FRASE)
  } else return(NULL)
})




output$plot_ho_2c_10 <- renderPlot({ 
  
  
  if(paso_BASE(BASE_goku())) {
            
            DATOS <- BASE_goku()
            
            
            
            X <- DATOS[,1]
            Y <- DATOS[,2]
            
            plot(X, Y,
                 #main=input$columna_graf_torta,
                 col= "red",
                 xlab= colnames(DATOS)[1],   
                 ylab= colnames(DATOS)[2])
            
            
          } else return(NULL)
     
})





Opc_ho_2c_10 <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2) {
    
    
    div(
      h3("Resumen de Requisitos Estadísticos de Pearson"), br(),
      tableOutput("tabla01_ho_2c_10"), br(),
      uiOutput("frase01_ho_2c_10"), br(),
      
      h3("Test de Correlación de Pearson"), br(),
      tableOutput("tabla02_ho_2c_10"), br(),
      uiOutput("frase02_ho_2c_10"), br(),
      
      h3("Gráfico XY"), br(),
      plotOutput("plot_ho_2c_10", width = "400px", height = "400px"), br()
    
      
    )
    
  } else return(NULL)
  
})





