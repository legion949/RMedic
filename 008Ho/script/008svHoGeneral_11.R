
# # # # #
# 2C - 11 - Correlacion de Spearman



# Test de Spearman
Test11_2C_Spearman <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2 && sum(tipo_var_goku() == "Numérica") == 2) {
    if(!is.null(decimales_goku())) {
      if(!is.null(input$alfa_ho)) {
        
        
        
        DATOS <- BASE_goku()
        
        
        DECIMALES <- decimales_goku()
        ALFA <- input$alfa_ho
        
        
        
        ANALISIS <-   suppressWarnings(
                                 COR_MASTER( input_datos = DATOS,
                                 input_decimales = DECIMALES,
                                 input_metodo="Spearman",
                                 input_alfa = ALFA)
                      )
        
        ANALISIS
        
        
      } else return(NULL)
    } else return(NULL)
  } else return(NULL)
  
})
#######################################################


# Tabla de Requisitos
observe( output$tabla01_ho_2c_11 <- renderTable(rownames = FALSE, digits=decimales_goku(), {
  
  if (!is.null(Test11_2C_Spearman())){
    Test11_2C_Spearman()$SALIDA_ARMADA$RESUMEN_NH
  } else return(NULL)
}))


# Tabla Resumen de Correlacion
observe( output$tabla02_ho_2c_11 <- renderTable(rownames = FALSE, digits=decimales_goku(), {
  
  if (!is.null(Test11_2C_Spearman())){
    Test11_2C_Spearman()$SALIDA_ARMADA$RESUMEN_COR
  } else return(NULL)
}))



# Frase de los requisitos
output$frase01_ho_2c_11 <- renderUI({
  if (!is.null(Test11_2C_Spearman())){
    HTML(Test11_2C_Spearman()$SALIDA_ARMADA$FRASE_NH)
  } else return(NULL)
})


# Frase del test de Correlacion
output$frase02_ho_2c_11 <- renderUI({
  if (!is.null(Test11_2C_Spearman())){
    HTML(Test11_2C_Spearman()$SALIDA_ARMADA$FRASE)
  } else return(NULL)
})




output$plot_ho_2c_11 <- renderPlot({ 
  
  
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





Opc_ho_2c_11 <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2) {
    
    
    div(
      h3("Test de Correlación de Spearman"), br(),
      tableOutput("tabla02_ho_2c_11"), br(),
      uiOutput("frase02_ho_2c_11"), br(),
      
      h3("Gráfico XY"), br(),
      plotOutput("plot_ho_2c_11", width = "400px", height = "400px"), br()
    )
    
  } else return(NULL)
  
})





