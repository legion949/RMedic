


# # # # #
# 2C - 09 - Homogeneidad de Varianzas



# Test de Homogeneidad de Varianzas
Test09_2C_Homogeneidad <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2 && sum(tipo_var_goku() == "Numérica") == 2) {
    if(!is.null(decimales_goku())) {
      if(!is.null(input$alfa_ho)) {
              
              
              
        DATOS <- BASE_goku() 
        
        
        DECIMALES <- decimales_goku()
        ALFA <- input$alfa_ho
        
        
        
        ANALISIS <-   BARTLETT_MASTER( input_datos = DATOS, 
                                       input_decimales = DECIMALES, 
                                       input_alfa = ALFA, 
                                       input_ingreso = "clasic")
        
        ANALISIS
              
  
      } else return(NULL)
    } else return(NULL)
  } else return(NULL)
  
})




# Salida de valores resumen Homogeneidad de Bartlett
observe( output$tabla_ho_2c_09 <- renderTable(rownames = F, digits=decimales_goku(), {
  
  if (!is.null(Test09_2C_Homogeneidad())){
    Test09_2C_Homogeneidad()$SALIDA_ARMADA[[2]]
  } else return(NULL)
}))


# Frase1 de Homogeneidad de Varianzas de Bartlett
output$frase01_ho_2c_09 <- renderUI({
  if (!is.null(Test09_2C_Homogeneidad())){
    HTML(Test09_2C_Homogeneidad()$SALIDA_ARMADA[[1]])
  } else return(NULL)
})

# Frase2 de Homogeneidad de Varianzas de Bartlett
output$frase02_ho_2c_09 <- renderUI({
  if (!is.null(Test09_2C_Homogeneidad())){
    HTML(Test09_2C_Homogeneidad()$SALIDA_ARMADA[[3]])
  } else return(NULL)
})

# Grafico Boxplot par Homogeneidad de Varianzas
output$plot_ho_2c_09 <- renderPlot({ 
  
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2 && sum(tipo_var_goku() == "Numérica") == 2) {
    if(!is.null(decimales_goku())) {
      if(!is.null(input$alfa_ho)) {
              
              MINI <- BASE_goku()
              
              
              X <- MINI[,1]
              Y <- MINI[,2]
              VR <- c(X, Y)
              
              
              COL_X <- colnames(MINI)[1]
              COL_Y <- colnames(MINI)[2]
              
              FACTOR <- c(rep(COL_X, nrow(MINI)), rep(COL_Y, nrow(MINI)))
              FACTOR <- as.factor(as.character(FACTOR))
              
              
              boxplot(VR ~ FACTOR, 
                      #main=input$columna_graf_torta,
                      col= "red",
                      xlab= "Grupos",   
                      ylab= "Variable")
              
              
              
              
          } else return(NULL)
        } else return(NULL)
      } else return(NULL)
})






Opc_ho_2c_09 <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2) {
    
    
    div(
      h3("Test de Homogeneidad de Bartlett"), br(),
      tableOutput("tabla_ho_2c_09"), br(),
      uiOutput("frase01_ho_2c_09"), br(),
      uiOutput("frase02_ho_2c_09"), br(),
      h3("Gráfico Boxplot"), br(),
      plotOutput("plot_ho_2c_09", width = "400px", height = "400px")

    )
      
    
  } else return(NULL)
  
})



