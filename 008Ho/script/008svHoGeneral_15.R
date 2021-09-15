





# # # # #
# 2C - 15 - Test Wilcoxon apareado





# Test W apareado
Test15_2C_testWapareado <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2 && sum(tipo_var_goku() == "Numérica") == 2) {
    if(!is.null(decimales_goku())) {
      if(!is.null(input$alfa_ho)) {
        
        
        
        DATOS <- BASE_goku()
        DECIMALES <- decimales_goku()
        ALFA <- input$alfa_ho
        
        
        
        ANALISIS <-   suppressWarnings(
                      WT02_MASTER( input_datos = DATOS,
                                   input_decimales = DECIMALES,
                                   input_alfa = ALFA)
                      )
        
        
        
        ANALISIS
        
      } else return(NULL)
    } else return(NULL)
  } else return(NULL)
  
})





# Salida tabla requisitos test t apareado
observe( output$tabla01_ho_2c_15 <- renderTable(rownames = FALSE, digits=decimales_goku(), {
  if(!is.null(Test14_2C_testTapareado())) {
   Test15_2C_testWapareado()$SALIDA_ARMADA$RESUMEN_NH
  } else return(NULL)
}))


# Salida tabla resumen test t apareado
observe( output$tabla02_ho_2c_15 <- renderTable(rownames = FALSE, digits=decimales_goku(), {
  if(!is.null(Test14_2C_testTapareado())) {
   Test15_2C_testWapareado()$SALIDA_ARMADA$RESUMEN_TT
  } else return(NULL)
}))

# Frase1, requisitos
observe(output$frase01_ho_2c_15 <- renderUI({
  if(!is.null(Test14_2C_testTapareado())) {
    HTML(Test14_2C_testTapareado()$SALIDA_ARMADA$FRASE_NH)
  } else return(NULL)
}))


# Frase2, test t apareado
observe(output$frase02_ho_2c_15 <- renderUI({
  if(!is.null(Test14_2C_testTapareado())) {
    HTML(Test14_2C_testTapareado()$SALIDA_ARMADA$FRASE_TT)
  } else return(NULL)
}))


output$plot_ho_2c_15 <- renderPlot({ 
  
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2 && sum(tipo_var_goku() == "Numérica") == 2) {
    if(!is.null(decimales_goku())) {
      if(!is.null(input$alfa_ho)) {
        
        DATOS <- BASE_goku()
        
        
        
        X1 <- DATOS[,1]
        X2 <- DATOS[,2]
        DIF <- X2 - X1
        DIF <- as.vector(DIF)
        
        REF <- rep(1,length(DIF))
        
        detalle <- paste0("Diferencia entre ",colnames(DATOS)[1], " y ", colnames(DATOS)[2])
        
        plot(REF, DIF,
             #main=input$columna_graf_torta,
             col= "red",
             xlab= "",
             ylab= detalle,
             axes = F
        )
        axis(2)
        # plot(BASE_goku())
        
        
        
      } else return(NULL)
    } else return(NULL)
  } else return(NULL)
})





Opc_ho_2c_15 <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2) {
    
    
    div(
      h3("Test W apareado"),
      fluidRow(
        # h3("Resumen Requisitos del Test W Apareado"),
        # tableOutput("tabla01_ho_2c_15"),
        # uiOutput("frase01_ho_2c_15"),
        h3("Resumen Test W Apareado"),
        tableOutput("tabla02_ho_2c_15"),
        uiOutput("frase02_ho_2c_15"), br(),
        
        h3("Gráfico"),
        plotOutput("plot_ho_2c_15", width = "400px", height = "400px"),
        br()       
        
      )
      
    )
    
  } else return(NULL)
  
})




