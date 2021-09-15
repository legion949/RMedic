


# # # # #
# QC - 17 - Test t




# Test de Diferencia de Proporciones
Test17_QC_testT  <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2) {
    if(!is.null(decimales_goku())) {
      if(!is.null(input$alfa_ho)) {
              
              
              
              DATOS <- BASE_goku()
              
              
              DATOS[,2] <- as.factor(as.character(DATOS[,2]))
              DECIMALES <- decimales_goku()
              ALFA <- input$alfa_ho
              
              
              
              ANALISIS <-  TT01_MASTER( input_datos = DATOS,
                                        input_decimales = DECIMALES,
                                        input_alfa = ALFA)
              
              
              
              ANALISIS
              
       
      } else return(NULL)
    } else return(NULL)
  } else return(NULL)
  
})
#######################################################





# Salida de valores resumen test t
observe( output$tabla01_ho_qc_17 <- renderTable(rownames = FALSE, digits=decimales_goku(), {
  if(!is.null(Test17_QC_testT())) {
    Test17_QC_testT()$SALIDA_ARMADA$RESUMEN_TT
  } else return(NULL)
}))

# Salida de requisitos resumen test t
observe( output$tabla02_ho_qc_17 <- renderTable(rownames = FALSE, digits=decimales_goku(), {
  if(!is.null(Test17_QC_testT())) {
    Test17_QC_testT()$SALIDA_ARMADA$RESUMEN_NH
  } else return(NULL)
}))


observe(output$frase01_ho_qc_17 <- renderUI({
  if(!is.null(Test17_QC_testT())) {
    HTML(Test17_QC_testT()$SALIDA_ARMADA$FRASE_TT)
  } else return(NULL)
}))


observe(output$frase02_ho_qc_17 <- renderUI({
  if(!is.null(Test17_QC_testT())) {
    HTML(Test17_QC_testT()$SALIDA_ARMADA$FRASE_NH)
  } else return(NULL)
}))



# Grafico Boxplot para test t
output$plot_ho_qc_17 <- renderPlot({ 
  
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2 ) {
    if(!is.null(decimales_goku())) {
      if(!is.null(input$alfa_ho)) {
        
        MINI <- BASE_goku()
        
        
        
        VR <- MINI[,1]
        
        FACTOR <- as.factor(MINI[,2])
        
        
        boxplot(VR ~ FACTOR, 
                #main=input$columna_graf_torta,
                col= "red",
                xlab= "Grupos",   
                ylab= "Variable")
        
        
        
        
      } else return(NULL)
    } else return(NULL)
  } else return(NULL)
})




Opc_ho_qc_17 <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2) {
    
    
    div(
      h3("Test t (dos muestras independientes)"),
               h3("Resumen de Requisitos Estadísticos"),
               tableOutput("tabla02_ho_qc_17"),
               uiOutput("frase02_ho_qc_17"),
               h3("Resumen del Test t"),
               tableOutput("tabla01_ho_qc_17"),
               uiOutput("frase01_ho_qc_17"), br(),
      
      h3("Gráfico Boxplot"),
      plotOutput("plot_ho_qc_17", width = "400px", height = "400px")
      
        )
  
  } else return(NULL)
  
})





