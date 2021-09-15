

# # # # #
# 2Q - 07 - Test Chi Cuadrado




# Chi Cuadrado para 2 variables Categoricas
Test07_2Q_ChiCuadrado <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2 && sum(tipo_var_goku() == "Categórica") == 2) {
      if(!is.null(decimales_goku())) {
          if(!is.null(input$alfa_ho)) {
            
            CHI_MASTER(input_datos = BASE_goku(), 
                       input_decimales = decimales_goku(), 
                       input_alfa = input$alfa_ho,
                       input_sep = "R")
            
       
          } else return(NULL)
    } else return(NULL)
  } else return(NULL)
  
  
  
  
})
#######################################################



# Salida de valores resumen estadistico Chi Cuadrado
observe( output$tabla_ho_2q_07 <- renderTable(rownames = FALSE, digits=decimales_goku(), {
  
  if (!is.null(Test07_2Q_ChiCuadrado())){
    Test07_2Q_ChiCuadrado()$ANALISIS$CONSULTORA$RESUMEN
  } else return(NULL)
}))





# Frase1 Chi Cuadrado
output$frase01_ho_2q_07 <- renderUI({
  if(!is.null(Test07_2Q_ChiCuadrado())) {
    HTML( Test07_2Q_ChiCuadrado()$ANALISIS$CONSULTORA$FRASE1)
  } else return(NULL)
})



# Frase2 Chi Cuadrado
output$frase02_ho_2q_07 <- renderUI({
  if(!is.null(Test07_2Q_ChiCuadrado())) {
    HTML( Test07_2Q_ChiCuadrado()$ANALISIS$CONSULTORA$FRASE2)
  } else return(NULL)
})



observe(
  output$tabla_obs <- renderTable(rownames = T, digits=0,{
  if(!is.null(Test07_2Q_ChiCuadrado())) {
 Test07_2Q_ChiCuadrado()$ANALISIS$ORIGINAL$Test_Chi$observed
  #  TABLA[1,] <- as.character(TABLA[1,])
  } else return(NULL)
})
)


observe( output$tabla_esp <- renderTable(rownames = T, digits=decimales_goku(),{
  if(!is.null(Test07_2Q_ChiCuadrado())) {
    Test07_2Q_ChiCuadrado()$ANALISIS$ORIGINAL$Test_Chi$expected
  } else return(NULL)
})
)

  


observe(
  output$tabla_residuos <- renderTable(rownames = T, digits=decimales_goku(),{
  if(!is.null(Test07_2Q_ChiCuadrado())) {
    Test07_2Q_ChiCuadrado()$ANALISIS$ORIGINAL$Test_Chi$residuals
  } else return(NULL)
})
)


observe(
  output$tabla_restandard <- renderTable(rownames = T, digits=decimales_goku(),{
  if(!is.null(Test07_2Q_ChiCuadrado())) {
    Test07_2Q_ChiCuadrado()$ANALISIS$ORIGINAL$Test_Chi$stdres
  } else return(NULL)
})
)
  
Rejunte_Tablas <- reactive({
  
  if(!is.null(Test07_2Q_ChiCuadrado())) {
  
    div(
    tabsetPanel(
      tabPanel("Observados", 
               ref2q_goku()[1], br(),
               ref2q_goku()[2], br(),
               tableOutput("tabla_obs")),
      tabPanel("Esperados",
               ref2q_goku()[1], br(),
               ref2q_goku()[2], br(),
               tableOutput("tabla_esp")),
      tabPanel("Residuos", 
               ref2q_goku()[1], br(),
               ref2q_goku()[2], br(),
               tableOutput("tabla_residuos")),
      tabPanel("Residuos Estandarizados", 
               ref2q_goku()[1], br(),
               ref2q_goku()[2], br(),
               tableOutput("tabla_restandard"))
    )
    )
      
  } else return(NULL)  
})



Opc_ho_2q_07 <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2) {
    
    
    div(
      h3("Test Chi Cuadrado"),
      tableOutput("tabla_ho_2q_07"),
      uiOutput("frase01_ho_2q_07"),
      uiOutput("frase02_ho_2q_07"),
      br(),
      Rejunte_Tablas(), br()
    )
  } else return(NULL)
})

