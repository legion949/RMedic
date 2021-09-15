

# # # # #
# 1C - 04 - Test de Wilcoxon (una muestra)

output$ho_1c_04 <- renderUI({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 1) {
    
    div(
      numericInput("mu_ho_1c_04", "Valor de Mediana Poblacional bajo Hipótesis:", min=-1e20,  max=1e20, step= 1,  value=0), br(),
      radioButtons("prueba_ho_1c_04", "Tipo de Prueba de Hipótesis:", 
                  choices = c("Bilateral"="two.sided",
                              "Unilateral Izquierda"="less",
                              "Unilateral Derecha"="greater"
                  ))
      
      
    )
    
  } else return(NULL)
  
})

# Test de Proporciones

# Tabla de Frecuencias para las 2 variables
Test04_1C_testT <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 1 && tipo_var_goku() == "Numérica") {
    if(!is.null(input$mu_ho_1c_04)) {
      if(!is.null(input$prueba_ho_1c_04)) {
        if(!is.null(decimales_goku())) {
          if(!is.null(input$alfa_ho)) {
            
            wt01_master(input_base = BASE_goku(), input_decimales = decimales_goku(), input_mediana = input$mu_ho_1c_04, 
                        input_alfa = input$alfa_ho, input_formato = input$prueba_ho_1c_04, input_cadena = NULL)
            
            
            
          } else return(NULL)
        } else return(NULL)
      } else return(NULL)
    } else return(NULL)
  } else return(NULL)
  
  
  
  
})
#######################################################

# Salida de valores resumen estadistico Chi Cuadrado
observe( output$tabla_ho_1c_04 <- renderTable(rownames = FALSE, digits=decimales_goku(), align = "c", {
  if(!is.null(Test04_1C_testT())) {
    Test04_1C_testT()$tablas$Test_Wilcoxon
  } else return(NULL)
}))

observe(output$frase01_ho_1c_04 <- renderUI({
  if(!is.null(Test04_1C_testT())) {
    HTML(Test04_1C_testT()$frases$Validacion_w1)
  } else return(NULL)
}))


observe(output$frase02_ho_1c_04 <- renderUI({
  if(!is.null(Test04_1C_testT())) {
    HTML(Test04_1C_testT()$frases$Explicacion_w1)
  } else return(NULL)
}))


Opc_ho_1c_04 <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 1) {
    
    
    div(
      h3("Test Wilcoxon (una muestra)"),
      h3("Detalles"),
      uiOutput("ho_1c_04"), br(),
      h3("Tabla Resumen"), br(),
      tableOutput("tabla_ho_1c_04"),
      uiOutput("frase01_ho_1c_04"),
      uiOutput("frase02_ho_1c_04"),
      br(), br()
    )
    
    
    
  } else return(NULL)
  
})





