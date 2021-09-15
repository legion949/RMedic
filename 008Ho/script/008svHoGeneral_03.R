

# # # # #
# 1C - 03 - Test t (una muestra)

output$ho_1c_03 <- renderUI({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 1) {
    
    div(
      numericInput("mu_ho_1c_03", "Valor de Media Poblacional bajo Hipótesis:", 
                   min=-1e20,  max=1e20, step= 1,  value=0), br(),
      
      radioButtons("prueba_ho_1c_03", "Tipo de Prueba de Hipótesis:", 
                  choices = c("Bilateral"="two.sided",
                              "Unilateral Izquierda"="less",
                              "Unilateral Derecha"="greater"
                  ))
      
      
    )
    
  } else return(NULL)
  
})

# Test de Proporciones

# Tabla de Frecuencias para las 2 variables
Test03_1C_testT <- reactive({
  

            tt01_master(input_base = BASE_goku(), input_decimales = decimales_goku(), input_mu = input$mu_ho_1c_03, 
                        input_alfa = input$alfa_ho, input_formato = input$prueba_ho_1c_03, input_cadena = NULL)
            
          
            
})
#######################################################



# Salida de requisitos para el test t
observe( output$tabla_ho_1c_03_V2 <- renderTable(rownames = FALSE, digits=decimales_goku(), align = "c", {
  if(!is.null(Test03_1C_testT())) {
    Test03_1C_testT()$tablas$Test_Normalidad
  } else return(NULL)
}))

observe(output$frase01_ho_1c_03_V2 <- renderUI({
  if(!is.null(Test03_1C_testT())) {
    HTML(Test03_1C_testT()$Validacion_normalidad)
  } else return(NULL)
}))


observe(output$frase02_ho_1c_03_V2 <- renderUI({
  if(!is.null(Test03_1C_testT())) {
    HTML(Test03_1C_testT()$frases$Explicacion_normalidad)
  } else return(NULL)
}))



#################################################################################################
# Salida de valores resumen del test t
observe( output$tabla_ho_1c_03 <- renderTable(rownames = FALSE, digits=decimales_goku(), align = "c", {
  if(!is.null(Test03_1C_testT())) {
    Test03_1C_testT()$tablas$Test_t
  } else return(NULL)
}))

observe(output$frase01_ho_1c_03 <- renderUI({
  if(!is.null(Test03_1C_testT())) {
    HTML(Test03_1C_testT()$Validacion_t)
  } else return(NULL)
}))


observe(output$frase02_ho_1c_03 <- renderUI({
  if(!is.null(Test03_1C_testT())) {
    HTML(Test03_1C_testT()$frases$Explicacion_t)
  } else return(NULL)
}))


Opc_ho_1c_03 <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 1) {
    
    
    div(
      uiOutput("ho_1c_03"), br(),
      h3("Requisitos del Test t"), br(),
      tableOutput("tabla_ho_1c_03_V2"),
      uiOutput("frase01_ho_1c_03_V2"),
      uiOutput("frase02_ho_1c_03_V2"),
      br(),
      h3("Test t (una muestra)"), br(),
      tableOutput("tabla_ho_1c_03"),
      uiOutput("frase01_ho_1c_03"),
      uiOutput("frase02_ho_1c_03"),
      br(), br()
    )
    
    
    
  } else return(NULL)
  
})





