

# # # # #
# 1C - 02 - Test de Normalidad


# Tabla de Frecuencias para las 2 variables
Test02_1C_Normalidad <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 1 && tipo_var_goku() == "Numérica") {
    if(!is.null(var_goku()) && sum(colnames(BASE_goku()) == var_goku()[1]) == 1) {
        if(!is.null(decimales_goku())) {
          if(!is.null(input$alfa_ho)) {
            
            
            shapiro_master(input_base = BASE_goku(), input_decimales = decimales_goku(),
                           input_alfa = input$alfa_ho, input_cadena = NULL)
            
            
        } else return(NULL)
      } else return(NULL)
    } else return(NULL)
  } else return(NULL)
  
  
  
  
})
#######################################################




# Salida de valores resumen 
observe( output$tabla_ho_1c_02 <- renderTable(rownames = FALSE, digits=decimales_goku(), align = "c", {
#  if(!is.null(Test02_1C_Normalidad())) {
    Test02_1C_Normalidad()$tablas$Test_Normalidad
 # } else return(NULL)
}))

observe(output$frase01_ho_1c_02 <- renderUI({
  if(!is.null(Test02_1C_Normalidad())) {
    HTML(Test02_1C_Normalidad()$frases$Validacion)
  } else return(NULL)
}))


observe(output$frase02_ho_1c_02 <- renderUI({
  if(!is.null(Test02_1C_Normalidad())) {
    HTML(Test02_1C_Normalidad()$frases$Explicacion)
  } else return(NULL)
}))





Opc_ho_1c_02 <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 1) {
    
    
    div(
      h3("Test de Normalidad de Shapiro-Wilk"), br(),
      h3(span(uiOutput("frase01_ho_1c_02"), style="color:red")),
      tableOutput("tabla_ho_1c_02"),
      uiOutput("frase02_ho_1c_02"),
      br(), br()
    )
    
    
    
  } else return(NULL)
  
})


