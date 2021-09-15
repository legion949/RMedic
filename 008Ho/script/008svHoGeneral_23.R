


# # # # #
# QC - 23 - SW




# Test de Normalidad de Shapiro Wilks (Por grupos)
Test23_QC_SW  <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2) {
    if(!is.null(decimales_goku())) {
      if(!is.null(input$alfa_ho)) {
        
        
        shapiro2_master(input_base = BASE_goku(),
                        input_decimales = decimales_goku(),
                        input_alfa = input$alfa_ho, 
                        input_cadena = NULL)
        
        
        
        
        
      } else return(NULL)
    } else return(NULL)
  } else return(NULL)
  
})



# Salida resumen de SW
observe( output$tabla01_ho_qc_23 <- renderTable(rownames = FALSE, digits=decimales_goku(), align = "c", {
  
  if (!is.null(Test20_QC_KW())){
    Test23_QC_SW()$tablas$Test_Normalidad
  } else return(NULL)
}))







# FRASE1 KW
output$frase01_ho_qc_23 <- renderUI({
  if (!is.null(Test20_QC_KW())){
    HTML(Test23_QC_SW()$frases$Validacion)
  } else return(NULL)
})



# FRASE3 Dunn
output$frase02_ho_qc_23 <- renderUI({
  if (!is.null(Test20_QC_KW())){
    HTML( Test23_QC_SW()$frases$Explicacion)
  } else return(NULL)
})







Opc_ho_qc_23 <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2) {
    
    
    div(
      h3("Test de Normalidad de Shapiro-Wilks por grupos"),
      
      
      h3("Resumen del test de Normalidad"),
      tableOutput("tabla01_ho_qc_23"),
      uiOutput("frase01_ho_qc_23"),
      br(),
      uiOutput("frase02_ho_qc_23")
      
    )
    
  } else return(NULL)
  
})




