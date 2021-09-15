




output$planeta_goku <- renderUI({
  
  if (!is.null(input$goku)) {
    
    # Tablas
    if(input$goku == 3){
    
    div(
      detalle_goku()[1], br(),
      detalle_goku()[2], br(),
      uiOutput("planeta_tablas")
    )
      
    # Graficos  
    } else  if(input$goku == 4){
      
      div(
        detalle_goku()[1], br(),
        detalle_goku()[2], br(),
        uiOutput("planeta_graf")
      )
      
    # Pruebas de Ho
    } else  if(input$goku == 5){
      
      div(
        detalle_goku()[1], br(),
        detalle_goku()[2], br(),
        uiOutput("planeta_ho"),
        conditionalPanel("input.menu_ho < 1000", uiOutput("planeta_tablas"))
      )
      
    }
    
  } else return(NULL)
  
})
