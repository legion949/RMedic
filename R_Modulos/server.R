function(input, output, session) {
  
  
  
  
  BaseSalida <- reactive({
    base_interna <- mtcars
    base_interna[,2] <- as.character(base_interna[,2])
    base_interna
  })
  
  valores <- callModule(module = selectorSERVER, id =  "planeta", base = BaseSalida)

 
  output$MiTexto <- renderText({
    
    #cat(valores[[1]]())
   # "AAAA"
    unlist(valores$batalla_naval())
  })  
  

}