## Segmento del UI
MiniBaseUI <- function(id) {
  ns <- NS(id)
  
  div(tableOutput(ns("MiniBase")))

}




## Segmento del server
MiniBaseSERVER <- function(input, output, session, base, 
                          batalla_naval, verbatim) {

  
  minibase <- reactive({
    
    if(is.null(base())) return(NULL)
    if(is.null(batalla_naval())) return(NULL)
    
    vars <- batalla_naval()[[1]]
    type_vars <- batalla_naval()[[3]]
    
    # The minibase
    minibase <- na.omit(base()[vars])
    
    # Data type
    for (k in 1:length(type_vars)){
      
      if(type_vars[k] == "Character") {
        minibase[,k] <- as.character(minibase[,k])
      } # End if
    } # End for
    
    return(minibase)
  })
  
  output$MiniBase <- renderTable({
    
    if(is.null(verbatim)) return(NULL)
    if(!verbatim) return(NULL)
    
    minibase()
  })
  
  # Modul Return!!!
  return(minibase)
  
  # return(
  #   list(
  #     frec_input = reactive({ 3333 }),
  #     max_input = reactive({ 4444 })
  #   )
  # )
  
  
  
  
  
}


