


Graficos1Q_02_Barras_UI <- function(id) {
  
  ns <- NS(id)
  
  
  
 
  
}






## Segmento del server
Graficos1Q_02_Barras_SERVER <- function(input, output, session, 
                              minibase, 
                              batalla_naval,
                              decimales) {
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  
  # Caso 1: 1Q
  casoRMedic <- reactive({
    
    if(is.null(batalla_naval())) return(NULL)
    if(is.null(batalla_naval()[[4]])) return(NULL)
    if(length(batalla_naval()[[4]]) == 0) return(NULL)
    if(batalla_naval()[[4]] == '') return(NULL)
    casoRMedic <- batalla_naval()[[4]]
    #casoRMedic <- as.numeric(as.character(as.vector(batalla_naval()[[4]])))
    casoRMedic
    
  })
  
  
  DF_interna <-  reactive({RMedic_1q_tablas(minibase(), decimales())[[1]] })
  
 
   
}