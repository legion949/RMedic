# Cantidad de Variables
qtty_var_planeta <- reactive({
  
  if (!is.null(input$PanelRMedic)) {
    
    if(input$PanelRMedic == 3) { 
      if(paso_detalle(input$qtty_var_tablas))     as.numeric(input$qtty_var_tablas)
      
      
      
      
      
      
    } else  if(input$PanelRMedic == 4) {
      if(paso_detalle(input$qtty_var_graf))    as.numeric(input$qtty_var_graf)
      
      
      
    } else  if(input$PanelRMedic == 5) {
      if(paso_detalle(input$qtty_var_ho))    as.numeric(input$qtty_var_ho)
      
      
      
    } else return(NULL)
    
  } else return(NULL)
  
  
})

# Decimales que usara
decimales_planeta <- reactive({
  
  if (!is.null(input$PanelRMedic)) {
    
    if(input$PanelRMedic == 3) { 
      if(!is.null(input$decimales_tablas))     as.numeric(input$decimales_tablas)
      
      
    } else  if(input$PanelRMedic == 4) {
      if(!is.null(input$decimales_graf))    as.numeric(input$decimales_graf)
      
      
      
    } else  if(input$PanelRMedic == 5) {
      if(!is.null(input$decimales_ho))    as.numeric(input$decimales_ho)
      
      
      
    } else return(NULL)
    
  } else return(NULL)
  
  
})


# Variable de Goku + Tipo de Var
engarce_planeta <- reactive({
  
  var_planeta <- NULL
  tipo_var_planeta <- NULL
  
  # Si cargo el menu general...
  if (!is.null(input$PanelRMedic)) {
    
    # Si esta en la pestania 3 "TABLAS"
    if(input$PanelRMedic == 3) { 
      
      # Si hay una cantidad de variables seleccionada
      if (paso_detalle(input$qtty_var_tablas)) {
        
        var_planeta <- rep(NA, input$qtty_var_tablas)
        var_planeta <- na.omit(var_planeta)
        
        
        tipo_var_planeta <- rep(NA, input$qtty_var_tablas)
        tipo_var_planeta <- na.omit(tipo_var_planeta)
        
        
        # Si la cantidad de variables es == 1
        if (as.numeric(input$qtty_var_tablas) == 1){
          if (paso_detalle(input$var1_tablas)) {
            
            var_planeta <- c(input$var1_tablas)
            tipo_var_planeta <- c(input$tipo_var1_tablas)
            
          } else return(NULL)
          
          # Si la cantidad de variables es == 2
        } else   if (as.numeric(input$qtty_var_tablas == 2)) {
          
          # Si ya selecciono las dos variables...
          if (paso_detalle(input$var1_tablas) && paso_detalle(input$var2_tablas)) {
            
            # Y de las dos variables se sabe de que tipo son...
            if (paso_detalle(input$tipo_var1_tablas) && paso_detalle(input$tipo_var2_tablas)) {
              
              var_planeta <- c(input$var1_tablas, input$var2_tablas)
              tipo_var_planeta <- c(input$tipo_var1_tablas, input$tipo_var2_tablas)
              
              # Si las dos variables son del mismo tipo...
              if (input$tipo_var1_tablas == input$tipo_var2_tablas) {
                
                # Si el orden_var es igual a la 2da variable...
                if (paso_detalle(input$orden_var_tablas) && input$orden_var_tablas == input$var2_tablas) {
                  
                  var_planeta <- var_planeta[c(2,1)]
                  tipo_var_planeta <- tipo_var_planeta[c(2,1)]
                  
                } 
                
                # Si las variables son de diferente tipo...
              } else if (input$tipo_var1_tablas != input$tipo_var2_tablas) {
                
                # Si la 2da variable es categorica... cambiamos el orden de las cosas...
                if (input$tipo_var1_tablas == "Categórica") {
                  var_planeta <- var_planeta[c(2,1)]
                  tipo_var_planeta <- tipo_var_planeta[c(2,1)]
                  
                }
              } else return(NULL)
            } else return(NULL)   
          } else return(NULL)
        } else return(NULL)
      } else return(NULL)
      
    } else      if(input$PanelRMedic == 4) { 
      
      # Si hay una cantidad de variables seleccionada
      if (paso_detalle(input$qtty_var_graf)) {
        
        var_planeta <- rep(NA, input$qtty_var_graf)
        var_planeta <- na.omit(var_planeta)
        
        
        tipo_var_planeta <- rep(NA, input$qtty_var_graf)
        tipo_var_planeta <- na.omit(tipo_var_planeta)
        
        
        # Si la cantidad de variables es == 1
        if (as.numeric(input$qtty_var_graf) == 1){
          if (paso_detalle(input$var1_graf)) {
            
            var_planeta <- c(input$var1_graf)
            tipo_var_planeta <- c(input$tipo_var1_graf)
            
          } else return(NULL)
          
          # Si la cantidad de variables es == 2
        } else   if (as.numeric(input$qtty_var_graf == 2)) {
          
          # Si ya selecciono las dos variables...
          if (paso_detalle(input$var1_graf) && paso_detalle(input$var2_graf)) {
            
            # Y de las dos variables se sabe de que tipo son...
            if (paso_detalle(input$tipo_var1_graf) && paso_detalle(input$tipo_var2_graf)) {
              
              var_planeta <- c(input$var1_graf, input$var2_graf)
              tipo_var_planeta <- c(input$tipo_var1_graf, input$tipo_var2_graf)
              
              # Si las dos variables son del mismo tipo...
              if (input$tipo_var1_graf == input$tipo_var2_graf) {
                
                # Si el orden_var es igual a la 2da variable...
                if (paso_detalle(input$orden_var_graf) && input$orden_var_graf == input$var2_graf) {
                  
                  var_planeta <- var_planeta[c(2,1)]
                  tipo_var_planeta <- tipo_var_planeta[c(2,1)]
                  
                } 
                
                # Si las variables son de diferente tipo...
              } else if (input$tipo_var1_graf != input$tipo_var2_graf) {
                
                # Si la 2da variable es categorica... cambiamos el orden de las cosas...
                if (input$tipo_var1_graf == "Categórica") {
                  var_planeta <- var_planeta[c(2,1)]
                  tipo_var_planeta <- tipo_var_planeta[c(2,1)]
                  
                }
              } else return(NULL)
            } else return(NULL)   
          } else return(NULL)
        } else return(NULL)
      } else return(NULL)
      
    } else      if(input$PanelRMedic == 5) { 
      
      # Si hay una cantidad de variables seleccionada
      if (paso_detalle(input$qtty_var_ho)) {
        
        var_planeta <- rep(NA, input$qtty_var_ho)
        var_planeta <- na.omit(var_planeta)
        
        
        tipo_var_planeta <- rep(NA, input$qtty_var_ho)
        tipo_var_planeta <- na.omit(tipo_var_planeta)
        
        
        # Si la cantidad de variables es == 1
        if (as.numeric(input$qtty_var_ho) == 1){
          if (paso_detalle(input$var1_ho)) {
            
            var_planeta <- c(input$var1_ho)
            tipo_var_planeta <- c(input$tipo_var1_ho)
            
          } else return(NULL)
          
          # Si la cantidad de variables es == 2
        } else   if (as.numeric(input$qtty_var_ho == 2)) {
          
          # Si ya selecciono las dos variables...
          if (paso_detalle(input$var1_ho) && paso_detalle(input$var2_ho)) {
            
            # Y de las dos variables se sabe de que tipo son...
            if (paso_detalle(input$tipo_var1_ho) && paso_detalle(input$tipo_var2_ho)) {
              
              var_planeta <- c(input$var1_ho, input$var2_ho)
              tipo_var_planeta <- c(input$tipo_var1_ho, input$tipo_var2_ho)
              
              # Si las dos variables son del mismo tipo...
              if (input$tipo_var1_ho == input$tipo_var2_ho) {
                
                # Si el orden_var es igual a la 2da variable...
                if (paso_detalle(input$orden_var_ho) && input$orden_var_ho == input$var2_ho) {
                  
                  var_planeta <- var_planeta[c(2,1)]
                  tipo_var_planeta <- tipo_var_planeta[c(2,1)]
                  
                } 
                
                # Si las variables son de diferente tipo...
              } else if (input$tipo_var1_ho != input$tipo_var2_ho) {
                
                # Si la 2da variable es categorica... cambiamos el orden de las cosas...
                if (input$tipo_var1_ho == "Categórica") {
                  var_planeta <- var_planeta[c(2,1)]
                  tipo_var_planeta <- tipo_var_planeta[c(2,1)]
                  
                }
              } else return(NULL)
            } else return(NULL)   
          } else return(NULL)
        } else return(NULL)
      } else return(NULL)
      
    } else  return(NULL)
    
    
  } else return(NULL)
  
  # Sale una lista con dos objetos...
  list(var_planeta, tipo_var_planeta)
  
})

# Variable de Goku
var_planeta <- reactive({
  engarce_planeta()[[1]]
})


# Tipo Variable de Goku
tipo_var_planeta <- reactive({
  engarce_planeta()[[2]]
})


# Variable y letra de las variables seleccionadas
detalle_planeta <- reactive({
  
  detalle <- c("", "")
  
  if (length(var_planeta()) >= 1)  detalle[1] <- paste0("Variable 1: ", id_var(var_planeta()[1], colnames(BASE_SALIDA())))  
  if (length(var_planeta()) >= 2)  detalle[2] <- paste0("Variable 2: ", id_var(var_planeta()[2], colnames(BASE_SALIDA())))  
  
  detalle
  
})


# Para 2Q... Quien va en filas y quien en columnas
ref2q_planeta <- reactive({
  
  detalle <- c("", "")
  
  if (length(var_planeta()) >= 1)  detalle[1] <- paste0("En filas: ", id_var(var_planeta()[1], colnames(BASE_SALIDA())))  
  if (length(var_planeta()) >= 2)  detalle[2] <- paste0("En columnas: ", id_var(var_planeta()[2], colnames(BASE_SALIDA())))  
  
  detalle
  
})
