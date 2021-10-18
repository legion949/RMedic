function(input, output, session) {
  
  
  observeEvent(input$toggleSidebar, {
    shinyjs::toggle(id = "MySidebar")
  })
  
  observeEvent(input$MiniButton, {
    shinyjs::toggle(id = "MySidebar")
  })
  
  
  observeEvent(input$showpanel, {
    
    if(input$showpanel == TRUE) {
      removeCssClass("Main", "col-sm-12")
      addCssClass("Main", "col-sm-8")
      shinyjs::show(id = "Sidebar")
      shinyjs::enable(id = "Sidebar")
    }
    else {
      removeCssClass("Main", "col-sm-8")
      addCssClass("Main", "col-sm-12")
      shinyjs::hide(id = "Sidebar")
    }
  })
  
  
  RMedic_general <- reactiveVal(T)
  reseteo_conteo <- reactiveVal(0)
  
  
  
  MyFileName <- reactive({
    
    
    
    if(!is.null(input$FileTypePicker)){
      
      if(!is.null(Control01())) {
        if(Control01()[[1]]) {
          
          if(input$FileTypePicker == "Excel") { 
            
            if (!is.null(input$xls_file)) my_file <- input$xls_file[[1]] else return(NULL)
            
            
          } else 
            if(input$FileTypePicker == "CSV") { 
              
              if (!is.null(input$csv_file)) my_file <- input$csv_file[[1]] else return(NULL)
              
              
            } else 
              if(input$FileTypePicker == "Ejemplos") { 
                
                
                if (!is.null(input$ejemplo_file)) my_file <- input$ejemplo_file else return(NULL)
                
                
              } else return(NULL)
          
          
          # Return of the king...
          return(my_file)
          
        } else return(NULL)  
      } else return(NULL)
    } else return(NULL)
    
    
    
  })
  
  
  

  

  
  
  ##############################################
  
  BaseSalida <- reactive({
    base_interna <- mtcars
    base_interna[,2] <- as.character(base_interna[,2])
    base_interna
  })
   
  
  ########################################################
  
  
  callModule(module = SideBarBaseSERVER, id =  "tablas05")
  
  
  
  UserSelection <- callModule(module = BatallaNavalSERVER, id =  "tablas01", 
                             base = BaseSalida, 
                             verbatim = FALSE)

  MiniBase <- callModule(module = MiniBaseSERVER, id =  "tablas02",
                         base = BaseSalida,
                         batalla_naval = UserSelection$batalla_naval,
                         verbatim = FALSE)

 
  
  callModule(module = Tablas1Q_SERVER, id =  "tablas03", 
             minibase = MiniBase,
             batalla_naval = UserSelection$batalla_naval,
             decimales = UserSelection$decimales)
  
  
  callModule(module = Tablas1C_SERVER, id =  "tablas04",
             minibase = MiniBase,
             batalla_naval = UserSelection$batalla_naval,
             decimales = UserSelection$decimales)

  
  
  ######################################
  
  
  
  
}