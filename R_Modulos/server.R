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
  
  
  
  

  

  
  
  ##############################################
   
  
  
  
  ########################################################
  
 
  
 BaseSalida <- callModule(module = SideBarBaseSERVER, id =  "tablas05")
  
 # observe(cat("La 1: ", colnames(BaseSalida()), "\n"))
  #observe(cat("La 2: ", colnames(BaseSalida2[[1]]()), "\n"))
  
 # if(!is.null(BaseSalida2)) {
  UserSelection <- callModule(module = BatallaNavalSERVER, id =  "tablas01",
                             base = BaseSalida,
                             verbatim = FALSE)
  #}
  
  # MiniBase <- callModule(module = MiniBaseSERVER, id =  "tablas02",
  #                        base = BaseSalida,
  #                        batalla_naval = UserSelection$batalla_naval,
  #                        verbatim = FALSE)
  # 
  # 
  
  # callModule(module = Tablas1Q_SERVER, id =  "tablas03", 
  #            minibase = MiniBase,
  #            batalla_naval = UserSelection$batalla_naval,
  #            decimales = UserSelection$decimales)
  # 
  # 
  # callModule(module = Tablas1C_SERVER, id =  "tablas04",
  #            minibase = MiniBase,
  #            batalla_naval = UserSelection$batalla_naval,
  #            decimales = UserSelection$decimales)

  
  
  ######################################
  
  
  
  
}