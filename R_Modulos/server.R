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
  
 
  
  # 1 - Base ------------------------------------------------
  {
  ###
    
  # Modulo01... Nos otorga
  # 1) BaseSalida
  # 2) zocalo_CIE
  # 3) RMedic_general
  # 4) status_BaseSalida
 Modulo01 <- callModule(module = SideBarBaseSERVER, id =  "base01")
 RMedic_general <- Modulo01$RMedic_general
 status_BaseSalida <- Modulo01$status_BaseSalida
  
  ###
  }
  ###########################################################
  
  
  # 2 - Tablas ----------------------------------------------
  {
  ###
  
      
  
  UserSelection.Tablas <- callModule(module = BatallaNavalSERVER, 
                              id =  "tablas01",
                              base = Modulo01$BaseSalida,
                              verbatim = FALSE)

  
  MiniBase.Tablas <- callModule(module = MiniBaseSERVER, id =  "tablas02",
                         base = Modulo01$BaseSalida,
                         batalla_naval = UserSelection.Tablas$batalla_naval,
                         verbatim = FALSE)


  
  callModule(module = Tablas1Q_SERVER, id =  "tablas03",
             minibase = MiniBase.Tablas,
             batalla_naval = UserSelection.Tablas$batalla_naval,
             decimales = UserSelection.Tablas$decimales)


  callModule(module = Tablas1C_SERVER, id =  "tablas04",
             minibase = MiniBase.Tablas,
             batalla_naval = UserSelection.Tablas$batalla_naval,
             decimales = UserSelection.Tablas$decimales)

  ###
  }
  ###########################################################
  
  
  
  
  
  
  menuBASE <- reactive({
    
    
    tabs <- list()
    
    tabs[[1]] <-    tabPanel(title = "Base de Datos", 
                             icon = icon("user-md"), 
                             value = 1,
                             br(),
                             fluidRow(

                                      MiBase01_UI("base01")#,

                             ),
                             br(), br()
    )
    
    tabs
    
  })
  
  
  menuCONTROL <- reactive({
    
    # Si no hay orden de salir a la cancha... Nadie sale...
    if(is.null(RMedic_general())) return(NULL)
    if(!RMedic_general()) return(NULL)
    
    # Si no hay status de BaseSalida(), nos vamos...
    if(is.null(status_BaseSalida())) return(NULL)
    if(!status_BaseSalida()) return(NULL)
  
    
    tabs <- list()
    
    
    tabs[[1]] <-  tabPanel(
      title = "Control", 
      icon = icon("user-md"), 
      value = 2,
      h3("Menú para Control")
     
    ) # End TabPanel
    
    
    
    tabs
    
  })
  
  menuTABLAS <- reactive({
    
    # Si no hay orden de salir a la cancha... Nadie sale...
    if(is.null(RMedic_general())) return(NULL)
    if(!RMedic_general()) return(NULL)
    
    # Si no hay status de BaseSalida(), nos vamos...
    if(is.null(status_BaseSalida())) return(NULL)
    if(!status_BaseSalida()) return(NULL)
    
    
    tabs <- list()
    
    
    tabs[[1]] <-  tabPanel(
      title = "Tablas", 
      icon = icon("user-md"), 
      value = 3,
      fluidRow(
        column(1),
        column(10,
      h3("Menú para Tablas"),
      BatallaNavalUI("tablas01"),
      MiniBaseUI("tablas02"),
      Tablas1Q_UI("tablas03"),
      Tablas1C_UI("tablas04")
        ),
      column(1)
      )
      
    ) # End TabPanel
    
    
    
    tabs
    
  })
  
  menuGRAFICOS <- reactive({
    
    # Si no hay orden de salir a la cancha... Nadie sale...
    if(is.null(RMedic_general())) return(NULL)
    if(!RMedic_general()) return(NULL)
    
    # Si no hay status de BaseSalida(), nos vamos...
    if(is.null(status_BaseSalida())) return(NULL)
    if(!status_BaseSalida()) return(NULL)
    
    
    tabs <- list()
    
    
    tabs[[1]] <-  tabPanel(
      title = "Gráficos", 
      icon = icon("user-md"), 
      value = 4,
      h3("Menú para Gráficos")
      
    ) # End TabPanel
    
    
    
    tabs
    
  })
  
  menuHO <- reactive({
    
    # Si no hay orden de salir a la cancha... Nadie sale...
    if(is.null(RMedic_general())) return(NULL)
    if(!RMedic_general()) return(NULL)
    
    # Si no hay status de BaseSalida(), nos vamos...
    if(is.null(status_BaseSalida())) return(NULL)
    if(!status_BaseSalida()) return(NULL)
    
    
    tabs <- list()
    
    
    tabs[[1]] <-  tabPanel(
      title = "Pruebas de Hipótesis", 
      icon = icon("user-md"), 
      value = 5,
      h3("Menú para Pruebas de Hipótesis")
      
    ) # End TabPanel
    
    
    
    tabs
    
  })
  
  menuSOBREVIDA <- reactive({
    
    # Si no hay orden de salir a la cancha... Nadie sale...
    if(is.null(RMedic_general())) return(NULL)
    if(!RMedic_general()) return(NULL)
    
    # Si no hay status de BaseSalida(), nos vamos...
    if(is.null(status_BaseSalida())) return(NULL)
    if(!status_BaseSalida()) return(NULL)
    
    
    tabs <- list()
    
    
    tabs[[1]] <-  tabPanel(
      title = "Sobrevida", 
      icon = icon("user-md"), 
      value = 6,
      h3("Menú para Sobrevida")
      
    ) # End TabPanel
    
    
    
    tabs
    
  })
  
  observe(output[["RMedicSoft"]] <- renderUI({
    
    
    # do.call(tabsetPanel,  c(id="goku", tabs1,tabs2, tabs3, tabs4, tabs5, tabs6))
    do.call(tabsetPanel,  c(id="PanelRMedic", 
                            menuBASE(),
                            menuCONTROL() ,
                            menuTABLAS() ,
                            menuGRAFICOS() ,
                            menuHO(),
                            menuSOBREVIDA()
    )
    )
    
  }))
  
  # output$RMedicSoft <- renderUI({
  #   tabsetPanel(
  #     tabPanel("Base", value = 1, 
  #              MiBase01_UI("tablas05")),
  #     tabPanel("Control", value = 2),
  #     tabPanel("Tablas", value = 3,
  #              BatallaNavalUI("tablas01"))
  #   )
  # })
  
  
}