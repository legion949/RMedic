
source("lib.R")

#source("uiCode.R")

nombres1 <- c("Orden Original", "Alfabético Ascendente", "Alfabético Descendente")
opciones_carga1 <- namel(nombres1)

nombres3 <- c("Categórica", "Numérica")
opciones_carga3 <- namel(nombres3) 


TextServer_Variables <- '
        # Update Menu1  
        nombres1 <- c("Orden Original", "Alfabético Ascendente", "Alfabético Descendente")
        opciones_carga1 <- namel(nombres1)
        
        # Update Menu2  
        nombres2_original <- colnames(_MY_BASE_)
        opciones_carga2 <- OpcionesDeColumnas(nombres2_original)
        
        nombres3 <- c("Categórica", "Numérica")
        opciones_carga3 <- namel(nombres3) 
        
        
           observeEvent(_MENU1_, {
          
          
          nuevo_orden <- c(1:length(nombres2_original))
          if (_MENU1_ == "Alfabético Ascendente")  nuevo_orden <- order(nombres2_original, decreasing = F) else
            if (_MENU1_ == "Alfabético Descendente") nuevo_orden <- order(nombres2_original, decreasing = T)
            
            opciones_carga2 <- opciones_carga2[nuevo_orden]
      
            
            updateSelectInput(session, 
                              inputId = "_ONLY_NAME_INPUT_VAR1_",
                              label = "Variable 1",
                              choices=  c("Seleccione una variable..." = "", opciones_carga2),
                              selected = NA)  
            
            updateSelectInput(session, 
                              inputId = "_ONLY_NAME_INPUT_VAR2_",
                              label = "Variable 2",
                              choices=  c("Seleccione una variable..." = "", opciones_carga2),
                              selected = NA) 
            
            ###########
            updateRadioButtons(session,
                               inputId = "_ONLY_NAME_INPUT_TIPO_VAR1_",
                               label = "Tipo de Variable1",
                               choices = opciones_carga3,
                               selected = opciones_carga3[1])
                               
            updateRadioButtons(session,
                               inputId = "_ONLY_NAME_INPUT_TIPO_VAR2_",
                               label = "Tipo de Variable2",
                               choices = opciones_carga3,
                               selected = opciones_carga3[1])  
            ############
        })
        
        
           observeEvent(_INPUT_VAR1_, {
          
          if(AllEyesOnMe(ListBase = _THE_LISTBASE_, the_col = _INPUT_VAR1_)) {
            
            my_option <- 1
            if (is.numeric(_THE_LISTBASE_[[1]][,_INPUT_VAR1_])) my_option <- 2
            updateRadioButtons(session,
                               inputId = "_ONLY_NAME_INPUT_TIPO_VAR1_",
                               label = "Tipo de Variable1",
                               choices = opciones_carga3,
                               selected = opciones_carga3[my_option])
            
            # 
            
          }
        })
        
        
        
           observeEvent(_INPUT_VAR2_, {
          
          if(AllEyesOnMe(ListBase = _THE_LISTBASE_, the_col = _INPUT_VAR2_)) {
            
            my_option <- 1
            if (is.numeric(_MY_BASE_[,_INPUT_VAR2_])) my_option <- 2
            updateRadioButtons(session,
                               inputId = "_ONLY_NAME_INPUT_TIPO_VAR2_",
                               label = "Tipo de Variable2",
                               choices = opciones_carga3,
                               selected = opciones_carga3[my_option])
            
            # 
            
          }
          
          
          
        })
        ##################################################
        '



TextUI_Variables <- 'div(
  fluidRow(
    column(4,
           # Menu1: Orden de las seleccion de variables         
           selectInput(inputId = "menu1_control",
                       label = "Orden de las variables",
                       choices = opciones_carga1,                                                    selected = opciones_carga1[1]
           )
    ),
    column(4, 
           selectInput(inputId = "qtty_var_control",
                       label = "Cantidad de Variables",
                       choices = c("Seleccione..." = "", "Una" = 1, "Dos" = 2)
           )
    ),           
    column(4, 
           numericInput(inputId = "decimales_control", 
                        label = "Decimales:", 
                        min = 0,  max = 100, step = 1, value = 2
           )
    )
  ) # End FluidRow
  ,br(),
  fluidRow(
    conditionalPanel(condition = "input.qtty_var_control != \'\'",
                     conditionalPanel(condition = "input.qtty_var_control >= 1",
                                      fluidRow(
                                        column(4,
                                               selectInput(inputId = "var1_control",
                                                           label = "Variable 1",
                                                           choices=c("Seleccione una variable..." = "", opciones_carga2),
                                                           selected = NA
                                               )
                                        ),
                                        column(4,
                                               radioButtons(inputId = "tipo_var1_control",
                                                            label = "Tipo de Variable1",
                                                            choices = opciones_carga3,
                                                            selected = opciones_carga3[1]
                                               )
                                        )
                                      ) # End FluidRow
                     ), # End ConditionalPanel
                     conditionalPanel(condition = "input.qtty_var_control >= 2",
                                      fluidRow(
                                        column(4,
                                               selectInput(inputId = "var2_control",
                                                           label = "Variable 2",
                                                           choices=c("Seleccione una variable..." = "", opciones_carga2),
                                                           selected = NA
                                               )
                                        ),
                                        column(4,
                                               radioButtons(inputId = "tipo_var2_control",
                                                            label = "Tipo de Variable2",
                                                            choices = opciones_carga3,
                                                            selected = opciones_carga3[1]
                                               )
                                        )
                                      ) # End FluidRow
                     ) #End conditionalPanel
    ) #End conditionalPanel
  ) # End FluidRow
  
)'


function(input, output, session) {
  

  observeEvent(input$showSidebar, {
    shinyjs::show(id = "MySidebar")
  })
  
  observeEvent(input$hideSidebar, {
    shinyjs::hide(id = "MySidebar")
  })
  

  observeEvent(input$toggleSidebar, {
    shinyjs::toggle(id = "MySidebar")
  })

  
  RMedic_general <- reactiveVal(T)
  reseteo_conteo <- reactiveVal(0)
  
  
  # Seccion 01 - File Selection
  {
    
    
  output$OpcionesCIE_completo <- renderUI({
    
 
      
    conditionalPanel( 
      'input.cie_especificaciones == "2"',
      uiOutput("OpcionesCIE_parte01"),

      uiOutput("OpcionesCIE_parte02")

    )

  })
 
  output$OpcionesCIE_parte01 <- renderUI({
    
    
    if(!is.null(Base01()[[1]])) {

      letras <- num2let(c(1:length(colnames(Base01()[[1]]))))
      armado <- colnames(Base01()[[1]])  
      names(armado) <- paste0("(", letras, ") - ", colnames(Base01()[[1]]))
                        
                          
      selectInput(inputId = "cie_columna", 
                  label = "Criterio de Inclusión Estadístico (CIE) - Seleccione una variable:", 
                  choices =  c("Seleccione un CIE..." = "", armado),
                  selected = NA)  
   
    } else return(NULL)
    
  })
 
  output$OpcionesCIE_parte02 <- renderUI({
    
    if(!is.null(Base01())){
    if (!is.null(input$cie_columna)){
      if (!is.na(input$cie_columna)){
        if (input$cie_columna != ""){
      
      div(
      
      selectInput(inputId = "cie_categoria", 
                  label = "Categoría de Inclusión (solo una categoría):", 
                  choices = c("Seleccione una categoría..." = "", levels(as.factor(Base01()[[1]][,input$cie_columna])))
      ),
      actionButton("reset", "Quitar CIE", icon("paper-plane"), 
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      actionButton("cargar", "Aplicar CIE", class = "btn-warning")
      
    )
    
        } else return(NULL)
      } else return(NULL)
    } else return(NULL)
    } else return(NULL)
  })
  
  # Variable criterio de inclusion
  observeEvent(reseteo_conteo(),{
    
    # Reset selected step
    # # RMedic_general(F)
    
    updateRadioButtons(session, inputId = "cie_especificaciones", 
                       label = "Especificaciones", 
                       choices = c("Todas las filas" = 1, 
                                   "Aplicar un Criterio de Inclusión Estadístico" = 2))
  })
  
  
  
  
  # Update - Todo OK!
  {
    
  # Reseteo por tipo de archivo...
  observeEvent(input$FileTypePicker,{
    
    # Reset selected step
  #  # RMedic_general(F)
    
    reseteo_conteo(reseteo_conteo()+1)
    
    
  })
  
  
  # Reseteo por archivo excel seleccionado...
  observeEvent(input$xls_file,{
    
    # Reset selected step
  #  # RMedic_general(F)
    
    reseteo_conteo(reseteo_conteo()+1)
  })
  
  # Reseteo por archivo csv seleccionado...
  observeEvent(input$csv_file,{
    
    # Reset selected step
  #  # RMedic_general(F)
    
    reseteo_conteo(reseteo_conteo()+1)
    
  })
  
  # Reseteo por archivo csv seleccionado...
  observeEvent(input$ejemplo_file,{
    
    # Reset selected step
   # # RMedic_general(F)
    
    reseteo_conteo(reseteo_conteo()+1)
    
  })
  
  # Reseteo por especificaciones del criterio de inclusion estadistico
  observeEvent(input$cie_especificaciones,{
    
    # Reset selected step
    if(input$cie_especificaciones == 2) RMedic_general(F) else 
      if(input$cie_especificaciones == 1) RMedic_general(T)
  })
  
  # Reseteo por cie_columna
  observeEvent(input$cie_columna,{
    
    # Reset selected step
     RMedic_general(F)
    
  })
  
  # Reseteo por cie_categoria
  observeEvent(input$cie_categoria,{
    
    # Reset selected step
     RMedic_general(F)
    
  })
  
  ####################################################################
  
  observeEvent(input$reset,{
    
    # Reset selected step
    # RMedic_general(F)
    
    reseteo_conteo(reseteo_conteo()+1)
    
    })
  
  observeEvent(input$cargar,{
    
    # Reset selected step
    if(!is.null(input$cie_categoria)) 
      if(input$cie_categoria != "")
        RMedic_general(T)
    
  })
  
 #####################################################################
  
  }
  ###################################
  
  } # Fin Seccion 01
  ##########################################################
  
  
  # Seccion 02 - Internal Controls
  {
  ###
    
  # Control (Que sea un archivo valido)
  Control01 <- reactive({ 
    
    if(!is.null(input$FileTypePicker)){  
      
      if(input$FileTypePicker == "Excel") { 
        
        if (!is.null(input$xls_file)) {
          
          the_file <-  input$xls_file[[1]]
          
          
          correct_format <- c(".xls$", ".xlsx$")
          dt_format <- rep(NA, length(correct_format))
          dt_format[1] <- grepl(correct_format[1], the_file)
          dt_format[2] <- grepl(correct_format[2], the_file)
          ok_format <- sum(dt_format) > 0
          
          frase_yes <- ""
          frase_no  <- "Indicaste que subirías un archivo tipo Excel pero 
                        seleccionaste un archivo de otro tipo."
          
          if(ok_format) frase_alert <- frase_yes else frase_alert <- frase_no
          
          my_exit <- list(ok_format, frase_alert)
          
          
        } else return(NULL)
        
        
      } else 
        if(input$FileTypePicker == "CSV") { 
          
          if (!is.null(input$csv_file)) {
            
            the_file <-  input$csv_file[[1]]
            
            correct_format <- c(".csv$")
            dt_format <- rep(NA, length(correct_format))
            dt_format[1] <- grepl(correct_format[1], the_file)
            ok_format <- sum(dt_format) > 0
            
            frase_yes <- ""
            frase_no  <- "Indicaste que subirías un archivo CSV pero 
                        seleccionaste un archivo de otro tipo."
            
            if(ok_format) frase_alert <- frase_yes else frase_alert <- frase_no
            
            my_exit <- list(ok_format, frase_alert)
            
            
          } else return(NULL)
          
          
          
          
          
        } else 
          if(input$FileTypePicker == "Ejemplos") { 
            
            if (!is.null(input$ejemplo_file)) {
              
              ok_format <- TRUE
              frase_yes <- ""
              frase_no  <- ""
              
              if(ok_format) frase_alert <- frase_yes else frase_alert <- frase_no
              
              my_exit <- list(ok_format, frase_alert)
              
              
            } else return(NULL)
            
            
            
            
            
          } else return(NULL)
      
      return(my_exit)
      
    } 
  })
  
  
  # Control02 - La base tiene al menos una columna...
  Control02 <- reactive({ 
    
    if(!is.null(Base01())){
      
      
      dt_ok_01 <- ncol(Base01()[[1]]) > 0
      dt_ok_02 <- nrow(Base01()[[1]]) > 0
      dt_ok_03 <- sum(dt_ok_01, dt_ok_02) == 0
      dt_ok_04 <- sum(dt_ok_01, dt_ok_02) == 2
      
      frase_FALSE_01 <- "La base de datos que has seleccionado no posee
                               columnas con datos."
      
      frase_FALSE_02 <- "La base de datos que has seleccionado no posee
                               filas con datos."
      
      frase_FALSE_03 <- "La base de datos que has seleccionado es un archivo
                                completamente vacío. Sin datos."
      
      frase_TRUE <- ""
      
      # Frase
      if(dt_ok_04) frase_salida <- frase_TRUE else
        if(dt_ok_03) frase_salida <- frase_FALSE_03 else
          if(dt_ok_02) frase_salida <- frase_FALSE_02 else
            if(dt_ok_01) frase_salida <- frase_FALSE_01 
      
      # dt_ok
      if(dt_ok_04) dt_ok <- dt_ok_04 else
        if(dt_ok_03) dt_ok <- dt_ok_03 else
          if(dt_ok_02) dt_ok <- dt_ok_02 else
            if(dt_ok_01) dt_ok <- dt_ok_01 
      
      my_exit <- list(dt_ok, frase_salida)
      return(my_exit)
      
      
    } else return(NULL)
  })
  
  # Control 03 (Que la variable CIE tiene al menos una categoria)
  Control03 <- reactive({ 
    
    if(!is.null(Base01())){
      
      if(!is.null(input$cie_especificaciones)){
        
        if(input$cie_especificaciones == 2){
          
          if( Control02()[[1]]) {
            
            if(!is.null(input$cie_columna)){
              
              if(sum(colnames(Base01()[[1]]) == input$cie_columna) > 0){
                
                mis_categorias <- levels(as.factor(Base01()[[1]][,input$cie_columna]))
                
                dt_ok <- length(mis_categorias) > 0
                
                frase_TRUE <- ""
                frase_FALSE <- "La variable que cumplirá el rol de criterio de inclusión
                            estadístico debe tener al menos una categoría."
                
                if (dt_ok) frase_salida <- frase_TRUE else frase_salida <- frase_FALSE
                
                my_exit <- list(dt_ok, frase_salida)
                return(my_exit)
                
              } else return(NULL)
            } else return(NULL)
          } else return(NULL)
          
        } else return(NULL)
        
      } else return(NULL)
      
    } else return(NULL)
    
  })
  
  
  
  # Control 04 (Que haya subido la base antes de elegir un CIE)
  Control04 <- reactive({ 
    
    dt_ok <- TRUE
    
    frase_TRUE <- ""
    frase_FALSE <- "Para poder seleccionar un Criterio de Inclusión Estadística y una Categoría de Inclusión primero debe subir una base de datos."
    
    
    if (is.null(Base01()) && input$cie_especificaciones == 2) dt_ok <- FALSE          
    
    
    if (dt_ok) frase_salida <- frase_TRUE else frase_salida <- frase_FALSE
    
    # cat("frase04: ", dt_ok, "\n")
    
    my_exit <- list(dt_ok, frase_salida)
    return(my_exit)
    
    
    
    
  })
  
  # Contorl 05 (Parecido a Control04 - Que haya subido la base antes de elegir un CIE)
  Control05 <- reactive({ 
    
    
    dt_ok <- FALSE
    
    frase_TRUE <- ""
    frase_FALSE <- "
                    Para poder aplicar un Criterio de Inclusión Estadística (CIE): <br/>
                    &nbsp&nbsp 1) Seleccione un CIE (una variable).<br/>
                    &nbsp&nbsp 2) Elija una categoría de inclusión (solo una categoría).<br/>
                    &nbsp&nbsp 3) Presione 'Aplicar CIE'.
                    "
    
    frase_salida <- frase_TRUE
    
    
    if (!is.null(Base01())) {
      if(!is.null(input$cie_especificaciones)) {
        if(input$cie_especificaciones == 2) {
          if(!is.null(input$cie_columna)) {
            if(!RMedic_general()) {
              
              dt_ok <- FALSE
              frase_salida <- frase_FALSE
              
              mi_salida <- HTML(frase_salida)
              
              my_exit <- list(dt_ok,  mi_salida)
              return(my_exit)
              
            } else return(NULL)
          } else return(NULL)
        } else return(NULL)
      } else return(NULL)
    } else return(NULL)
    
    
  })
  
  # Contorl 06 - Que la columna elegida pertenezca a la base de datos
  Control06_1 <- reactive({ 
    

    AllEyesOnMe(ListBase = BaseSalida(), the_col = input$var1_control)
      
    # dt_ok <- FALSE
    # 
    # if(!is.null(BaseSalida()))
    #   if(!is.null(input$var1_control))
    #     if(input$var1_control != "")
    #       if(sum(colnames(BaseSalida()[[1]]) == input$var1_control) > 0)
    #         dt_ok <- TRUE
    # 
    # 
    # dt_ok
   
    
  })
  
  Control06_2 <- reactive({ 
    
    
    dt_ok <- FALSE
    
    if(!is.null(BaseSalida())) 
      if(!is.null(input$var2_control)) 
        if(input$var2_control != "")
          if(sum(colnames(BaseSalida()[[1]]) == input$var2_control) > 0)
            dt_ok <- TRUE
    
    
    dt_ok
    
  })
  
  
  status_BaseSalida <- reactive({
    
    if (!is.null(BaseSalida())) {
      if (ncol(BaseSalida()[[1]]) > 0) {
        
        paso <- TRUE
        
      } else paso <- FALSE
    } else paso <- FALSE
    
    paso
  })
  
  ###
  } # Fin Seccion 02
  ########################################################
  
  
  
  # Seccion 03 - Base de Datos
  {
  ###
    
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
  
  
  # Carga de la Toda la base de datos
  Base01 <- reactive({
    
    if(!is.null(input$FileTypePicker)){
      
      if(!is.null(Control01())) {
        if(Control01()[[1]]) {
          
          if(input$FileTypePicker == "Excel") { 
            
            if (!is.null(input$xls_file)) {
              
              # Detalles varios...
              inFile <- input$xls_file
              
              # La direccion del archivo...
              temporal_file <- inFile$datapath
              
              # cat("inFile: ", inFile, "\n" )
              # cat("archivo: ", archivo, "\n" )
              # cat("archivo2: ", input$xls_file[[1]], "\n" )
              # cat("temporal_file: ", temporal_file, "\n" )
              
              # 1) DataSet
              library(readxl)
              DataSet <- as.data.frame(read_excel(temporal_file, col_names= TRUE, sheet = 1, trim_ws = FALSE))
              
              } else return(NULL)
            
            } else
              if(input$FileTypePicker == "CSV") { 
                
                if (!is.null(input$csv_file)) {
                  
                  # Detalles varios de direccion
                  inFile <- input$csv_file
                  
                  # La carga de datos formato CSV
                  DataSet <- read.csv(inFile$datapath, header=input$header, sep=input$sep, dec=input$dec, quote=input$quote)
                  
                } else return(NULL)
                
              } else 
                if(input$FileTypePicker == "Ejemplos") { 
                  
                  
                  if (!is.null(input$ejemplo_file)) {
                    
                    
                    # cat("ejemplo_file: ", input$ejemplo_file, "\n")
                    # La carga de datos formato CSV
                    DataSet <- eval(parse(text = input$ejemplo_file))
          
                  }  else return(NULL)
                }  else return(NULL)
                  
                  # Salida
                  my_exit <- list(DataSet)
                  return(my_exit)
                
        } else return(NULL)
        
          } else return(NULL)
      
          } else return(NULL)
    
    
  })

  Base02 <- reactive({
    
    if(!is.null(Base01())){
      
      if(!is.null(Control02())){
        
        if(!is.null(Control03())){
          
          mi_filtro <- Base01()[[1]][,input$cie_columna]
          dt_filtro <- mi_filtro == input$cie_categoria
          base2 <- Base01()[[1]][dt_filtro, ]
          
          my_exit <- list(base2)
          return(my_exit)
          
        } else return(NULL)
        
      } else return(NULL)
    } else return(NULL)   
    
    
    
  })
  
  
  BaseSalida <- reactive({
    
    if (!is.null(Base01())) {
      
      if(input$cie_especificaciones == 1) Base01() else
        if((input$cie_especificaciones == 2) && (!is.null(Base02()))) Base02()
      
    } else return(NULL)    
  })
  

  output$TextBase_Alert01 <- renderText({
    if (!is.null(Control01())) {
      # Tab01_Base()[[3]]
      Control01()[[2]]
      #  "AVER"
    } else return(NULL)
  })
  
  
  output$TextBase_Alert02 <- renderText({
    if (!is.null(Control02())) {
      # Tab01_Base()[[3]]
      Control02()[[2]]
      #  "AVER"
    } else return(NULL)
  })
  
  output$TextBase_Alert03 <- renderText({
    if (!is.null(Control03())) {
      # Tab01_Base()[[3]]
      Control03()[[2]]
      #  "AVER"
    } else return(NULL)
  })
  
  output$TextBase_Alert04 <- renderText({
    if (!is.null(Control04())) {
      
      Control04()[[2]]
      
    } else return(NULL)
  })
  
  output$TextBase_Alert05 <- renderText({
    if (!is.null(Control05())) {
      
      Control05()[[2]]
      
    } else return(NULL)
  })
  
  
  output$TextBase_InfoDataSet <- renderUI({
    
    texto_salida <- c()
    
    if (!is.null(Base01())) {
      if(RMedic_general()){
        
        
        texto_completo01 <- c("<b>Base:</b> _mi_archivo_ <br/>
                           <b>Variables (Columnas):</b> _ncolBase01_ variables.<br/>
                           <b>Unidades (Filas o repeticiones):</b> _nrowBase01_ unidades.<br/>")
        
        texto_salida <- texto_completo01
        
        
        
        if (!is.null(Base02()) && !is.null(input$cie_categoria)) {
          
          texto_completo02 <- c("
                            <b>Base:</b> _mi_archivo_ <br/>
                            <b>Variables (Columnas):</b> _ncolBase01_ variables.<br/>
                            <b>Unidades totales (Filas totales o repeticiones totales):</b> _nrowBase01_ unidades.<br/>
                            <br/>
                            <b>Criterio de Inclusión Estadístico (CIE):</b> _mi_CIE_.<br/>
                            <b>Categoría de Inclusión:</b> la categoría seleccionada es '_mi_categoria_'.<br/>
                            <b>Unidades seleccionadas (Filas seleccionadas o repeticiones seleccionadas):</b> _nrowBase02_ de _nrowBase01_ unidades.<br/>
                              ")
          
          agregado01 <- c("
                        <b>Nota Importante:</b> aplicando el criterio de inclusión estadístico sobre las
                        _nrowBase01_ unidades totales son seleccionadas e ingresan efectivamente a
                        tablas, gráficos y test estadístisticos solo _nrowBase02_ unidades. Estas _nrowBase02_ unidades
                        presentan la categoría '_mi_categoria_' en el CIE _mi_CIE_.
                        ")
          
          agregado02 <- c("
                        <b>Nota Importante:</b> Todas las unidades responden a la misma
                        categoría de inclusión. Trabajar con este criterio de inclusión
                        o directamente con el total de la base de datos, es lo mismo.
                        ")
          
          texto_salida <- texto_completo02
          
          if(nrow(Base01()[[1]]) != nrow(Base02()[[1]])) texto_salida <- paste0(texto_salida, "<br/>", agregado01) else texto_salida <- paste0(texto_salida, "<br/>", agregado02)
          
          
          
          dt_col <- colnames(Base02()[[1]]) == input$cie_columna
          #   cat(input$cie_categoria, "\n")
          #   cat(dt_col, "\n")
          orden_col <- c(1:length(dt_col))[dt_col]
          armado <- paste0("Variable '", input$cie_columna, "' - Letra Columna '",
                           num2let(orden_col), "'")
          
          #  armado <- "AVERRERRRRR"
          texto_salida <- gsub("_mi_CIE_", armado, texto_salida)
          
          texto_salida <- gsub("_mi_categoria_", input$cie_categoria, texto_salida)
          texto_salida <- gsub("_nrowBase02_", nrow(Base02()[[1]]), texto_salida)
          
        }
        
        #  HTML(paste(t1, t2, sep = '<br/>'))
        
        
        texto_salida <- gsub("_mi_archivo_", MyFileName(),texto_salida)
        texto_salida <- gsub("_ncolBase01_", ncol(Base01()[[1]]),texto_salida)
        texto_salida <- gsub("_nrowBase01_", nrow(Base01()[[1]]),texto_salida)
        
        mi_salida <- HTML(texto_salida)
        mi_salida
      } else return(NULL)
    } else return(NULL)
  })
  
  
  output$TextBase_Intro <- renderText({
    if (!is.null(Base01())) {
      if(RMedic_general()){
        "Visualización de la Base de Datos"
      } else return(NULL)
    } else return(NULL)
  })
  
  
  
  output$BASE_SALIDA <- renderDataTable({
    
    if (status_BaseSalida()) {
      
      #cat("RMedic_general(): ", RMedic_general(), "\n")
      if(RMedic_general()){
        
        
        
        #   rownames(mi_base) <- rep("", nrow(mi_base))
        # mi_base
        #  cantidad_columnas <-  ncol(Tab01_Base()[[2]][3])
        # cantidad_columnas <-  ncol(mi_base)
        # 
        # sketch = htmltools::withTags(table(
        #   class = 'display',
        #   thead(
        #     tr(
        #        lapply(num2let(c(1:(cantidad_columnas)), th, colspan = 1)
        #      ),
        #      tr(
        #        lapply(c(colnames(mi_base)), th)
        #      )
        #    )
        #  )
        # ))
        
        
        
        #container = sketch,
        datatable(BaseSalida()[[1]], rownames = F,  options = list(
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#fff', 'color': '#000'});",
            "}"), language = list(
              search = "Búsqueda:",
              lengthMenu = "Mostrar _MENU_ registros",
              info = "Mostrando registros del _START_ al _END_ de un total de _TOTAL_ registros",
              infoFiltered = "(filtrados de un total de _MAX_ registros)",
              paginate = list(previous =    "Anterior", `next` = "Siguiente")
            )
        ))
        
        
        # sketch = htmltools::withTags(table(
        #   tableHeader(iris),
        #   tableFooter(iris)
        # ))
        # datatable(
        #   head(iris, 10),
        #   container = sketch, options = list(pageLength = 5, dom = 'tip'), rownames = FALSE
        # )
      } else return(NULL)
    } else return(NULL)
  })
  
  
  menuBASE <- reactive({
    
    
    tabs <- list()
    
    tabs[[1]] <-    tabPanel(title = "Base de Datos", 
                             icon = icon("user-md"), 
                             value = 1,
                             br(),
                             fluidRow(
                               #   column(4, OpcionesDeCarga),
                               column(8, 
                                      h3(textOutput("TextBase_Alert01")), 
                                      h3(textOutput("TextBase_Alert02")), 
                                      h3(textOutput("TextBase_Alert03")),
                                      h3(textOutput("TextBase_Alert04")),
                                      h3(htmlOutput("TextBase_Alert05")),
                                      htmlOutput("TextBase_InfoDataSet"), br(),
                                      h3(textOutput("TextBase_Intro")),
                                      dataTableOutput('BASE_SALIDA')
                               ),
                               
                             ),
                             br(), br()
    )
    
    tabs
    
  })
  
  ###
  } # End Seccion 03
  #############################################
  
  
  
  # Seccion 04 - Control de RMedic
  {
  ###
    menuCONTROL <- reactive({
      
      if (status_BaseSalida()){
        
        the_text_control <-  ModifyMe(the_text = TextServer_Variables, end_var = "_control")
        
        eval(parse(text = the_text_control))
        
     
         
        
        
        tabs <- list()
        
        
        tabs[[1]] <-  tabPanel(
          title = "Control", 
          icon = icon("user-md"), 
          value = 2,
          eval(parse(text = gsub("_control", "_control",TextUI_Variables)))
               ) # End TabPanel
        
        
        
        tabs
        
      } else return(NULL)
      
    }) 
    
  ###  
  } # End Seccion 04 - Control de RMedic
  ############################################
  
  
  # Seccion 05 - Tablas
  {
  ###
    
    menuTABLAS <- reactive({
      
      if (status_BaseSalida()){
        
 
        
        the_text_tablas <-  ModifyMe(the_text = TextServer_Variables, end_var = "_tablas")
          
        eval(parse(text = the_text_tablas))
        
        
        
        
        
        
        
        # 
        ##################################
        tabs <- list()
        
        
        tabs[[1]] <-  tabPanel(
          title = "Tablas", 
          icon = icon("user-md"), 
          value = 3,
          eval(parse(text = gsub("_control", "_tablas",TextUI_Variables)))
        )
        
        
        
        tabs
        
      } else return(NULL)
      
    }) 
    
  ###  
  } # End Seccion 05 - Tablas
  ##################################
  
  
 
  
  # Seccion 06 - Graficos
  {
    ###
    
    menuGRAFICOS <- reactive({
      
      if (status_BaseSalida()){
        
        
        
        the_text_graficos <-  ModifyMe(the_text = TextServer_Variables, end_var = "_graficos")
        
        eval(parse(text = the_text_graficos))
        
        
        
        
        
        
        
        # 
        ##################################
        tabs <- list()
        
        
        tabs[[1]] <-  tabPanel(
          title = "Graficos", 
          icon = icon("user-md"), 
          value = 4,
          eval(parse(text = gsub("_control", "_graficos",TextUI_Variables)))
        )
        
        
        
        tabs
        
      } else return(NULL)
      
    }) 
    
    ###  
  } # End Seccion 06 - Graficos
  ##################################
  
  
  # Seccion 07 - Ho
  {
    ###
    
    menuHO <- reactive({
      
      if (status_BaseSalida()){
        
        
        
        the_text_ho <-  ModifyMe(the_text = TextServer_Variables, end_var = "_ho")
        
        eval(parse(text = the_text_ho))
        
        
        
        
        
        
        
        # 
        ##################################
        tabs <- list()
        
        
        tabs[[1]] <-  tabPanel(
          title = "Pruebas de Hipótesis", 
          icon = icon("user-md"), 
          value = 5,
          eval(parse(text = gsub("_control", "_ho",TextUI_Variables)))
        )
        
        
        
        tabs
        
      } else return(NULL)
      
    }) 
    
    ###  
  } # End Seccion 07 - Ho
  ##################################
  
  
  # Seccion 08 - Sobrevida
  {
    ###
    
    menuSOBREVIDA <- reactive({
      
      if (status_BaseSalida()){
        
        
        # # # Esta es server para las variables igual que las otras pestanias
        # the_text_sobrevida <-  ModifyMe(the_text = TextServer_Variables, end_var = "_sobrevida")
        # eval(parse(text = the_text_sobrevida))
        
        
        # # # Pasa que sobrevida necesita otra forma de seleccion de variables,
        # # # a si que lo vamos a armar diferente. Especialmente para esta parte.
        
        
        
        
        
        
        # 
        ##################################
        tabs <- list()
        
        
        tabs[[1]] <-  tabPanel(
          title = "Sobrevida", 
          icon = icon("user-md"), 
          value = 6,
          br()
          
        )
        
        
        
        tabs
        
      } else return(NULL)
      
    }) 
    
    ###  
  } # End Seccion 07 - Ho
  ##################################
  
  
  observe(output[["RMedicSoft"]] <- renderUI({
    
    # tabs1 <- menuBASE()
    # tabs2 <- menuCONTROL()
    # tabs3 <- menuDESCRIPTIVAS()
    # tabs4 <- menuGRAFICOS()
    # tabs5 <- menuHO()
    # tabs6 <- menuSOBREVIDA()
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
  
}