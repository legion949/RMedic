
source("lib.R")

#source("uiCode.R")
source("script/PreServerCode.R")


function(input, output, session) {
  


  observeEvent(input$toggleSidebar, {
    shinyjs::toggle(id = "MySidebar")
  })

  observeEvent(input$MiniButton, {
    shinyjs::toggle(id = "MySidebar")
  })
  
  RMedic_general <- reactiveVal(T)
  reseteo_conteo <- reactiveVal(0)
  
  
  # Seccion 01 - File Selection y CIE
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
      actionButton("cargar", "Aplicar CIE", icon("paper-plane"), #class = "btn-warning", 
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
      
    )
    
        } else return(NULL)
      } else return(NULL)
    } else return(NULL)
    } else return(NULL)
  })
  
  
  zocalo_CIE <- reactive({
    
    if(!is.null(input$cie_columna)) paste0("<b>CIE: </b>", input$cie_columna, " - Columna ",
                                           MyLetter(ListBase = BaseSalida(), 
                                                    the_col = input$cie_columna),"<br/>",
                                           "<b>Categoría del CIE: </b>", input$cie_categoria) else NULL
    
  })
  
  
  # Variable criterio de inclusion
  observeEvent(reseteo_conteo(),{
    
    # Reset selected step
    # # RMedic_general(F)
    # freezeReactiveValue(input, "cie_especificaciones")
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
  # source("script/01_BaseDeDatos/ServerBaseDeDatos.R", local = T)
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
          #   head(iris, ),
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
    
    Variables_Tablas <- reactive({
      
  #    TextServer_ValidacionVariables <- '
      # Este objeto reativo calcula 3 cosas:
      # Cosa 1) Un objeto llamado "ok_tablas"
      # Cosa 2) Un objeto llamado "caso_tablas"
      # Cosa 3) Un objeto llamada "mis_variables"
      
      # Cosa 1) Un objeto llamado "ok_tablas"
      # Este objeto es logico. Tiene un TRUE si hay efectivamente
      # variables ya seleccionadas o un FALSE si todavia nada ha sido
      # seleccionado. Por defecto es FALSE.
      ok_tablas <- F
      
      

      # Cosa 2) Un objeto llamado "caso_tablas"
      # Hay 5 casos. Hay que ver en cual estamos.
      # Vamos a usar esto como director para que solo genere las
      # tablas que deben ser generadas
      # # Caso Tablas
      # 1) Una variable cualitativa
      # 2) Una variable cuantitativa
      # 3) Dos variables cualitativas
      # 4) Dos variables cuantitativas
      # 5) Una y una
      tipo_variables_tablas <- c(NA, NA)
      caso_tablas <- c(NA)
      numeros_tipo_variables_tablas <- c(NA, NA)
      
      # Cosa 3) Un objeto llamada "mis_variables"
      # Aqui vamos a detallar las variables seleccionadas, y lo vamos a usar
      # para armar la minibase de datos para tablas.
      variables_tablas <- c(NA, NA)  
      eyes_tablas <- c(NA, NA)
      zocalo_tablas <- c(NA, NA)
  
    #  cat("input$PanelRMedic: ", input$PanelRMedic, "\n")
      
  
      if(!is.null(input$qtty_var_tablas)) {
        if(input$qtty_var_tablas >= 1) {
    if(!is.null(input$var1_tablas)) {
      if(input$var1_tablas != "") {
        if(!is.null(input$tipo_var1_tablas)) {
          if(!is.na(input$tipo_var1_tablas)) {
            if(input$tipo_var1_tablas != "") {
        
    variables_tablas[1] <- input$var1_tablas
    tipo_variables_tablas[1] <- input$tipo_var1_tablas
    eyes_tablas[1] <- AllEyesOnMe(ListBase = BaseSalida(), the_col = input$var1_tablas)
    zocalo_tablas[1] <- paste0("<b>Variable 1:</b> ", input$var1_tablas, " - Columna ", 
                               MyLetter(ListBase = BaseSalida(), the_col = input$var1_tablas))
    
    if (tipo_variables_tablas[1] == "Categórica") numeros_tipo_variables_tablas[1] <- 1 else
      if (tipo_variables_tablas[1] == "Numérica") numeros_tipo_variables_tablas[1] <- 10
            }   
    }
          }
        }
    }
        }
      }
      
      
      if(!is.null(input$qtty_var_tablas)) {
        if(input$qtty_var_tablas >= 2) {
      if(!is.null(input$var2_tablas)) {
        if(input$var2_tablas != "") {
          if(!is.null(input$tipo_var2_tablas)) {
            if(!is.na(input$tipo_var2_tablas)) {
              if(input$tipo_var2_tablas != "") {
                
                variables_tablas[2] <- input$var2_tablas
                tipo_variables_tablas[2] <- input$tipo_var2_tablas
                eyes_tablas[2] <- AllEyesOnMe(ListBase = BaseSalida(), the_col = input$var2_tablas)
                
               
                zocalo_tablas[2] <- paste0("<b>Variable 2:</b> ", input$var2_tablas, " - Columna ", 
                                           MyLetter(ListBase = BaseSalida(), the_col = input$var2_tablas))
                
                
                if (tipo_variables_tablas[2] == "Categórica") numeros_tipo_variables_tablas[2] <- 1 else
                  if (tipo_variables_tablas[2] == "Numérica") numeros_tipo_variables_tablas[2] <- 10        
              }   
            }
          }
        }
      }
        }
      }

      
      variables_tablas <- na.omit(variables_tablas)
      tipo_variables_tablas <- na.omit(tipo_variables_tablas)
      numeros_tipo_variables_tablas <- na.omit(numeros_tipo_variables_tablas)
      eyes_tablas <- na.omit(eyes_tablas)
      zocalo_tablas <- na.omit(zocalo_tablas)
   
      
      if (length(numeros_tipo_variables_tablas) > 0) {
    resultados_posibles <- c(1, 10, 2, 20, 11)
    casos_posibles <- c(1:length(resultados_posibles))
    dt_caso <- sum(numeros_tipo_variables_tablas) == resultados_posibles
    caso_tablas <- casos_posibles[dt_caso]
      }
      
      
      
      
   #   if (length(variables_tablas) ==  input$qtty_var_tablas){
         if(sum(eyes_tablas) == input$qtty_var_tablas) {
        ok_tablas <- T
      }  else {
        
        ok_tablas <- F
        tipo_variables_tablas <- c()
        caso_tablas <- c()
        numeros_tipo_variables_tablas <- c()
        variables_tablas <- c()
        eyes_tablas <- c()
        zocalo_tablas <- c()
  
      }
    
  
        
    
  
      #tipo_variables_tablas
      # cat("variables_tablas: ",variables_tablas, "\n")
      # cat("tipo_variables_tablas: ", tipo_variables_tablas, "\n")
      # cat("numeros_tipo_variables_tablas: ", numeros_tipo_variables_tablas, "\n")
      # cat("caso_tablas: ", caso_tablas, "\n")
      # cat("eyes_tablas: ", eyes_tablas, "\n")
      # cat("zocalo_tablas: ", zocalo_tablas, "\n\n\n")
      
      my_exit <- list(ok_tablas, variables_tablas, tipo_variables_tablas, 
                      caso_tablas,
                      eyes_tablas, zocalo_tablas)
      
      my_exit
  #    '
      
     # eval(parse(text = TextServer_ValidacionVariables))
      
    })
    
  
  
    output$zocalo_Tablas <- renderText({
      
      if (Variables_Tablas()[[1]]) {
        armado <- c(Variables_Tablas()[[6]][1], Variables_Tablas()[[6]][2], zocalo_CIE())
    
        armado <- na.omit(armado)
        
        armado <- paste0(armado, sep ="<br/>")
      
        armado <- HTML(armado)
        } else return(NULL)
    })
    
    
    output$download_excel <- downloadHandler(
      filename = function() {
        paste0("RMedic - ", MyDate(), ".xlsx")
      },
      content = function(file) {
        my_workbook <- createWorkbook()
        
        addWorksheet(
          wb = my_workbook,
          sheetName = paste0("RMedic - Prueba")
        )
        
        setColWidths(
          my_workbook,
          1,
          cols = 1:10,
          widths = rep(20, 10)
        )
        
        writeData(
          my_workbook,
          sheet = 1,
          c(
            c("RMedic - HOLA ARN!!!! - VLL!!!", paste0("Descarga fecha: ", Sys.time()))
          ),
          startRow = 2,
          startCol = 1
        )
        
        addStyle(
          my_workbook,
          sheet = 1,
          style = createStyle(
            fontSize = 24,
            textDecoration = "bold"
          ),
          rows = c(2,3),
          cols = 1
        )
        
        writeData(
          my_workbook,
          sheet = 1,
          c(
            "Medidas de Posición"
          ),
          startRow = 5,
          startCol = 1
        )
        
        addStyle(
          my_workbook,
          sheet = 1,
          style = createStyle(
            fontSize = 24,
            textDecoration = "bold"
          ),
          rows = c(5,9,13,17),
          cols = 1
        )
        
        writeData(
          my_workbook,
          sheet = 1,
          Reactive_tabla_1c_RMedic()[[1]],
          startRow = 6,
          startCol = 1
        )
        #############################
        writeData(
          my_workbook,
          sheet = 1,
          c(
            "Medidas de Dispersión"
          ),
          startRow = 9,
          startCol = 1
        )
        writeData(
          my_workbook,
          sheet = 1,
          Reactive_tabla_1c_RMedic()[[2]],
          startRow = 10,
          startCol = 1
        )
        #############################
        
        #############################
        writeData(
          my_workbook,
          sheet = 1,
          c(
            "Percentiles"
          ),
          startRow = 13,
          startCol = 1
        )
        writeData(
          my_workbook,
          sheet = 1,
          Reactive_tabla_1c_RMedic()[[3]],
          startRow = 14,
          startCol = 1
        )
        #############################
        
        #############################
        writeData(
          my_workbook,
          sheet = 1,
          c(
            "Intervalos de Confianza"
          ),
          startRow = 17,
          startCol = 1
        )
        writeData(
          my_workbook,
          sheet = 1,
          Reactive_tabla_1c_RMedic()[[4]],
          startRow = 18,
          startCol = 1
        )
        #############################
        
        addStyle(
          my_workbook,
          sheet = 1,
          style = createStyle(
            fontSize = 13,
            fgFill = "#1a5bc4",
            halign = "center",
            fontColour = "#ffffff"
          ),
          rows = c(6, 10, 14, 18),
          cols = 1:6,
          gridExpand = TRUE
        )
        
        #c(2,6,10,14)
        addStyle(
          my_workbook,
          sheet = 1,
          style = createStyle(
            fontSize = 13,
            fgFill = "#7dafff",
            halign = "center",
          ),
          rows = c(7, 11, 15, 19:21),
          cols = 1:6,
          gridExpand = TRUE
        )
        
        saveWorkbook(my_workbook, file)
      }
    )
    
    # Generacion de Tablas...
    # # # q, c, qq, cc, qc...
    # # # Objetos Reactivos y Outputs...
    # source("script/03_Tablas/TablasRMedic.R", local = T)
    
    
    # Tablas para 1 variable numerica (c) - Reactiave()!
    {
      ### 
      
      
      Reactive_tabla_1q_RMedic <- reactive({
      #  if (Variables_Tablas()[[1]]) {
        if (!is.null(pos_planeta())) {
          
       #   cat("Hola", "\n")
         salida <-  RMedic_1q_tablas(Base_Planeta(), decimales_planeta())
          salida[[1]][,2] <- as.character(salida[[1]][,2])
          salida[[1]][,3] <- as.character(salida[[1]][,3])
          
          salida
          
       #   cat("Chau", "\n")
        } else return(NULL)
      })
      
   
    observe(
        output$Salida_tabla_1q_RMedic_01 <- renderTable(digits = decimales_planeta(),
                                                        align= "c",{
                                                          
                                                          if(!is.null(Reactive_tabla_1q_RMedic())) {
                                                           # Reactive_tabla_1q_RMedic()[[1]][[2]]
                                                            Reactive_tabla_1q_RMedic()[[1]]
                                                            
                                                          } else return(NULL)
                                                        })
      )
      
      observe(
        output$Salida_tabla_1q_RMedic_02 <- renderTable(digits=decimales_planeta(), 
                                                        align= "c",{
                                                          
                                                          if(!is.null(Reactive_tabla_1q_RMedic())) {
                                                            # Reactive_tabla_1q_RMedic()[[2]][[2]]
                                                            Reactive_tabla_1q_RMedic()[[2]]
                                                          } else return(NULL)
                                                        })
      )
      # 
      # 
      observe(
        output$Salida_tabla_1q_RMedic_03 <- renderTable(digits=decimales_planeta(),
                                                        align= "c",{
                                                          
                                                          if(!is.null(Reactive_tabla_1q_RMedic())) {
                                                            # Reactive_tabla_1q_RMedic()[[3]][[2]]
                                                            Reactive_tabla_1q_RMedic()[[3]]
                                                          } else return(NULL)
                                                        })
      )
      # 
      # 
      # 
      observe(
        output$Salida_tabla_1q_RMedic_04 <- renderTable(digits=decimales_planeta(), align= "c",{
          
          if(!is.null(Reactive_tabla_1q_RMedic())) {
           #  Reactive_tabla_1q_RMedic()[[4]][[2]]
            Reactive_tabla_1q_RMedic()[[4]]
          } else return(NULL)
        })
      )
      
      
      output$MegaSalida_tabla_1q_RMedic <- renderUI ({
        
        if(!is.null(Reactive_tabla_1q_RMedic())) {
          
          div(
            
            lapply(1:length(Reactive_tabla_1q_RMedic()), function(i) {
              nombre_fusion <- paste0('Salida_tabla_1q_RMedic_', CifrasPerfectas(i)) 
              div(
                h3(names(Reactive_tabla_1q_RMedic())[i]),
                tableOutput(nombre_fusion), br()
              )
            })
            
            # h3(names(Reactive_tabla_1q_RMedic())[1]),
            # tableOutput("Salida_tabla_1q_RMedic_01"), br(),
            # h3(names(Reactive_tabla_1q_RMedic())[2]),
            # tableOutput("Salida_tabla_1q_RMedic_02"), br(),
            # h3(names(Reactive_tabla_1q_RMedic())[3]),
            # tableOutput("Salida_tabla_1q_RMedic_03"), br(),
            # h3(names(Reactive_tabla_1q_RMedic())[4]),
            # tableOutput("Salida_tabla_1q_RMedic_04")
          )
          
        } else return(NULL)
      })
      
      
      ###
    } # End Tablas para 1 variable numerica (c) - Reactiave()!
    ###############################################################
    
    
    # Tablas para 1 variable categorica (q) - Reactiave()!
    {
      ###
      
      Reactive_tabla_1c_RMedic <- reactive({
       # if (Variables_Tablas()[[1]]) {
        if (!is.null(pos_planeta())) {
          if(!is.null(Base_Planeta())){
          
       #   cat("Hola", "\n")
       #   cat(colnames(Base_Planeta()), "\n")
       #   cat(nrow(Base_Planeta()), "\n")
        #  cat(sum(is.na(Base_Planeta())), "\n\n")
          tablas <- RMedic_1c_tablas(input_base = Base_Planeta(),
                                     input_decimales = decimales_planeta(),
                                     input_min = input$x_min,
                                     input_max = input$x_max,
                                     input_breaks = input$x_breaks,
                                     input_side = input$x_side
                                       )
          tablas[[8]][,2] <- as.character(tablas[[8]][,2])
          tablas[[8]][,3] <- as.character(tablas[[8]][,3])
       #   cat("CAHU", "\n")
          tablas
          
          } else return(NULL)
        } else return(NULL)
      })
      


        
        observe( 
          # Medidas de Posicion
          output$Salida_tabla_1c_RMedic_01 <- renderTable(digits=decimales_planeta(), align= "c",{
            
            if(!is.null(Reactive_tabla_1c_RMedic())) {
              Reactive_tabla_1c_RMedic()[[1]]
            } else return(NULL)
          })
        )
        
        
        
        observe( 
          # Medidas de Posicion
          output$Salida_tabla_1c_RMedic_02 <- renderTable(digits=decimales_planeta(), align= "c",{
            
            if(!is.null(Reactive_tabla_1c_RMedic())) {
              Reactive_tabla_1c_RMedic()[[2]]
            } else return(NULL)
          })
        )
        
        observe(
          # Medidas de Dispersion
          output$Salida_tabla_1c_RMedic_03 <- renderTable(digits=decimales_planeta(), align= "c",{
            
            if(!is.null(Reactive_tabla_1c_RMedic())) {
              Reactive_tabla_1c_RMedic()[[3]]
            } else return(NULL)
          })
        )
        
        
        # Percentiles
        observe(
          output$Salida_tabla_1c_RMedic_04 <- renderTable(digits=decimales_planeta(), align= "c",{
            
            if(!is.null(Reactive_tabla_1c_RMedic())) {
              Reactive_tabla_1c_RMedic()[[4]]
            } else return(NULL)
          })
        )
        
        
        # IC
        observe( 
          output$Salida_tabla_1c_RMedic_05 <- renderTable(digits= decimales_planeta(), align= "c",{
            
            if(!is.null(Reactive_tabla_1c_RMedic())) {
              Reactive_tabla_1c_RMedic()[[5]]
            } else return(NULL)
          })
        )
        
      
        # IC
        observe( 
          output$Salida_tabla_1c_RMedic_06 <- renderTable(digits= decimales_planeta(), align= "c",{
            
            if(!is.null(Reactive_tabla_1c_RMedic())) {
              Reactive_tabla_1c_RMedic()[[6]]
            } else return(NULL)
          })
        )
        
  
        # IC
        observe( 
          output$Salida_tabla_1c_RMedic_07 <- renderTable(digits= decimales_planeta(), align= "c",{
            
            if(!is.null(Reactive_tabla_1c_RMedic())) {
              Reactive_tabla_1c_RMedic()[[7]]
            } else return(NULL)
          })
        )
        
        
        # IC
        observe( 
          output$Salida_tabla_1c_RMedic_08 <- renderTable(digits= decimales_planeta(), align= "c",{
            
            if(!is.null(Reactive_tabla_1c_RMedic())) {
              Reactive_tabla_1c_RMedic()[[8]]
            } else return(NULL)
          })
        )
        
        
        output$Controlador_1c_RMedic <- renderUI({
          
          div(
            fluidRow(
              column(4,
            numericInput(
              inputId = "x_min",
              label = "Valor mínimo",
              value = min(na.omit(Base_Planeta()[,1])),
              min = NA,
              max = min(na.omit(Base_Planeta()[,1])),
              step = 0.01,
              width = NULL
            ),
            numericInput(
              inputId = "x_max",
              label = "Valor máximo: ",
              value = max(na.omit(Base_Planeta()[,1])),
              min = max(na.omit(Base_Planeta()[,1])),
              max = NA,
              step = 0.01,
              width = NULL
            ),
            numericInput(
              inputId = "x_breaks",
              label = "Cantidad de intervalos: ",
              value = nclass.Sturges(na.omit(Base_Planeta()[,1])),
              min = 1,
              max = NA,
              step = 1,
              width = NULL
            )
            ),
            column(3,
            radioButtons(inputId = "x_side", 
                         label = "Cierre", choices = c("Derecha" = T , "Izquierda" = F)
            )
            )
            )
          )
          
        })
        
        output$MegaSalida_tabla_1c_RMedic <- renderUI ({
          
          if(!is.null(Reactive_tabla_1c_RMedic())) {
            
            div(
              
              lapply(1:length(Reactive_tabla_1c_RMedic()), function(i) {
                nombre_fusion <- paste0('Salida_tabla_1c_RMedic_', CifrasPerfectas(i)) 
                div(
                  h3(names(Reactive_tabla_1c_RMedic())[i]),
                  tableOutput(nombre_fusion), br()
                )
              }),
              
             
            )
            
          } else return(NULL)
        })
        
        
      ###
    } # Fin Tablas para 1 variable categorica (q) - Reactiave()!
    ################################################################################
    
    
    
     
    
    # 2 Variable Categoricas (QQ)
    {
      ###
      
   
      
      Reactive_tabla_2q_RMedic <- reactive({
        if (paso_BASE(Base_Planeta())) {
          
          
          general <- df02(Base_Planeta(), decimales_planeta())$df02
          
          
          armado <- list()
          
          #Clasico
          armado[[1]] <- list("Frecuencias Absolutas",
                              general[[1]][[1]])
          
          armado[[2]] <- list("Cociente al Total",
                              general[[1]][[2]])
          
          armado[[3]] <- list("Frecuencias Relativas al Total",
                              general[[1]][[3]])
          
     
          armado[[4]] <- list("Porcentajes al Total",
                              general[[1]][[4]])
          
          # Al total
          armado[[5]] <- list("Frecuencias Absolutas",
                              general[[2]][[1]])
          
          armado[[6]] <- list("Cociente al Total",
                              general[[2]][[2]])
          
          armado[[7]] <- list("Frecuencias Relativas al Total",
                              general[[2]][[3]])
          
          
          armado[[8]] <- list("Porcentajes al Total",
                              general[[2]][[5]])
          
          # Al Por Fila
          armado[[9]] <- list("Frecuencias Absolutas por Filas",
                              general[[3]][[1]])
          
          armado[[10]] <- list("Cociente por Filas",
                              general[[3]][[2]])
          
          armado[[11]] <- list("Frecuencias Relativas por Filas",
                              general[[3]][[3]])
          
          
          armado[[12]] <- list("Porcentajes por Filas",
                              general[[3]][[5]])
          
          # Al Por Columna
          armado[[13]] <- list("Frecuencias Absolutas por Columnas",
                              general[[4]][[1]])
          
          armado[[14]] <- list("Cociente por Columnas",
                               general[[4]][[2]])
          
          armado[[15]] <- list("Frecuencias Relativas por Columnas",
                               general[[4]][[3]])
          
          
          armado[[16]] <- list("Porcentajes por Columnas",
                               general[[4]][[5]])
          
          # Simple Entrada
          armado[[17]] <- list("Simple Entrada",
                               general[[6]])
          
          
          armado
        } else return(NULL)
      })
      
      
      output$Menu_tabla_2q_RMedic <- renderUI({
        tabsetPanel(id= "kayak",
                    tabPanel("Clásico", value = 1),
                    tabPanel("Por filas", value = 3),
                    tabPanel("Por columnas", value = 4),
                    tabPanel("Al Total", value = 2),
                    tabPanel("Simple entrada", value = 5)
        )
      })
      
      # Clasico...
      {
      ###
        
      # # Frecuencias Absolutas
      observe(  
        output$Salida_tabla_2q_RMedic_01 <- renderTable(digits=0, rownames = TRUE, align= "c",{
          
          if(!is.null(Reactive_tabla_2q_RMedic())) {
            Reactive_tabla_2q_RMedic()[[1]][[2]]
          } else return(NULL)
        })
      )
      
      # # Cociente al Total
      observe(  
        output$Salida_tabla_2q_RMedic_02 <- renderTable(digits=decimales_planeta(),rownames = TRUE, align= "c", {
          
          if(!is.null(Reactive_tabla_2q_RMedic())) {
            Reactive_tabla_2q_RMedic()[[2]][[2]]
          } else return(NULL)
        })
      )
      
      # # Frecuencias Relativas al Total
      observe(  
        output$Salida_tabla_2q_RMedic_03 <- renderTable(digits=decimales_planeta(), rownames = TRUE, align= "c",{
          
          if(!is.null(Reactive_tabla_2q_RMedic())) {
            Reactive_tabla_2q_RMedic()[[3]][[2]]
          } else return(NULL)
        })
      )
      
      # # Porcentajes al Total
      observe(  
        output$Salida_tabla_2q_RMedic_04 <- renderTable(digits=decimales_planeta(), rownames = TRUE, align= "c",{
          
          if(!is.null(Reactive_tabla_2q_RMedic())) {
            Reactive_tabla_2q_RMedic()[[4]][[2]]
          } else return(NULL)
        })
      )
      
      ###
      } # End Clasico
      ##########################
      
      # Al total...
      {
      ###
        
      observe(  
        output$Salida_tabla_2q_RMedic_05 <- renderTable(digits=0, rownames = TRUE, align= "c",{
          
          if(!is.null(Reactive_tabla_2q_RMedic())) {
            Reactive_tabla_2q_RMedic()[[5]][[2]]
          } else return(NULL)
        })
      )
      
      observe(  
        output$Salida_tabla_2q_RMedic_06 <- renderTable(digits=decimales_planeta(),rownames = TRUE, align= "c", {
          
          if(!is.null(Reactive_tabla_2q_RMedic())) {
            Reactive_tabla_2q_RMedic()[[6]][[2]]
          } else return(NULL)
        })
      )
      
      observe(  
        output$Salida_tabla_2q_RMedic_07 <- renderTable(digits=decimales_planeta(), rownames = TRUE, align= "c",{
          
          if(!is.null(Reactive_tabla_2q_RMedic())) {
            Reactive_tabla_2q_RMedic()[[7]][[2]]
          } else return(NULL)
        })
      )
      
      observe(  
        output$Salida_tabla_2q_RMedic_08 <- renderTable(digits=decimales_planeta(), rownames = TRUE, align= "c",{
          
          if(!is.null(Reactive_tabla_2q_RMedic())) {
            Reactive_tabla_2q_RMedic()[[8]][[2]]
          } else return(NULL)
        })
      )
      
      ###
      } # End Al Total
      ###########################################
      
      # Por filas...
      {
      ###
        
      observe(  
        output$Salida_tabla_2q_RMedic_09 <- renderTable(digits=0, rownames = TRUE, align= "c",{
          
          if(!is.null(Reactive_tabla_2q_RMedic())) {
            Reactive_tabla_2q_RMedic()[[9]][[2]]
          } else return(NULL)
        })
      )
      
      observe(  
        output$Salida_tabla_2q_RMedic_10 <- renderTable(digits=decimales_planeta(),rownames = TRUE, align= "c", {
          
          if(!is.null(Reactive_tabla_2q_RMedic())) {
            Reactive_tabla_2q_RMedic()[[10]][[2]]
          } else return(NULL)
        })
      )
      
      observe(  
        output$Salida_tabla_2q_RMedic_11 <- renderTable(digits=decimales_planeta(), rownames = TRUE, align= "c",{
          
          if(!is.null(Reactive_tabla_2q_RMedic())) {
            Reactive_tabla_2q_RMedic()[[11]][[2]]
          } else return(NULL)
        })
      )
      
      observe(  
        output$Salida_tabla_2q_RMedic_12 <- renderTable(digits=decimales_planeta(), rownames = TRUE, align= "c",{
          
          if(!is.null(Reactive_tabla_2q_RMedic())) {
            Reactive_tabla_2q_RMedic()[[12]][[2]]
          } else return(NULL)
        })
      )
      
      ###
      } # End por FIlas
      ###########################################
      
      
      # Por columnas...
      {
      ###
        
      observe(  
        output$Salida_tabla_2q_RMedic_13 <- renderTable(digits=0, rownames = TRUE, align= "c",{
          
          if(!is.null(Reactive_tabla_2q_RMedic())) {
            Reactive_tabla_2q_RMedic()[[13]][[2]]
          } else return(NULL)
        })
      )
      
      observe(  
        output$Salida_tabla_2q_RMedic_14 <- renderTable(digits=decimales_planeta(),rownames = TRUE, align= "c", {
          
          if(!is.null(Reactive_tabla_2q_RMedic())) {
            Reactive_tabla_2q_RMedic()[[14]][[2]]
          } else return(NULL)
        })
      )
      
      observe(  
        output$Salida_tabla_2q_RMedic_15 <- renderTable(digits=decimales_planeta(), rownames = TRUE, align= "c",{
          
          if(!is.null(Reactive_tabla_2q_RMedic())) {
            Reactive_tabla_2q_RMedic()[[15]][[2]]
          } else return(NULL)
        })
      )
      
      observe(  
        output$Salida_tabla_2q_RMedic_16 <- renderTable(digits=decimales_planeta(), rownames = TRUE, align= "c",{
          
          if(!is.null(Reactive_tabla_2q_RMedic())) {
            Reactive_tabla_2q_RMedic()[[16]][[2]]
          } else return(NULL)
        })
      )
      
      ###
      } # End Por Columna
      #############################################################
      
      # Simple Entrada
      {
      ###
        
      observe(  
        output$Salida_tabla_2q_RMedic_17 <- renderTable(digits=decimales_planeta(), rownames = T, align= "c",{
          
          if(!is.null(Reactive_tabla_2q_RMedic())) {
            Reactive_tabla_2q_RMedic()[[17]][[2]]
          } else return(NULL)
        })
      )
      
      ###
      } # End Simple Entrada
      #############################################################
      
      
      # DF QQ PACK
      observe(  
        output$MegaSalida_tabla_2q_RMedic <- renderUI({
          
          if(!is.null(Reactive_tabla_2q_RMedic())) {
            if(!is.null(input$kayak)) {
              
              
              # Al total
              if(input$kayak == 1) {
                div(
                  #h3("Frecuencias Absolutas"),
                  h3(Reactive_tabla_2q_RMedic()[[1]][[1]]),
                  ref2q_planeta()[1], br(),
                  ref2q_planeta()[2], br(),
                  tableOutput("Salida_tabla_2q_RMedic_01"), br(),
                  
                  #h3("Cociente al Total"),
                  h3(Reactive_tabla_2q_RMedic()[[2]][[1]]),
                  ref2q_planeta()[1], br(),
                  ref2q_planeta()[2], br(),
                  tableOutput("Salida_tabla_2q_RMedic_02"), br(),
                  
                  # h3("Frecuencias Relativas al Total"),
                  h3(Reactive_tabla_2q_RMedic()[[3]][[1]]),
                  ref2q_planeta()[1], br(),
                  ref2q_planeta()[2], br(),
                  tableOutput("Salida_tabla_2q_RMedic_03"),  br(),
                  
                  # h3("Porcentajes al Total"),
                  h3(Reactive_tabla_2q_RMedic()[[4]][[1]]),
                  ref2q_planeta()[1], br(),
                  ref2q_planeta()[2], br(),
                  tableOutput("Salida_tabla_2q_RMedic_04")
                )
                
                # Al total  
              } else  if(input$kayak == 2) {
                div(
                  # h3("Frecuencias Absolutas"),
                  h3(Reactive_tabla_2q_RMedic()[[5]][[1]]),
                  ref2q_planeta()[1], br(),
                  ref2q_planeta()[2], br(),
                  tableOutput("Salida_tabla_2q_RMedic_05"), br(),
                  
                  # h3("Cociente al Total"),
                  h3(Reactive_tabla_2q_RMedic()[[6]][[1]]),
                  ref2q_planeta()[1], br(),
                  ref2q_planeta()[2], br(),
                  tableOutput("Salida_tabla_2q_RMedic_06"), br(),
                  
                  # h3("Frecuencias Relativas al Total"),
                  h3(Reactive_tabla_2q_RMedic()[[7]][[1]]),
                  ref2q_planeta()[1], br(),
                  ref2q_planeta()[2], br(),
                  tableOutput("Salida_tabla_2q_RMedic_07"), br(),
                  
                  # h3("Porcentajes al Total"),
                  h3(Reactive_tabla_2q_RMedic()[[8]][[1]]),
                  ref2q_planeta()[1], br(),
                  ref2q_planeta()[2], br(),
                  tableOutput("Salida_tabla_2q_RMedic_08")
                )
                
                # por filas  
              } else    if(input$kayak == 3) {
                div(
                  # h3("Frecuencias Absolutas por Filas"),
                  h3(Reactive_tabla_2q_RMedic()[[9]][[1]]),
                  ref2q_planeta()[1], br(),
                  ref2q_planeta()[2], br(),
                  tableOutput("Salida_tabla_2q_RMedic_09"), br(),
                  
                  # h3("Cociente por Filas"),
                  h3(Reactive_tabla_2q_RMedic()[[10]][[1]]),
                  ref2q_planeta()[1], br(),
                  ref2q_planeta()[2], br(),
                  tableOutput("Salida_tabla_2q_RMedic_10"), br(),
                  
                  # h3("Frecuencias Relativas por Filas"),
                  h3(Reactive_tabla_2q_RMedic()[[11]][[1]]),
                  ref2q_planeta()[1], br(),
                  ref2q_planeta()[2], br(),
                  tableOutput("Salida_tabla_2q_RMedic_11"), br(),
                  
                  # h3("Porcentajes por Filas"),
                  h3(Reactive_tabla_2q_RMedic()[[12]][[1]]),
                  ref2q_planeta()[1], br(),
                  ref2q_planeta()[2], br(),
                  tableOutput("Salida_tabla_2q_RMedic_12")
                )
                
                # Por columnas
              } else    if(input$kayak == 4) {
                div(
                  # h3("Frecuencias Absolutas por Columnas"),
                  h3(Reactive_tabla_2q_RMedic()[[13]][[1]]),
                  ref2q_planeta()[1], br(),
                  ref2q_planeta()[2], br(),
                  tableOutput("Salida_tabla_2q_RMedic_13"), br(),
                  
                  # h3("Cociente por Columnas"),
                  h3(Reactive_tabla_2q_RMedic()[[14]][[1]]),
                  ref2q_planeta()[1], br(),
                  ref2q_planeta()[2], br(),
                  tableOutput("Salida_tabla_2q_RMedic_14"), br(),
                  
                  # h3("Frecuencias Relativas por Columnas"),
                  h3(Reactive_tabla_2q_RMedic()[[15]][[1]]),
                  ref2q_planeta()[1], br(),
                  ref2q_planeta()[2], br(),
                  tableOutput("Salida_tabla_2q_RMedic_15"), br(),
                  
                  # h3("Porcentajes por Columnas"),
                  h3(Reactive_tabla_2q_RMedic()[[16]][[1]]),
                  ref2q_planeta()[1], br(),
                  ref2q_planeta()[2], br(),
                  tableOutput("Salida_tabla_2q_RMedic_16")
                )
                
                # Simple entrada  
              } else  if(input$kayak == 5) {
                div(
                  # h3("Simple Entrada"),
                  h3(Reactive_tabla_2q_RMedic()[[17]][[1]]),
                  tableOutput("Salida_tabla_2q_RMedic_17")
                )
                
                
              } else return(NULL)
              
            } else return(NULL)
          } else return(NULL)
        })
      )
      
      ###
    } # Fin 2 Variable Categoricas (QQ)
    ################################################################################
    
    
    # 2 Variable Numericass (CC)
    {
      ###
      
      Reactive_tabla_2c_RMedic <- reactive({
        if (paso_BASE(Base_Planeta())) {
          
          
          MINIBASE <- Base_Planeta()
          estos_decimales <- decimales_planeta()
          
          
          salida <- list()
          
          # Medidas de Posicion simultaneas
          salida[[1]] <-   list("Medidas Resumen",
                                mps(MINIBASE, estos_decimales)$mps$tabla1_mps
          )
          
          # Medidas de Posicion simultaneas
          salida[[2]] <-   list("Medidas de Posición",
                                mps(MINIBASE, estos_decimales)$mps$tabla1_mps
                                )
          
          # Medidas de Dispersion simultaneas
          salida[[3]] <-    list("Medidas de Dispersión",
                                 mds(MINIBASE, estos_decimales)$mds$tabla1_mds
                                )
          
          # Percentiles Simultaneos
          salida[[4]] <-    list("Percentiles",
                                 percentiles2(MINIBASE, estos_decimales, 
                                              input_busqueda = c(5, 10, 90, 95))$perc2$tabla_per2
                                 )
          
          # Intervalos de confianza simultaneos
          salida[[5]] <-    list("Intervalos de Confianza del 90%",
                                 mps(MINIBASE, estos_decimales)$mps$tabla3_mps[[1]]
                                 )
          
          # Intervalos de confianza simultaneos
          salida[[6]] <-    list("Intervalos de Confianza del 95%",
                                 mps(MINIBASE, estos_decimales)$mps$tabla3_mps[[2]]
          )
          
          # Intervalos de confianza simultaneos
          salida[[7]] <-    list("Intervalos de Confianza del 99%",
                                 mps(MINIBASE, estos_decimales)$mps$tabla3_mps[[3]]
          )
          
          return(salida)
          
        } else return(NULL)
      })
      
      # Medidas Resumen
      observe( 
         output$Salida_tabla_2c_RMedic_01 <- renderTable(digits=decimales_planeta(), align= "c",{
          
          if(!is.null(Reactive_tabla_2c_RMedic())) {
            Reactive_tabla_2c_RMedic()[[1]][[2]]
          } else return(NULL)
        })
      )
      
      # Medidas de Posicion
      observe( 
        
        output$Salida_tabla_2c_RMedic_02 <- renderTable(digits=decimales_planeta(), align= "c",{
          
          if(!is.null(Reactive_tabla_2c_RMedic())) {
            Reactive_tabla_2c_RMedic()[[2]][[2]]
          } else return(NULL)
        })
      )
      
      # Medidas de Dispersion
      observe(
        output$Salida_tabla_2c_RMedic_03 <- renderTable(digits=decimales_planeta(), align= "c",{
          
          if(!is.null(Reactive_tabla_2c_RMedic())) {
            Reactive_tabla_2c_RMedic()[[3]][[2]]
          } else return(NULL)
        })
      )
      
      
      # Percentiles
      observe(
        output$Salida_tabla_2c_RMedic_04 <- renderTable(digits=decimales_planeta(), align= "c",{
          
          if(!is.null(Reactive_tabla_2c_RMedic())) {
            Reactive_tabla_2c_RMedic()[[4]][[2]]
          } else return(NULL)
        })
      )
      
 
      # IC de la Media 90%     
      observe( 
         output$Salida_tabla_2c_RMedic_05 <- renderTable(digits=decimales_planeta(), align= "c",{
          
          if(!is.null(Reactive_tabla_2c_RMedic())) {
            Reactive_tabla_2c_RMedic()[[5]][[2]]
          } else return(NULL)
        })
      )
      
      
      # IC de la Media 95%      
      observe( 
        output$Salida_tabla_2c_RMedic_06 <- renderTable(digits=decimales_planeta(), align= "c",{
          
          if(!is.null(Reactive_tabla_2c_RMedic())) {
            Reactive_tabla_2c_RMedic()[[6]][[2]]
          } else return(NULL)
        })
      )
      
      
      # IC de la Media 99%
      observe( 
        output$Salida_tabla_2c_RMedic_07 <- renderTable(digits=decimales_planeta(), align= "c",{
          
          if(!is.null(Reactive_tabla_2c_RMedic())) {
            Reactive_tabla_2c_RMedic()[[7]][[2]]
          } else return(NULL)
        })
      )
      
      
      output$MegaSalida_tabla_2c_RMedic <- renderUI ({
        
        if(!is.null(Reactive_tabla_2c_RMedic())) {
          
        div(
          #h3("Medidas Resumen"),
          h3(Reactive_tabla_2c_RMedic()[[1]][[1]]),
          tableOutput("Salida_tabla_2c_RMedic_01"), br(),
          
          #h3("Medidas de Posición"),
          h3(Reactive_tabla_2c_RMedic()[[2]][[1]]),
          tableOutput("Salida_tabla_2c_RMedic_02"), br(),
          
          #h3("Medidas de Dispersión"),
          h3(Reactive_tabla_2c_RMedic()[[3]][[1]]),
          tableOutput("Salida_tabla_2c_RMedic_03"), br(),
          
          #h3("Percentiles"),
          h3(Reactive_tabla_2c_RMedic()[[4]][[1]]),
          tableOutput("Salida_tabla_2c_RMedic_04"), br(),
          
          #h3("Intervalo de Confianza del 90%"),
          h3(Reactive_tabla_2c_RMedic()[[5]][[1]]),
          tableOutput("Salida_tabla_2c_RMedic_05"), br(),
          
          #h3("Intervalo de Confianza del 95%"),
          h3(Reactive_tabla_2c_RMedic()[[6]][[1]]),
          tableOutput("Salida_tabla_2c_RMedic_06"), br(),
          
          #h3("Intervalo de Confianza del 99%"),
          h3(Reactive_tabla_2c_RMedic()[[7]][[1]]),
          tableOutput("Salida_tabla_2c_RMedic_07")
        )
          
        } else return(NULL)
      })
      
       
      ###
    } # Fin 2 Variable Numericass (CC)
    ################################################################################
    
  

    # 2 Variables... Q y C
    {
      ###
      
      Reactive_tabla_qc_RMedic <- reactive({
        if (paso_BASE(Base_Planeta())) {
          
          
          MINIBASE <- na.omit(Base_Planeta())
          estos_decimales <- decimales_planeta()
          
          
          
          salida <- list()
          
          salida[[1]] <-  list("Medidas Resumen Particionadas",
                               mpp(input_base = MINIBASE, input_decimales = estos_decimales)$mpp$tabla1_mpp
                               )
          
          salida[[2]] <-  list("Medidas de Posición Particionadas",
                               mpp(input_base = MINIBASE, input_decimales = estos_decimales)$mpp$tabla1_mpp
                              )
          
          salida[[3]] <-  list("Medidas de Dispersión Particionadas",
                               mdp(input_base = MINIBASE, input_decimales = estos_decimales)$mdp$tabla1_mdp
                               )
          
          salida[[4]] <-  list("Percentiles Particionados",
                               percentiles3(input_base = MINIBASE, input_decimales = estos_decimales, input_busqueda = c(5, 10, 90, 95))$perc3$tabla1_percp  
                              )
          
          salida[[5]] <-  list("Intervalo de Confianza del 90% Particionados",
                               mpp(input_base = MINIBASE, input_decimales = estos_decimales)$mpp$tabla3_icp2[[1]]
                              )
          
          salida[[6]] <-  list("Intervalo de Confianza del 95% Particionados",
                               mpp(input_base = MINIBASE, input_decimales = estos_decimales)$mpp$tabla3_icp2[[2]]
                              )
          
          salida[[7]] <-  list("Intervalo de Confianza del 99% Particionados",
                               mpp(input_base = MINIBASE, input_decimales = estos_decimales)$mpp$tabla3_icp2[[3]]
                              ) 
          
          salida
          
        } else return(NULL)
      })
      
      
      observe( 
        # Medidas de Posicion Particionada
        output$Salida_tabla_qc_RMedic_01 <- renderTable(digits=decimales_planeta(), align= "c",{
          
          if(!is.null(Reactive_tabla_qc_RMedic())) {
            Reactive_tabla_qc_RMedic()[[1]][[2]]
          } else return(NULL)
        })
      )
      
      observe( 
        # Medidas de Posicion Particionada
        output$Salida_tabla_qc_RMedic_02 <- renderTable(digits=decimales_planeta(), align= "c",{
          
          if(!is.null(Reactive_tabla_qc_RMedic())) {
            Reactive_tabla_qc_RMedic()[[2]][[2]]
          } else return(NULL)
        })
      )
      
      
      observe( 
        # Medidas de Posicion Particionada
        output$Salida_tabla_qc_RMedic_03 <- renderTable(digits=decimales_planeta(), align= "c",{
          
          if(!is.null(Reactive_tabla_qc_RMedic())) {
            Reactive_tabla_qc_RMedic()[[3]][[2]]
          } else return(NULL)
        })
      )
      
      
      observe( 
        # Medidas de Posicion Particionada
        output$Salida_tabla_qc_RMedic_04 <- renderTable(digits=decimales_planeta(), align= "c",{
          
          if(!is.null(Reactive_tabla_qc_RMedic())) {
            Reactive_tabla_qc_RMedic()[[4]][[2]]
          } else return(NULL)
        })
      )
      
      observe( 
        # Medidas de Posicion Particionada
        output$Salida_tabla_qc_RMedic_05 <- renderTable(digits=decimales_planeta(), align= "c",{
          
          if(!is.null(Reactive_tabla_qc_RMedic())) {
            Reactive_tabla_qc_RMedic()[[5]][[2]]
          } else return(NULL)
        })
      )
      
      observe( 
        # Medidas de Posicion Particionada
        output$Salida_tabla_qc_RMedic_06 <- renderTable(digits=decimales_planeta(), align= "c",{
          
          if(!is.null(Reactive_tabla_qc_RMedic())) {
            Reactive_tabla_qc_RMedic()[[6]][[2]]
          } else return(NULL)
        })
      )
      
      
      observe( 
        # Medidas de Posicion Particionada
        output$Salida_tabla_qc_RMedic_07 <- renderTable(digits=decimales_planeta(), align= "c",{
          
          if(!is.null(Reactive_tabla_qc_RMedic())) {
            Reactive_tabla_qc_RMedic()[[7]][[2]]
          } else return(NULL)
        })
      )
      
      
    
      
      ###
    } # Fin  2 Variables... Q y C
    ################################################################################
    

   
    
   
    
    output$salida_TABLAS_RMedic <- renderUI ({
      
      # if(!is.null(Variables_Tablas())) {
      #   if(Variables_Tablas()[[1]]) {
      #     
      # if(!is.null(ok_planeta())) {
      #   if(ok_planeta()) {
      #   if (Variables_Tablas()[[4]] == 1) {
      if (!is.null(pos_planeta())) {
        if (pos_planeta() == 1) {
      div(
        h3("Variables Seleccionadas"), 
        htmlOutput("zocalo_Tablas"),
        br(),
        uiOutput("MegaSalida_tabla_1q_RMedic")
      )
        }  else 
         # if (Variables_Tablas()[[4]] == 2) {
          if (pos_planeta() == 2) {
            
            div(
              h3("Variables Seleccionadas"), 
              
              fluidRow(
                column(6,htmlOutput("zocalo_Tablas")),
                column(6,  downloadButton("download_excel", 
                                          "Download RMedic",
                                          width = "350px",
                                          style ="color: #fff; background-color: #337ab7; 
                                                  border-color: #2e6da4; height:100px;
                                                  font-size:300%")
                       )
                ),             
              br(),
              uiOutput("MegaSalida_tabla_1c_RMedic"),
              uiOutput("Controlador_1c_RMedic")
            )
          }  else 
           # if(Variables_Tablas()[[4]] == 3) { 
            if (pos_planeta() == 3) {
             # var.opts <- c(input$var1_tablas, input$var2_tablas)
              var.opts <- colnames(Base_Planeta())
              
              div(
                selectInput(inputId = "orden_var_tablas", 
                            label = "En filas",
                            choices = c("Seleccione una" = "", var.opts),
                            selected = var.opts[1]),
                h3("Tablas de Contingencia"),
                uiOutput("Menu_tabla_2q_RMedic"),
                uiOutput("MegaSalida_tabla_2q_RMedic")
              )
              
              
              } else 
                #if(Variables_Tablas()[[4]] == 4) { 
                if (pos_planeta() == 4) {
                  var.opts <- c(input$var1_tablas, input$var2_tablas)
                  
                  div(
                    selectInput(inputId = "orden_var_tablas", 
                                label = "Eje X...",
                                choices = c("Seleccione una" = "", var.opts),
                                selected = var.opts[1]),
                   # h3("Tablas de Contingencia"),
                   # uiOutput("Menu_tabla_2q_RMedic"),
                    uiOutput("MegaSalida_tabla_2c_RMedic")
                  )
                  
                  
                } else 
                 # if(Variables_Tablas()[[4]] == 5) { 
                  if (pos_planeta() == 5) {
                   
                    
                 
                     uiOutput("MegaSalida_tabla_qc_RMedic")
                
                    
                    
                  } else return(NULL)
        }  else return(NULL)
      #   }  else return(NULL)
      # }  else return(NULL)
     #  }  else return(NULL)
    })
    
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
          br(),
          h3("Menú para Tablas Descriptivas"),
          eval(parse(text = gsub("_control", "_tablas",TextUI_Variables))),
          br(),
          uiOutput("salida_TABLAS_RMedic"),
          br(),
          br()
                
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
          br(),
          h3("Menú para Gráficos"),
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
  
  
  
################

  # Seccion 08 -  Planeta
  {
  ###
    
 # Cargamos el codigo Server de Planeta - Objetos Reactivos
 # source("script/ServerPlaneta.R", local = T) 
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

  
 
  
  
  ###############
  # Variable de Goku + Tipo de Var
  engarce_planeta <- reactive({
    
    var_planeta <- NULL
    tipo_var_planeta <- NULL
    
  #  cat("input$PanelRMedic2222: ", input$PanelRMedic, "\n")
    # Si cargo el menu general...
    if (!is.null(input$PanelRMedic)) {
      
      # Si esta en la pestania 3 "TABLAS"
      if(input$PanelRMedic == 3) { 
        
        # Si hay una cantidad de variables seleccionada
        if (paso_detalle(input$qtty_var_tablas)) {
          
          var_planeta <- c()
          tipo_var_planeta <- c()
          
          
          # Si la cantidad de variables es == 1
          if (as.numeric(input$qtty_var_tablas) == 1){
            
            # Si hay algo cargado en var1_tablas
            if (paso_detalle(input$var1_tablas)) {
              
              # Agendamos a var1_tablas
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
  
 
  # Variable de Goku + Tipo de Var



  
  # Variable de Goku
  var_planeta <- reactive({
   engarce_planeta()[[1]]
  })
  
  
  # Tipo Variable de Goku
  tipo_var_planeta <- reactive({
    engarce_planeta()[[2]]
  })
  
  pos_planeta <- reactive({
    
    if(!is.null(tipo_var_planeta())) {
 
      tipo_var_planeta <-   tipo_var_planeta()
    numero_pos <- rep(NA, length(tipo_var_planeta))
    
    for (k in 1:length(tipo_var_planeta)) {
      if (tipo_var_planeta[k] == "Categórica") numero_pos[k] <- 1 else
        if (tipo_var_planeta[k] == "Numérica") numero_pos[k] <- 10
      
    }
    
    posibles_sumas <- c(1, 10, 2, 20, 11)
    orden_suma <- c(1:length(posibles_sumas))
    the_suma <- sum(numero_pos)
    dt_suma <- posibles_sumas == the_suma
    
    the_orden <- orden_suma[dt_suma]
    
    the_orden
    } else return(NULL)
  })
  
  
  
  # Variable y letra de las variables seleccionadas
  detalle_planeta <- reactive({
    
    detalle <- c("", "")
    
    if (length(var_planeta()) >= 1)  detalle[1] <- paste0("Variable 1: ", id_var(var_planeta()[1], colnames(BaseSalida()[[1]])))  
    if (length(var_planeta()) >= 2)  detalle[2] <- paste0("Variable 2: ", id_var(var_planeta()[2], colnames(BaseSalida()[[1]])))  
    
    detalle
    
  })
  
  
  # Para 2Q... Quien va en filas y quien en columnas
  ref2q_planeta <- reactive({
    
    detalle <- c("", "")
    
    if (length(var_planeta()) >= 1)  detalle[1] <- paste0("En filas: ", id_var(var_planeta()[1], colnames(BaseSalida()[[1]])))  
    if (length(var_planeta()) >= 2)  detalle[2] <- paste0("En columnas: ", id_var(var_planeta()[2], colnames(BaseSalida()[[1]])))  
    
    detalle
    
  })
  
  
  # Base de datos seleccionada
  Base_Planeta <- reactive({
    
    if (status_BaseSalida()) {
      if (!is.null(var_planeta())) {
        
        BaseSalida()[[1]][var_planeta()]
        
      } else return(NULL)
    } else return(NULL)
  })   
  
  ###
  } # End Seccion 08 - Planeta
  ##################################
 
  
  
############################  
  
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
  
  
  # observe(cat(input$PanelRMedic, "\n"))
  # observe(cat(input$AVER, "\n"))
}



