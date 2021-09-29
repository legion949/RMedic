
source("lib.R")

#source("uiCode.R")
source("script/PreServerCode.R")


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
  source("script/01_BaseDeDatos/ServerBaseDeDatos.R", local = T)
  
  
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
    source("script/03_Tablas/TablasRMedic.R", local = T)
    
    
     
    
    # 2 Variable Categoricas (QQ)
    {
      ###
      
      pack_tabla_2q_df_RMedic <- reactive({
        if (paso_BASE(Base_Tablas())) {
          
          
          df02(Base_Tablas()[[1]], input$decimales_tablas)$df02
          
        } else return(NULL)
      })
      ###
    } # Fin 2 Variable Categoricas (QQ)
    ################################################################################
    
    
    # 2 Variable Cuantitativas - Objetos Reactivos
    {
      ###
      
      pack_tabla_2c_RMedic <- reactive({
        if (paso_BASE(Base_Tablas())) {
          
          
          MINIBASE <- Base_Tablas()[[1]]
          estos_decimales <- input$decimales_tablas
          
          
          salida <- list()
          
          # Medidas de Posicion simultaneas
          salida[[1]] <-   mps(MINIBASE, estos_decimales)$mps$tabla1_mps
          
          # Medidas de Dispersion simultaneas
          salida[[2]] <-   mds(MINIBASE, estos_decimales)$mds$tabla1_mds
          
          # Percentiles Simultaneos
          salida[[3]] <-   percentiles2(MINIBASE, estos_decimales, input_busqueda = c(5, 10, 90, 95))$perc2$tabla_per2
          
          # Intervalos de confianza simultaneos
          salida[[4]] <-   mps(MINIBASE, estos_decimales)$mps$tabla3_mps
          
          return(salida)
          
        } else return(NULL)
      })
      ###
    } # Fin 2 Varaibles Cuantitativas - Objetos Reactivos
    ################################################################################
    
  



   
    
   
    
    output$salida_TABLAS_RMedic <- renderUI ({
      
      if(!is.null(Variables_Tablas())) {
        if(Variables_Tablas()[[1]]) {
          
        if (Variables_Tablas()[[4]] == 1) {
          
      div(
        h3("Variables Seleccionadas"), 
        htmlOutput("zocalo_Tablas"),
        br(),
        h3(Reactive_tabla_1q_RMedic()[[1]][[1]]),
        tableOutput("Salida_tabla_1q_RMedic_01"), br(),
        h3(Reactive_tabla_1q_RMedic()[[2]][[1]]),
        tableOutput("Salida_tabla_1q_RMedic_02"), br(),
        h3(Reactive_tabla_1q_RMedic()[[3]][[1]]),
        tableOutput("Salida_tabla_1q_RMedic_03"), br(),
        h3(Reactive_tabla_1q_RMedic()[[4]][[1]]),
        tableOutput("Salida_tabla_1q_RMedic_04")
      )
        }  else 
          if (Variables_Tablas()[[4]] == 2) {
            
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

              h3(Reactive_tabla_1c_RMedic()[[1]][[1]]),
              uiOutput("Salida_tabla_1c_RMedic_01"), br(),
              h3(Reactive_tabla_1c_RMedic()[[2]][[1]]),
              uiOutput("Salida_tabla_1c_RMedic_02"), br(),
              h3(Reactive_tabla_1c_RMedic()[[3]][[1]]),
              uiOutput("Salida_tabla_1c_RMedic_03"), br(),
              h3(Reactive_tabla_1c_RMedic()[[4]][[1]]),
              uiOutput("Salida_tabla_1c_RMedic_04"), br(),
            )
          }  else return(NULL)
        }  else return(NULL)
       }  else return(NULL)
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
  
 # Cargamos el codigo Server de Planeta - Objetos Reactivos
 source("script/ServerPlaneta.R", local = T) 
  
  
  
 
  
  
############################  
  
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
  
  
  # observe(cat(input$PanelRMedic, "\n"))
  # observe(cat(input$AVER, "\n"))
}