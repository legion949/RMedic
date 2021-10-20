## Segmento del UI
SideBarBaseUI <- function(id) {
  ns <- NS(id)

  
  # Este es el sidebarpanel completo
  div(  
    selectInput(inputId = ns("FileTypePicker"), 
                    label = 'Qué tipo de Archivo quieres subir?',
                    choices = c('Excel' = 'Excel', 
                                'CSV' = 'CSV', 
                                'Ejemplos' = 'Ejemplos'), 
                    selected = 'Excel',
                    selectize = T),
        conditionalPanel( 
          'input.FileTypePicker == "Excel"', ns = ns, 
          fileInput(ns('xls_file'), 'Elige tu archivo Excel',
                    accept = c(".xls", ".xslx"))
          #             accept=c('application/vnd.ms-excel', 
          #                      'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'))
        ),
        # un CSV
        conditionalPanel( 
          'input.FileTypePicker == "CSV"', ns = ns, 
          
          # Carga de CSV
          fileInput(ns('csv_file'), 'Elige tu archivo CSV',
                    accept = c(".csv")),
          # accept=c('text/csv', 
          #          'text/comma-separated-values,text/plain', '.csv')),
          # 
          # Detalles para CSV
          # # Header
          checkboxInput(ns('header'), 'Encabezado', TRUE),
          
          # # Separador de columna
          radioButtons(ns('sep'), 'Separador',
                       c(Coma=',',
                         'Punto y Coma'=';',
                         Tabulación='\t'),
                       ';'),
          
          # # Separador Decimal
          radioButtons(ns('dec'), 'Decimal',
                       c(Coma=',', Punto='.'),
                       '.'),
          # # Quote
          radioButtons(ns('quote'), 'Comillas',
                       c('Sin Comillas'='',
                         'Comillas Dobles'='"',
                         'Comillas Simples'="'"),
                       '"')
        ),
        conditionalPanel( 
          'input.FileTypePicker == "Ejemplos"', ns = ns, 
          selectInput(ns("ejemplo_file"), "Elige una base ejemplo:", 
                      choices = c("rock", "pressure", "cars", "mtcars", "iris"),
                      selected = "mtcars")
        ),
        radioButtons(inputId = ns("cie_especificaciones"), 
                     label = "Especificaciones", 
                     choices = c("Todas las filas" = 1, 
                                 "Aplicar un Criterio de Inclusión Estadístico" = 2)),
        br(),
       uiOutput(ns("OpcionesCIE_completo")),
        br()
        
        
  )
  
  
  
}

MiBase01_UI <- function(id) {
  ns <- NS(id)
  
  div(
    fluidRow(
      column(1),
      column(10,
    h3(textOutput(ns("TextBase_Alert01"))), 
    h3(textOutput(ns("TextBase_Alert02"))), 
    h3(textOutput(ns("TextBase_Alert03"))),
    h3(textOutput(ns("TextBase_Alert04"))),
    h3(htmlOutput(ns("TextBase_Alert05"))),
    h3(htmlOutput(ns("TextBase_Alert06"))),
    h3(htmlOutput(ns("TextBase_Alert07"))),
    htmlOutput(ns("TextBase_InfoDataSet_01")), br(),
    h3(textOutput(ns("TextBase_Intro"))),
    dataTableOutput(ns("BaseSalida")),
    br(),  br(), br(),
    htmlOutput(ns("TextBase_InfoDataSet_02")), br()
  ), 
  column(1)
    )
  )
  }



## Segmento del server
SideBarBaseSERVER <- function(input, output, session) {
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  
  
  RMedic_general <- reactiveVal(T)
  reseteo_conteo <- reactiveVal(0)
  
  
  # Control (Que sea un archivo valido)
  Control01 <- reactive({ 
    
    # Si no hay un tipo de archivo seleccionado... Paramos
    if (is.null(input$FileTypePicker)) return(NULL) 
    
    if(input$FileTypePicker == "Excel") { 
      
      # Si no fue seleccionado un archivo... Paramos
      if (is.null(input$xls_file)) return(NULL)
      
      
      the_file <-  input$xls_file[[1]]
      correct_format <- c(".xls$", ".xlsx$")
      dt_format <- rep(NA, length(correct_format))
      dt_format[1] <- grepl(correct_format[1], the_file)
      dt_format[2] <- grepl(correct_format[2], the_file)
      ok_format <- sum(dt_format) > 0
      
      frase_yes <- ""
      frase_no  <- "Indicaste que subirías un archivo tipo Excel pero 
                        seleccionaste un archivo de otro tipo."
      
      # Seleccion de texto...    
      if(ok_format) frase_alert <- frase_yes else frase_alert <- frase_no
      
      # My exit    
      my_exit <- list(ok_format, frase_alert)
      
      
    } else 
      if(input$FileTypePicker == "CSV") { 
        
        # Si no hay un archivo csv seleccionado... Paramos
        if (is.null(input$csv_file)) return(NULL)
        
        # El archivo seleccionado
        the_file <-  input$csv_file[[1]]
        
        # Formato de archivo correcto
        correct_format <- c(".csv$")
        dt_format <- rep(NA, length(correct_format))
        dt_format[1] <- grepl(correct_format[1], the_file)
        ok_format <- sum(dt_format) > 0
        
        # Texto
        frase_yes <- ""
        frase_no  <- "Indicaste que subirías un archivo CSV pero 
                        seleccionaste un archivo de otro tipo."
        
        # Seleccion de texto
        if(ok_format) frase_alert <- frase_yes else frase_alert <- frase_no
        
        # Armado de salida
        my_exit <- list(ok_format, frase_alert)
        
        
        
        
        
        
        
        
      } else 
        if(input$FileTypePicker == "Ejemplos") { 
          
          # Si no fue seleccionado un ejemplo... Paramos...
          if (is.null(input$ejemplo_file)) return(NULL)
          
          ok_format <- TRUE
          frase_yes <- ""
          frase_no  <- ""
          
          # Seleccion de mensaje de aviso
          if(ok_format) frase_alert <- frase_yes else frase_alert <- frase_no
          
          # Armado de la salida
          my_exit <- list(ok_format, frase_alert)
          
          
          
        } else return(NULL)
    
    # Return Exitoso
    return(my_exit)
    
    
  })
  
  # Nombre del archivo seleccionado
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
    
    # Si no hay un control de la base... Paramos
    if (is.null(Control01())) return(NULL) 
    if (!Control01()[[1]]) return(NULL)
    

    # Si el tipo de archivo es Excel...      
    if(input$FileTypePicker == "Excel") { 
      
      # Si no fue seleccionado aun el archivo... Paramos
      if (is.null(input$xls_file)) return(NULL)
      
      # Detalles varios...
      inFile <- input$xls_file
      
      # La direccion del archivo...
      temporal_file <- inFile$datapath
      
      
      # 1) DataSet
      library(readxl)
      DataSet <- as.data.frame(read_excel(path = temporal_file,
                                          col_names= TRUE,
                                          sheet = 1, trim_ws = FALSE))
      
      
      
    } else
      if(input$FileTypePicker == "CSV") { 
        
        # Si el archivo csv no fue seleccionado... Paramos 
        if (is.null(input$csv_file)) return(NULL)
        
        # Detalles varios de direccion
        inFile <- input$csv_file
        
        
        # La carga de datos formato CSV
        DataSet <- read.csv(file = inFile$datapath, 
                            header=input$header, sep=input$sep,
                            dec=input$dec, quote=input$quote)
        
        
        
      } else 
        if(input$FileTypePicker == "Ejemplos") { 
          
          # Si el ejemplo no fue elegido... Paramos
          if (is.null(input$ejemplo_file)) return(NULL)
          
          # La carga de datos formato CSV
          DataSet <- eval(parse(text = input$ejemplo_file))
          
          
        }  else return(NULL)
    
    # Salida
    my_exit <- DataSet
    return(my_exit)
    
    
    
    
    
    
  })

  # Opcion de seleccion de las columnas de la base
  AspectosColumnas_BaseSalida <- reactive({
    
    # Si aun no se establecio un status de Base01(), nos vamos...
    if (is.null(Base01())) return(NULL)
    
    
    nombres_originales <- colnames(Base01())
    numero_orden <- c(1:length(nombres_originales))
    letras <- num2let(numero_orden)
    
    combinado <- paste0("(", letras, ") - ", nombres_originales)
    
    
    armado <- nombres_originales 
    names(armado) <- combinado
    
    return(list(nombres_originales, combinado, armado))
  })
  
  
  # Control02 - La Base01() tiene al menos una columna y al menos 1 fila...
  Control02 <- reactive({ 
    
    # Si no un Control01() o no lo pasamos...
    if(is.null(Control01())) return(NULL)
    if(!Control01()[[1]]) return(NULL)
    
    # Si no hay una base... Nos retiramos...
    if(is.null(Base01())) return(NULL)
    
    # Deteccion de detalles varios...
    dt_ok_01 <- ncol(Base01()) > 0
    dt_ok_02 <- nrow(Base01()) > 0
    dt_ok_03 <- sum(dt_ok_01, dt_ok_02) == 0
    dt_ok_04 <- sum(dt_ok_01, dt_ok_02) == 2
    
    
    # Frases...
    frase_FALSE_01 <- "La base de datos que has seleccionado no posee
                               columnas con datos."
    
    frase_FALSE_02 <- "La base de datos que has seleccionado no posee
                               filas con datos."
    
    frase_FALSE_03 <- "La base de datos que has seleccionado es un archivo
                                completamente vacío. Sin datos."
    
    frase_TRUE <- ""
    
    # Seleccion de frase...
    if(dt_ok_04) frase_salida <- frase_TRUE else
      if(dt_ok_03) frase_salida <- frase_FALSE_03 else
        if(dt_ok_02) frase_salida <- frase_FALSE_02 else
          if(dt_ok_01) frase_salida <- frase_FALSE_01 
    
    # dt_ok
    if(dt_ok_04) dt_ok <- dt_ok_04 else
      if(dt_ok_03) dt_ok <- dt_ok_03 else
        if(dt_ok_02) dt_ok <- dt_ok_02 else
          if(dt_ok_01) dt_ok <- dt_ok_01 
    
    # Armado de salida...
    my_exit <- list(dt_ok, frase_salida)
    
    # Return Exitoso
    return(my_exit)
    
    
    
  })
  
  
 
  # Armamos la eleccion de CIE
  output$OpcionesCIE_parte01 <- renderUI({
    
    
    if(is.null(Base01())) return(NULL)
    
    if(is.null(AspectosColumnas_BaseSalida())) return(NULL)
    
    
    armado <- AspectosColumnas_BaseSalida()[[3]]
    
    
    
    selectInput(inputId = ns("cie_columna"), 
                label = "Criterio de Inclusión Estadístico (CIE) - Seleccione una variable:", 
                choices =  c("Seleccione un CIE..." = "", armado)
    )  
    
    
  })
  
  
  # Control 03 (Quiere elegir CIE y todavia no cargo la base de datos)
  Control03 <- reactive({ 
    
    # Si no hay especificaciones... Nos fuimos...
    if(is.null(input$cie_especificaciones)) return(NULL)
    
    
    # En principio esta todo OK. 
    # El error puntual que intentamos encontrar es que el usuario se apuro
    # y el indica que va a usar un CIE (coloca la opcion 2) cuando todavia
    # no cargo una base de datos (is.null(Base01())).
    
    # Valores por defecto
    dt_ok <- TRUE
    frase_TRUE <- ""
    frase_FALSE <- "Para poder seleccionar un Criterio de Inclusión Estadística y una Categoría de Inclusión primero debe subir una base de datos."
    
    
    
    # Caso Buscado...
    if(input$cie_especificaciones == 1) dt_ok <- TRUE else
      if (!is.null(Base01()) && input$cie_especificaciones == 2) dt_ok <- TRUE
    if (is.null(Base01()) && input$cie_especificaciones == 2) dt_ok <- FALSE
    
    
    # Seleccion de frase
    if (dt_ok) frase_salida <- frase_TRUE else frase_salida <- frase_FALSE
    
    # Armamos salida
    my_exit <- list(dt_ok, frase_salida)
    
    # Return Exitoso
    return(my_exit)
    
    
    
    
  })
  
  
  # Control04 - Que la variable CIE ha sido elegida y que
  #             pertenece efectivamente a la base de datos
  Control04 <- reactive({ 
    
    # Si no hay un Control02() o no lo pasamos...
    # Esto es que la Base01 tiene al menos una columna...
    if(is.null(Control02())) return(NULL)
    if(!Control02()[[1]]) return(NULL)
    
    # Si no hay un Control03() o no lo pasamos...
    # Esto es que la base se cargo y que el usuario quiere elegir un CIE...
    # Tenemos a input$cie_especificaciones == 2
    if(is.null(Control03())) return(NULL)
    if(!Control03()[[1]]) return(NULL)
    
    
    # Si el CIE no fue seleccionado... Nos vamos...      
    if(is.null(input$cie_columna)) return(NULL)
    if(input$cie_columna == "") return(NULL)
    if(is.na(input$cie_columna)) return(NULL)
    
    # Si el CIE seleccionado pertenece a la base de datos
    # vamos a dar un TRUE y sino, sera FALSE
    if(sum(colnames(Base01()) == input$cie_columna) > 0) dt_ok <- TRUE else dt_ok <- FALSE
    
    # Frases
    frase_TRUE <- ""
    frase_FALSE <- "Ver Control04() - CIE no pertenece a la base de datos"
    
    
    # Seleccion de frase      
    if (dt_ok) frase_salida <- frase_TRUE else frase_salida <- frase_FALSE
    
    # Armado de salida
    my_exit <- list(dt_ok, frase_salida)
    return(my_exit)
    
    
  })
  
  
  
  
  # Opcion de eleccion de categorias del CIE
  Categorias_CIE_Base01 <- reactive({
    
    if(is.null(Control04())) return(NULL)
    if(!Control04()[[1]]) return(NULL)
    
    if(sum(colnames(Base01()) == input$cie_columna) > 0) {        
      levels(as.factor(Base01()[,input$cie_columna]))
    } else return(NULL)
    
  })
  
  # Eleccion de la categoria de CIE
  output$OpcionesCIE_parte02 <- renderUI({
    
    # Si no hizo o no da bien el Control04, nos vamos.
    # Este control dice que se cargo Base01(), que hay una columna,
    # que el usuario quiere elegir un CIE, y que el CIE tiene al menos una 
    # categoria
    if(is.null(Control04())) return(NULL)
    if(!Control04()[[1]]) return(NULL)
    if(is.null(Categorias_CIE_Base01())) return(NULL)
    
    
    categorias <- Categorias_CIE_Base01()
    
    div(
      
      
      selectInput(inputId = ns("cie_categoria"), 
                  label = "Categoría de Inclusión (solo una categoría):", 
                  choices = c("Seleccione una categoría..." = "", categorias)
                  
      ),
      actionButton(ns("reset"), "Quitar CIE", icon("paper-plane"), 
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      actionButton(ns("cargar"), "Aplicar CIE", icon("paper-plane"), #class = "btn-warning", 
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
      
    )
    
    #        } else return(NULL)
    #   } else return(NULL)
    # } else return(NULL)
    # } else return(NULL)
  })
  
  # Eleccion de CIE y categoria CIE
  output$OpcionesCIE_completo <- renderUI({
    
    
    
    conditionalPanel( 
      'input.cie_especificaciones == "2"', ns = ns, 
      uiOutput(ns("OpcionesCIE_parte01")),
      
      uiOutput(ns("OpcionesCIE_parte02"))
      
    )
    
  })
  
  
  
  # Control 05 (Que la variable CIE tiene al menos una categoria)
  Control05 <- reactive({ 
    
    # Si no un Control04() o no lo pasamos...
    # Esto es que ha cargado la base, la base tiene una columna al menos,
    # ha elegido un CIE y el CIE pertenece efectivamente a la base de datos.
    if(is.null(Control04())) return(NULL)
    if(!Control04()[[1]]) return(NULL)
    
    
    # Categocias del CIE
    mis_categorias <- levels(as.factor(na.omit(Base01()[,input$cie_columna])))
    
    # Vemos si hay categorias y ahi vemos como sigue esto...
    dt_ok <- length(mis_categorias) > 0
    
    # Frases
    frase_TRUE <- ""
    frase_FALSE <- "La variable que cumplirá el rol de criterio de inclusión
                            estadístico (CIE) debe tener al menos una categoría."
    
    # Seleccion de frase      
    if (dt_ok) frase_salida <- frase_TRUE else frase_salida <- frase_FALSE
    
    # Armado de salida
    my_exit <- list(dt_ok, frase_salida)
    return(my_exit)
    
    
    
  })
  
  # Control 06 (Base cargada en Base01(), CIE elegido y categoria CIE elegida...)
  Control06 <- reactive({ 
    
    # Si no un Control05() o no lo pasamos...
    # Esto es que: ha cargado la base, la base tiene al menois una columna,
    # ha elegido un CIE y el CIE pertenece efectivamente a la base de datos,
    # y ya sabemos que tiene al menos una categoria que pueda elegir el usuario
    if(is.null(Control05())) return(NULL)
    if(!Control05()[[1]]) return(NULL)
    
    # Frases
    frase_TRUE <- ""
    frase_FALSE <- ""
    
    # Valores por defecto
    dt_ok <- FALSE
    frase_salida <- frase_FALSE
    mi_salida <- HTML(frase_salida)
    my_exit_default <- list(dt_ok,  mi_salida)
    
    
    # Si es null la categoria CIE... Nos fuimos
    if(is.null(input$cie_categoria)) return(my_exit_default)
    
    # Si no eligio todavia una categoria CIE... Nos fuimos
    if(input$cie_categoria == "") return(my_exit_default)
    
    
    dt_ok <- TRUE
    frase_salida <- frase_TRUE
    mi_salida <- HTML(frase_salida)
    
    my_exit <- list(dt_ok,  mi_salida)
    return(my_exit)
    
    
  })
  
  # Control 07 (input$cie_especificaciones == 2 + RMedic_general())
  # Control especial para aparicion de texto para CIE
  Control07 <- reactive({ 
    
    # Si es null la categoria CIE... Nos fuimos
    if(is.null(input$cie_categoria)) return(NULL)
    
    # Frases
    frase_TRUE <- ""
    frase_FALSE <- "
                    Para poder aplicar un Criterio de Inclusión Estadística (CIE): <br/>
                    &nbsp&nbsp 1) Seleccione un CIE (una variable).<br/>
                    &nbsp&nbsp 2) Elija una categoría de inclusión (solo una categoría).<br/>
                    &nbsp&nbsp 3) Presione 'Aplicar CIE'.
                    "
    
    # Valores por defecto
    dt_ok <- TRUE
    frase_salida <- frase_TRUE
    mi_salida <- HTML(frase_salida)
    my_exit_default <- list(dt_ok,  mi_salida)
    
    
    
    
    
    # Si es null la categoria CIE... Nos fuimos
    if(input$cie_especificaciones == 2 && !RMedic_general()) {
      dt_ok <- FALSE
      frase_salida <- frase_FALSE
      mi_salida <- HTML(frase_salida)
      my_exit_FALSE <- list(dt_ok,  mi_salida)
      return(my_exit_FALSE)
    } 
    
    
    
    return(my_exit_default)
    
    
    
    
  })
  
  # Base 2: con CIE aplicado
  Base02 <- reactive({
    
    # Si no hay una base cargada en Base01()... Paramos
    if (is.null(Base01())) return(NULL)
    
    # Si no hay Control06()...
    # Esto es... Una base... Con al menos una columna y una fila...
    # quiere usar un CIE... Eligio un CIE... El CIE si es una columna
    # de la base de datos... El CIE tiene al menos 1 categoria...
    # Ya eligio la categoria...
    if (is.null(Control06())) return(NULL)
    if (!Control06()[[1]]) return(NULL)
    
    mi_filtro <- Base01()[,input$cie_columna]
    dt_filtro <- mi_filtro == input$cie_categoria
    dt_filtro[is.na(dt_filtro)] <- FALSE
    base2 <- Base01()[dt_filtro, ]
    
    my_exit <- base2
    return(my_exit)
    
  })
  
  zocalo_CIE <- reactive({
    
     
    if(is.null(input$cie_especificaciones)) return(NULL)
    if(input$cie_especificaciones != 2) return(NULL)
    if(is.null(input$cie_columna)) return(NULL)
    if(input$cie_columna == '') return(NULL)
    if(is.null(input$cie_categoria)) return(NULL) 
    if(input$cie_categoria == '') return(NULL)
    
    armado <- list()
    
    # Salida HTML
    armado[[1]] <- paste0("<b>Criterio de Inclusión Estadístico (CIE): </b>", input$cie_columna, " - Columna ",
                          MyLetter(Base = BaseSalida(), the_col = input$cie_columna),"<br/>",
                          "<b>Categoría del CIE: </b>", input$cie_categoria)
    
    # Salida Normal
    armado[[2]] <- c(paste0("CIE: ", input$cie_columna, " - Columna ",
                            MyLetter(Base = BaseSalida(), 
                                     the_col = input$cie_columna)), 
                     paste0("Categoría del CIE: ", input$cie_categoria))
    
    return(armado)    
  })
  
  # Update - Todo OK!
  {
    
    # Variable criterio de inclusion
    observeEvent(reseteo_conteo(),{
      
      # Reset selected step
      # RMedic_general(F)
      freezeReactiveValue(input, "cie_especificaciones")
      updateRadioButtons(session, 
                         inputId = "cie_especificaciones", 
                         label = "Especificaciones", 
                         choices = c("Todas las filas" = 1, 
                                     "Aplicar un Criterio de Inclusión Estadístico" = 2))
    })
    
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
      
      freezeReactiveValue(input, "cie_especificaciones")
      updateRadioButtons(session, inputId = "cie_especificaciones",
                         label = "Especificaciones",
                         choices = c("Todas las filas" = 1,
                                     "Aplicar un Criterio de Inclusión Estadístico" = 2)
      )
      
    })
    
    # Reseteo por especificaciones del criterio de inclusion estadistico
    observeEvent(input$cie_especificaciones,{
      
      # Reset selected step
      if(input$cie_especificaciones == 2) RMedic_general(F) else 
        if(input$cie_especificaciones == 1) {
          
          RMedic_general(T)
          
          if(!is.null(Base01())) {
            letras <- num2let(c(1:length(colnames(Base01()))))
            armado <- colnames(Base01())  
            names(armado) <- paste0("(", letras, ") - ", colnames(Base01()))
            
            
            
            freezeReactiveValue(input, "cie_columnas")
            updateSelectInput(session, inputId = "cie_columna",
                              label = "Criterio de Inclusión Estadístico (CIE) - Seleccione una variable:",
                              choices =  c("Seleccione un CIE..." = "", armado)
            )
            
            
            
          }
          
        }
    })
    
    # Reseteo por cie_columna
    observeEvent(input$cie_columna,{
      
      
      # Reset selected step
      if(input$cie_especificaciones == 2) RMedic_general(F)
      
      
      # freezeReactiveValue(input, "cie_categoria")
      # updateSelectInput(session, inputId = "cie_categoria", 
      #                   label = "Categoría de Inclusión (solo una categoría):", 
      #                   choices = c("Seleccione una categoría..." = "", 
      #                               levels(as.factor(Base01()[,input$cie_columna])))
      # )
    })
    
    # Reseteo por cie_categoria
    observeEvent(input$cie_categoria,{
      
      # Reset selected step
      if(input$cie_especificaciones == 2) RMedic_general(F)
      
    })
    
    ####################################################################
    
    observeEvent(input$reset,{
      
      # Reset selected step
      # RMedic_general(F)
      if(!is.null(input$cie_categoria)) 
        if(input$cie_categoria != "")
          #RMedic_general(F)
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
  
  
#  Base utilizada finalmente
  BaseSalida <- reactive({
    
    # # Si no hay base cargada... Nos fuimos
    # if (is.null(Base01())) return(NULL) 
    # Si no hay un Control02() o no lo pasamos...
    # Esto es que la Base01() tiene al menos una columna...
    if(is.null(Control02())) return(NULL)
    if(!Control02()[[1]]) return(NULL)
    
    # Si aun no cargo el input$cie_especificaciones... Nos vamos...
    if(is.null(input$cie_especificaciones)) return(NULL)
    
    if(!RMedic_general()) return(NULL)
    
    
    # cat("colnames(Base01()): ", colnames(Base01()), "\n")
    # cat("colnames(Base02()): ", colnames(Base02()), "\n")
    
    if(input$cie_especificaciones == 1) return(Base01()) else
      if((input$cie_especificaciones == 2) &&
         (!is.null(Base02()))) return(Base02())
    
    
  })
  
  
  # Controlamos que efectivamente la base ha sido cargada
  # en BaseSalida() y que tiene al menos una columna
  status_BaseSalida <- reactive({
    
    if (!is.null(BaseSalida())) {
      if (ncol(BaseSalida()) > 0) {
        
        paso <- TRUE
        
      } else paso <- FALSE
    } else paso <- FALSE
    
    paso
  })
  
  # Salida de la base a utilizar finalmente
  output$BaseSalida <- renderDataTable({
    
    # Si aun no se establecio un status de BaseSalida(), nos vamos...
    if (is.null(status_BaseSalida())) return(NULL)
    
    # Si el status nos da negativo... Nos vamos...
    if (!status_BaseSalida()) return(NULL)
    
    # Si no se debe mostrar la base de datos... Nos vamos...
    if(!RMedic_general()) return(NULL)
    
    
    # Si esta todo OK, tenemos base y hay que mostrarla, esta es!
    
    datatable(BaseSalida(), rownames = F,  options = list(
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
    
    
    
    
    
  })
  

  ###############################################
  
  output$TextBase_Alert01 <- renderText({
    
    # Si no hay un control 1... Nos vamos...
    if (is.null(Control01())) return(NULL)
    
    # Return Exitoso
    return(Control01()[[2]])
  })
  
  output$TextBase_Alert02 <- renderText({
    
    # Si no hay un control 2... Nos vamos...
    if (is.null(Control02())) return(NULL)
    
    # Return Exitoso
    return(Control02()[[2]])
    
    
  })
  
  output$TextBase_Alert03 <- renderText({
    # Si no hay un control 3... Nos vamos...
    if (is.null(Control03())) return(NULL)
    
    # Return Exitoso
    return(Control03()[[2]])
  })
  
  output$TextBase_Alert04 <- renderText({
    # Si no hay un control 4... Nos vamos...
    if (is.null(Control04())) return(NULL)
    
    # Return Exitoso
    return(Control04()[[2]])
  })
  
  output$TextBase_Alert05 <- renderText({
    # Si no hay un control 1... Nos vamos...
    if (is.null(Control05())) return(NULL)
    
    # Return Exitoso
    return(Control05()[[2]])
  })
  
  output$TextBase_Alert06 <- renderText({
    # Si no hay un control 1... Nos vamos...
    if (is.null(Control06())) return(NULL)
    
    # Return Exitoso
    return(Control06()[[2]])
  })
  
  output$TextBase_Alert07 <- renderText({
    # Si no hay un control 1... Nos vamos...
    if (is.null(Control07())) return(NULL)
    
    # Return Exitoso
    return(Control07()[[2]])
  })
  
  output$TextBase_InfoDataSet_01 <- renderText({
    
    texto_salida <- c()
    
    # Si no hay Base01()... Nos vamos...
    if (is.null(Base01())) return(NULL)
    

    # Si no hay permisos... Nos vamos...
    if(!RMedic_general()) return(NULL)
    
 
    # Textos varios
    {
      texto_completo01 <- c("<b>Base:</b> _mi_archivo_ <br/>
                           <b>Variables (Columnas):</b> _ncolBase01_ variables.<br/>
                           <b>Unidades (Filas o repeticiones):</b> _nrowBase01_ unidades.<br/>")
      
      
      
      
      
      
      
      texto_completo02 <- c("
                            <b>Base:</b> _mi_archivo_ <br/>
                            <b>Variables (Columnas):</b> _ncolBase01_ variables.<br/>
                            <b>Unidades totales (Filas totales o repeticiones totales):</b> _nrowBase01_ unidades.<br/>
                            <br/>
                            <b>Criterio de Inclusión Estadístico (CIE):</b> _mi_CIE_.<br/>
                            <b>Categoría de Inclusión:</b> la categoría seleccionada es '_mi_categoria_'.<br/>
                            <b>Unidades seleccionadas (Filas seleccionadas o repeticiones seleccionadas):</b> _nrowBase02_ de _nrowBase01_ unidades.<br/>
                              ")
      
    }
    #####################################
    
    
    # Por defecto...
    texto_salida <- texto_completo01
    
    
    if(input$cie_especificaciones == 2) {
      if (!is.null(Base02()) && !is.null(input$cie_categoria)) {
        
        # Cambiamos de texto
        texto_salida <- texto_completo02
        
        dt_col <- colnames(Base02()) == input$cie_columna
        orden_col <- c(1:length(dt_col))[dt_col]
        armado <- paste0("Variable '", input$cie_columna, "' - Letra Columna '",
                         num2let(orden_col), "'")
        
        #  armado <- "AVERRERRRRR"
        texto_salida <- gsub("_mi_CIE_", armado, texto_salida)
        
        texto_salida <- gsub("_mi_categoria_", input$cie_categoria, texto_salida)
        texto_salida <- gsub("_nrowBase02_", nrow(Base02()), texto_salida)
        
      }
    }
    #  HTML(paste(t1, t2, sep = '<br/>'))
    
    
    texto_salida <- gsub("_mi_archivo_", MyFileName(),texto_salida)
    texto_salida <- gsub("_ncolBase01_", ncol(Base01()),texto_salida)
    texto_salida <- gsub("_nrowBase01_", nrow(Base01()),texto_salida)
    
    mi_salida <- HTML(texto_salida)
    
    return(mi_salida)
    
    
  })
  
  output$TextBase_InfoDataSet_02 <- renderText({
    
    texto_salida <- c()
    
    # Si no hay Base01()... Nos vamos...
    if (is.null(Base01())) return(NULL)
    
    # Si no hay permisos... Nos vamos...
    if(!RMedic_general()) return(NULL)
    
    
    # Textos varios
    {
      
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
    }
    ######################
    
    
    # Si tenemos ya una Base02 y tenemos elegida una cateogria CIE
    if(input$cie_especificaciones == 2) {
      if (!is.null(Base02()) && !is.null(input$cie_categoria)) {
        
        # El texto camabia de acuerdo a si efectivamnete
        # el CIE ha sacado filas de la base o no.
        if(nrow(Base01()) != nrow(Base02())) texto_salida <- agregado01 else texto_salida <- agregado02
        
        
        
        dt_col <- colnames(Base02()) == input$cie_columna
        orden_col <- c(1:length(dt_col))[dt_col]
        armado <- paste0("Variable '", input$cie_columna, "' - Letra Columna '",
                         num2let(orden_col), "'")
        
        
        texto_salida <- gsub("_mi_CIE_", armado, texto_salida)
        texto_salida <- gsub("_mi_categoria_", input$cie_categoria, texto_salida)
        texto_salida <- gsub("_nrowBase02_", nrow(Base02()), texto_salida)
        
        
        
        
        
        texto_salida <- gsub("_mi_archivo_", MyFileName(),texto_salida)
        texto_salida <- gsub("_ncolBase01_", ncol(Base01()),texto_salida)
        texto_salida <- gsub("_nrowBase01_", nrow(Base01()),texto_salida)
        
      }
    }
    
    
    mi_salida <- HTML(texto_salida)
    
    return(mi_salida)
    
    
  })
  
  output$TextBase_Intro <- renderText({
    if (!is.null(Base01())) {
      if(RMedic_general()){
        "Visualización de la Base de Datos"
      } else return(NULL)
    } else return(NULL)
  })
  
  ################################################
  
  
  # Return del Modulo
  if(is.null(BaseSalida)) return(NULL)
  
  
  # Return del Modulo!
  return(
    list(
      BaseSalida = BaseSalida,
      zocalo_CIE = zocalo_CIE,
      RMedic_general = RMedic_general,
      status_BaseSalida = status_BaseSalida
      )
  )
  
  
}


