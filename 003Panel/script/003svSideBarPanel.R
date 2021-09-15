
# # # 003 - Server - Sider Bar Panel
# # # Son dos partes.
# La parte 1... tiene los elementos en si del menu laterial.
# La parte 2... son los update de los elemntos ya designados


# Parte 1
output$uiSideBarPanel <- renderUI({
  
  conditionalPanel("1 == 1",
  # Tipo de Archivo o Carpeta
  selectInput(inputId = "selector_tipo_archivo", 
              label = 'Qué tipo de Archivo quieres subir?',
              choices = c('Excel' = 'Excel', 
                          'CSV' = 'CSV'), 
              selected = 'Excel',
              selectize = T),  
  
  # Un Excel
  conditionalPanel( 
    'input.selector_tipo_archivo == "Excel"',
    fileInput('xls_file', 'Elige tu archivo Excel',
              accept=c('application/vnd.ms-excel', 
                       'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'))
  ),
  
  
  # un CSV
  conditionalPanel( 
    'input.selector_tipo_archivo == "CSV"',
    
    # Carga de CSV
    fileInput('csv_file', 'Elige tu archivo CSV',
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', '.csv')),
    
    # Detalles para CSV
    # # Header
    checkboxInput('header', 'Encabezado', TRUE),
    
    # # Separador de columna
    radioButtons('sep', 'Separador',
                 c(Coma=',',
                   'Punto y Coma'=';',
                   Tabulación='\t'),
                 ';'),
    
    # # Separador Decimal
    radioButtons('dec', 'Decimal',
                 c(Coma=',', Punto='.'),
                 '.'),
    # # Quote
    radioButtons('quote', 'Comillas',
                 c('Sin Comillas'='',
                   'Comillas Dobles'='"',
                   'Comillas Simples'="'"),
                 '"')
  ),
  
  conditionalPanel("input.selector_tipo_archivo == 'Cursos 2018'",
                   selectInput(inputId = "selector_cursos2018", 
                               label = 'Elija uno de los cursos',
                               choices = "Este cursooooo", 
                               selected = 'Este cursooooo',
                               selectize = T),
                   
                   selectInput(inputId = "selector_base_guardada", 
                               label = 'Elija una Base de Datos',
                               choices = "Esta Base", 
                               selected = 'Esta Base',
                               selectize = T)
                   
                   
                   
                   
  ), # Fin Conditional Panel
  
  conditionalPanel( 
    'input.selector_tipo_archivo == "Ejemplo"',
    selectInput("ejemplo_file", "Elige una base ejemplo:", 
                choices = c("Base Ejemplo", "rock", "pressure", "cars", "mtcars", "AVER", "iris"),
                selected = "Base Ejemplo",
                selectize = T)
  ),
  
  
  conditionalPanel( 
    'input.selector_tipo_archivo == "UCC"',
    selectInput("bases_ucc", "Bases UCC:", 
                choices = "Bases UCC - Bases UCC",
                selected = "Bases UCC - Bases UCC",
                selectize = T)
  )
  
  ) # Fin conditional
})
  

# Parte 2
observe({
  
  basico <- c("Excel", "CSV", "Ejemplo")
  
  # Nombres de archivos o carpetas, segun corresponda
  nombres_carpetas  <- list.files("../data")
  nombres_carpetas2 <- c(basico, nombres_carpetas)
  nombres_carpetas3 <- list.files("../data/Cursos 2018")
  nombres_carpetas4 <- list.files(paste0("../data/Cursos 2018/", input$selector_cursos2018))
  nombres_carpetas5 <- list.files(paste0(GoUp("data"), "/UCC"))
  

  # Opciones disponibles por "namel"
  opciones_carga2 <- namel(nombres_carpetas2)
  opciones_carga3 <- namel(nombres_carpetas3)
  opciones_carga4 <- namel(nombres_carpetas4)
  opciones_carga5 <- namel(nombres_carpetas5)

  
  # Update Tipo de Archivo
  updateSelectInput(session, 
                    inputId = "selector_tipo_archivo", 
                    label = 'Qué tipo de Archivo quieres subir?',
                    choices = opciones_carga2, 
                    selected = opciones_carga2[1])  

  # Update Carpeta de Cursos
  updateSelectInput(session, 
                    inputId = "selector_cursos2018", 
                    label = 'Elija uno de los cursos',
                    choices = opciones_carga3, 
                    selected = opciones_carga3[1])  

  # Update de Bases de Datos
  updateSelectInput(session, 
                    inputId = "selector_base_guardada", 
                    label = 'Elija una Base de Datos',
                    choices = opciones_carga4, 
                    selected = opciones_carga4[1])  


  # Update Bases UCC
  updateSelectInput(session, inputId = "bases_ucc", 
                  label = "Bases UCC:",
                  choices = opciones_carga5, 
                  selected = opciones_carga5[1])  

})



