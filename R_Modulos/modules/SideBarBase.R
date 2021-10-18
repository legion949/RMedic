## Segmento del UI
SideBarBaseUI <- function(id) {
  ns <- NS(id)
  
  # Este es el sidebarpanel completo
  div(  
    selectInput(inputId = ns("FileTypePicker"), 
                label = "Qué tipo de Archivo quieres subir?",
                choices = c("Excel" = "Excel", 
                            "CSV" = "CSV", 
                            "Ejemplos" = "Ejemplos"), 
                    selected = "Excel",
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
        uiOutput(ns("OpcionesCIE_completo"))
        
        
        
  )
  
  
}




## Segmento del server
SideBarBaseSERVER <- function(input, output, session) {
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  
  
  
}


