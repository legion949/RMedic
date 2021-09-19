
OpcionesDeCarga <- div(  selectInput(inputId = "FileTypePicker", 
                                     label = 'Qué tipo de Archivo quieres subir?',
                                     choices = c('Excel' = 'Excel', 
                                                 'CSV' = 'CSV', 
                                                 'Ejemplos' = 'Ejemplos'), 
                                     selected = 'Excel',
                                     selectize = T),
                         conditionalPanel( 
                           'input.FileTypePicker == "Excel"',
                           fileInput('xls_file', 'Elige tu archivo Excel',
                                     accept=c('application/vnd.ms-excel', 
                                              'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'))
                         ),
                         # un CSV
                         conditionalPanel( 
                           'input.FileTypePicker == "CSV"',
                           
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
                         conditionalPanel( 
                           'input.FileTypePicker == "Ejemplos"',
                           selectInput("ejemplo_file", "Elige una base ejemplo:", 
                                       choices = c("rock", "pressure", "cars", "mtcars", "iris"),
                                       selected = "mtcars")
                         ),
                         radioButtons(inputId = "cie_especificaciones", 
                                      label = "Especificaciones", 
                                      choices = c("Todas las filas" = 1, 
                                                  "Aplicar un Criterio de Inclusión Estadístico" = 2)),
                         br(),
                       uiOutput("OpcionesCIE_completo")
                        
                        
                         
)
