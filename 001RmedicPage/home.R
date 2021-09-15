tabPanel(title = "Inicio", icon = icon("home"),
         
         tagList(
           tags$head(
             includeScript("tools/google-analytics.js"),
             tags$script(type="text/javascript", src = "busy.js"),
             tags$link(rel="shortcut icon", href="./GlioVis_logo_bar.bmp"),
             tags$script(type="text/javascript", "var switchTo5x=true"),
             tags$script(type="text/javascript", src="http://w.sharethis.com/button/buttons.js"),
             tags$script(type="text/javascript",'stLight.options({publisher: "675b3562-a081-470a-9fc4-3dd6a712209d", doNotHash: true, doNotCopy: true, hashAddressBar: false})')
           )
         ),
         
         div(id = "home",
             div(class="pull-right",
                 span(class='st_twitter', displayText='Tweet'),
                 span(class='st_linkedin', displayText='LinkedIn'),
                 span(class='st_facebook', displayText='Facebook'),
                 span(class='st_email', displayText='Email')
             ),
             br(),
             a(href ="http://www.lavoz.com.ar/",div(class="simple-ss",id="simple-ss")),
             # img(src = "new_logo_transparent.png", width = 900),
             
         
#          titlePanel("submitButton example"),
#          fluidRow(
#            column(3, wellPanel(
#              sliderInput("n", "N:", min = 10, max = 1000, value = 200,
#                          step = 10),
#              textInput("text", "Text:", "text here"),
#              submitButton("Submit")
#            )),
#            column(6,
#                   plotOutput("plot1", width = 400, height = 300),
#                   verbatimTextOutput("text")
#            )
#          ),
         h4(class = "outer", "¿Cómo funciona RMedic?"),
         p(class = "outer"," RMedic es muy fácil de usar:"),
         tags$ol(
           tags$li('Selecciona la pestaña RMedic del menú superior'),
           tags$li("Elije una base de datos o carga una desde tu celular o computadora"),
           tags$li("Selecciona qué estadística quieres realizar"),
           tags$li("Elige qué gráfico quieres obtener"),
           tags$li("Descarga tus resultados!")
         ),
         
         h4(class = "outer", "¿Debo instalar RMedic?"),
         p(class = "outer",strong("No es necesario!"), 'RMedic es una plataforma de trabajo online.', br(), 
           'Puedes ingresar desde cualquier computadora o celular con internet, los 7 días de la semana, las 24 hs, todo el año.'),
         
         
         h4(class = "outer", "¿Qué tipos de datos puedo analizar?"),
         p(class = "outer",'RMedic es una herramienta muy versátil, capaz de analizar diferentes tipos de información médica.',
           'Puedes subir tus propias bases de datos, o desarrollar experiencia estadística con bases de datos de otros grupos médicos.'),
            tabsetPanel(id = "datasets", 
                         tabPanel(title=p("Adult"),
                                dataTableOutput("adult_table"),
                                helpText('* multiple datasets combined; ** multiple samples from different tumor features; *** more than one sample for each patients')
                         ),
                        tabPanel(title= p("Pediatric"),
                               dataTableOutput("pediatric_table"),
                               helpText('* includes adult samples')
                         )
              ), br(),
# #              h4(class = "outer", "Which gene ID can I use?"),
#              p(class = "outer",'Currently only', a("HGNC-approved", href="http://www.genenames.org"), '"Gene Symbols" are supported.'),
#              h4(class = "outer", "Can I download the plots?"),
#              p(class = "outer",'Yes, all the plots can be downloaded in various formats: .pdf, .bmp, .jpeg or .jpg.'),
#              
#              h4(class = "outer", "Can I download the data?"),
#              p(class = "outer",'Yes, it is', strong("highly recommended"), 'for reproducibility issues. For each plot there is a "Data" tab containing
#                the actual data used to generate the plot. The data can also be downloaded at "Explore/Summary/Data".'),
h4(class = "outer", "¿Puedo usar RMedic para los resultados de mis publicaciones?"),
p(class = "outer", strong("Claro que si!"), 'Si lo haces, por favor incluye en las referencias que has usado y cita:', a("Mangeaud A. y Elías Panigo D.", href="#addRef", target="_blank"), " (manuscript in preparation)."),


h4(class = "outer", "¿Qué otras herramientas están disponibles?"),
p(class = "outer",'El proyecto RMedic comprende otras posibilidades:'),
tags$ol(
  tags$li('Análisis estadísticos en tiempo real'),
  tags$li("Generación de reportes automatizados (se actualiza la base de datos, se actualiza el informe instantáneamente!)."),
  tags$li("Reportes estadísticos en formato tipo libro (listos para ser impresos y presentados!)"),
  tags$li("Posibilidad de compartir los resultados estadísticos por internet."),
  tags$li("Posibilidad de amplicar el proyecto según las necesidades de cada institución, empresa u organismo.")
),


h4(class = "outer", "Tengo más dudas...¿Cómo puedo contactarlos?"),
p(class = "outer",'Puedes contactarnos por correo electrónico', a("email", href="mailto:david.elias949@gmail.com")),
hr(), 
tags$blockquote(class="pull-right",
                tags$p(style = 'font-style:italic;',"Vamoo los locoooooo."),
                tags$small("Dante Glakvax")),
br(),br(),br(),br(),br()

))