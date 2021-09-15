
# Tab Inicio (HOME)
tabPanel(title = "Inicio", icon = icon("home"),
         
         tagList(
           tags$head(
      #         includeScript("tools/google-analytics.js"),  # Esto lo saque... por que tal vez este mandando informacion que no quiero que mande...
             tags$script(type="text/javascript", src = "busy.js"),
             tags$link(rel="shortcut icon", href="./rmediclogo.jpg"),
             tags$script(type="text/javascript", "var switchTo5x=true"),
      #         tags$script(type="text/javascript", src="http://w.sharethis.com/button/buttons.js"),
             tags$script(type="text/javascript",'stLight.options({publisher: "675b3562-a081-470a-9fc4-3dd6a712209d", doNotHash: true, doNotCopy: true, hashAddressBar: false})')
           )
         ),
         
         div(id = "home",
             # div(class="pull-right",
             #     span(class='st_twitter', displayText='Tweet'),
             #     span(class='st_linkedin', displayText='LinkedIn'),
             #     span(class='st_facebook', displayText='Facebook'),
             #     span(class='st_email', displayText='Email')
             # ),
             br(),
             # fluidRow(
             #   column(4,img(src = "ucc_logo.jpg", width = 300, height = 150)),
             #   column(2),
             #   column(4,img(src = "LOGO_ERGO_2.jpg", width = 300, height = 150))),
          #     column(4,img(src = "reinafabiola.png", width = 350, height = 100))),
          #   a(href ="http://www.lavoz.com.ar/",div(class="simple-ss",id="simple-ss")),
             # img(src = "new_logo_transparent.png", width = 900),
             
             # div(class="intro-divider"),
       ###      p(class = "lead", "Bienvenido a", strong("RMedic"),": Estadística y Desarrollos en R para ",strong("profesionales de la Salud.")),
             
          fluidRow(column(4),
                   column(4,img(src = "rmediclogo.jpg", width = 300, height = 300)),
                   column(4)),
       h4(class = "outer", "¿Qué es R-Medic?"),
       p(class = "outer", strong("R-Medic"), " es un programa  de  análisis  estadísticos  sencillo e intuitivo.", br(),
         "Podrás subir tu base de datos, generar tablas, gráficos, análisis estadísticos y descargar todo el contenido.", br(),
         "Brinda junto a los análisis estadísticos ayuda en la interpretación de los resultados obtenidos."),
       br(),
       h4(class = "outer", "¿Puedo usar RMedic para los resultados de mis publicaciones?"),
       p(class = "outer", strong("Claro que si!"), br(), 
         'Si lo haces, debes:',
         tags$ol(
           tags$li('Incluir en "Materiales y Métodos" a ',strong("R-Medic"),'como software estadístico:'),
           tags$li('Citar en tu "Bibliografía" textualmente la siguiente frase: ', h4(class = "outer",strong('"Mangeaud A , Elías Panigo DH. 2018  R-Medic. Un programa de análisis estadísticos sencillo e intuitivo. Revista Methodo 3 (1) 18-22."')))
         ),
           a("Archivo para citar RMedic",target="_blank",href="RMedic_Cita.pdf"),
       br(),
       br(),
          # div(style = "border: 1px solid; padding:10px; border-radius: 15px;",
          #        # p(style = "background-color: #68cbd0; border-radius: 15px; padding-left:10px; border: 1px solid #E3E3E3;",
          #        #   strong("NEWS")),
          #        includeMarkdown("tools/news_home.Rmd")
          #    ),




            

             h4(class = "outer", "¿Qué otras herramientas están disponibles?"),
             p(class = "outer",'El proyecto RMedic comprende otras posibilidades:'),
             tags$ol(
               tags$li('Análisis estadísticos en tiempo real'),
               tags$li("Generación de reportes automatizados (se actualiza la base de datos, se actualiza el informe instantáneamente!)."),
               tags$li("Reportes estadísticos en formato tipo libro (listos para ser impresos y presentados!)"),
               tags$li("Posibilidad de compartir los resultados estadísticos por internet."),
               tags$li("Posibilidad de ampliar el proyecto según las necesidades de cada institución, empresa u organismo.")
             ),
       br(),
       h4(class = "outer", "Tengo más dudas...¿Cómo puedo contactarlos?"),
       p(class = "outer",'Puedes contactarnos por correo electrónico: ', a("d.eliaspanigo@gmail.com", href="mailto:david.elias949@gmail.com"))
       
),
             
             
                         hr(), 
             tags$blockquote(class="pull-right",
                             tags$p(style = 'font-style:italic;',"La salud no lo es todo; pero sin ella, todo lo demás es nada."),
                             tags$small("Shopenhauer")),
             br(),br(),br(),br(),br()
             
         )
)