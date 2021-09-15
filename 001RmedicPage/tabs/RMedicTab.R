# UI-elements for News tab
tabPanel(title = "RMedic", icon = icon("glyphicon glyphicon-plus"),
         # link to the style.css file.
       #  tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
         br(),
       #  fluidRow(column(4),column(4,img(src = "reinafabiola.png")), column(4), column(4,img(src = "ucc_logo.png"))),
          # fluidRow(column(4), fluidRow(column(4,
          #                                     img(src = "ucc_logo.jpg", width = 300, height = 150)),
          #                              column(2), 
          #                              column(4,img(src = "LOGO_ERGO_2.jpg", width = 300, height = 150)))),
          # 
       
       br(),
         sidebarLayout(fluid = FALSE,
                       sidebarPanel(
                         
                         # Cargamos el Main Panel
                         uiOutput("uiSideBarPanel")
                         
                         
                       ),
                       
                       # Show a summary of the dataset and an HTML table with the
                       # requested number of observations. Note the use of the h4
                       # function to provide an additional header above each output
                       # section.
                       mainPanel(
                         uiOutput('uiMainPanel')
                       )
                         
         ) # Fin siderbarLayout
) # Fin tabPanel
         
         