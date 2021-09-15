# UI-elements for News tab
tabPanel(title = "Herramientas", icon = icon("glyphicon glyphicon-plus"),
         # link to the style.css file.
         tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
         br(),
       #  fluidRow(column(4),column(4,img(src = "reinafabiola.png")), column(4), column(4,img(src = "ucc_logo.png"))),
       # fluidRow(column(4), fluidRow(column(4,
       #                                     img(src = "ucc_logo.png", width = 300, height = 150)),
       #                              column(2), 
       #                              column(4,img(src = "reinafabiola.png", width = 350, height = 100)))),
       # br(),
         sidebarLayout(fluid = FALSE,
                       
                       sidebarPanel(
                        
                        # uiOutput("file_aula"),
                      source("../009App/AppGeneral/script/002_sidebarPanel.R", local = TRUE)$value
                        ),  # Fin sidebarPanel
                       
                       mainPanel(
                        source("../009App/AppGeneral/script/003_mainPanel.R", local = TRUE)$value
                         )
                         )
                         
         ) # Fin siderbarLayout

         
         
