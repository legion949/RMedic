# UI-elements for News tab
tabPanel(title = "Manual", icon = icon("newspaper-o"),
         includeMarkdown("tools/manual.Rmd"),
       #  a("google",href="http://www.google.com"),
    #     a("Manual e Iniciación - Rmedic",target="_blank",href="Manual_Rmedic.pdf"),
   
    
                  # tags$iframe(style="height:400px; width:100%; scrolling=yes", 
                  #             src="Manual_Rmedic.pdf"),
      # downloadButton('Manual_Rmedic'),
       br(),
    a("Archivo para citar RMedic",target="_blank",href="RMedic_Cita.pdf"),
    br(),
    a("Base de Datos - Ejemplo (xls)",target="_blank",href="p1_ej1.xls"),
    br(),
    a("Manual e Iniciación - Rmedic (Word)",target="_blank",href="Manual_e_Iniciacion_Rmedic.doc"),
    br(),
    a("Manual e Iniciación - Rmedic (PDF)",target="_blank", href="Manual_e_Iniciacion_Rmedic.pdf"),
    br(),
    a("Manual y Base de Datos - Rmedic (ZIP)",target="_blank", href="RMedic_Manual_y_Base.zip"),
    # br(),
    # tags$iframe(style="height:400px; width:100%; scrolling=yes", 
    #             src="Manual_e_Iniciacion_Rmedic.pdf"),
    
       br(),
       br(),
       br()
)