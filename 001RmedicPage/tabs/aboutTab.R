# UI-elements for About tab
tabPanel(title = "Proyecto", icon = icon("info-circle"),
         div(class="about",
             includeMarkdown("tools/about.Rmd")
         )
)