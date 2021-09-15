"datasets"  "utils"     "grDevices" "graphics"  "stats"     "methods" 


getOption("defaultPackages")



listado_inicial <- c("datasets",  "utils",     "grDevices", "graphics",  "stats",     "methods"  )



listado_agregado <- c("shiny", "xtable", "rCharts", "plotly", "colourpicker", "readxl", "DT", "Hmisc", "fmsb", "RCurl", "XML", "gdata", "XLConnect", "ggplot2", "shinyjs", "ggvis", "sfsmisc", "gplots", "agricolae", "coin")


options(defaultPackages = c(listado_inicial, listado_agregado))


getOption("defaultPackages")
