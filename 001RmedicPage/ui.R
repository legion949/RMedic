
library(shinyjs)
# Puchin 2

shinyUI(
  navbarPage(theme = "styles.css",inverse=TRUE,
             title = strong("I need a RMEDIC here!"),
             windowTitle = "RMedic - Medicina y R", 
             fluid = TRUE, footer = includeHTML("tools/footer.html"),
             id = "nav",
             
            # source("home.R", local = TRUE)$value,
            source("tabs/homeTab.R", local = TRUE)$value,
            source("tabs/RMedicTab.R", local = TRUE)$value,

############################################################
source("tabs/herramientasTab.R", local = TRUE)$value,
############################################################
source("tabs/aboutTab.R", local = TRUE)$value,
# source("tabs/aboutTab.R", local = TRUE)$value,
source("tabs/newsTab.R", local = TRUE)$value,
source("tabs/quienesTab.R", local = TRUE)$value,
source("tabs/manualTab.R", local = TRUE)$value,
source("tabs/serviciosTab.R", local = TRUE)$value
))

