

conditionalPanel ("1 == 1",
# source("home.R", local = TRUE)$value,
source("tabs/homeTab.R", local = TRUE)$value,
source("tabs/RMedicTab.R", local = TRUE)$value,

############################################################
source("tabs/herramientasTab.R", local = TRUE)$value,
############################################################
source("tabs/aboutTab.R", local = TRUE)$value,
source("tabs/aboutTab.R", local = TRUE)$value,
source("tabs/newsTab.R", local = TRUE)$value,
source("tabs/quienesTab.R", local = TRUE)$value,
source("tabs/serviciosTab.R", local = TRUE)$value
)