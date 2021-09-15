


# Server del SideBarPanel
source("script/008svSideBarPanel.R", local = T)$value



# Server de la Pestania "Base de Datos"
source("../004Base/script/004svMainPanel.R", local = T)$value

# Server de la Pestania "Control"
source("../005Control/script/005svMainPanel.R", local = T)$value

# Carga NOX
source("../ZZZ_NOX/NOX.R", local = T)$value
source("../ZZZ_NOX/NOX_01.R", local = T)$value
source("../ZZZ_NOX/NOX_02.R", local = T)$value
source("../ZZZ_NOX/NOX_03.R", local = T)$value
source("../ZZZ_NOX/NOX_04.R", local = T)$value
source("../ZZZ_NOX/NOX_05.R", local = T)$value

# Server de la Pestania "Tablas"
source("../006Tablas/script/006svDescGeneral.R", local = T)$value
source("../006Tablas/script/006svMainPanel.R", local = T)$value


# Server elementos Graficos Generales
source("../007Graficos/script/007svGrafGeneral_01.R", local = T)$value
source("../007Graficos/script/007svGrafGeneral_02.R", local = T)$value
source("../007Graficos/script/007svGrafGeneral_03.R", local = T)$value

# Server Main Panel de Graficos
source("../007Graficos/script/007svMainPanel.R", local = T)$value



# # Server del MainPanel Local
source("script/008svMainPanel.R", local = T)$value





# Server elementos Ho Generales
source("script/008svHoGeneral_00.R", local = T)$value # Generalidad de Ho
source("script/008svHoGeneral_01.R", local = T)$value # 1Q - 01 - Test de Proporciones
source("script/008svHoGeneral_22.R", local = T)$value # 1Q - 02 - Test de Uniformidad

source("script/008svHoGeneral_02.R", local = T)$value # 1C - 02 - Normalidad de Shapiro-Wilks
source("script/008svHoGeneral_03.R", local = T)$value # 1C - 03 - Test t
source("script/008svHoGeneral_04.R", local = T)$value # 1C - 04 - Test Mann-Whitney

source("script/008svHoGeneral_05.R", local = T)$value # 2Q - 05 - Test de Proporciones
source("script/008svHoGeneral_06.R", local = T)$value # 2Q - 06 - RegLog
source("script/008svHoGeneral_07.R", local = T)$value # 2Q - 07 - Chi Cuadrado
source("script/008svHoGeneral_08.R", local = T)$value # 2Q - 08 - Otros

source("script/008svHoGeneral_09.R", local = T)$value # 2C - 09 - Homogeneidad de Varianzas
source("script/008svHoGeneral_10.R", local = T)$value # 2C - 10 - Correlacion de Pearson
source("script/008svHoGeneral_11.R", local = T)$value # 2C - 11 - Correlacion de Spearman
source("script/008svHoGeneral_12.R", local = T)$value # 2C - 12 - Regresion Lineal Simple
source("script/008svHoGeneral_13.R", local = T)$value # 2C - 13 - RegLog
source("script/008svHoGeneral_14.R", local = T)$value # 2C - 14 - Test t apareado
source("script/008svHoGeneral_15.R", local = T)$value # 2C - 15 - Test W apareado

source("script/008svHoGeneral_16.R", local = T)$value # QC - 16 - Homogeneidad
source("script/008svHoGeneral_17.R", local = T)$value # QC - 17 - Test t
source("script/008svHoGeneral_18.R", local = T)$value # QC - 18 - Mann-Whitney
source("script/008svHoGeneral_19.R", local = T)$value # QC - 19 - ANOVA 1 FACTOR
source("script/008svHoGeneral_20.R", local = T)$value # QC - 20 - Kruskal-Wallis
source("script/008svHoGeneral_21.R", local = T)$value # QC - 21 - RegLog



# Objeto Salida ui para el MainPanel
output$uiMainPanel <- renderUI({
  
  fluidPage(
    uiOutput("SALIDAmenuHo"),
    #tableOutput("SAYA")
    uiOutput("planeta_goku")
  )
  
})

