



# Solo sobrevida General... objetos...

output$MOD_general <- renderUI({
  
  if(paso_BASE(BASE_SALIDA())) {
    if(!is.null(input$tiempo) && input$tiempo != "") {
      if(!is.null(input$mortalidad) && input$mortalidad != "") {
        
        fluidRow(
          column(6,
                 h3("Gráfico de Sobrevida General"),
                 plotOutput("grafico_SobGen", width = "400px", height = "400px")
          ),
          column(6,
                 h3("Tabla Resumen"),
                 tableOutput("tabla_SobGen")
          )
        )
        
      } else return(NULL)
    } else return(NULL)
  } else return(NULL)
  
})



output$MOD_grupo <- renderUI({
  
  if(paso_BASE(BASE_SALIDA())) {
    if(!is.null(input$tiempo) && input$tiempo != "") {
      if(!is.null(input$mortalidad) && input$mortalidad != "") {
        if(!is.null(input$grupo) && input$grupo != "") {
          
        
          fluidRow(
            column(6,
                   h3("Gráfico de Sobrevida General por Grupos"),
                   plotOutput("grafico_SobGrupo", width = "400px", height = "400px")
            ),
            column(6,
                   h3("Test Estadístico Kaplan-Meier"),
                   tableOutput("resumen_km"),
                   textOutput("texto_km"), br(),
                   h3("Medidas Resumen"),
                   tableOutput("tabla_SobGrupo")
            ))
        
        } else return(NULL)
      } else return(NULL)
    } else return(NULL)
  } else return(NULL)
  
})

# Prueba de Hipotesis para 2 Variables Cuantitativas
output$MODmenu_sobrevida <- renderUI ({
  
  if(paso_BASE(BASE_SALIDA())) {
    if(!is.null(input$tiempo) && input$tiempo != "") {
      if(!is.null(input$mortalidad) && input$mortalidad != "") {
        
    tabsetPanel(id="menu_sobrevida",
                tabPanel("Sobrevida General", value=1, 
                         uiOutput("MOD_general")
                         ),
                tabPanel("Sobrevida General por Grupos", value=2,
                         selectInput("grupo", label="Grupo", 
                                     choices = c("Seleccione..." = "",colnames(BASE_SALIDA()))),
                         
                         uiOutput("MOD_grupo")
    )
    )
    
  } else return(NULL)
  } else return(NULL)
  } else return(NULL)
})



MINI1 <- reactive({
  
  v1 <- input$tiempo
  v2 <- input$mortalidad
  
  MINI <- BASE_SALIDA()
  MINI <- MINI[,c(v1, v2)]
  MINI
  
})

MINI2 <- reactive({
  
  v1 <- input$tiempo
  v2 <- input$mortalidad
  v3 <- input$grupo
  
  MINI <- BASE_SALIDA()
  MINI <- MINI[,c(v1, v2, v3)]
  MINI
  
})



# Calculos de Sobrevida General
SobGen_TABLA <- reactive({
  
  # Cargamos librerias
  library(xtable)
  library(survival)
  
  
  
  # Juntamos los elementos en un objeto llamado BASE
  BASE <- MINI1()
  
  # Armamos la MINIBASE
  MINIBASE <- BASE
  MINIBASE <- na.omit(MINIBASE)
  
  
  # Separamos los elementos
  #  orden  <-  MINIBASE[,1]
  tiempo <- MINIBASE[,1]
  status <- MINIBASE[,2]
  #  grupo  <-  MINIBASE[,4]
  
  # Sobrevida General con R
  SGen <- survfit(Surv(tiempo, status) ~ 1, conf.type="log", conf.int=0.95, type="kaplan-meier", error="greenwood")
  SGen
  
  
  # La salida anterior, no es un objeto en R.
  # La muestra como texto, pero no se puede utilizar para armar una salida
  # reactiva en RMedic.
  # A si que... la vamos a hacer a mano.
  
  # Guardamos la poca informacion que nos da
  TABLA <- summary(SGen)
  surv_mediana <- TABLA[["surv"]]
  orden_mediana <- c(1:length(surv_mediana))
  
  # Armamos algunos elementos
  TABLA1 <- data.frame(cbind(orden_mediana, TABLA[["surv"]], TABLA[["time"]]))
  TABLA2 <- data.frame(cbind(orden_mediana, TABLA[["lower"]], TABLA[["time"]]))
  TABLA3 <- data.frame(cbind(orden_mediana, TABLA[["upper"]], TABLA[["time"]]))
  
  
  # Encontramos la mediana
  TABLA1 <- na.omit(TABLA1)
  mediana_KM <- NA
  if(ncol(TABLA1) > 1)   if (nrow(TABLA1) > 0) if (min(TABLA1[,2]) <= 0.5) {
    
    dt <- TABLA1[,2] <= 0.5
    TABLA1 <- TABLA1[dt,]
    mediana_KM <- TABLA1[1,3]
  }
  #  mediana_KM
  
  
  
  # Encontramos el low de la mediana
  TABLA2 <- na.omit(TABLA2)
  mediana_low <- NA
  if(ncol(TABLA2) > 1)   if (nrow(TABLA2) > 0) if (min(TABLA2[,2]) <= 0.5) {
    
    dt <- TABLA2[,2] <= 0.5
    TABLA2 <- TABLA2[dt,]
    mediana_low <- TABLA2[1,3]
  }
  #  mediana_low
  
  
  # Encontramos el upp de la mediana
  TABLA3 <- na.omit(TABLA3)
  mediana_upp <- NA
  if(ncol(TABLA3) > 1)   if (nrow(TABLA3) > 0) if (min(TABLA3[,2]) <= 0.5) {
    
    dt <- TABLA3[,2] <= 0.5
    TABLA3 <- TABLA3[dt,]
    mediana_upp <- TABLA3[1,3]
  }
  #  mediana_upp
  
  # Armamos una tabla nueva con la informacion que recolectamos
  nombres <- c("n", "Eventos", "Mediana", "Límite Inferior", "Límite Superior")
  TABLA_KM <- c(nrow(MINIBASE), sum(status), mediana_KM, mediana_low, mediana_upp)
  TABLA_KM <- as.character(TABLA_KM)
  dim(TABLA_KM) <- c(1, length(TABLA_KM))
  colnames(TABLA_KM) <- nombres
  
  # Tabla KM General
  TABLA_KM
  
})


output$tabla_SobGen <- renderTable({
  
  SobGen_TABLA()
})



SobGen_GRAFICO <- reactive({
  
  # Cargamos librerias
  library(xtable)
  library(survival)
  
  
  
  # Juntamos los elementos en un objeto llamado BASE
  BASE <- MINI1()
  
  # Armamos la MINIBASE
  MINIBASE <- BASE
  MINIBASE <- na.omit(MINIBASE)
  
  
  # Separamos los elementos
  #  orden  <-  MINIBASE[,1]
  tiempo <- MINIBASE[,1]
  status <- MINIBASE[,2]
  #  grupo  <-  MINIBASE[,4]
  
  # Sobrevida General con R
  SGen <- survfit(Surv(tiempo, status) ~ 1, conf.type="log", conf.int=0.95, type="kaplan-meier", error="greenwood")
  SGen
  
  
})


output$grafico_SobGen <- renderPlot({
  
  SGen <-   SobGen_GRAFICO()
  
  
  #### Grafico
  plot(SGen, conf.int=FALSE, mark.time=TRUE, lty=c(1,3), col=c("blue"), xlab= "Tiempo", ylab= "Probabilidad de Sobrevida" ,main= "Análisis Kaplan-Meier")
  
  
})


# # # # # # # Por GRUPOS

########

SobGrupo_TABLA <- reactive({
  
  # Cargamos librerias
  library(xtable)
  library(survival)
  
  
  
  # Juntamos los elementos en un objeto llamado BASE
  BASE <- MINI2()
  
  # Armamos la MINIBASE
  MINIBASE <- BASE
  MINIBASE <- na.omit(MINIBASE)
  
  
  # Separamos los elementos
  #  orden  <-  MINIBASE[,1]
  tiempo <- MINIBASE[,1]
  status <- MINIBASE[,2]
  grupo  <-  as.factor(MINIBASE[,3])
  
  
  # Sobrevida General por Grupos con R
  SGrupo <- survfit(Surv(tiempo, status) ~ grupo, conf.type="log", conf.int=0.95, type="kaplan-meier", error="greenwood")
  SGrupo
  
  
  # La salida anterior, no es un objeto en R.
  # La muestra como texto, pero no se puede utilizar para armar una salida
  # reactiva en RMedic.
  # A si que... la vamos a hacer a mano.
  
  # Guardamos la poca informacion que nos da
  
  # Sobrevida por grupo
  SGrupoPartes <- list()
  nombres <- c("n", "Eventos", "Mediana", "Límite Inferior", "Límite Superior")
  TABLA_KMgg <- data.frame(matrix(NA, length(levels(grupo)), length(nombres)))
  colnames(TABLA_KMgg) <- nombres
  rownames(TABLA_KMgg) <- levels(grupo)
  
  
  for (n in 1:length(levels(grupo))) {
    estos <- grupo == levels(grupo)[n]
    t1 <- tiempo[estos]
    s1 <- status[estos]
    g1 <- grupo[estos]
    
    SGrupoPartes[[n]] <- list()
    SGrupoPartes[[n]]  <- survfit(Surv(t1, s1) ~ g1, conf.type="log", conf.int=0.95, type="kaplan-meier", error="greenwood")
    SGrupoPartes[[n]] 
    
    # Guardamos la poca informacion que nos da
    TABLA <- summary(SGrupoPartes[[n]])
    surv_mediana <- TABLA[["surv"]]
    orden_mediana <- c(1:length(surv_mediana))
    
    # Armamos algunos elementos
    TABLA1 <- data.frame(cbind(orden_mediana, TABLA[["surv"]], TABLA[["time"]]))
    TABLA2 <- data.frame(cbind(orden_mediana, TABLA[["lower"]], TABLA[["time"]]))
    TABLA3 <- data.frame(cbind(orden_mediana, TABLA[["upper"]], TABLA[["time"]]))
    
    
    # Encontramos la mediana
    TABLA1 <- na.omit(TABLA1)
    mediana_KM <- NA
    if(ncol(TABLA1) > 1) if (nrow(TABLA1) > 0) if (min(TABLA1[,2]) <= 0.5) {
      
      dt <- TABLA1[,2] <= 0.5
      TABLA1 <- TABLA1[dt,]
      mediana_KM <- TABLA1[1,3]
    }
    #    mediana_KM
    
    
    
    # Encontramos el low de la mediana
    TABLA2 <- na.omit(TABLA2)
    mediana_low <- NA
    if(ncol(TABLA2) > 1)  if (nrow(TABLA2) > 0) if (min(TABLA2[,2]) <= 0.5) {
      
      dt <- TABLA2[,2] <= 0.5
      TABLA2 <- TABLA2[dt,]
      mediana_low <- TABLA2[1,3]
    }
    #    mediana_low
    
    
    # Encontramos el upp de la mediana
    TABLA3 <- na.omit(TABLA3)
    mediana_upp <- NA
    if(ncol(TABLA3) > 1)  if (nrow(TABLA3) > 0) if (min(TABLA3[,2]) <= 0.5) {
      
      dt <- TABLA3[,2] <= 0.5
      TABLA3 <- TABLA3[dt,]
      mediana_upp <- TABLA3[1,3]
    }
    #    mediana_upp
    
    # Armamos una tabla nueva con la informacion que recolectamos
    mini <- c(length(s1), sum(s1), mediana_KM, mediana_low, mediana_upp)
    TABLA_KMgg[n,] <- mini
  #  cat("Va el ", n, "\n")
  }
  TABLA_KMgg[,1] <- as.character(TABLA_KMgg[,1])
  TABLA_KMgg[,2] <- as.character(TABLA_KMgg[,2])
  TABLA_KMgg <- cbind(levels(grupo), TABLA_KMgg)
  colnames(TABLA_KMgg)[1] <- "Grupo"
  TABLA_KMgg
  
})
output$tabla_SobGrupo <- renderTable({
  
  SobGrupo_TABLA()
})



SobGrupo_GRAFICO <- reactive({
  
  # Cargamos librerias
  library(xtable)
  library(survival)
  
  
  
  # Juntamos los elementos en un objeto llamado BASE
  BASE <- MINI2()
  
  # Armamos la MINIBASE
  MINIBASE <- BASE
  MINIBASE <- na.omit(MINIBASE)
  
  
  # Separamos los elementos
  #  orden  <-  MINIBASE[,1]
  tiempo <- MINIBASE[,1]
  status <- MINIBASE[,2]
  grupo  <-  as.factor(MINIBASE[,3])
  
  
  # Sobrevida General por Grupos con R
  SGrupo <- survfit(Surv(tiempo, status) ~ grupo, conf.type="log", conf.int=0.95, type="kaplan-meier", error="greenwood")
  SGrupo
  
  
  
  
})
output$grafico_SobGrupo <- renderPlot({
  
  SGrupo <-   SobGrupo_GRAFICO()
  
  # Juntamos los elementos en un objeto llamado BASE
  BASE <- MINI2()
  
  # Armamos la MINIBASE
  MINIBASE <- BASE
  MINIBASE <- na.omit(MINIBASE)
  
  
  # Separamos los elementos
  #  orden  <-  MINIBASE[,1]
  tiempo <- MINIBASE[,1]
  status <- MINIBASE[,2]
  grupo  <-  as.factor(MINIBASE[,3])
  #### Grafico
  colores <- as.numeric(as.factor(levels(grupo)))
  plot(SGrupo, conf.int=FALSE, mark.time=TRUE, lty=c(1,3),col=colores, xlab= "Tiempo", ylab= "Probabilidad de Sobrevida" ,main= "Análisis Kaplan-Meier")
  
  
  legend ("topright", levels(grupo), lty=1, col=colores)
  
})
##########






KM_Grupo_Estadistica <- reactive({
  
  # Cargamos librerias
  library(xtable)
  library(survival)
  
  
  
  # Juntamos los elementos en un objeto llamado BASE
  BASE <- MINI2()
  
  # Armamos la MINIBASE
  MINIBASE <- BASE
  MINIBASE <- na.omit(MINIBASE)
  
  
  # Separamos los elementos
  #  orden  <-  MINIBASE[,1]
  tiempo <- MINIBASE[,1]
  status <- MINIBASE[,2]
  grupo  <-  as.factor(MINIBASE[,3])
  
  
  .S <-survdiff(Surv(tiempo, status) ~ grupo, rho=0.0)
  #  .S
  
  valor_chi <- .S[["chisq"]]
  valor_chi <- round(valor_chi, 2)
  gl <- length(levels(grupo)) - 1
  #  gl
  valor_p_interno <- pchisq(valor_chi, gl, ncp = 0, lower.tail = FALSE, log.p = FALSE)
  valor_p_interno <- round(valor_p_interno, 2)
  
  valor_p_externo <- valor_p_interno
  if (valor_p_interno < 0.01) valor_p_externo <- c("<<0.01")
  
  frase1 <- c("No Rechazo Ho")
  if (valor_p_interno < 0.05) frase1<- c("Rechazo Ho")
  
  frase2A <- c("El valor p es mayor que el valor de alfa.", "\n",
               "No se rechaza la Hipótesis Nula.", "\n",
               "No existen diferencias estadísticamente significativas entre los grupos.", "\n",
               "Los grupos son estadísticamente iguales.")
  
  
  frase2B <- c("El valor p es igual que el valor de alfa.", "\n",
               "No se rechaza la Hipótesis Nula.", "\n",
               "No existen diferencias estadísticamente significativas entre los grupos.", "\n",
               "Los grupos son estadísticamente iguales.")
  
  
  frase2C<- c("El valor p es menor que el valor de alfa.", "\n",
              "Se rechaza la Hipótesis Nula.", "\n",
              "Existen diferencias estadísticamente significativas entre los grupos.", "\n",
              "Los grupos son estadísticamente diferentes.")
  
  frase2 <- frase2A
  if (valor_p_interno == 0.05) frase2<- frase2B
  if (valor_p_interno < 0.05) frase2 <- frase2C
  
  
  nombres <- c("Valor Chi", "G.L.", "Valor p", "Desición")
  salida <- matrix(NA, 1, length(nombres))
  colnames(salida) <- nombres
  
  salida[1,] <- c(valor_chi, gl, valor_p_externo, frase1)
  
  
  
  
  KM <- list()
  
  KM$SALIDA_ARMADA <- list()
  
  KM$SALIDA_ARMADA$RESUMEN <- salida
  KM$SALIDA_ARMADA$FRASE2 <- frase2
  
  KM
  
  
})

output$resumen_km <- renderTable({
  
  KM <- KM_Grupo_Estadistica()$SALIDA_ARMADA$RESUMEN
  KM
})


output$texto_km <- renderText({
  
  KM <- KM_Grupo_Estadistica()$SALIDA_ARMADA$FRASE
  KM
})


##############################



output$texto_prueba <- renderUI({
  
  if (!is.null(input$somevalue)) {
    
  #if (input$somevalue == F) "ESTO ES FALSO"

  if (input$somevalue == T) salida_texto_km()
  
  } else return(NULL)
})

# Juntamos todo lo de Sobrevida y armamos la salida
menuSOBREVIDA <- reactive({
  if (paso_BASE(BASE_SALIDA())) {
    
    tabs <- list()
    tabs[[1]] <- tabPanel(title = "Sobrevida", 
                          icon = icon("user-md"), 
                          value = 6, 
                          #id = "prueba1",
                          h3("Menú Sobrevida (Test de Kaplan-Meier)"),
                          checkboxInput("somevalue", "Ayuda Kaplan-Meier", FALSE),
                          br(),
                            uiOutput("texto_prueba"), br(),
                        
                          selectInput("tiempo", label="Tiempo", 
                                      choices = c("Seleccione..." = "",colnames(BASE_SALIDA()))
                          ),
                          selectInput("mortalidad", label="Mortalidad (Toma '0' como 'Vivo', '1' como 'Muerte')", 
                                      choices = c("Seleccione..." = "",colnames(BASE_SALIDA()))
                          ),
                          uiOutput("MODmenu_sobrevida")
    )
    tabs
    
  } else return(NULL)
})



# El siguiente objeto...
# Es la salida para observar solo como queda la pestaña "Bade de Datos".
observe(output[["SALIDAmenuHo"]] <- renderUI({
  
  tabs1 <- menuBASE()
  tabs2 <- menuCONTROL()
  tabs3 <- menuDESCRIPTIVAS()
  tabs4 <- menuGRAFICOS()
  tabs5 <- menuHO()
  tabs6 <- menuSOBREVIDA()
  do.call(tabsetPanel,  c(id="goku", tabs1,tabs2, tabs3, tabs4, tabs5, tabs6))
  
}))
