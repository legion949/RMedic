

# # # # #
# 2Q - 08 - Otros


# Detalle de niveles "cero" de cada variable
output$ho_2q_08 <- renderUI({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2) {
    
    div(
      radioButtons("detalle1_ho_2q_08", paste0("Referencia '0' para Variable 1 (filas) - '", colnames(BASE_goku())[1], "':"), 
                  choices = levels(as.factor(BASE_goku()[,1]))),
      radioButtons("detalle2_ho_2q_08", paste0("Referencia '0' para Variable 2 (columnas) - '", colnames(BASE_goku())[2], "':"), 
                  choices = levels(as.factor(BASE_goku()[,2])))
      
      
    )
    
  } else return(NULL)
  
})


# Tabla de Referencia "Otros"
output$tabla01_ref_ho_2q_08 <- renderTable(rownames = T, bordered = T,{

    
    TABLA <- matrix(NA, 2, 2)
    TABLA[1,] <- c("VN", "FN")
    TABLA[2,] <- c("FP", "VP")
    
    colnames(TABLA) <- c(0, 1)
    rownames(TABLA) <- c(0, 1)
    
    
    TABLA
    
    
    
})


# Tabla FA
output$tabla02_fa_otros <- renderTable(rownames = T, bordered = T, digits = 0,{
  if(!is.null(pack_tabla_2q_df_goku())) {
  pack_tabla_2q_df_goku()[[1]][[1]]
  } else return(NULL)
})


tabla_cambio1 <- reactive({
  if(!is.null(pack_tabla_2q_df_goku())) {
    if(!is.null(input$detalle1_ho_2q_08)) {
      if(!is.null(input$detalle2_ho_2q_08)) {
              
      tabla_original <- pack_tabla_2q_df_goku()[[1]][[1]]
      
      nombres_columnas <- colnames(tabla_original)
      nombres_filas <- rownames(tabla_original)
      
      tabla_cambio <- tabla_original
      
      if (input$detalle1_ho_2q_08 != nombres_filas[1]) tabla_cambio <- tabla_cambio[c(2,1), ]
      if (input$detalle2_ho_2q_08 != nombres_columnas[1]) tabla_cambio <- tabla_cambio[,c(2,1) ]
      
    
      tabla_cambio

} else return(NULL)
    } else return(NULL)
  } else return(NULL)
})

tabla_cambio2 <- reactive({
  if(!is.null(pack_tabla_2q_df_goku())) {
    if(!is.null(input$detalle1_ho_2q_08)) {
      if(!is.null(input$detalle2_ho_2q_08)) {
        

        
        tabla_cambio <- tabla_cambio1()
        

        # Referencias internas
        tabla_cambio[1,1] <- paste(tabla_cambio[1,1], "(VN)")
        tabla_cambio[1,2] <- paste(tabla_cambio[1,2], "(FN)")
        tabla_cambio[2,1] <- paste(tabla_cambio[2,1], "(FP)")
        tabla_cambio[2,2] <- paste(tabla_cambio[2,2], "(VP)")
        
        
        # Referencias externas
        colnames(tabla_cambio) <- paste0(colnames(tabla_cambio), c(" (0)", " (1)"))
        rownames(tabla_cambio) <- paste0(rownames(tabla_cambio), c(" (0)", " (1)"))
        
        tabla_cambio
        
      } else return(NULL)
    } else return(NULL)
  } else return(NULL)
})

# Tabla FA2
output$tabla02_fa_otros2 <- renderTable(rownames = T, bordered = T, digits = 0,{
  if(!is.null(pack_tabla_2q_df_goku())) {
    tabla_cambio2()
  } else return(NULL)
})


##################################################################################

# ODD RATIOS
{
###

# Generacion de Detalles de OddRatios  
OR_OTROS <- reactive({
  
  if(!is.null(pack_tabla_2q_df_goku())) {
    if(!is.null(input$detalle1_ho_2q_08)) {
      if(!is.null(input$detalle2_ho_2q_08)) {
        if(!is.null(input$alfa_ho)) {
        
        #    cat("Estoy adentro", "\n")
        TABLA <- tabla_cambio1()
        DECIMALES <- decimales_goku()
        
        ALFA <- input$alfa_ho
        porcentaje <- 1 - ALFA
        porcentaje <- porcentaje*100
        porcentaje <- paste0(porcentaje, "%")
        porcentaje <- paste0("(", porcentaje, ")")
        
        n00 = as.numeric(TABLA[1,1])
        n01 = as.numeric(TABLA[1,2])
        n10 = as.numeric(TABLA[2,1])
        n11 = as.numeric(TABLA[2,2])
        
        #  p1 <- n00 * n11
        #  p2 <- n01*n10
        
        p1 <-  round2(n11/n10, DECIMALES)
        p2 <-  round2(n01/n00, DECIMALES)
        
        #      OR <- (n00 * n11)/(n01 * n10)
        OR <- round2((n11/n10) / (n01/n00), DECIMALES)
        #
        #  Compute the Wald confidence intervals:
        #
        siglog <- sqrt((1/n00) + (1/n01) + (1/n10) + (1/n11))
        zalph <- qnorm(1 - ALFA/2)
        logOR <- log(OR)
        loglo <- logOR - zalph * siglog
        loghi <- logOR + zalph * siglog
        #
        ORlo <- exp(loglo)
        ORhi <- exp(loghi)
        #
        
        oframe <- data.frame(ORlo, OR, ORhi, ALFA)
        colnames(oframe) <- c(paste0("Límite Inferior", porcentaje), "Odd Ratio", paste0("Límite Superior", porcentaje), "Alfa")
        #    oframe
        
        OR_SALIDA <- list()
        #  OR_SALIDA$TABLA <- list()
        #  OR_SALIDA$FRASE1 <- list()
        OR_SALIDA$TABLA <- oframe
        #   OR_SALIDA$FRASE1 <- paste0("OR = (VN * VP)/(FN * FP) = (", n11 , "*", n00, ")/(", n01, "*", n10, ") = ", p1, "/", p2, "=", OR)
        OR_SALIDA$FRASE1 <- paste0("OR = (VP/FP) / (FN/VN) = (", n11 , "/", n10, ") / (", n01, "/", n00, ") = ", p1, " / ", p2, " = ", OR)
        
        
        #  OR_SALIDA$FRASE1 <- "JKA" 
        OR_SALIDA
      } else return(NULL)  
    } else return(NULL)
  } else return(NULL)
} else return(NULL)
  
  
})

# Tabla OddRatios
output$TABLA_OR <- renderTable(rownames = F, bordered = T,{
 if(!is.null(OR_OTROS())) {
        TABLA <- OR_OTROS()$TABLA
        
        TABLA
        
      } else return(NULL)
})

# Frase1 de OddRatios
output$FRASE1_OR <- renderText({
  if(!is.null(OR_OTROS())) {
        FRASE <- OR_OTROS()$FRASE1
        
        FRASE
        
  } else return(NULL)
})
#####################################################################################

###
} # Fin ODD RATIOS
##################################################################################


# RIESGO RELATIVO
{
###
  # Detalles de Riesgos Relativos
  RR_OTROS <- reactive({
    
    if(!is.null(pack_tabla_2q_df_goku())) {
      if(!is.null(input$detalle1_ho_2q_08)) {
        if(!is.null(input$detalle2_ho_2q_08)) {
          if(!is.null(input$alfa_ho)) {
          
          #    cat("Estoy adentro", "\n")
          TABLA <- tabla_cambio1()
          DECIMALES <- decimales_goku()
          
          ALFA <- input$alfa_ho
          porcentaje <- 1 - ALFA
          porcentaje <- porcentaje*100
          porcentaje <- paste0(porcentaje, "%")
          porcentaje <- paste0("(", porcentaje, ")")
          
          n00 = as.numeric(TABLA[1,1])
          n01 = as.numeric(TABLA[1,2])
          n10 = as.numeric(TABLA[2,1])
          n11 = as.numeric(TABLA[2,2])
          
          # Totales por fila
          tf1 <- n00 + n01
          tf2 <- n10 + n11
          
          # Totales por columna
          tc1 <- n00 + n10
          tc2 <- n01 + n11
          
          p1 <- n11/tf2
          p1 <- round2(p1, DECIMALES)
          p2 <- n01/tf1
          p2 <- round2(p2, DECIMALES)
    #      cat((p1/p2), "\n")
          res <- riskratio(n11, n01, tf2,tf1)
          
          RR_OBS <- p1/p2
          RR_OBS <- round2(RR_OBS, DECIMALES)
          
          RR <- res$estimate   #
          RR <- round2(RR, DECIMALES)
          RRlo <- res$conf.int[1]
          RRhi <- res$conf.int[2]
          p_interno <- res$p.value
          if (p_interno < 0.01) p_externo <- "<<0.01" else p_externo <- p_interno
          if (p_interno < ALFA) decision <- "Rechazo Ho" else decision <- "No Rechazo Ho"
          
          
          oframe <- data.frame(RRlo, paste0(RR, "(*)"), RRhi, p_interno, ALFA, decision)
          colnames(oframe) <- c(paste0("Límite Inferior", porcentaje), "Riesgo Relativo", paste0("Límite Superior", porcentaje), "Valor p", "Alfa", "Decisión")
          
          
          RR_SALIDA <- list()
          #  OR_SALIDA$TABLA <- list()
          #  OR_SALIDA$FRASE1 <- list()
          RR_SALIDA$TABLA <- oframe
          RR_SALIDA$FRASE1 <- paste0("RR = (VP/(VP+FP)) / (FN/(FN+VN))) = (", n11 , "/(",n11, "+", n10, ")) / (", n01, "/(", n01, "+", n00,")) = (", n11, "/", tf2, ") / (", n01, "/", tf1, ") = ", p1, " / ", p2, " = ", RR_OBS) 
          #  OR_SALIDA$FRASE1 <- "JKA" 
          RR_SALIDA
          
        } else return(NULL)  
      } else return(NULL)
    } else return(NULL)
  } else return(NULL)
    
    
  })
  
  
  # Tabla de Riesgos Relativos
  output$TABLA_RR <- renderTable(rownames = F, bordered = T,{
    if(!is.null(RR_OTROS())) {
          TABLA <- RR_OTROS()$TABLA
          
          TABLA
          
    } else return(NULL)
  })

  
  
  output$FRASE1_RR <- renderText({
    if(!is.null(RR_OTROS())) {
          FRASE <- RR_OTROS()$FRASE1
          
          FRASE
    } else return(NULL)
  })
  
  
###  
} # Fin RIESGO RELATIVO
################################################################################


# VALORES PREDICTIVOS
{
###
  
  # Valores Predictivos
  VP_OTROS <- reactive({
    
    if(!is.null(pack_tabla_2q_df_goku())) {
      if(!is.null(input$detalle1_ho_2q_08)) {
        if(!is.null(input$detalle2_ho_2q_08)) {
          if(!is.null(input$alfa_ho)) {
          
          #    cat("Estoy adentro", "\n")
          TABLA <- tabla_cambio1()
          ALFA <- input$alfa_ho
          DECIMALES <- decimales_goku()
          
          porcentaje <- 1 - ALFA
          porcentaje <- porcentaje*100
          porcentaje <- paste0(porcentaje, "%")
          porcentaje <- paste0("(", porcentaje, ")")
          
          n00 = as.numeric(TABLA[1,1])
          n01 = as.numeric(TABLA[1,2])
          n10 = as.numeric(TABLA[2,1])
          n11 = as.numeric(TABLA[2,2])
          
          tf1 <- n00 + n01
          tf2 <- n10 + n11
          
          VPP <- n11/tf2
          VPN <- n00/tf1
          
          VPP <- round2(VPP,   DECIMALES)
          VPN <- round2(VPN, DECIMALES)
          nombres <- c("Detalle", "Estimado")
          TABLA <- matrix(NA, 2, 2)
          colnames(TABLA) <- nombres
          TABLA[,1] <- c("VPP", "VPN")
          TABLA[,2] <- c(VPP, VPN)
          
          VP_SALIDA <- list()
          VP_SALIDA$TABLA <- TABLA
          VP_SALIDA$FRASE1 <- paste0("VPP = VP/(VP + FP) = ", n11, " / (", n11, "+", n10, ") = ", n11 , "/", tf2, " = ", VPP)
          VP_SALIDA$FRASE2 <- paste0("VPN = VN/(VN + FN) = ", n00, " / (", n00, "+", n01, ") = ", n00 , "/", tf1, " = ", VPN)
          
          VP_SALIDA
        } else return(NULL)  
      } else return(NULL)
    } else return(NULL)
  } else return(NULL)  
    
  })
  
  
  # Tabla de Valores Predictivos  
  output$TABLA_VP <- renderTable(rownames = F, bordered = T,{
    if (!is.null(VP_OTROS())){
          TABLA <- VP_OTROS()$TABLA
          
          TABLA
          
    } else return(NULL)
  })
 
  # Frase 1 Valores Predictivos
  output$FRASE1_VP <- renderText({
    if (!is.null(VP_OTROS())){
          FRASE <- VP_OTROS()$FRASE1
          
          FRASE
          
    } else return(NULL)
  })

  # Frase 2 Valores Predictivos
  output$FRASE2_VP <- renderText({
    if (!is.null(VP_OTROS())){
      FRASE <- VP_OTROS()$FRASE2
      
      FRASE
      
    } else return(NULL)
  })
  
    
###  
} # Fin VALORES PREDICTIVOS
###################################################################################


# SENSIBILIDAD Y ESPECIFICIDAD
{
###  

  
  
  # Sensibilidad y Especificidad
  SE_OTROS <- reactive({
    
    if(!is.null(pack_tabla_2q_df_goku())) {
      if(!is.null(input$detalle1_ho_2q_08)) {
        if(!is.null(input$detalle2_ho_2q_08)) {
          if(!is.null(input$alfa_ho)) {
          
          #    cat("Estoy adentro", "\n")
          TABLA <- tabla_cambio1()
          ALFA <- input$alfa_ho
          DECIMALES <- decimales_goku()
          
          porcentaje <- 1 - ALFA
          porcentaje <- porcentaje*100
          porcentaje <- paste0(porcentaje, "%")
          porcentaje <- paste0("(", porcentaje, ")")
          
          n00 = as.numeric(TABLA[1,1])
          n01 = as.numeric(TABLA[1,2])
          n10 = as.numeric(TABLA[2,1])
          n11 = as.numeric(TABLA[2,2])
          
          tc1 <- n00 + n10
          tc2 <- n01 + n11
          
          SENSIBILIDAD <- n11/tc2
          ESPECIFICIDAD <- n00/tc1
          
          SENSIBILIDAD <- round2(SENSIBILIDAD,   DECIMALES)
          ESPECIFICIDAD <- round2(ESPECIFICIDAD, DECIMALES)
          nombres <- c("Detalle", "Estimado")
          TABLA <- matrix(NA, 2, 2)
          colnames(TABLA) <- nombres
          TABLA[,1] <- c("Sensibilidad", "Especificidad")
          TABLA[,2] <- c(SENSIBILIDAD, ESPECIFICIDAD)
          
          SE_SALIDA <- list()
          SE_SALIDA$TABLA <- TABLA
          SE_SALIDA$FRASE1 <- paste0("Sensibilidad  = VP/(VP + FN) = ", n11, " / (", n11, "+", n01, ") = ", n11 , "/", tc2, " = ", SENSIBILIDAD)
          SE_SALIDA$FRASE2 <- paste0("Especificidad = VN/(VN + FP) = ", n00, " / (", n00, "+", n10, ") = ", n00 , "/", tc1, " = ", ESPECIFICIDAD)
          
          SE_SALIDA
          
        } else return(NULL)  
      } else return(NULL)
    } else return(NULL)
  } else return(NULL)    
    
  })
  
  
  # Tabla SE
  output$TABLA_SE <- renderTable(rownames = F, bordered = T,{
    if (!is.null(SE_OTROS())){
          TABLA <- SE_OTROS()$TABLA
          
          TABLA
          
    } else return(NULL)
  })
  
  # Frase1 SE
  output$FRASE1_SE <- renderText({
    if (!is.null(SE_OTROS())){
          FRASE <- SE_OTROS()$FRASE1
          
          FRASE
          
    } else return(NULL)
  })
  
  # Frase2 SE
  output$FRASE2_SE <- renderText({
    if (!is.null(SE_OTROS())){
      FRASE <- SE_OTROS()$FRASE2
      
      FRASE
      
    } else return(NULL)
  })
  
###    
} # Fin SENSIBILIDAD Y ESPECIFICIDAD
###################################################################################


Opc_ho_2q_08 <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2) {

    div(
      fluidRow(
        h3("Tabla de Referencia (Teórica)"), br(),
        column(4,
               "Variable 1 (filas)", br(),
               "Variable 2 (columnas)", br(),
        tableOutput("tabla01_ref_ho_2q_08")
        ),
        column(4,
        fluidRow( 
          br(),
          "VN: Verdadero Negativo", br(),
          "FN: Falso Negativo", br(),
          "FP: Falso Positivo", br(),
          "VP: Verdadero Positivo", br()
      )
      )
      ), br(),
      
    
        
      h3("Tabla de Frecuencias Absolutas Observadas"), br(),
      ref2q_goku()[1], br(),
      ref2q_goku()[2], br(),
      tableOutput("tabla02_fa_otros"), br(),
    
      h3("Selección de reordenamiento"),br(),
      uiOutput("ho_2q_08"), br(),    
     
      h3("Tabla de Frecuencias Absolutas Observadas reordenada y referenciada"), br(),
      ref2q_goku()[1], br(),
      ref2q_goku()[2], br(),
      tableOutput("tabla02_fa_otros2"), br(),
      
      tabsetPanel(
        id = 'otros_qq',
        tabPanel("OR y RR",
                 br(),
                 h3("Odd Ratios"),
                 tableOutput("TABLA_OR"),
                 br(),  br(),
                 h3("Riesgo Relativo"),
                 tableOutput("TABLA_RR"),
                 "(*)Pueden existir diferencias decimales entre el cálculo de R y el cálculo a manual.",
                 br(),
                 h3("Cálculos manuales"),
                 textOutput("FRASE1_OR"),
                 textOutput("FRASE1_RR")
        ),
        tabPanel("VPP y VPN",
                 h3("Valores Predictivos"),
                 tableOutput("TABLA_VP"),
                 textOutput("FRASE1_VP"),
                 textOutput("FRASE2_VP"),
                 br()),
        tabPanel("Sensibilidad y Especificidad",
                 h3("Sensibilidad y Especificidad"),
                 tableOutput("TABLA_SE"),
                 textOutput("FRASE1_SE"),
                 textOutput("FRASE2_SE"),
                 br())
      ),
      br(), br(), br()
      
      
      
    )
    
        
  } else return(NULL)
})
