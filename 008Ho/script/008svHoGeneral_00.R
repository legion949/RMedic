
# Generacion Dinamica de HO



# Prueba de Hipotesis para 1 Variable Categorica
output$menu_1q_ho <- renderUI ({
  
  if(paso_BASE(BASE_goku())) {
    
    
    tabsetPanel(id="menu_ho",
                tabPanel("RMedic Help!", value=1001),
                tabPanel("Test de Proporciones", value=1),
                tabPanel("Test de Uniformidad", value=22)
    )
    
    
  } else return(NULL)
})



# Prueba de Hipotesis para 1 Variable Cuantitativa
output$menu_1c_ho <- renderUI ({
  
  if(paso_BASE(BASE_goku())) {
    
    tabsetPanel(id="menu_ho",
                tabPanel("RMedic Help!", value=1002),
                tabPanel("Test t (Una muestra)", value=3),
                tabPanel("Test de Wilcoxon (Una muestra)", value=4),
                tabPanel("Test de Normalidad de Shapiro-Wilk", value=2)
    )
    
    
    
  } else return(NULL)
})



# Grafico para 2 Variables Cualitativas
output$menu_2q_ho <- renderUI ({
  
  if(paso_BASE(BASE_goku())) {
    
    tabsetPanel(id="menu_ho",
                tabPanel("RMedic Help!", value=1003),
                tabPanel("Test Chi Cuadrado", value=7),
                tabPanel("Diferencia de Proporciones", value=5),
                tabPanel("Regresión Logística Simple", value=6),
                tabPanel("Otros", value=8)
    )
    
  } else return(NULL)
})



# Prueba de Hipotesis para 2 Variables Cuantitativas
output$menu_2c_ho <- renderUI ({
  
  if(paso_BASE(BASE_goku())) {
    
    tabsetPanel(id="menu_ho",
                tabPanel("RMedic Help!", value=1004),
                tabPanel("Correlación de Pearson", value=10),
                tabPanel("Correlación de Spearman", value=11),
                tabPanel("Regresión Lineal Simple", value=12),
                tabPanel("Regresión Logística Simple", value=13),
                tabPanel("Test t Apareado", value=14),
                tabPanel("Test Wilcoxon Apareado", value=15),
                tabPanel("Test de Homogeneidad de Varianzas de Bartlett", value=9)
             
    )
    
  } else return(NULL)
})




# Prueba de Hipotesis para 2 Variables... una Q y una C
output$menu_qc_ho <- renderUI ({
  
  if(paso_BASE(BASE_goku())) {
    
    tabsetPanel(id="menu_ho",
                tabPanel("RMedic Help!", value=1005),
                tabPanel("Test t (dos muestras independientes)", value=17),
                tabPanel("Test Mann-Whitney (dos muestras independientes)", value=18),
                tabPanel("ANOVA a 1 Factor", value=19),
                tabPanel("Test Kruskal-Wallis", value=20),
                tabPanel("Regresión Logística Simple", value=21),
                tabPanel("Test de Homogeneidad de Varianzas de Bartlett", value=16),
                tabPanel("Test de Normalidad de Shapiro-Wilks (Particionado)", value=23)
    )
    
  } else return(NULL)
})



# Menu Ho
output$menu_ho <- renderUI({
  
  if (paso_BASE(BASE_goku())) {
    if(!is.null(tipo_var_goku())) {
      
      # Si es 1 sola variable  
      if(ncol(BASE_goku()) == 1) {
        
        # Si es categorica... o si no lo es...
        if(tipo_var_goku() == "Categórica")   uiOutput("menu_1q_ho")  else   uiOutput("menu_1c_ho")
        
      } else  if(ncol(BASE_goku()) == 2) {
        
        # Si las dos son numericas
        if(sum(tipo_var_goku() == "Categórica") == 0)   uiOutput("menu_2c_ho")  else   if(sum(tipo_var_goku() == "Categórica") == 1)   uiOutput("menu_qc_ho") else if(sum(tipo_var_goku() == "Categórica") == 2)  uiOutput("menu_2q_ho")  else return(NULL)
        
      }
    } else return(NULL)
  } else return(NULL)
})

# # # # ## # ## # # ## # # # ## # ## # # ## # # # ## # ## # # ## # # # ## # ## #

# Grafico en si mismo
observe(
  output$literal_ho <- renderUI({
    
    # Si ya hay base de datos...  
    if(paso_BASE(BASE_goku())) {
      
        # y ya esta el menu cargado...
        if (!is.null(input$menu_ho)) {
        
      
        # Help 1Q 
        if (input$menu_ho == 1001) {
          Help_ho_1q()
            
        # Help 1C
        } else if (input$menu_ho == 1002) {
          Help_ho_1c()
              
        # Help 2Q 
        } else if (input$menu_ho == 1003) {
            Help_ho_2q()
            
        # Help 2C
        } else if (input$menu_ho == 1004) {
              Help_ho_2c()
              
        # Help QC  
        } else if (input$menu_ho == 1005) {
                Help_ho_qc()
                
           
        # 1) 1Q - 01 - Test de Proporciones
        } else if (input$menu_ho == 1) {
          Opc_ho_1q_01() 
        
          # 2) 1C - 02 - Test de Normalidad    
        } else   if (input$menu_ho == 2) {
          Opc_ho_1c_02()
          
          # 3) 1C - 03 - Test t
        } else   if (input$menu_ho == 3) {
          Opc_ho_1c_03() 
          
          # 4) 1C - 04 - Test Mann-Whitney
        } else   if (input$menu_ho == 4) {
          Opc_ho_1c_04() 
          
          # 5) 2Q - 05 - Test de Proporciones
        } else   if (input$menu_ho == 5) {
          Opc_ho_2q_05() 
          
          # 6) 2Q - 06 - RegLog
        } else   if (input$menu_ho == 6) {
          Opc_ho_2q_06() 
          
          # 7) 2Q - 07 - Chi Cuadrado
        } else   if (input$menu_ho == 7) {
          Opc_ho_2q_07() 
          
          # 8) 2Q - 08 - Otros
        } else   if (input$menu_ho == 8) {
          Opc_ho_2q_08() 
          
          # 9) 2C - 09 - Homogeneidad de Varianzas
        } else   if (input$menu_ho == 9) {
          Opc_ho_2c_09() 
          
          # 10) 2C - 10 - Correlacion de Pearson
        } else   if (input$menu_ho == 10) {
          Opc_ho_2c_10() 
          
          # 11) 2C - 11 - Correlacion de Spearman
        } else   if (input$menu_ho == 11) {
          Opc_ho_2c_11() 
          
          # 12) 2C - 12 - Regresion Lineal Simple
        } else   if (input$menu_ho == 12) {
          Opc_ho_2c_12() 
          
          # 13) 2C - 13 - RegLog
        } else   if (input$menu_ho == 13) {
          Opc_ho_2c_13() 
          
          # 14) 2C - 14 - Test t apareado
        } else   if (input$menu_ho == 14) {
          Opc_ho_2c_14() 
          
          # 15) 2C - 15 - Test W apareado
        } else   if (input$menu_ho == 15) {
          Opc_ho_2c_15() 
          
          # 16) QC - 16 - Homogeneidad
        } else   if (input$menu_ho == 16) {
          Opc_ho_qc_16() 
          
          # 17) QC - 17 - Test t
        } else   if (input$menu_ho == 17) {
          Opc_ho_qc_17()
          
          # 18) QC - 18 - Test Mann-Whitney
        } else   if (input$menu_ho == 18) {
          Opc_ho_qc_18() 
          
          # 19) QC - 19 - Test Anova 1 Factor
        } else   if (input$menu_ho == 19) {
          Opc_ho_qc_19() 
          
          # 20) QC - 20 - KW
        } else   if (input$menu_ho == 20) {
          Opc_ho_qc_20() 
          
          # 21) QC - 21 - regLog
        } else   if (input$menu_ho == 21) {
          Opc_ho_qc_21() 
          
          
          # 22) 1Q - 02 - Test de Uniformidad
        } else if (input$menu_ho == 22) {
          Opc_ho_1q_02()      
          
          # 23) QC - 23 - Test de Normalidad de Shapiro-Wilk por grupos
        } else if (input$menu_ho == 23) {
          Opc_ho_qc_23()       
        }
        
        
        # else  if (input$menu_ho == 2) {
        # #  Opc_ho_1c_02() 
        #     
        # } else  if (input$menu_ho == 3) Opc_ho_1c_03() 
        #   
          
        
        
                   
        
        
          
        
      } else return(NULL)
      } else return(NULL)

  })
)






# Planeta Ho
output$planeta_ho <- renderUI({
  
  if (paso_BASE(BASE_goku())) {
    
    
    #barplot(table(BASE_goku()[,1]), col = mis_colores()) 
    
    
    
    div(
      uiOutput("menu_ho"),
      uiOutput("literal_ho")
      #uiOutput("TEXTO_00_H0")
    )
    
  } else return(NULL)
})













