
nombres1 <- c("Orden Original", "Alfabético Ascendente", "Alfabético Descendente")
opciones_carga1 <- namel(nombres1)

nombres3 <- c("Categórica", "Numérica")
opciones_carga3 <- namel(nombres3)
# nombres3 <- c("Categórica", "Numérica")
# opciones_carga3 <- c(1, 7)
# names(opciones_carga3) <- nombres3

TextServer_Variables <- '
 # Update Menu1  
      # nombres1 <- c("Orden Original", "Alfabético Ascendente", "Alfabético Descendente")
      # opciones_carga1 <- namel(nombres1)
      
      # Update Menu2  
      nombres2_original <- colnames(BaseSalida())
      opciones_carga2 <- OpcionesDeColumnas(nombres2_original)
      
    
      # nombres3 <- c("Categórica", "Numérica")
      # opciones_carga3 <- namel(nombres3)
      
      output$MODtipo_var1_tablas <- renderUI({
        
        if(!AllEyesOnMe(Base = BaseSalida(), the_col = input$var1_tablas)) return(NULL)
       
        
            mini <- BaseSalida()[,input$var1_tablas]
            dt <- is.numeric(mini)
            
            # nombres3 <- c("Categórica", "Numérica")
            # opciones_carga3 <- namel(nombres3) 
            
            if (dt == TRUE) salida <- 2 else salida <- 1
            
            radioButtons(inputId = "tipo_var1_tablas",
                         label = "Tipo de Variable1",
                         choices = opciones_carga3,
                         selected = opciones_carga3[salida]
                         #selected ="Categórica"
                          )
     
       
      })
      

      output$MODtipo_var2_tablas <- renderUI({
        
        if(!AllEyesOnMe(Base = BaseSalida(), the_col = input$var2_tablas)) return(NULL)
        
        
        
        mini <- BaseSalida()[,input$var2_tablas]
        dt <- is.numeric(mini)
        
        # nombres3 <- c("Categórica", "Numérica")
        # opciones_carga3 <- namel(nombres3) 
        
        if (dt == TRUE) salida <- 2 else salida <- 1
        
        radioButtons(inputId = "tipo_var2_tablas",
                     label = "Tipo de Variable2",
                     choices = opciones_carga3,
                     selected = opciones_carga3[salida]
                     #selected ="Categórica"
        )
        
        
      })
      
      
      observeEvent(input$menu1_tablas, {
        
        
        nuevo_orden <- c(1:length(nombres2_original))
        if (input$menu1_tablas == "Alfabético Ascendente")  nuevo_orden <- order(nombres2_original, decreasing = F) else
          if (input$menu1_tablas == "Alfabético Descendente") nuevo_orden <- order(nombres2_original, decreasing = T)
          
          opciones_carga2 <- opciones_carga2[nuevo_orden]
          
          # freezeReactiveValue(input, "var1_tablas")
          updateSelectInput(session, 
                            inputId = "var1_tablas",
                            label = "Variable 1",
                            choices=  c("Seleccione una variable..." = "", opciones_carga2),
                            selected = NA)  
          
          # freezeReactiveValue(input, "var2_tablas")
          updateSelectInput(session, 
                            inputId = "var2_tablas",
                            label = "Variable 2",
                            choices=  c("Seleccione una variable..." = "", opciones_carga2),
                            selected = NA) 
          
          
      })
        '



TextUI_Variables <- '      
      div(
        fluidRow(
          column(4,
                 # Menu1: Orden de las seleccion de variables
                 selectInput(inputId = "menu1_tablas",
                             label = "Orden de las variables",
                             choices = opciones_carga1,                                                    selected = opciones_carga1[1]
                 )
          ),
          column(4,
                 selectInput(inputId = "qtty_var_tablas",
                             label = "Cantidad de Variables",
                             choices = c("Seleccione..." = "", "Una" = 1, "Dos" = 2)
                 )
          ),
          column(4,
                 numericInput(inputId = "decimales_tablas",
                              label = "Decimales:",
                              min = 0,  max = 100, step = 1, value = 2
                 )
          )
        ) # End FluidRow
        ,br(),
        fluidRow(
          conditionalPanel(condition = "input.qtty_var_tablas != \'\'",
              conditionalPanel(condition = "input.qtty_var_tablas >= 1",
                  fluidRow(
                  column(4,
                    selectInput(inputId = "var1_tablas",
                    label = "Variable 1",
                    choices=c("Seleccione una variable..." = "", opciones_carga2)
                    )
                  ), # End column
                  column(4,
                  conditionalPanel(condition = "input.var1_tablas != \'\'",
                    uiOutput("MODtipo_var1_tablas")
                    ) # End ConditionalPanel
                    ) # End Column
                  ) # End FluidRow
                  ), # End ConditionalPanel
                  conditionalPanel(condition = "input.qtty_var_tablas >= 2",
                  fluidRow(
                    column(4,
                  selectInput(inputId = "var2_tablas",
                  label = "Variable 2",
                  choices=c("Seleccione una variable..." = "", opciones_carga2)
                  )
                  ), # End Column
                  column(4,
                  conditionalPanel(condition = "input.var2_tablas != \'\'",
                  uiOutput("MODtipo_var2_tablas")
                  ) # End conditional
                  ) # End column
                  ) # End FluidRow
                  ) #End conditionalPanel
          ) #End conditionalPanel
        ) # End FluidRow
        ) # End div()
'



my_texto <- '
        if(is.null(input$PanelRMedic)) return(NULL)
        if(is.null(BaseSalida())) return(NULL)
        
        
        if(is.null(input$qtty_var_control)) return(NULL)
        if(is.na(input$qtty_var_control)) return(NULL)
        if(input$qtty_var_control == "") return(NULL)
        
      

        
        variables <- c()
        tipo_variables <- c()
        numero_tipo <- c()
        caso_tipo_variables <- c()
        
        # Todo para 1 variable
        if(input$qtty_var_control == 1) {
          
          if(is.null(input$var1_control)) return(NULL)
          if(is.na(input$var1_control)) return(NULL)
          if(input$var1_control == "") return(NULL)
          if(is.null(input$tipo_var1_control)) return(NULL)
          if(is.na(input$tipo_var1_control)) return(NULL)
          if(input$tipo_var1_control == "") return(NULL)
          
          variables[1] <- input$var1_control
          tipo_variables[1] <- input$tipo_var1_control
          
          
        }
        
        
        # Todo para 2 variables
        if(input$qtty_var_control == 2) {
          
          if(is.null(input$var1_control)) return(NULL)
          if(is.na(input$var1_control)) return(NULL)
          if(input$var1_control == "") return(NULL)
          if(is.null(input$tipo_var1_control)) return(NULL)
          if(is.na(input$tipo_var1_control)) return(NULL)
          if(input$tipo_var1_control == "") return(NULL)
          
          if(is.null(input$var2_control)) return(NULL)
          if(is.na(input$var2_control)) return(NULL)
          if(input$var2_control == "") return(NULL)
          if(is.null(input$tipo_var2_control)) return(NULL)
          if(is.na(input$tipo_var2_control)) return(NULL)
          if(input$tipo_var2_control == "") return(NULL)
          
          variables[1] <- c(input$var1_control)
          variables[2] <- c(input$var2_control)
          
          tipo_variables[1] <- input$tipo_var1_control
          tipo_variables[2] <- input$tipo_var2_control
          
          modelo_cambio <- c("Numérica", "Categórica")
          
          if (identical(tipo_variables, modelo_cambio)){
            variables <- variables[c(2,1)]
            tipo_variables <- tipo_variables[c(2,1)]
          }
          
          
        } # Fin para 2 variables
        
        
        # Determinamos pos_RMedic
        {
          ###  
          numero_tipo[tipo_variables == "Categórica"] <- 1
          numero_tipo[tipo_variables == "Numérica"] <- 10
          
          suma_caso <- sum(numero_tipo)
          casos_posibles <- c(1, 10, 2, 20, 11)
          
          # Determinamos los 5 casos para RMedic
          # 1) 1Q =  1 puntos
          # 2) 1C = 10 puntos
          # 3) 2Q =  2 puntos
          # 4) 2C = 20 puntos
          # 5) QC o CQ = 11 puntos
          orden_casos <- c(1:length(casos_posibles))
          dt_caso <- suma_caso == casos_posibles
          caso_tipo_variables[1] <- orden_casos[dt_caso]
          
          ###
        }
        ###########################################################
        
        
        
        
        cat("Variables Control: ", variables, "\n")
        cat("Tipo Variables Control: ", tipo_variables, "\n")
        cat("caso_tipo_variables Control: ", caso_tipo_variables, "\n\n")   
      #  cat("mi_opcion Control: ", mi_opcion, "\n")
        return(list(variables, tipo_variables, caso_tipo_variables))
        
      
        '