function(input, output, session) {
  
  
  
  
  BaseSalida <- reactive({
    base_interna <- mtcars
    base_interna[,2] <- as.character(base_interna[,2])
    base_interna
  })
  
  UserSelection <- callModule(module = BatallaNavalSERVER, id =  "tablas01", 
                             base = BaseSalida, 
                             verbatim = FALSE)

  MiniBase <- callModule(module = MiniBaseSERVER, id =  "tablas02",
                         base = BaseSalida,
                         batalla_naval = UserSelection$batalla_naval,
                         verbatim = FALSE)

 
  
  callModule(module = TablasSERVER, id =  "tablas03", 
             minibase = MiniBase,
             batalla_naval = UserSelection$batalla_naval,
            # casoRMedic = UserSelection$batalla_naval[[4]], #reactive(1),
            # casoRMedic = UserSelection$batalla_naval$caso_tipo_variables,
             decimales = UserSelection$decimales)
  
  


}