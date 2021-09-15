


observe(output[["SALIDAmenuR"]] <- renderUI({
  
  tabs1 <- menuBASE()
  tabs2 <- menuCONTROL()
  tabs3 <- menuDESCRIPTIVAS()
  tabs4 <- menuGRAFICOS()
  tabs5 <- menuHO()
  tabs6 <- menuSOBREVIDA()
  do.call(tabsetPanel,  c(id="goku", tabs1,tabs2, tabs3, tabs4, tabs5, tabs6))
  
}))




