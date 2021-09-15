# # https://www.uam.es/personal_pdi/ciencias/jspinill/CFCUAM2013/GLM-RegresionLogistica-CFCUAM2013.html
# 
# PA <- mtcars[,9]
# 
# VR <- mtcars[,1]
# 
# M1 <- glm(PA ~ VR, family = binomial)
# summary(M1)
# 
# 
# 
# MyData_X <- data.frame(VR = seq(min(VR), max(VR), by=0.01))
# PREDICCION <- predict(M1,  newdata=MyData_X, type = "response")
# 
# MyData_X <- as.vector(as.matrix(MyData_X))
# plot(VR, PA)
# lines(MyData_X, PREDICCION)
# ####################################################
# 
# 
# library(mgcv)
# library(lattice)
# 
# Bees$PA <- ifelse(Bees$Parasites > 0, 1, 0)
# 
# plot(x = Bees$CellSize, y = Bees$PA)
# MyData <- data.frame(CellSize = seq(0.35, 0.66, by=0.01, length = 50))
# MyData$P <- predict(M1, newdata = MyData, type = "response")
# lines(MyData$CellSize, MyData$P)
