install.packages("NCA") #Instalar NCA
library (NCA) # Cargar la librería

#Directorio
setwd("X:/yaret universidad/6ºaño/TFT/codigo")
#Cargamos los datos

data1 <- read.csv("variables_latentes.csv") 

head(data1)
names(data1)
#Realizamos los análisis

#cambiar corner 1 y 4 para las esquinas
model1 <- nca_analysis(data1, "jobSatisfaction", "performance", corner = 1, test.rep = 10000 )
model1
nca_output(model1, plots = TRUE, bottlenecks = T)



model2 <- nca_analysis(data1, "orgSupport", "performance", corner = 1, test.rep = 10000)
model2
nca_output(model2, plots = TRUE, bottlenecks = T)



model3 <- nca_analysis(data1, "orgSupport", "jobSatisfaction", corner = 1, test.rep = 10000)
model3
nca_output(model3, plots = TRUE, bottlenecks = T)

