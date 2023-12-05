choose.files()

pampers <- read.csv("C:\\Users\\HP\\Downloads\\pampers.csv")

library(arules)
library(arulesViz)


#convertimos a un objeto tipo transaccion
View(pampers)

pampers <- as.matrix(pampers)
head(pampers)

t_pampers <- as(pampers, "transactions") #el formato transsaccion con el que trabaja el algoritmo
t_pampers@itemInfo #nos permite ver los productos

summary(t_pampers)

#Calcular cuantos productos se compraron

100*3*0.506

#¿Cuantos productos ha comprado en cada transacción?

tam_pampers <- size(t_pampers) #Tamaño de cada transacción
head(tam_pampers)

#Ver las ventas o transacciones realizadas 
labels(t_pampers)[100]
labels(t_pampers) [1:5]

#Visualizar la matriz de ventas 
t_pampers@itemInfo 
image(t_pampers[1:15])

#Mostrar el soporte/frecuencia de cada producto 

itemFrequency(t_pampers[, 1:3])

itemFrequencyPlot(t_pampers[, 1:3])

#Visualizar el soporte de los itemas (aquellos que tengan una proporcion de 10%)
itemFrequencyPlot(t_pampers, support= 0.10)

#Algoritmo a priori 

regla_pampers <- apriori(t_pampers, parameter = 
                           list(supp= 0.1, conf = 0.80, target = "rules", minlen=2)) #se genero la regla

#nos fijamos en el writing (que indica cuantas reglas o asociaciones se identifico)

#Visualizamos el contenido del modelo generado con la regla

inspect(regla_pampers)
#¿Que pasa si hay reglas de un solo producto? ---> se deben eliminar 
### se añade minlen = 2

labels(regla_pampers)
summary(regla_pampers)

#Exportar resultados 

write(regla_pampers, file = "pampers.csv", sep = ",", quote = TRUE, row.names = FALSE)


#Buscar subreglas 

regla_pampersub <- subset(regla_pampers, items = c("dommelsch"))

inspect(regla_pampersub)

#lhs ---> la causa
#rhs ---> el efecto

#regla_pampersub <- subset(regla_pampers, rhs o lhs = c("dommelsch"))


#Visualizacion 

#   1
plot(regla_pampers)

library(plotly)
plot(regla_pampers, engine = "plotly")

#   2

subregla_pampers <- head(regla_pampers, n = 10, by = "confidence")

plot(subregla_pampers, method = "graph", engine = "htmlwidget")

plot(subregla_pampers, method = "paracoord")
