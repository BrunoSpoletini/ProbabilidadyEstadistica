

#table <- read_excel("Base1.xls")

#attach(table)

# Analisis univariado
# Altura

x <- table (cut(altura, c(1,5,10,15,20,25,30,35,40) ,include.lowest=TRUE, right = FALSE))

porc <- round((x/dim(table)[1]),2)
y <- data.frame(x, porc, cumsum(porc))[,-3]
colnames(y) = c("Intervalo","Frecuencia absoluta", "Frecuencia relativa", "Frecuencia relativa acumulada")
tablaAltura <- xtable(y,caption="Altura de los árboles censados")

print(tablaAltura, include.rownames = FALSE)


# -- Grafico de histograma que todavia no dimos --


#Diametro

frecuencia <- table(cut(diametro, c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140) ,include.lowest=TRUE, right = FALSE))
porc <- round((frecuencia/dim(table)[1]),2)
y <- data.frame(frecuencia, porc, cumsum(porc))[,-3]
colnames(y) = c("Intervalo","Frecuencia absoluta", "Frecuencia relativa", "Frecuencia relativa acumulada")
tablaAltura <- xtable(y)
print(tablaAltura, include.rownames = FALSE)



#Inclinacion

# Arboles con inclinacion 0:
cant <- length(inclinacio[inclinacio == 0])

frec <- table(cut(inclinacio, c(0,10,20,30,40), right = FALSE))
porc <- round((frec/dim(table)[1]),2)
y <- data.frame(frec, porc*100)[,-3]
colnames(y) = c("Intervalo","Frecuencia absoluta", "Porcentaje (%)")
tabla <- xtable(y)
print(tabla, include.rownames = FALSE)



#Especie
#tabla
frec <- table(especie)
porc <- round(frec/dim(table)[1],2)
y <- data.frame(frec, porc, cumsum(porc))[,-3]
colnames(y) = c("Intervalo","Frecuencia absoluta", "Frecuencia relativa", "Frecuencia relativa acumulada")
tabla <- xtable(y)
print(tabla, include.rownames = FALSE)

#grafico
plot(frec, xlim=c(1,10), ylim=c(1,70), xlab="Especie", ylab="Frecuencia" ) #cex.axis=1.4


#Origen

#Brotes




