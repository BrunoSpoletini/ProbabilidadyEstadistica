#Para empezar a usar R, ejecutar en la consola:\\
#install.packages("readxl") \\
#library("readxl") \\
#install.packages("xtable")\\
#library(xtable)\\
#
#Los datos que tenemos son el censo de los arboles, o una muestra del censo?
#En base a la respuesta de eso, corregir.
#










table <- read_excel("Base1.xls")

#attach(table)

library("readxl")
library("xtable")
attach(table)

# Analisis univariado
# Altura

x <- table (cut(altura, seq(0, 37.5, by = 2.5) ,include.lowest=TRUE, right = FALSE))

porc <- round((x/dim(table)[1]),2)
y <- data.frame(x, porc, cumsum(porc))[,-3]
colnames(y) = c("Intervalo","Frecuencia absoluta", "Frecuencia relativa", "Frecuencia relativa acumulada")
tablaAltura <- xtable(y,caption="Altura de los árboles censados")

print(tablaAltura, include.rownames = FALSE)



hist(altura, main = "Histograma de altura", xlab = "Altura (m)", ylab = "Frecuencia", 
     ylim = range(0:80), col = "darkgoldenrod1", right = FALSE, 
     breaks = seq(0, 37.5, by = 2.5), 
     xlim = range(0:40), xaxt = "n") # xaxt = "n" turns off the x-axis labels


axis(side = 1, at = seq(0, 37.5, by = 2.5), labels = seq(0, 37.5, by = 2.5), cex.axis = 1)


plot(main="Polígono de frecuencia relativa de altura", seq(0, 35, by = 2.5), porc, type="o", col="blue",xlim=c(0,37.5), ylim=c(0,0.2), xlab="Intervalo", ylab="Frecuencias relativas",xaxt="n")
axis(side = 1, at = seq(0, 37.5, by = 2.5) - 1.25, labels = seq(0, 37.5, by = 2.5), cex.axis = 1, )
axis(2)

plot(main="Polígono de frecuencia relativa acumulativa de altura",seq(0, 35, by = 2.5), cumsum(porc), type="o", col="red",xlim=c(0,37.5), ylim=c(0,1), xlab="Intervalo", ylab="Frecuencias relativas acumuladas",xaxt="n")
axis(side = 1, at = seq(0, 37.5, by = 2.5) - 1.25, labels = seq(0, 37.5, by = 2.5), cex.axis = 1, )
axis(2)



#Diametro

frecuencia <- table(cut(diametro, seq(0,140,by=10) ,include.lowest=TRUE, right = FALSE))
porc <- round((frecuencia/dim(table)[1]),2)
y <- data.frame(frecuencia, porc, cumsum(porc))[,-3]
colnames(y) = c("Intervalo","Frecuencia absoluta", "Frecuencia relativa", "Frecuencia relativa acumulada")
tablaAltura <- xtable(y)
print(tablaAltura, include.rownames = FALSE)


boxplot(diametro, horizontal=TRUE,xlab="Diámetro (cm)", xaxt="n",col = "chartreuse4", main="Boxplot de diámetro")
axis(side = 1, at = seq(0, 140, by = 10), labels = seq(0, 140, by = 10), cex.axis = 1)




#Inclinacion

# Arboles con inclinacion 0:
cant <- length(inclinacio[inclinacio == 0])

frec <- table(cut(inclinacio, seq(0, 42, by = 3), right = FALSE))
porc <- round((frec/dim(table)[1]),3)
y <- data.frame(frec, porc, cumsum(porc))[,-3]
colnames(y) = c("Intervalo","Frecuencia absoluta", "Frecuencia relativa", "Frecuencia relativa acumulada")
tabla <- xtable(y)
print(tabla, include.rownames = FALSE)



hist(inclinacio, main = "Distribución de la inclinación de los arboles muestrados", xlab = "Inclinación (º)", ylab = "Frecuencia", 
     ylim = range(0:300), col = "darkgoldenrod1", right = FALSE, 
     breaks = seq(0, 42, by = 3), 
     xlim = range(0:42), xaxt = "n") # xaxt = "n" turns off the x-axis labels


axis(side = 1, at = seq(0, 42, by = 3), labels = seq(0, 42, by = 3), cex.axis = 1)

plot(main="Polígono de frecuencia relativa de la inclinación", seq(0, 39, by = 3), porc, type="o", col="blue",xlim=c(0,42),ylim=c(0,0.8), xlab="Intervalo", ylab="Frecuencias relativas",xaxt="n")
axis(side = 1, at = seq(0, 42, by = 3) - 1.5, labels = seq(0, 42, by = 3), cex.axis = 1, )
axis(2)

plot(main="Polígono de frecuencia relativa acumulativa de la inclinación",seq(0, 39, by = 3), cumsum(porc), type="o", col="red",xlim=c(0,42), ylim=c(0,1), xlab="Intervalo", ylab="Frecuencias relativas acumuladas",xaxt="n")
axis(side = 1, at = seq(0, 42, by = 3) - 1.25, labels = seq(0, 42, by = 3), cex.axis = 1, )
axis(2)



#Especie
#tabla
frec <- table(especie)
porc <- round(frec/dim(table)[1],3)

cumsum(porc)

y <- data.frame(frec, round(porc,2), round(cumsum(porc),2))[,-3]
colnames(y) = c("Intervalo","Frecuencia absoluta", "Frecuencia relativa", "Frecuencia relativa acumulada")
tabla <- xtable(y)
print(tabla, include.rownames = FALSE)

#grafico
plot(frec, xlim=c(1,10), ylim=c(1,70), xlab="Especie", ylab="Frecuencia", main="Gráfico de bastones de las especies de los árboles de la muestra" ) #cex.axis=1.4
#axis(1, at = (0:8), labels = c("Acacia","Álamo", "Casuarina", "Ceibo" ,"Eucalipto", "Ficus", "Fresno", "Jacarandá", "Palo borracho") )


aux <- c(frec,41)
names(aux) <- c("Acacia","Álamo", "Casuarina", "Ceibo" ,"Eucalipto", "Ficus", "Fresno", "Jacarandá", "Palo borracho", "Palo borracho")
plot(aux, xlim=c(1,10), ylim=c(1,70), xlab="Especie", ylab="Frecuencia", type='s',axes = FALSE, main= "Gráfico escalonado de las especies de los árboles de la muestra" ) #cex.axis=1.4 #axes = FALSE
axis(1, at = (1:9) + 0.5, labels = c("Acacia","Álamo", "Casuarina", "Ceibo" ,"Eucalipto", "Ficus", "Fresno", "Jacarandá", "Palo borracho"), tick = FALSE) # desplazamos el eje x
axis(1, at = (1:10), tick = TRUE, labels = FALSE) # ponemos las barras separadoras
axis(2) # dejamos el eje y por defecto.


#Origen
frec <- table(origen)
porc <- round(frec/dim(table)[1],3)

y <- data.frame(frec, round((100*porc),2))[,-3]
colnames(y) = c("Origen","Frecuencia Absoluta","Porcentaje (%)")
tabla <- xtable(y)
print(tabla, include.rownames = FALSE)
y

pie(table(origen), labels = c("Exótico", "Nativo/Autóctono"), main="Orígen de los árboles censados")



#Brotes

#tabla
frec <- table(brotes)
porc <- round(frec/dim(table)[1],4)

cumsum(porc)

y <- data.frame(frec, round(porc,2), round(cumsum(porc),2))[,-3]
colnames(y) = c("Nº brotes","Frecuencia absoluta", "Frecuencia relativa", "Frecuencia relativa acumulada")
tabla <- xtable(y)
print(tabla, include.rownames = FALSE)

#grafico
plot(frec, xlim=c(1,8), ylim=c(1,100), xlab="Nº brotes", ylab="Frecuencia", main= "Gráfico de bastones de número de brotes de los árboles de la muestra" ) #cex.axis=1.4



plot(frec, xlim=c(0,8), ylim=c(1,100), xlab="Nº brotes", ylab="Frecuencia",type = 's', axes = FALSE, main= "Gráfico escalonado del número de brotes de los árboles de la muestra") #axes = FALSE
axis(1, at = (0:8) + 0.5, labels = 0:8, tick = FALSE) # desplazamos el eje x
axis(1, at = (0:8), tick = TRUE, labels = FALSE) # ponemos las barras separadoras
axis(2) # dejamos el eje y por defecto.



aux <- c(frec,2)
names(aux) <- 0:9
plot(aux, xlim=c(1,10), ylim=c(1,100), xlab="Nº brotes", ylab="Frecuencia",type = 's', axes = FALSE) #axes = FALSE
axis(1, at = (0:8) + 1.5, labels = 0:8, tick = FALSE) # desplazamos el eje x
axis(1, at = (0:10), tick = TRUE, labels = FALSE) # ponemos las barras separadoras
axis(2) # dejamos el eje y por defecto.





#Analisis bivariado

#analizaremos la altura y la especie de los arboles




boxplot(altura~especie, ylab = "altura (m)", col = "aquamarine4", cex=1.5,cex.lab= 1.2, main= "Análisis de la altura respecto a la especie de los árboles de la muestra")



q <-split(table, table$especie) # separamos por especie
quantile(q$Acacia$altura)# armamos el quartil por cada especie.
quantile(q$Álamo$altura)
quantile(q$Casuarina$altura)
quantile(q$Ceibo$altura)
quantile(q$Eucalipto$altura)
quantile(q$Ficus$altura)
quantile(q$Fresno$altura)
quantile(q$Jacarandá$altura)
quantile(q$`Palo borracho`$altura)

y <- data.frame( c("Mínimo","1er Cuartil","Mediana","3er Cuartil","Máximo") , quantile(q$Acacia$altura), quantile(q$Álamo$altura),quantile(q$Casuarina$altura),
                 quantile(q$Ceibo$altura), quantile(q$Eucalipto$altura), quantile(q$Ficus$altura),
                 quantile(q$Fresno$altura), quantile(q$Jacarandá$altura), quantile(q$`Palo borracho`$altura)
                 )
colnames(y) = c("Medidas de interés","Acacia","Álamo", "Casuarina", "Ceibo" ,"Eucalipto", "Ficus", "Fresno", "Jacarandá", "Palo borracho")
tabla <- xtable(y)
print(tabla, include.rownames = FALSE)



y= cut(altura, seq(0, 35, by = 2.5))

j = table(y, especie)
tabla <- xtable(j)
print(tabla, include.rownames = FALSE)







quantile(altura)
quantile(diametro)
quantile(inclinacio)
quantile(brotes)

y <- data.frame( c("Mínimo","1er Cuartil","Mediana","3er Cuartil","Máximo") , quantile(brotes)
)
colnames(y) = c("Medidas de interés","Brotes")
tabla <- xtable(y)
print(tabla, include.rownames = FALSE)


# mean(altura)

# desviacion estandar: sqrt(var(altura))






