N = 20

cinco = rexp(N, rate = 0.5 )
suma = sum(cinco)
vector = c(suma)
print( paste("A = " , toString(suma) ) )

# exponencial:
# esperanza: 1/lambda
# desvio: 1/lambda , varianza = 1/lambda^2 

# la esperanza de la suma nos da la suma de las esperanzas
# la varianza es la raiz de la suma de las varianza


for (i in 1:(10000)) {
  
  cinco = rexp(N, rate = 0.5 )
  suma = sum(cinco)
  vector = c(vector,suma)
  
}

# hacemos esto varias veces y vemos como se distribuyen estos valores.
hist(vector,main = "Histograma de 10.000 simulaciones de la variable C",ylim = range(0:2500), xlab = "Suma de 100 variables con distribucion exponencial de tasa 0,5", ylab = "Frecuencia")

media = mean(vector)
desvio = sd(vector)

print( paste( "media aritmetica: ", toString( media ) ) )
print( paste( "desvio: ", toString( desvio ) ) )


#asi se ve una normal
y <- rnorm( 10000 , media, desvio)

hist(y)


#asi se ve una exponencial
z <- rexp(10000, rate = 0.5 )

hist(z)





#deberian ser parecidas las 3 graficas?
# lo son.