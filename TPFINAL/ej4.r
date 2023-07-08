

#a y b
puntos = 4
hora = rexp(puntos, rate = 4)
acum = c( c( c(0), cumsum(hora)), c(1))

xdata = acum
ydata = c(0:puntos, c(puntos))

plot(main="Simulación de Nº de clientes en la primera hora de la jornada", xdata, ydata, type="s", col="red",xlim=c(0,1),ylim=c(0,5), xlab="Tiempo (horas)", ylab="Nº de Clientes",xaxt="n")
axis( side = 1, at = seq(0, 1, by = 0.1), labels = seq(0, 1, by = 0.1), cex.axis = 1 )
axis(2)

#caracteristicas fundamentales: que hayan 4 clientes por hora, que no ocurran 2 al mismo tiempo,
# 


#https://es.wikipedia.org/wiki/Proceso_de_Poisson#:~:text=En%20estad%C3%ADstica%20y%20simulaci%C3%B3n%2C%20un,a%20lo%20largo%20del%20tiempo.

#plot( ecdf(acum) )

#c

#9 am a 6 pm = 9 horas.

t <- function(horas, lambda) {
  
  init = c()
  llevo = 0
  
    sig = rexp(1, rate = lambda)
  while(llevo + sig <= horas ){
    init = c( init , c(sig) )
    llevo = llevo+sig
    sig = rexp(1, rate = lambda)
  }
  return (init)
  
}


#por propiedad de falta de memoria, se podria hacer de una, haciendo 9*5 horas, y lambda = 4.
tiempos = t(9*5,4)

hist(tiempos)


media = mean(tiempos)
desvio = sd(tiempos)

print( paste( "media aritmetica: ", toString( media ) ) )
print( paste( "desvio: ", toString( desvio ) ) )

#tanto la media como el desvio estan cerca de 1/lambda, ademas la grafica parece la de una exponencial.
# La variable T podria ajustarse por una distribucion exponencial.





