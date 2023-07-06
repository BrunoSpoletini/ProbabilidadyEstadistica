# Instalar el paquete markovchain
# https://cran.r-project.org/web/packages/markovchain/index.html
if (!require(markovchain)) {
  install.packages("markovchain")
}

# Cargar la librería markovchain
library(markovchain)

# Crear una matriz de transición
transition_matrix <- matrix(c(0.8, 0.2, 
                              0.4, 0.6), nrow = 2, byrow = TRUE)

# Crear la cadena de Markov
markov_chain <- new("markovchain", name = "MiCadenaDeMarkov1", 
                    states = c("A", "B"), transitionMatrix = transition_matrix)

# Verificar la estructura del objeto markov_chain
# str viene de structure, no string
str(markov_chain)

# Obtener información sobre la cadena de Markov
summary(markov_chain)

# Ver si es irreducible
is.irreducible(markov_chain)

# Ver período de la cadena
period(markov_chain)

# Ver estados transitorios
transientStates(markov_chain)

# Ver los estados absorbentes
absorbingStates(markov_chain)

# Obtener la distribución estacionaria/invariante
steadyStates(markov_chain)

# Veamos con otras cadenas

matriz_transicion <- matrix(c(0.2, 0.5, 0.2, 0.1,
                              0.3, 0.1, 0.4, 0.2,
                              0.1, 0.3, 0.2, 0.4,
                              0, 0, 0, 1), nrow = 4, byrow = TRUE)

mc <- new("markovchain", states = c("A", "B", "C", "D"), 
          transitionMatrix = matriz_transicion)

summary(mc)
steadyStates(mc)

trans_matrix <- matrix(c(1, 0, 0, 
                         0, 0.8, 0.2, 
                         0, 0, 1), nrow = 3, byrow = TRUE)


mc_2dsitest <- new("markovchain", transitionMatrix = trans_matrix)

summary(mc_2dsitest)
steadyStates(mc_2dsitest)

# Ver grafo asociado
plot(markov_chain) # Ojo con los graficos y las probabilidades
plot(mc)
plot(mc_2dsitest)
# Para grafos lindos les recomiendo graphviz o, si usan latex para
# el informe, tikzpicture


# Obtener la probabilidad de ir de un estado a otro
transitionProbability(markov_chain,"A","B")
markov_chain[1,2]
# No olvidemos que podemos tener probabilidades iniciales!


# Generar una secuencia de estados - simulación
sequence1 <- rmarkovchain(n = 4, object = markov_chain)
# Imprimir la secuencia generada
print(sequence1)

# Generar una secuencia de estados - a mano
sequence2 <- c("A", "B", "B", "A")
# Imprimir la secuencia generada
print(sequence2)


# Calcular la probabilidad de la secuencia
calc_prob_seq <- function(seq) {
  prob_sequence <- 1 # porque no tenemos una probabilidad inicial // si no iría aquí la del estado de inicio
  # Recorremos la secuencia desde el primer estado hasta el penúltimo estado
  for (i in 1:(length(seq) - 1)) {
    # Multiplicamos la probabilidad de transición por la probabilidad acumulada
    prob_transition <- markov_chain@transitionMatrix[seq[i], seq[i + 1]]
    prob_sequence <- prob_sequence * prob_transition
  }
  # Imprimimos la probabilidad
  print(prob_sequence)
}

calc_prob_seq(sequence1)
calc_prob_seq(sequence2)

# Número de pasos
n <- 3

# Calcular la matriz de transiciones en n=3 pasos
matriz_trans_n_steps <- matriz_transicion %*% matriz_transicion %*% matriz_transicion
print(matriz_trans_n_steps)
str(matriz_trans_n_steps)
# Con markovchain
mc_n_steps <- mc^n
print(mc_n_steps)
str(mc_n_steps)
# trans_matrix_n_steps <- mc%*%mc%*%mc -> no, porque usamos objetos markovchain



# Funciones por fuera de la libreria markovchain pero que les resultarán útiles

# Función para visualizar una caminata
visualize_walk <- function(walk) {
  steps <- length(walk)  # Número de pasos en la caminata
  time <- 1:steps  # Vector de tiempo
  
  plot(time, walk, type = "n", xlab = "Tiempo", ylab = "Posición", ylim=c(0,7), 
       main = "Caminata Aleatoria", OrdDf)  # Crear un gráfico vacío
  
  lines(time, walk, type = "s", col = "blue")  # Dibujar líneas para la caminata
  points(time, walk, pch = 19, col = "red")  # Dibujar puntos para resaltar cada posición
  
  legend("bottomleft", legend = c("Caminata", "Posiciones"), col = c("blue", "red"), lty = c(1, 0), pch = c(NA, 19))
}

# Generar una caminata aleatoria
generate_random_walk <- function(n) {
  walk <- numeric(n)  # Crear un vector vacío de longitud n
  walk[1] <- 3 # k inicial
  for (i in 2:n) {
    if ( walk[i-1] == 7 || walk[i-1] == 0 ){
      f <- i
      return(walk[1:i-1])
      
    } else {
      step <- sample(c(-1, 1),1, prob=c(0.9,0.1))  # Elegir un paso aleatorio hacia la izquierda (-1) o hacia la derecha (1)
      walk[i] <- walk[i-1] + step  # Calcular la nueva posición sumando el paso al valor anterior
    }
  }
  
  return(walk)
}

# Generar una caminata aleatoria de longitud 10
walk <- generate_random_walk(1000)

# Visualizar la caminata utilizando la función visualize_walk
visualize_walk(walk)


# Acá mostramos una caminata aleatoria pero esto se podría pensar para cualquier
# simulación respetando los modelos de cadenas de markov (rmarkovchain()), 
# procesos de poisson (rpois()) o procesos de bernoulli (rbinom()).












