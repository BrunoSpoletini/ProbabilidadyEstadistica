

#a

library(markovchain)

# Crear una matriz de transición

#  1 2 3 4 5
#1 0 1 1 0 0
#2 0 0 0 0 0
#3 1 1 0 1 0
#4 0 0 0 0 1
#5 0 0 1 1 0
transition_matrix <- matrix(c(  0, 1/2, 1/2,   0,   0,
                              1/4,   0, 1/4, 1/4, 1/4,
                              1/3, 1/3,   0, 1/3,   0,
                                0,   0,   0,   0,   1,
                                0,   0, 1/2, 1/2,   0), nrow = 5, byrow = TRUE)


matriz_trans_2 <- transition_matrix %*% transition_matrix

matriz_trans_4 <- matriz_trans_2 %*% matriz_trans_2

matriz_trans_8 <- matriz_trans_4 %*% matriz_trans_4

matriz_trans_16 <- matriz_trans_8 %*% matriz_trans_8

matriz_trans_32 <- matriz_trans_16 %*% matriz_trans_16

otra <- matrix(c(0.2,0.2,0.2,0.2,0.2), nrow = 5, byrow = TRUE)

ver <- matriz_trans_32 %*% otra

# Crear la cadena de Markov
markov_chain <- new("markovchain", name = "MiCadenaDeMarkov1", 
                    states = c("1", "2" , "3" , "4", "5"), transitionMatrix = transition_matrix)


plot(markov_chain)
# Para grafos lindos les recomiendo graphviz o, si usan latex para
# el informe, tikzpicture

#grafico en latex.


#b: mandar un vector de 20% a cada uno y ver como termina? seria el steadyStates(markov_chain)?
