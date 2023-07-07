install.packages("xtable")
install.packages("markovchain")
library(xtable)
library(markovchain)

# Veamos con otras cadenas
matriz_transicion <- matrix(c(0, 0, 0, 0, 1,
                              0, 8/13, 3/13, 1/13, 1/13,
                              1/16, 3/16, 3/8, 1/4, 1/8,
                              0, 1/11, 4/11, 5/11, 1/11,
                              0, 1/8, 1/2, 1/8, 1/4), nrow = 5, byrow = TRUE)

vectorinicial = c(0,0,0,0,1)

mc <- new("markovchain", states = c("80","135","139","445","NoA"), 
          transitionMatrix = matriz_transicion)

A <- matriz_transicion
A3<-round(A%*%A%*%A %*% vectorinicial,2)
A3

print(xtable(transition_matrix), include.rownames = FALSE)

steadyStates(matriz_transicion)
