rm(list = ls())

metodoHorner = function (polinomio, x0){
  resultado = polinomio[1]
  derivada = polinomio[1]
  tam = length(polinomio)
  tam = tam - 1
  s = 0
  m = 0
  for(i in 2:tam){
    resultado = x0 * resultado + polinomio[i]
    derivada = x0 * derivada + resultado
    m = m + 2
    s = s + 2
  }
  cat ("Derivada: ", derivada, "\n")
  cat("Número de multiplicaciones: ", m, "\n")
  cat("Número de sumas: ", s)
}

x0 = -2
polinomio <- c(2,0,-3,3,-4)
metodoHorner (polinomio, x0)
 
