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

x = c(2,3,4,5,6) 
y = c(3,6,10,15,21)
h = c(2,4,6,8,10)
plot(x, y, type='l', col = 'red', xlab = "Grado", ylab = "# multiplicaciones", xlim = c(2,6), ylim = c(2,21))
par(new = TRUE)
plot(x, h, type='l', col = 'blue', xlab = "Grado", ylab = "# multiplicaciones", xlim = c(2,6), ylim = c(2,21))
