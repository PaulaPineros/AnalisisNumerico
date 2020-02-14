distanciaE = function(velocidad, errorV, tiempo, errorT){
  
  distancia = velocidad * tiempo
  errorA = (velocidad * errorT) + (tiempo * errorV)
  cat("Error absoluto: ", errorA, "\n")
  
  errorR = (errorV/ velocidad) + (errorT/ tiempo)
  errorR = errorR * 100
  cat("Error relativo: ", errorR, "%")
}

v = 4
eV = 0.1
t = 5
eT = 0.1
distanciaE(v, eV, t, eT)
