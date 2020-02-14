rm(list = ls())

metodoHorner = function (polinomio, x0){
  resultado = polinomio[1]
  sumas = 0
  multiplicaciones = 0
  
  for(k in polinomio[2:length(polinomio)]){
    
    resultado = x0 * resultado + k
    sumas = sumas + 1
    multiplicaciones = multiplicaciones + 1
    
  }
  cat ("Resultado:",resultado,"\n")
  cat ("Numero de sumas:",sumas, "\n")
  cat ("Numero de Multiplicaciones:", multiplicaciones,"\n")
  return(resultado)
}


#Tercer punto. Evaluar P(x_0)=1.0001 por el metodo horner  e incontrar el error

polinomio <- rep (1 , 50)
P <- metodoHorner(polinomio , 1.0001)
Q = ((1.0001)^51-1)/(1.0001-1)

cat("El error absoluto es: ", abs(P-Q) ,"\n")
cat("El error relativo es: ", abs(P-Q)/Q*100 ,"\n")
