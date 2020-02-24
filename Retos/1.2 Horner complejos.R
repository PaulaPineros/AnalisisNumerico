rm(list = ls())

numMult = function(x, r){
    n = 0
    m = 0
    rv = Re(x)
    iv = Im(x)
    rr = Re(r)
    ir = Im(r)
    if(rv != 0){
      n = n + 1
    }
    if(iv != 0){
      n = n + 1
    }
    if(rr != 0){
      n = n + 1
    }
    if(ir != 0){
      n = n + 1
    }
    if(n == 4){
      m = 4
    }
    if(n == 3){
      m = 2
    }
    if(n == 2){
      m = 1
    }
    return (m)
} 

metodoHorner = function (polinomio, x0){
  resultado = polinomio[1]
  derivada = polinomio[1]
  s = 0
  m = 0
  nm = 0
  sd = 0
  md = 0
  nmd = 0
  tam = length(polinomio)
  tam = tam - 1
  
  for(i in 2:tam){
    nm = numMult(x0, resultado)
    nmd = numMult(x0, derivada)
    resultado = x0 * resultado + polinomio[i]
    derivada = resultado + x0 * derivada
    s = s + 1
    m = m + nm
    sd = sd + 2
    md = nmd + nm + md
  }
  tam = tam + 1
  nm = numMult(x0, resultado)
  resultado = x0 * resultado + polinomio[tam]
  s = s + 1
  m = m + nm
  
  cat ("Resultado:",resultado,"\n")
  cat ("Numero de sumas:", s, "\n")
  cat ("Numero de Multiplicaciones:", m,"\n")
  cat("\n")
  cat ("Derivada:",derivada,"\n")
  cat ("Numero de sumas:", sd, "\n")
  cat ("Numero de Multiplicaciones:", md,"\n")
}

x0 = 2 - 3i
polinomio <- c(2,0,-3,3,-4)
metodoHorner (polinomio, x0)




