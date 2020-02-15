# sumatoria primeros numeros naturales al cuadrado

numeros = c(seq(0,200,by=20))

suma <- function (numeros) {
  
  total <- c()
  for (i in numeros) {
    
    aux = 0
    
    for (j in 1 : i) {
      
      aux = aux + j^2
    }
    
    total = c (total, aux)
  }
  
  return (total)
}

graficar = suma (numeros)
plot (numeros , graficar , type = 'b')