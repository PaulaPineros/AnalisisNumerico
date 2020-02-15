# Sub matriz triangular

numeros = c(seq(1,100,by=10))

matriz_triangular <- function (numeros) {
  
  total <- c()
  
  for (i in numeros) {
    
    matriz_triangular = matrix (1 , ncol = i, nrow = i)
    aux = 0
    
    for (j in 1:i) {
      for (k in 1:j) {
        
        aux = aux + matriz_triangular[j,k]
      }   
    }
    
    total = c(total , aux)
    aux = 0
  }
  
  return (total)
}

grafica = matriz_triangular (numeros)

plot (numeros , grafica , type = 'b',col = 1)

