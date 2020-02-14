raizCuadrada = function(dato, errorP)
{
  valorI = 0
  valorF = 1
  inicial = 0
  error = 100
  aux = 1
  while (valorF < dato)
  {
    valorI = aux * aux
    aux = aux+1
    valorF = aux * aux
  }
  valorF=aux
  valorI=aux-1
  inicial = valorI + valorF
  inicial = inicial / 2
  
  while(error > errorP)  
  {
    y = (1/2)*(inicial + (dato/inicial))
    error = y - inicial
    inicial = y 
  }
  
  return (inicial)
}