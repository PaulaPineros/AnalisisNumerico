redondeoPiso = function(numero,decimales)
{
  numeroAux=numero
  numeroFloor=floor(numero)
  cifras = 0
  numeroFinal = 0
  
  aux=numeroFloor
  while(aux>=1)
  {
    aux=aux/10
    cifras = cifras+1
  }
  n=cifras-decimales
  aux = numeroFloor
  if(n>0)
  {
    for(i in 1:n)
    {
      aux=aux/10
    } 
    numeroFinal= floor(aux)
    return(numeroFinal)  
  }
  aux=numero
  if(n<0)
  {
    n=n*-1
    aux=aux*(10^n)
    aux= floor(aux)
    aux= aux/(10^n)
    numeroFinal=aux
    return (numeroFinal)
  }
  return(numeroFloor)
  
}

taylor = function(cifras, dato)
{
  taylor = 1
  flag = TRUE
  contador = 1
  grado = 1
  while (flag == TRUE)
  {
    factorial = 1
    aux = grado
    while(aux >= 1)
    {
      factorial = factorial * aux
      aux = aux - 1
    }
    taylor2 = taylor + (1/factorial)* ( dato^grado )
    grado = grado + 1
    if(redondeoPiso(taylor2, cifras) == redondeoPiso(taylor, cifras))
    {
      flag = FALSE
    }else
    {
      taylor = taylor2
    }
  }
  return (taylor2)
}

