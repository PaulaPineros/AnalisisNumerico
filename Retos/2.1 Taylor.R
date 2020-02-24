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

factorial = function(x)
{
  fac=1
  for(i in 1:x)
  {
    fac=fac*i
  }
  return (fac)
  
}

cifras = function (x)
{
  cifrasval=0
  flag=TRUE
  while(flag == TRUE)
  {
    aux=floor(x)
    aux2=x-aux
    
    if(aux2!=0)
    {
      cifrasval=cifrasval+1
      x = x*10
    }
    else
    {
      flag=FALSE
    }
  }
  return(cifrasval)
  
}



taylorSeno  = function(precision, dato)
{
  precision=cifras(precision)
  flag = TRUE
  grado = 1
  taylor=dato
  iteraciones = 0
  results=c (1:100)
  while (flag == TRUE)
  {
    
    taylor2 = taylor + (((-1^grado)/(factorial((2*grado)+1)))* ( dato^((2*grado)+1 )))
    grado = grado + 1
    
    if(redondeoPiso(taylor2, precision) == redondeoPiso(taylor, precision))
    {
      flag = FALSE
    }else
    {
      results[iteraciones+1]=taylor
      iteraciones = iteraciones + 1
      
      taylor = taylor2
    }
  }
  print(iteraciones)

  return (taylor2)
}

