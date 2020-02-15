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
funcion1= function(x)
{
  aux=(3*(sin(x)^3))-1
  return(aux)
}
funcion2 = function(t)
{
  aux=4*sin(t)*cos(t)
  return(aux)
}

x=0
t=0
dt=0.001
maximo=100000
recorrer = 1
aux=1
valorest =  c(1:maximo)
valoresf1= c(1:maximo)
valoresf2= c(1:maximo)
while(t<100)
{
  valorest[recorrer]= t
  valoresf1[recorrer] = redondeoPiso(funcion1(t),6)
  valoresf2[recorrer]=  redondeoPiso(funcion2(t),6)
  
  t=t+dt
  recorrer=recorrer + 1
  
  
}
while(aux<maximo)
{
  if(valoresf1[aux]==valoresf2[aux])
  {
    x = aux
  }
  aux = aux+1
  
}
print(x)

