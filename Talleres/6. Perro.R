y=c(3,3.7,3.9,4.5,5.7,6.69,7.12,6.7,4.45,7,6.1,5.6,5.87,5.15,4.1,4.3,4.1,3,2)
x=c(1,2,5,6,7.5,8.1,10,13,17.6,20,23.5,24.5,25,26.5,27.5,28,29,30,31)
dx=10
npuntos=9
crearCoeficientes=function(x1,y1,x2,y2,x3,y3)
{
  a = matrix(c(x1^2,x2^2,x3^2,x1,x2,x3,1,1,1),ncol=3,nrow=3)
  b = c(y1,y2,y3)
  c = solve(a,b)
  return(c)
}
funcion=function(c1,c2,c3,x)
{
  y = (c1*(x^2))+(c2*x)+c3
  return(y)
}
crearIntervalos=function(x1,x2)
{
  aux1= x2-x1
  aux1= aux1/dx
  return(aux1)
}


colum=1
coeficientes = matrix(1:(npuntos*3) ,ncol = npuntos, nrow = 3) 
valoresx= matrix(1:(npuntos*dx) ,ncol = npuntos, nrow = dx) 
valoresy= matrix(1:(npuntos*dx) ,ncol = npuntos, nrow = dx)
i=1
while(i<19)
{
  c = crearCoeficientes(x[i],y[i],x[i+1],y[i+1],x[i+2],y[i+2])
  for(k in 1:3)
  {  
    coeficientes[k,colum]=c[k]
  }
  colum= colum +1
  i=i+2
  
}
colum=1
i=1
while(i<19)
{
  intervalo=crearIntervalos(x[i],x[i+2])
  aux= x[i]
  for(k in 1:dx)
  {  
    valoresx[k,colum]=aux
    aux=aux+intervalo
  }
  
  i=i+2
  colum=colum +1
}  
colum=1
i=1

while(i<=npuntos)
{
  
  
  for(k in 1:dx)
  { 
    valoresy[k,i]= funcion(coeficientes[1,i],coeficientes[2,i],coeficientes[3,i],valoresx[k,i])
    
  }
  
  i=i+1
  
}  
plot(valoresx,valoresy,type = "o",xlim =c(0,40))

