con1=function(n)
{
  aux = cos(1/n)
  return(aux)
}
con2 = function(n)
{
  aux=(con1(n+2)-con1(n+1))^2
  aux2 = con1(n+2)-(2*con1(n+1))+con1(n)
  result = con1(n+2)-(aux/aux2)
}
fin = 10
n=1
convergencia = c(1:(fin-1))
while (n<fin)
{
  
  convergencia[n]=con2(n)
  n=n+1
  
}
print(convergencia)
plot(convergencia, type = 'l')
