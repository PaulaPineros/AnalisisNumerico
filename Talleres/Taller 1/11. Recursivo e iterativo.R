f = function(x){log(x+2)}

g = function(x){sin(x)}

tole = 0.00000001

q = function(x){log(x+2)-sin(x)}

teo = uniroot(q, c(-1.8,-1), tol = 1e-9) 

plot(f, xlim = c(-4,3), ylim=c(-3,3), ylab = "f(x) Y g(x)", col = "blue")
par(new = TRUE)
plot(g, xlim = c(-4,3), ylim=c(-3,3), ylab = "", col = "salmon")
plot(q, xlim = c(-3,5), ylim=c(-4,4), ylab = "f(x) - g(x)", col = "purple")
par(new = TRUE)
abline(0,0, col = "red")

a = -1.8
b = -1
arreglo = c(0)

arreglo[1] = a
arreglo[2] = b
i=3

e1 = c() 
eM1 = c()

while(abs(teo$root-arreglo[i-2]) > tole){
  e1[i-2] = abs(teo$root-arreglo[i-2])
  arreglo[i] = arreglo[i-1] - ((q(arreglo[i-1]) * (arreglo[i-1] - arreglo[i-2])) / (q(arreglo[i-1]) - q(arreglo[i-2])))
  i = i + 1
}

i = 1
while(i < length(e1) + 1){
  eM1[i] = e1[i+1]
  i = i + 1
}

numIteraciones = length(arreglo)

metodo2 = c(0)
e2 = c(0)
e2M1 = c(0)
metodo2[1] = a
metodo2[2] = b
i = 3

while(abs(teo$root-metodo2[i-2]) > tole){
  e2[i-2] = abs(teo$root-arreglo[i-2])
  metodo2[i] = metodo2[i-1] - ((q(metodo2[i-1])) * (metodo2[i-1] - metodo2[i-2]) / (q(metodo2[i-1]) - q(metodo2[i-2])))
  i = i + 1
}

i = 1
while(i < length(e2) + 1){
  e2M1[i] = error2[i+1]
  i = i + 1
}

plot(e2,e2M1, type ="l")
