




n = 7
incio = -pi/64
fin = pi/64

Error = 10e-5



Chebyshev <- function (fin , incio , numeros ) {
  
  chebyshevPoints <- c()
  
  for (i in numeros){
    chevyshevPoints <- c(chevyshevPoints, 1/2 * (fin + inicio) + 1/2 * (inicio - fin) * cos((2*i-1)/(2*length(numeros))*pi ) )
  }
  return(chevyshevPoints)
}


Remez <- function (inicio , fin , n ) {
  coeficientes <- Chebyshev (-pi/64 , pi/64 , seq(1 , n+2))
  fx <-sin (coeficientes)
  
  sitemaEcuacionesLineales <- matrix (nrow = n+2,ncol=n+2)
  i = 1
  
  for(filas in 1:(n+2)){
    sistemaEcuacionesLineales[filas,1] = 1
    for (columnas in 2:(n+1)) {
      sistemaEcuacionesLineales[filas,columnas] = coeficientes [filas]^(columnas+1)
      
    }
    if( i %% 2 == 0){
      sistemaEcuacionesLineales[filas,n+2] = -Error
    }else{
      sistemaEcuacionesLineales[filas,n+2] = Error
    }
    i = i+1;
  }
  
  MiniMax <- solve (sistemaEcuacionesLineales , fx)

}



print(MiniMax)















