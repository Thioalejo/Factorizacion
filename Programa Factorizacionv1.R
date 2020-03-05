#factorización es un programa que resuelve las ecuaciones:

#a*X^4 + b*X^3 + c*X^2 + d*X + e
#a*X^3 + b*X^2 + c*X + d
#a*X^2 + b*X + c

#El programa usa los conseptos de division sintetica y formula del estudiante 

factorizacion <- function() {
  
  cat("Ingresar 1 para polinomios de grado 4, 2 para polinomios de grado 3 y 3 para polinomios de grado 2 ","\n")
  opcion <- as.numeric(readline())
  
  if(opcion == 1){
  
  cat("Ingresar el valor 'a' que corresponde a factor a*X^4 del polinomio:","\n")
  a <- as.numeric(readline())
  
  cat("Ingresar el valor 'b' que corresponde a factor b*X^3 del polinomio:","\n")
  b <- as.numeric(readline())
  
  cat("Ingresar el valor 'c' que corresponde a factor c*X^2 del polinomio::","\n")
  c <- as.numeric(readline())
  
  cat("Ingresar el valor 'd' que corresponde a factor d*X del polinomio:","\n")
  d <- as.numeric(readline())
  
  cat("Ingresar el valor 'e' que corresponde a valor independiente del polinomio:","\n")
  e <- as.numeric(readline())
  
  cat("Se quiere hallar las raíces del polinomio:","\n")
  cat(a,"*X^4"," + ",b,"*X^3"," + ",c,"*X^2"," + ",d,"*X"," + ",e,"\n",sep="")
  
  Fx <- function(x) a*x^4+b*x^3+c*x^2+d*x+e
  
  i <- 1
  aux <- 0
  
  #Se usa el teorema del factor para allar una raiz 
  
  while (i < abs(e)){
    if(Fx(i)==0){
      aux <- i
      break
    }
    else if (Fx(i*-1)==0){
      aux <- i*-1
      break
    }
    i <- i+1
  } 
  
  #Se realiza la division sintetica
  
  aux1 <- a*aux + b
  aux2 <- aux*aux1 + c
  aux3 <- aux*aux2 + d
  #aux4 <- aux*aux3 + e
  
  r1 <-aux
  
  b <- aux1
  c <- aux2
  d <-aux3
  
  
  #Se hace division sintetica para el plinomio resultante de grado
  
  Fx <- function(x) a*x^3 + b*x^2 + c*x + d
  
  i <- 1
  aux <- 0
  
  #Se usa el teorema del factor para allar una raiz
  
  while (i < abs(d)){
    if(Fx(i)==0){
      aux <- i
      break
    }
    else if (Fx(i*-1)==0){
      aux <- i*-1
      break
    }
    i <- i+1
  } 
  
  #Se realiza la division sintetica
  
  aux1 <- a*aux+b
  aux2 <- aux1*aux+c
  #aux3 <- aux2*aux+d
  
  r2 <-aux
  
  b <- aux1
  c <- aux2
  
  
  #Estudiante
  
  w <- ((b^2) - (4*a*c))
  
  
  if(w < 0){
    
    b1 <- (sqrt(w*-1))/(2*a)
    
    b2 <- (-1*b)/(2*a)
    
    
    cat("raiz 1:",r1,"\n",sep="")
    cat("raiz 2:",r2,"\n",sep="")
    cat("raiz 3:",b2," + ",b1,"i","\n",sep="")
    cat("raiz 4:",b2," + ",b1,"i","\n",sep="")
  }
  else
  {
    
    r3= (-1*b + sqrt(w))/(2*a)
    r4= (-1*b - sqrt(w))/(2*a)
    
    cat("raiz 1:",r1,"\n",sep="")
    cat("raiz 2:",r2,"\n",sep="")
    cat("raiz 3:",r3,"\n",sep="")
    cat("raiz 4:",r4,"\n",sep="")
  }
  
  #Opcion 2
  
  }
  else if(opcion == 2){
    
    cat("Ingresar el valor 'a' que corresponde a factor a*X^3 del polinomio:","\n")
    a <- as.numeric(readline())
    
    cat("Ingresar el valor 'b' que corresponde a factor b*X^2 del polinomio:","\n")
    b <- as.numeric(readline())
    
    cat("Ingresar el valor 'c' que corresponde a factor c*X del polinomio::","\n")
    c <- as.numeric(readline())
    
    cat("Ingresar el valor 'd' que corresponde a valor independiente del polinomio:","\n")
    d <- as.numeric(readline())
    
    Fx <- function(x) a*x^3 + b*x^2 + c*x + d
    
    i <- 1
    aux <- 0
    
    #Se usa el teorema del factor para allar una raiz
    
    while (i < abs(d)){
      if(Fx(i)==0){
        aux <- i
        break
      }
      else if (Fx(i*-1)==0){
        aux <- i*-1
        break
      }
      i <- i+1
    } 
    
    #Se realiza la division sintetica
    
    aux1 <- a*aux+b
    aux2 <- aux1*aux+c
    #aux3 <- aux2*aux+d
    
    r1 <-aux
    
    b <- aux1
    c <- aux2
    
    
    #Estudiante
    
    w <- ((b^2) - (4*a*c))
    
    
    if(w < 0){
      
      b1 <- (sqrt(w*-1))/(2*a)
      
      b2 <- (-1*b)/(2*a)
      
      
      cat("raiz 1:",r1,"\n",sep="")
      cat("raiz 2:",b2," + ",b1,"i","\n",sep="")
      cat("raiz 3:",b2," - ",b1,"i","\n",sep="")
    }
    else
    {
      
      r2= (-1*b + sqrt(w))/(2*a)
      r3= (-1*b - sqrt(w))/(2*a)
      
      cat("raiz 1:",r1,"\n",sep="")
      cat("raiz 2:",r2,"\n",sep="")
      cat("raiz 3:",r3,"\n",sep="")

    
    }
  }
    else if (opcion==3){
      cat("Ingresar el valor 'a' que corresponde a factor a*X^2 del polinomio:","\n")
      a <- as.numeric(readline())
      
      cat("Ingresar el valor 'b' que corresponde a factor b*X del polinomio:","\n")
      b <- as.numeric(readline())
      
      cat("Ingresar el valor 'c' que corresponde a valor independiente del polinomio:","\n")
      c <- as.numeric(readline())
      
      cat("Se quiere hallar las raíces del polinomio:","\n")
      cat(a,"*X^2"," + ",b,"*X"," + ",c,"\n",sep="")
      
      
      w <- ((b^2) - (4*a*c))
      
      if(w < 0){
        
        b1 <- (sqrt(w*-1))/(2*a)
        
        b2 <- (-1*b)/(2*a)
        
        
        cat("raiz 1:",b2," - ",b1,"i","\n",sep="")
        cat("raiz 2:",b2," + ",b1,"i","\n",sep="")
      }
      else
      {
        cat("Se quiere hallar las raíces del polinomio:","\n")
        cat(a,"*X^2"," + ",b,"*X"," + ",c,"\n",sep="")
        
        r1= (-1*b + sqrt(w))/(2*a)
        r2= (-1*b - sqrt(w))/(2*a)
        
        cat("raiz 1:",r1,"\t","raiz 2:",r2,"\n")
      }
    }
}
factorizacion()


