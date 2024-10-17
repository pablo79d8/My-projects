#### funciones auxiliares

clave<-c(toupper(letters)[1:14],"Ñ",toupper(letters)[15:26]," ")

letra_a_numero<-function(a) match(a,clave) 
numero_a_letra<-function(n) clave[n]

## funcion que transforma mensaje de texto 
## a secuencia de numeros

mensaje_a_numero<-function(mensaje){
  mensajenumerado<-NULL
  for(i in 1:nchar(mensaje)){
    mensajenumerado<-c(mensajenumerado,letra_a_numero(substring(mensaje,i,i)))  
  }
  return(mensajenumerado)
}
## funcion que transforma secuencia de numeros
## a mensaje de texto 

mensaje_a_letra<-function(mensaje){
  mensajetexto<-NULL
  for(i in 1:length(mensaje)){
    mensajetexto<-paste(mensajetexto,numero_a_letra(mensaje[i]),sep='')  
  }
  return(mensajetexto)
}

bigramas_palabra <- function(palabra) {
  aaa<-unlist(strsplit(palabra,''))
  nnn<-length(aaa)
  if (nnn > 1) {
  bigramas<-paste0(aaa[1:(nnn-1)],aaa[2:nnn])
  }
  return(bigramas)
}

trigramas_palabra <- function(palabra) {
  aaa<-unlist(strsplit(palabra,''))
  nnn<-length(aaa)
  if (nnn > 2) {
  trigramas<-paste0(aaa[1:(nnn-2)],aaa[2:(nnn-1)],aaa[3:nnn])
  }
  return(trigramas)
}



## funcion que aplica el descifrado de sustitucion 
## dado por ordenacion

decodificador<-function(mensaje,ordenacion){
  fun.auxiliar<-function(i) ordenacion[i]
  mensaje_decodificado<-NULL
  for (j in 1:length(mensaje)) mensaje_decodificado<-c(mensaje_decodificado,fun.auxiliar(mensaje[j]))
  return(mensaje_decodificado)
}



metropolis <- function(mensaje_numerico, iteraciones) {
  mejor_ordenacion <- sample(1:28)  # Inicializar con una permutación aleatoria
  mensaje_decodificado <- decodificador(mensaje_numerico, mejor_ordenacion)
  mensaje_texto <- mensaje_a_letra(mensaje_decodificado)
  mejor_score <- calcular_score(mensaje_texto, mejor_ordenacion)
  
  for (iteracion in 1:iteraciones) {
    # Generar una permutación propuesta mediante una trasposición aleatoria
    nueva_ordenacion <- mejor_ordenacion
    indices <- sample(1:28, 2)
    nueva_ordenacion[indices] <- nueva_ordenacion[c(indices[2], indices[1])]
    mensaje_decodificado <- decodificador(mensaje_numerico, nueva_ordenacion)
    mensaje_texto <- mensaje_a_letra(mensaje_decodificado)
    
    # Calcular el score de la nueva permutación
    nuevo_score <- calcular_score(mensaje_texto, nueva_ordenacion)
    # Decidir si aceptar la nueva permutación
    print(exp(nuevo_score - mejor_score))
    if (runif(1) > exp(nuevo_score - mejor_score)){
      mejor_ordenacion <- nueva_ordenacion
    } else {
      mejor_score <- nuevo_score
    }
    
  }
  
  return(mejor_ordenacion)
}


calcular_score <- function(mensaje, ordenacion) {
  
  
  palabras <- unlist(strsplit(mensaje, split = " ")); palabras
  
  score1 <- 0
  score1 <- calc_score1(palabras)
  #print(score1)
  
  score2 <- 0
  score2 <- calc_score2(palabras)
  #print(score2)
  
  score3 <- 0
  score3 <- calc_score3(palabras)
  #print(score3)
  
  score <- log(score1^3)+log(score2)+log(score3^(1/2))
  
  
  return(score)
}


calc_score1 <- function(palabras) {
  
  valor1 <- 0
  for (i in 1:length(palabras)) {
    if (nchar(palabras[i])<24) {
    valor1 <- valor1 + log(frec_longitud[frec_longitud$longitud==length(palabras[i]),2])
    }
  }
  return(valor1)
}

calc_score2 <- function(palabras) {
  
  valor2 <- 0
  for(i in 1:length(palabras)){
  bigramas <- bigramas_palabra(palabras[i])
  
  for (j in 1:length(bigramas)) {
    if(bigramas[j] %in% frec_bigramas$bigrama) {
    valor <- log(frec_bigramas[frec_bigramas$bigrama==bigramas[j],2][2])
    }
    
    if (!is.na(valor)) {
      valor2 <- valor2 + log(valor)
    }
  }
  
  }
  return(valor2)
}


calc_score3 <- function(palabras) {
  
  valor2 <- 0
  for(i in 1:length(palabras)){
    trigramas <- trigramas_palabra(palabras[i])
  
  for (j in 1:length(trigramas)) {
    if(trigramas[j] %in% frec_trigramas$V1) {
      valor <- log(frec_trigramas[frec_trigramas$V1==trigramas[j],2])
    }
    
    if (!is.na(valor)) {
      valor2 <- valor2 + log(valor)
    }
  }
  }
  return(valor2)
}


mensaje <- readLines("mensaje_cifrado.txt")

frec_longitud <- read.table("long_palabras_espanol.txt")

frec_bigramas <- read.table("bigramas_espanol.txt", encoding = "UTF-8")

frec_trigramas <- read.table("trigramas_espanol.txt", encoding = "UTF-8")


mensaje_numerico <- mensaje_a_numero(mensaje)

mejor_ordenacion <- metropolis(mensaje_numerico, 100)
mensaje_a_letra(decodificador(mensaje_numerico, mejor_ordenacion))
