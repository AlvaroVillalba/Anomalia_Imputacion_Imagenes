### Empleo de Componentes Principales para detección de anomalías.
### Calsificación Perros y Gatos

### Debe comentar este código indicando los pasos más importantes.

# Preámbulo
rm(list=ls())
graphics.off()
load("gatosperros.RData")


### Misma función de graficación:
plotcd <- function(v){
  x <- matrix(v,64,64)
  image(1:65,1:65,t(apply(x,2,rev)),asp=1,xaxt="n",yaxt="n",
        col=grey((0:255)/255),ann=FALSE,bty="n")
}

### Visualización vector enmascarado
plotcdm <- function(v, m){
  ina <- which(m==0)
  w <- v
  w[ina] <- NA
  x <- matrix(w,64,64)
  image(1:65,1:65,t(apply(x,2,rev)),asp=1,xaxt="n",yaxt="n",
        col=grey((0:255)/255),ann=FALSE,bty="n")
}


### Partición Conjuntos Entrenamiento y Prueba.
### ¡NO USAR!
### Use la partición que generó la última vez.

#set.seed(7931)
ind.gatostest <- sample(1:99, 9, replace = FALSE)
gatos.test <- dm[ind.gatostest, ]
gatos.train <- dm[1:99, ][-ind.gatostest, ]

ind.perrostest <- sample(1:99, 9, replace = FALSE)
perros.test <- dm[100:198, ][ind.perrostest, ]
perros.train <- dm[100:198, ][-ind.perrostest, ]


### Centrado
centro.gatos <- colMeans(gatos.train)

gatos.train <- gatos.train -
  matrix(centro.gatos,nrow(gatos.train),ncol(gatos.train),byrow=TRUE)
gatos.test <- gatos.test -
  matrix(centro.gatos,nrow(gatos.test),ncol(gatos.test),byrow=TRUE)
perros.train <- perros.train -
  matrix(centro.gatos,nrow(perros.train),ncol(perros.train),byrow=TRUE)
perros.test <- perros.test -
  matrix(centro.gatos,nrow(perros.test),ncol(perros.test),byrow=TRUE)


### Función de generación de máscaras
### 0 en la máscara indica que el dato se omite
### 1 en la máscara indica que el dato se conserva
get.mask <- function(d1,# Número de filas
                     d2,# Número de columnas
                     p# probabilidad de omitir un dato
){
  return(matrix(sample(c(0,1),d1*d2,prob=c(p,1-p),replace=TRUE), d1, d2))
}


### Reconstrucción de un vector a partir de una base completa conocida

### Generación de la base
pc <- prcomp(gatos.train)
matriz.proyec <- pc$rotation

### Máscara
### Verificar experimentalmente para qué rango
### de p el resultado del algoritmo es "bueno"

mask.gatos.test <- get.mask(nrow(gatos.test),
                            ncol(gatos.test),
                            p=0.9)


### Parámetro clave: cuántos vectores propios tomar.
### ¿Cuál es el dominio de este parámetro?
### ¿Qué pasa si este parámetro cambia?
n <- 90
matriz.proyec.n <- matriz.proyec[, 1:n]

### Reconstrucción
gatos.test.filled <- 0*gatos.test
for(igs in 1:nrow(gatos.test)){ # Para cada gato de prueba
  gato <- gatos.test[igs, , drop=FALSE]
  mask <- mask.gatos.test[igs, , drop=FALSE]
  i.mask <- (which(mask==1))
  g.mask <- gato[1, i.mask, drop=FALSE]
  u.mask <- matriz.proyec.n[i.mask, ]
  M <- t(u.mask)%*%u.mask
  f <- t(g.mask %*% u.mask)
  a <- solve(M,f)
  
  gatos.test.filled[igs,] <- t(matriz.proyec.n %*% a)
}


i.gtest <- sample(1:nrow(gatos.test),1)

x11()
par(pty="s")
layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))
plotcd(gatos.test[i.gtest,])
title(xlab="Imagen original")
plotcdm(gatos.test[i.gtest,], mask.gatos.test[i.gtest,])
title(xlab="Imagen con datos faltantes")
plotcd(gatos.test.filled[i.gtest,])
title(xlab=paste("Imagen reconstruída con ", n, " componentes",sep=""))

gatos.train.proyec <- (gatos.train %*% matriz.proyec) %*% t(matriz.proyec)
gatos.train.resid <- gatos.train - gatos.train.proyec
mean(gatos.train.resid)

#****************************************************************************
#****************************************************************************

### Reconstruir los perros de prueba a partir de gatos de entrenamiento
### y analizar los resultados.

### Máscara
### Verificar experimentalmente para qué rango
### de p el resultado del algoritmo es "bueno"

mask.perros.test <- get.mask(nrow(perros.test),
                            ncol(perros.test),
                            p=0.2)


### Parámetro clave: cuántos vectores propios tomar.
### ¿Cuál es el dominio de este parámetro?
### ¿Qué pasa si este parámetro cambia?
n <- 75
matriz.proyec.n <- matriz.proyec[, 1:n]

### Reconstrucción
perros.test.filled <- 0*perros.test
for(igs in 1:nrow(perros.test)){ # Para cada gato de prueba
  perro <- perros.test[igs, , drop=FALSE]
  mask <- mask.perros.test[igs, , drop=FALSE]
  i.mask <- (which(mask==1))
  p.mask <- perro[1, i.mask, drop=FALSE]
  u.mask <- matriz.proyec.n[i.mask, ]
  M <- t(u.mask)%*%u.mask
  f <- t(p.mask %*% u.mask)
  a <- solve(M,f)
  
  perros.test.filled[igs,] <- t(matriz.proyec.n %*% a)
}


i.gtest <- sample(1:nrow(perros.test),1)

x11()
par(pty="s")
layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))
plotcd(perros.test[i.gtest,])
title(xlab="Imagen original")
plotcdm(perros.test[i.gtest,], mask.perros.test[i.gtest,])
title(xlab="Imagen con datos faltantes")
plotcd(perros.test.filled[i.gtest,])
title(xlab=paste("Imagen reconstruída con ", n, " componentes",sep=""))

#************************************************************
#************************************************************

p <- 0
n <- 0
mean_resid <- 0

pp <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
nn <- c(10, 20, 30, 40, 50, 60, 70, 80, 90)

datos <- data.frame(p, n, mean_resid)

for(pe in pp){

mask.gatos.test <- get.mask(nrow(gatos.test),
                            ncol(gatos.test),
                            p=pe)

 for(ne in nn){
### Parámetro clave: cuántos vectores propios tomar.
### ¿Cuál es el dominio de este parámetro?
### ¿Qué pasa si este parámetro cambia?
n <- ne
matriz.proyec.n <- matriz.proyec[, 1:n]

### Reconstrucción
gatos.test.filled <- 0*gatos.test
for(igs in 1:nrow(gatos.test)){ # Para cada gato de prueba
  gato <- gatos.test[igs, , drop=FALSE]
  mask <- mask.gatos.test[igs, , drop=FALSE]
  i.mask <- (which(mask==1))
  g.mask <- gato[1, i.mask, drop=FALSE]
  u.mask <- matriz.proyec.n[i.mask, ]
  M <- t(u.mask)%*%u.mask
  f <- t(g.mask %*% u.mask)
  a <- solve(M,f)
  
  gatos.test.filled[igs,] <- t(matriz.proyec.n %*% a)
}


i.gtest <- sample(1:nrow(gatos.test),1)


par(pty="s")
layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))
plotcd(gatos.test[i.gtest,])
title(xlab="Imagen original")
plotcdm(gatos.test[i.gtest,], mask.gatos.test[i.gtest,])
title(xlab="Imagen con datos faltantes")
plotcd(gatos.test.filled[i.gtest,])
title(xlab=paste("Imagen reconstruída con ", n, " componentes",sep=""))

gatos.train.proyec <- (gatos.train %*% matriz.proyec.n) %*% t(matriz.proyec.n)
gatos.train.resid <- gatos.train - gatos.train.proyec
mean_resid <- mean(gatos.train.resid)
z <- c(pe, ne, mean_resid)
datos <- rbind(datos, z)

}
}
