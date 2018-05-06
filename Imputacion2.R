#graphics.off()

### Máscara
mask.gatos.train <- get.mask(nrow(gatos.train),
                            ncol(gatos.train),
                            p=0.1)

i.gtrain <- sample(1:nrow(gatos.train),1)

par(pty="s",mar=rep(2,4))
layout(matrix(1:25,5,5,byrow=TRUE))
plotcd(gatos.train[i.gtrain,])
title(main="Imagen original")

gatos.train.masked <- gatos.train
i.gap <- which(mask.gatos.train==0)
gatos.train.masked[i.gap] <- NA
X <- gatos.train.masked

plotcd(gatos.train.masked[i.gtrain,])
title(main="Imagen 'Gappy'")

### Iteración 0: Llenar los faltantes con el promedio de cada pixel
X_0 <- matrix(colMeans(gatos.train.masked, na.rm = TRUE),
              nrow(gatos.train), ncol(gatos.train), byrow=TRUE)

X[i.gap] <- X_0[i.gap]
plotcd(X[i.gtrain,])
title(main="Iteración 0")

err <- norm(gatos.train[i.gtrain,]-X[i.gtrain, ],"2")
print(paste("Iteración 0:", err, sep=" "))

### Parámetro clave: cuántos vectores propios tomar.
### ¿Cuál es el dominio de este parámetro?
### ¿Qué pasa si este parámetro cambia?
n <- 25


### Lazo: 20 iteraciones
X_i <- 0*gatos.train
for(iter in 1:20){
  ### Generación de la base
  pc <- prcomp(X)
  matriz.proyec.n <- pc$rotation[, 1:n]
  for(igs in 1:nrow(gatos.train)){
    gato <- X[igs, , drop=FALSE]
    mask <- mask.gatos.train[igs, , drop=FALSE]
    i.mask <- (which(mask==1))
    g.mask <- gato[1, i.mask, drop=FALSE]
    u.mask <- matriz.proyec.n[i.mask, ]
    M <- t(u.mask)%*%u.mask
    f <- t(g.mask %*% u.mask)
    a <- solve(M,f)
    X_i[igs, ] <- t(matriz.proyec.n %*% a)
  }
  X[i.gap] <- X_i[i.gap]
  
  plotcd(X[i.gtrain,])
  title(main=paste("Iteración", iter, sep=" "))
  
  err <- norm(gatos.train[i.gtrain,]-X[i.gtrain, ],"2")
  print(paste("Iteración ", iter, ": ", err, sep=""))
}


### El libro de Kirby sugiere dos mejores condiciones de parada para este
### algoritmo. Implemente una.


# Perceptron
## Mejor Perceptron

# knn
## Frontera clasificación para Peso edad ~ est
## Frontera clasificación para Peso est ~ edad
## Frontera clasificación para Edad est ~ Peso

# PCA 
## Gatos Perros en R2
## Knn Gatos Perros en R2

# Anomalias
## Clasificadors Base Perros

# Imputación

# MDS


