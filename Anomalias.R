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


### Partición Conjuntos Entrenamiento y Prueba.
### ¡NO USAR!
### Use la partición que generó la última vez.

set.seed(7931)
ind.gatostest <- sample(1:99, 9, replace = FALSE)
gatos.test <- dm[ind.gatostest, ]
gatos.train <- dm[1:99, ][-ind.gatostest, ]

ind.perrostest <- sample(1:99, 9, replace = FALSE)
perros.test <- dm[100:198, ][ind.perrostest, ]
perros.train <- dm[100:198, ][-ind.perrostest, ]


### Centrado
### ¿Por qué se usa el promedio de los gatos para centrar los perros?

# Poder aplicar componentes principales, también se tiene el gato promedio o del origen 

### Grafique el gato promedio
### ¿Qué pasa si no se centran los datos de entrenamiento?
### Verifíquelo experimentalmente

centro.gatos <- colMeans(gatos.train)
plotcd(round(centro.gatos))

gatos.train <- gatos.train - matrix(centro.gatos,nrow(gatos.train),ncol(gatos.train),byrow=TRUE)
gatos.test <- gatos.test - matrix(centro.gatos,nrow(gatos.test),ncol(gatos.test),byrow=TRUE)
perros.train <- perros.train - matrix(centro.gatos,nrow(perros.train),ncol(perros.train),byrow=TRUE)
perros.test <- perros.test - matrix(centro.gatos,nrow(perros.test),ncol(perros.test),byrow=TRUE)


### Generación de la base
pc <- prcomp(gatos.train)
matriz.proyec <- pc$rotation
plot(cumsum(pc$sdev^2)/sum(pc$sdev^2), xlab = "Componentes", ylab = "Varianza Acumulada", main = "% Acum Varianza")
abline(h=0.5, col="red")
abline(h=0.8, col="blue")
abline(h=0.9, col="green")
abline(h=0.95, col="yellow")
abline(h=0.99, col="pink")
### Visualización de _____
x11()
plot(cumsum(pc$sdev^2)/sum(pc$sdev^2),type="l",ylim=c(0,1),
     ylab="Varianza acumulada normalizada", xlab="# Componentes principales")
abline(h=0.5, col="red")
abline(h=0.8, col="blue")
abline(h=0.9, col="green")
abline(h=0.95, col="yellow")
abline(h=0.99, col="pink")
### ¿Para cuántas componentes se conserva el {50, 80, 90, 95, 99}% de la varianza?


### Proyección de gatos de entrenamiento (Residuo 0)
### Explique qué ocurre en cada uno de estos pasos
# Pasar los datos de entrenamiento a dos dimensiones
gatos.train.proyec <- (gatos.train %*% matriz.proyec) %*% t(matriz.proyec)
# Tener el residuo
gatos.train.resid <- gatos.train - gatos.train.proyec

# El residuo es cero para los datos de entrenamiento porque tenemos los datos
x11(15,7)
par(pty="s")
layout(matrix(1:3,1,3))
ind <- sample(1:nrow(gatos.train),1)
plotcd(round(gatos.train[ind,]))
title(xlab = "Imagen Original")
plotcd(round(gatos.train.proyec[ind,]))
title(xlab="Proyección Base Gatos Entrenamiento")
plotcd(round(gatos.train.resid[ind,]))
title(xlab = "Residuo proyección")

gatos.train.resid.norm.compall <- apply(gatos.train.resid, 1, norm, type="2")


### Proyección de gatos de prueba
gatos.test.proyec <- (gatos.test %*% matriz.proyec) %*% t(matriz.proyec)
gatos.test.resid <- gatos.test - gatos.test.proyec

x11(15,7)
par(pty="s")
layout(matrix(1:3,1,3))
ind <- sample(1:nrow(gatos.test),1)
plotcd(round(gatos.test[ind,]))
title(xlab = "Imagen Original")
plotcd(round(gatos.test.proyec[ind,]))
title(xlab="Proyección Base Gatos Entrenamiento")
plotcd(round(gatos.test.resid[ind,]))
title(xlab = "Residuo proyección")

gatos.test.resid.norm.compall <- apply(gatos.test.resid, 1, norm, type="2")

### Proyección de perros de entrenamiento
perros.train.proyec <- (perros.train %*% matriz.proyec) %*% t(matriz.proyec)
perros.train.resid <- perros.train - perros.train.proyec

x11(15,7)
par(pty="s")
layout(matrix(1:3,1,3))
ind <- sample(1:nrow(perros.train),1)
plotcd(round(perros.train[ind,]))
title(xlab = "Imagen Original")
plotcd(round(perros.train.proyec[ind,]))
title(xlab="Proyección Base Gatos Entrenamiento")
plotcd(round(perros.train.resid[ind,]))
title(xlab = "Residuo proyección")

perros.train.resid.norm.compall <- apply(perros.train.resid, 1, norm, type="2")

### Interprete las gráficas.
### Compare los valores promedio de gatos.train.resid.norm, gatos.test.resid.norm y
### perros.train.resid.norm

# Para los de entrenamiento los residuos son minimos, para los gatos de test es buena y para los perros es 
# muy alto
mean(gatos.train.resid.norm.compall)
mean(gatos.test.resid.norm.compall)
mean(perros.train.resid.norm.compall)

#*************************************************************************************
#*************************************************************************************


### Evalúe el desempeño del método si no se conservan todas las componentes principales


gatos.train.proyec <- (gatos.train %*% matriz.proyec[,c(1,15)]) %*% t(matriz.proyec[,c(1,15)])
# Tener el residuo
gatos.train.resid <- gatos.train - gatos.train.proyec

# El residuo es cero para los datos de entrenamiento porque tenemos los datos
x11(15,7)
par(pty="s")
layout(matrix(1:3,1,3))
ind <- sample(1:nrow(gatos.train),1)
plotcd(round(gatos.train[ind,]))
title(xlab = "Imagen Original")
plotcd(round(gatos.train.proyec[ind,]))
title(xlab="Proyección Base Gatos Entrenamiento")
plotcd(round(gatos.train.resid[ind,]))
title(xlab = "Residuo proyección")

gatos.train.resid.norm.comp15 <- apply(gatos.train.resid, 1, norm, type="2")


### Proyección de gatos de prueba
gatos.test.proyec <- (gatos.test %*% matriz.proyec[,c(1,15)]) %*% t(matriz.proyec[,c(1,15)])
gatos.test.resid <- gatos.test - gatos.test.proyec

x11(15,7)
par(pty="s")
layout(matrix(1:3,1,3))
ind <- sample(1:nrow(gatos.test),1)
plotcd(round(gatos.test[ind,]))
title(xlab = "Imagen Original")
plotcd(round(gatos.test.proyec[ind,]))
title(xlab="Proyección Base Gatos Entrenamiento")
plotcd(round(gatos.test.resid[ind,]))
title(xlab = "Residuo proyección")

gatos.test.resid.norm.comp15 <- apply(gatos.test.resid, 1, norm, type="2")

### Proyección de perros de entrenamiento
perros.train.proyec <- (perros.train %*% matriz.proyec[,c(1,15)]) %*% t(matriz.proyec[,c(1,15)])
perros.train.resid <- perros.train - perros.train.proyec

x11(15,7)
par(pty="s")
layout(matrix(1:3,1,3))
ind <- sample(1:nrow(perros.train),1)
plotcd(round(perros.train[ind,]))
title(xlab = "Imagen Original")
plotcd(round(perros.train.proyec[ind,]))
title(xlab="Proyección Base Gatos Entrenamiento")
plotcd(round(perros.train.resid[ind,]))
title(xlab = "Residuo proyección")

perros.train.resid.norm.comp15 <- apply(perros.train.resid, 1, norm, type="2")

mean(gatos.train.resid.norm.compall); mean(gatos.train.resid.norm.comp15)
mean(gatos.test.resid.norm.compall); mean(gatos.test.resid.norm.comp15)
mean(perros.train.resid.norm.compall); mean(perros.train.resid.norm.comp15)

### Con base en los resultados anteriores, proponga un algoritmo de clasificación
### de perros y gatos y entrénelo usando los perros de entrenamiento.

data <- data.frame("animal" = c(rep(1,99), rep(0,90)), residuos_comp_all = c(gatos.train.resid.norm.compall, gatos.test.resid.norm.compall, perros.train.resid.norm.compall), residuos_com_15 = c(gatos.train.resid.norm.comp15, gatos.test.resid.norm.comp15, perros.train.resid.norm.comp15))

data$animal_nombre <- ifelse(data$animal==1, "Gato", "Perro")
data$id <- 1:length(data$animal)
data$type <- c(rep(1,90),rep(2,9),rep(3,90))
data$type_bd <- ifelse(data$type==1, "Gato Train", ifelse(data$type==2, "Gato Test", "Perro Train"))

names(data)
library(ggplot2)
ggplot(data, aes(id, residuos_comp_all)) + geom_point(aes(colour=animal_nombre))
ggplot(data, aes(id, residuos_comp_all)) + geom_point(aes(colour=type_bd)) 
ggplot(data, aes(id, residuos_com_15)) + geom_point(aes(colour=type_bd)) 

### Reporte los errores de clasificación de entrenamiento y prueba de su algoritmo

data$limite <- ifelse(data$residuos_comp_all>2250,0,1)

data$error <- ifelse(data$animal==data$limite, 0, 1)

sum(data$error) / length(data$animal)


data$limite_15 <- ifelse(data$residuos_com_15>2250,0,1)

data$error_15 <- ifelse(data$animal==data$limite_15, 0, 1)

sum(data$error_15) / length(data$animal)

head(data)
datos <- data.frame(p, n, mean_resid)

