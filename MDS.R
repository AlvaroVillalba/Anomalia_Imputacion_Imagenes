### Empleo de MDS para encontrar coordenadas
### de ciudades en Colombia a partir de tabla de
### distancias


# Preámbulo
rm(list=ls())
graphics.off()


### Datos distancias
### ¡NO USE ESTAS!
### Seleccione 10 ciudades al azar y tome los datos de 
### http://es.distancias.himmera.com/distancia_entre_ciudades-colombia_mapas_carreteras/
### Compare la reconstrucció para distancias por carretera y en línea recta (avión)
ciudades <- c("Bogotá", "Medellín", "Barranquilla", "Cali", "Bucaramanga")
D <- matrix(0,length(ciudades),length(ciudades))
D[1, 2:5] <- c(552, 1302, 484, 439)
D[2, 3:5] <- c(750, 462, 543)#
D[3, 4:5] <- c(1212, 739)
D[4, 5:5] <- c(923)

D <- D+t(D)
colnames(D) <- ciudades
rownames(D) <- ciudades


### Multi-Dimensional Scaling
n <- length(ciudades)
uno <- matrix(1,n,1)
I <- diag(n)

J <- I - (uno%*%t(uno))/n

B <- - J%*%D^2%*%J/2

eigen.B <- eigen(B, symmetric=TRUE)

E <- eigen.B$vectors[,1:2]
L <- matrix(0,2,2)
diag(L) <- eigen.B$values[1:2]

X <- E%*%sqrt(L)
rownames(X) <- ciudades

x11()
mm <- max(abs(X))
plot(-X[,2], -X[,1], pch=16, ann = FALSE, bty="n",
     xaxt="n",yaxt="n",
     ylim=c(-mm,mm)*1.2, xlim=c(-mm,mm)*1.2, asp=1)
text(-X[,2], -X[,1], ciudades, pos=3)



#******************************************************************
#***********************************
#¨¨¨¨¨¨¨¨DISTANCIA CARRETERA ***********

ciudades <- c("Bogotá", "Cartagena", "Villeta", "Barbosa", "Yopal", "San Gil")
D <- matrix(0,length(ciudades),length(ciudades))
D[1, 2:6] <- c(1060, 90, 185, 356, 300)
D[2, 3:6] <- c(994, 860, 1133, 751)
D[3, 4:6] <- c(260, 435, 375)
D[4, 5:6] <- c(275, 115)
D[5, 5:6] <- c(381)

D <- D+t(D)
colnames(D) <- ciudades
rownames(D) <- ciudades


### Multi-Dimensional Scaling
n <- length(ciudades)
uno <- matrix(1,n,1)
I <- diag(n)

J <- I - (uno%*%t(uno))/n

B <- - J%*%D^2%*%J/2

eigen.B <- eigen(B, symmetric=TRUE)

E <- eigen.B$vectors[,1:2]
L <- matrix(0,2,2)
diag(L) <- eigen.B$values[1:2]

X <- E%*%sqrt(L)
rownames(X) <- ciudades

mm <- max(abs(X))
plot(X[,2], -X[,1], pch=16, ann = FALSE, bty="n",
     xaxt="n",yaxt="n",
     ylim=c(-mm,mm)*1.2, xlim=c(-mm,mm)*1.2, asp=1)
text(X[,2], -X[,1], ciudades, pos=3)

library(ggplot2)
library(devtools)
library(ggwithimages)
colombia <- png::readPNG("colombia.png")
ggplot(BOD, aes(x=Time, y=demand)) + geom_line_with_image(dolar)

data <- as.data.frame(X)

ggplot(data, aes(800,800)) + geom_line_with_image(colombia) + 
  geom_point(aes(V2,-V1)) + xlim(-800,1000) + ylim(-1600,1100) +
  geom_label(aes(V2,-V1,label=rownames(data)), size=2)

text(X[,2], -X[,1], ciudades, pos=3)

-74.1152  -65.87952

4.6035  -167.74609

data$X <- data$V2*(-74.1152) / -65.87952
data$Y <- -data$V1*(4.6035) / -167.74609
data$ciudad <- rownames(data)
#***********************************
#¨¨¨¨¨¨¨¨DISTANCIA LINEA ***********

ciudades <- c("Bogotá", "Cartagena", "Villeta", "Barbosa", "Yopal", "San Gil")
D <- matrix(0,length(ciudades),length(ciudades))
D[1, 2:6] <- c(650, 55, 145, 198, 230)
D[2, 3:6] <- c(609, 536, 635, 515)
D[3, 4:6] <- c(139, 232, 226)
D[4, 5:6] <- c(149, 87)
D[5, 5:6] <- c(156)

D <- D+t(D)
colnames(D) <- ciudades
rownames(D) <- ciudades


### Multi-Dimensional Scaling
n <- length(ciudades)
uno <- matrix(1,n,1)
I <- diag(n)

J <- I - (uno%*%t(uno))/n

B <- - J%*%D^2%*%J/2

eigen.B <- eigen(B, symmetric=TRUE)

E <- eigen.B$vectors[,1:2]
L <- matrix(0,2,2)
diag(L) <- eigen.B$values[1:2]

X <- E%*%sqrt(L)
rownames(X) <- ciudades

x11()
mm <- max(abs(X))
plot(X[,2], -X[,1], pch=16, ann = FALSE, bty="n",
     xaxt="n",yaxt="n",
     ylim=c(-mm,mm)*1.2, xlim=c(-mm,mm)*1.2, asp=1)
text(X[,2], -X[,1], ciudades, pos=3)

colMeans(X)

# Aplicar cluster para separar gatos y perros
# Aplciar para separar razas


data <- as.data.frame(X)

ggplot(data, aes(800,800)) + geom_line_with_image(colombia) + 
  geom_point(aes(V2,-V1)) + xlim(-800,900) + ylim(-1500,1100) +
  geom_label(aes(V2,-V1,label=rownames(data)), size=2)
