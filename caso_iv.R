# Cargamos el archivo "BHD.txt"

bhd <- read.csv(file.choose(),header = FALSE, sep = "\t")

names(bhd) <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", 
  "DIS", "RAD", "TAX", "PTRATIO", "B", "LSTAT", "MEDV")

# Cargamos la librería LearnBayes
library(LearnBayes)

# Observamos los primeros datos del archivo
head(bhd)

# Vinculamos los nombres de las variables a los datos
attach(bhd)

# Resumen de cada una de las variables
summary(bhd)

# Hacemos un histograma de cada variable
par(mfrow=c(2,7))
for(i in 1:length(bhd)){
  hist(bhd[,i], xlab = names(bhd[i]), ylab = "Frecuencia", 
    main = paste("Histograma de ", names(bhd[i])))
}

# Cargamos la librería fBasics
library(fBasics)

for(i in 1:length(bhd)){
  cat(paste("La asimetría de", names(bhd[i]) , "es", skewness(bhd[,i]), "\n"))
}

# Aplicamos logaritmos a algunas variables
LOGCRIM <- log(CRIM)
LOGZN <- log(ZN) # Error, contiene ceros
LOGCHAS <- log(CHAS) # Error, contiene ceros
LOGB <- log(B)

# Resolvemos el primer modelo de regresión lineal
model1 <- lm(MEDV ~ CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + 
  RAD + TAX + PTRATIO + B + LSTAT, data = bhd, x = TRUE, y = TRUE)

# Resumen del modelo
summary(model1)

# Aplicamos logaritmos a MEDV
LOGMEDV <- log(MEDV)

# Resolvemos el modelo definitivo de regresión lineal
model2 <- lm(LOGMEDV ~ CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + 
  RAD + TAX + PTRATIO + B + LSTAT , data = bhd, x = TRUE, y = TRUE)
summary(model2)

# Representamos en un diagrama de dispersión cada una de las 
# cuatro variables más influyentes
par(mfrow=c(2,2))
plot(LSTAT, LOGMEDV)
plot(CRIM, LOGMEDV)
plot(PTRATIO, LOGMEDV)
plot(DIS, LOGMEDV)

# Calculamos la distribución a posteriori de beta y sigma
theta.sample <- blinreg(model2$y, model2$x, 5000)

S <- sum(model2$residuals^2)
shape <- model2$df.residual/2
rate <- S/2
sigma2 <- rigamma(1, shape, rate)

MSE <- sum(model2$residuals^2)/model2$df.residual
vbeta <- vcov(model2)/MSE
beta <- rmnorm(1, mean = model2$coef, varcov= vbeta*sigma2)

# Historgrama para los parámetros beta y sigma
par(mfrow=c(2,7))
for(i in 1:(length(bhd)-1)){
  hist(theta.sample$beta[,i+1], xlab = paste("beta", i), 
    ylab = "Frecuencia", main = names(bhd)[i])
}
hist(theta.sample$sigma, xlab = expression(sigma), 
  ylab = "Frecuencia", main = "ERROR")

# Calculamos los percentiles 0.05, 0.5 y 0.95 de la distribución 
# a posteriori
apply(theta.sample$beta, 2, quantile, c(0.05, 0.5, 0.95))
quantile(theta.sample$sigma, c(0.05, 0.5, 0.95))

# Calculamos los residuos
prob.out <- bayesresiduals(model2, theta.sample, 2)

# Creamos las covariables
aux <- c(1,unname(apply(bhd, 2, FUN = median)))
aux <- aux[-15]
aux<-aux[-5]

aux[14]<-0
cov1 <- aux

aux[14]<-1
cov2 <- aux


X1 <- rbind(cov1, cov2)
mean.draws=blinregexpected(X1, theta.sample)

# Representamos los histogramas
par(mfrow=c(1,2))
hist(mean.draws[,1], main="Covarianza de A", xlab = "LOGMEDV", 
  ylab = "Frecuencia")
hist(mean.draws[,2], main="Covarianza de B", xlab = "LOGMEDV", 
  ylab = "Frecuencia")

# Predecimos valores a partir de las covariables
mean.draws=blinregpred(X1, theta.sample)

# Representamos los histogramas
par(mfrow=c(1,2))
hist(mean.draws[,1], main="Covarianza de A", xlab = "LOGMEDV", 
  ylab = "Frecuencia")
hist(mean.draws[,2], main="Covarianza de B", xlab = "LOGMEDV", 
  ylab = "Frecuencia")



# Número de outliers con probabilidad mayor que 0.4
n=sum(prob.out>=.4)

# Representamos los outliers con respecto a la variable LSTAT
par(mfrow=c(1,1))
plot(LSTAT, prob.out)
identify(LSTAT, prob.out, label=MEDV, n=sum(prob.out>=.4))