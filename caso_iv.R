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

# Hacemos algunos gráficos de las variables
# par(mfrow=c(2,7))
for(i in length(bhd)){
  hist(bhd[,i], xlab = names(bhd[,i]), ylab = "Frecuencia", main = "Histograma de ", names(bhd[,i]))
}


model <- lm(MEDV ~. , data = bhd, x = TRUE, y = TRUE)
summary(model)

theta.sample <- blinreg(model$y, model$x, 5000)

S <- sum(model$residuals^2)
shape <- model$df.residual/2
rate <- S/2
sigma2 <- rigamma(1, shape, rate)

MSE <- sum(model$residuals^2)/model$df.residual
vbeta <- vcov(model)/MSE
beta <- rmnorm(1, mean = model$coef, varcov= vbeta*sigma2)

apply(theta.sample$beta, 2, quantile, c(0.05, 0.5, 0.95))

quantile(theta.sample$sigma, c(0.05, 0.5, 0.95))

prob.out <- bayesresiduals(model, theta.sample, 2)
