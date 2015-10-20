# Cargamos el archivo "birdextinct.txt"

birdextinct <- read.csv(file.choose(),header = TRUE, sep = "\t")

# Definimos las variables
time <- birdextinct$time
nesting <- birdextinct$nesting
size <- birdextinct$size
status <- birdextinct$status

# Calculamos el modelo lineal
linearmodel1 <- lm(log(time) ~ nesting + size + status)
summary(linearmodel1)$r.squared