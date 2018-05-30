########################
#     Trabajo ADM      #
#  Tree-based methods  #
########################

#install.package("tree")
library(tree)

# Carga de datos
train <- read.csv("train.csv")
test <- read.csv("test.csv")

# Construccion de arbol
mortality <- tree(mortality_rate~.-Id, train)

summary(mortality)
plot(mortality)
text(mortality, pretty = TRUE)

# Reduccion del arbol
mortality.cv <- cv.tree(mortality)
plot.tree.sequence(mortality.cv)

# Vemos que no necesitamos reducir el arbol

# Prediccion
yhat <- predict(mortality, newdata=test)

# Grafico
plot(yhat, test$mortality_rate,
     xlab = "Predicción", ylab = "Real",
     xlim = c(0.5,3.0), ylim = c(0.5, 3.0))
abline(0, 1, lty = 2)
# Cuanto mas alejado de la recta, peor.

# Ra�z cuadrada del Error cuadrático medio
(mse <- sqrt(mean((yhat - test$mortality_rate)^2)))
(r2 <- cor(yhat, test$mortality_rate)^2)

# Vamos a usar RandomForest para mejorar
#install.packages("randomForest")
library(randomForest)
mortality.forest <- randomForest(mortality_rate~.-Id,
                                 data = train)
yhat.forest <- predict(mortality.forest, newdata=test)

(mse.forest <- sqrt(mean((yhat.forest - test$mortality_rate)^2)))
(r2.forest <- cor(yhat.forest, test$mortality_rate)^2)

# Vamos a usar boost para mejorar
#install.packages("gbm")
library(gbm)
mortality.boost <- gbm(mortality_rate~.-Id,
                       data=train,
                       distribution="gaussian",
                       n.trees=5000,
                       interaction.depth=4)
summary(mortality.boost)
yhat.boost <- predict(mortality.boost,
                      newdata = test,
                      n.trees = 5000)

(mse.boost <- sqrt(mean((yhat.boost - test$mortality_rate)^2)))
(r2.boost <- cor(yhat.boost, test$mortality_rate)^2)

# Comparamos los errores de los 3 métodos.
error <- matrix(c(mse, mse.forest, mse.boost,
                  r2, r2.forest, r2.boost), 
                nrow=3)
colnames(error) <- c("MSE", "R^2")
barplot(error,
        col = c("darkred","blue","lightblue"),
        legend = rownames(data),
        beside = TRUE)
legend("topleft",
       fill = c("darkred","blue","lightblue"),
       legend = c("Normal", "Forest", "Boost"))

# La mejora al usar RandomForest y Boost es bastante baja.
