library(tree)

# Carga de datos
train <- read.csv("train.csv")
test <- read.csv("test.csv")

# Construccion de arbol
tree.mortality <- tree(mortality_rate~.-Id, train)

summary(tree.mortality)
plot(tree.mortality)
text(tree.mortality, pretty=0)

# Reduccion del arbol
cv.mortality <- cv.tree(tree.mortality)
plot(cv.mortality$size, cv.mortality$dev, type='b')
# Vemos que no necesitamos reducir el arbol

# Prediccion
yhat <- predict(tree.mortality, newdata=test)

# Grafico
plot(yhat, test$mortality_rate)
abline(0,1)
# Cuanto mas alejado de la recta, peor.

# Error cuadrático medio
(mse <- mean((yhat - test$mortality_rate)^2))

#install.packages("gbm")
library(gbm)
boost.mortality <- gbm(mortality_rate~.-Id,
                       data=train,
                       distribution="gaussian",
                       n.trees=5000,
                       interaction.depth=4)
summary(boost.mortality)
yhat.boost <- predict(boost.mortality,
                      newdata = test,
                      n.trees = 5000)
(mean((yhat.boost - test$mortality_rate)^2))
