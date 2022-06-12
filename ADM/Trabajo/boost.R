library(gbm)

# Carga de datos
train <- read.csv("train.csv")
test <- read.csv("test.csv")

nums = seq(1, 2501, by=50)
mse = vector("list", length(nums))

mortality.boost <- gbm(mortality_rate~.-Id,
                       data=train,
                       distribution="gaussian",
                       n.trees=2501,
                       interaction.depth=4,
                       shrinkage = 0.1)

for (i in 1:length(nums)) {
  yhat.boost <- predict(mortality.boost,
                        newdata = test,
                        n.trees = nums[i])
  mse[[i]] <- sqrt(mean((yhat.boost - test$mortality_rate)^2))
}

plot(nums, mse, 
     xlab = "Número de árboles",
     ylab = "Error cuadrático medio",
     type="l", col="red")

mortality.boost <- gbm(mortality_rate~.-Id,
                       data=train,
                       distribution="gaussian",
                       n.trees=2501,
                       interaction.depth=4,
                       shrinkage = 0.05)

for (i in 1:length(nums)) {
  yhat.boost <- predict(mortality.boost,
                        newdata = test,
                        n.trees = nums[i])
  mse[[i]] <- sqrt(mean((yhat.boost - test$mortality_rate)^2))
}

lines(nums,mse, col="green")

mortality.boost <- gbm(mortality_rate~.-Id,
                       data=train,
                       distribution="gaussian",
                       n.trees=2501,
                       interaction.depth=4,
                       shrinkage = 0.01)

for (i in 1:length(nums)) {
  yhat.boost <- predict(mortality.boost,
                        newdata = test,
                        n.trees = nums[i])
  mse[[i]] <- sqrt(mean((yhat.boost - test$mortality_rate)^2))
}

lines(nums, mse, col="blue")

lambda <- "lambda"
legend("topright",
       fill = c("red","green","blue"),
       legend = c(paste(lambda, "= 0.1"),
                  paste(lambda, "= 0.05"),
                  paste(lambda, "= 0.01")))

