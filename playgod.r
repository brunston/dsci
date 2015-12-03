#brupoon 2015, MIT license
#install.packages("caTools")
require(caTools)
x_1 <- rnorm(1000,5,7) #1000 values with a mean of 5 and a stdev of 7
x_1_hist <- hist(x_1, col="grey")
true_error <- rnorm(1000,0,2)
true_beta_0 <- 1.1
true_beta_1 <- -8.2
y_1 <- true_beta_0 + true_beta_1*x_1 + true_error #y = mx+b+true_error
plot_x_1_y_1 <- plot(x_1, y_1, pch=20, col="red")

model <- lm(y_1 ~ x_1)
x_2 <- rgamma(1000, 1)
y_2 <- 3 * x_1 + 2.3 * x_2
model_y2x1 <- lm(y_2 ~ x_1) #model based on x_1
model_y2x2 <- lm(y_2 ~ x_2) #model based on x_2
model_y2xx <- lm(y_2 ~ x_1 + x_2) #model based on x_1 and x_2

#using caTools to split data subset
#We want to use mean squared error so need to split the data set
#define mean squared error function
mse <- function(model) {
    mse <- mean(summary(model)$residuals^2)
    return(mse)
}

y_2_sample <- sample.split(y_2, SplitRatio = .8)
y_2_train <- subset(y_2, y_2_sample==TRUE)
y_2_test <- subset(y_2, y_2_sample==FALSE)
model_y2xx_train <- lm(y_2_train ~ x_1 + x_2)
mse_test <- mse(model_y2xx_train)
