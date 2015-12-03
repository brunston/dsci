#brupoon 2015, MIT license
x_1 <-- rnorm (1000,5,7) #1000 values with a mean of 5 and a stdev of 7
x_1_hist <- hist(x_1, col="grey")
true_error <- rnorm(1000,0,2)
true_beta_0 <- 1.1
true_beta_1 <- -8.2
y <- true_beta_0 + true_beta_1*x_1 + true_error #y = mx+b+true_error
plot_x_y <- plot(x_1, y, pch=20, col="red")
model <- lm(y ~ x_1)
