x <- c(90, 86, 67, 89, 81, 75, 85, 70, 81, 77)
y <- c(57.7, 56.2, 41.6, 58.5, 53.2, 48.2, 56.3, 47.1, 53.1, 52.5)

xbar <- mean(x)
ybar <- mean(y)

sx <- sum(x - xbar)/(length(x)-1)
sy <- sum(y - ybar)/(length(y)-1)

sxx = sum(x^2) - length(x)*(mean(x)^2)
sxy = sum((x - xbar)*(y - ybar))
syy = sum(y^2) - length(y)*(mean(y)^2)

b1 = sxy/sxx
b0 = ybar - b1*xbar

fit <- lm(y ~x)

summary(fit)
anova(fit)
