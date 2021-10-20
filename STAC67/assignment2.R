library(lawstat)
library(MASS)

A <- matrix(c(2,3,7,5,7,9,4,6,8), ncol=3, nrow=3)
B <- matrix(c(3,2,10,9,10,10,9,4,7), ncol=3, nrow=3)
y <- matrix(c(9,7,9), ncol=1, nrow=3)

Ay <- A%*%y
ypAy <- t(y)%*%Ay

data <- read.table("datatrans.txt", header=1)
x <- data[,1]
y <- data[,2]



bc <- boxcox(y~x)
lambda <- bc$x[which.max(bc$y)]
K2 <- prod(y)^(1/length(y))
yt <- (y^lambda-1)/(lambda*(K2^(lambda-1)))
new_fit <- lm(yt~x)

