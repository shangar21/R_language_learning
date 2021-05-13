x <- c(4.7,5.5,4.4,3.3,4.6,5.3,5.2,4.8,5.7,5.3)
mu <- mean(x)
print(mu)
sig <- 1/2
h_0 <- 5
z <- (mu - h_0)/sqrt((sig/length(x)))
print(z)
p_val <- 2*(1-pnorm(abs(z)))
print(p_val)
gamma <- 0.95
ci <- c(mu - (qnorm((1 + gamma)/2)*sqrt(sig/length(x))), mu + (qnorm((1 + gamma)/2)*sqrt(sig/length(x))))
print(ci)
