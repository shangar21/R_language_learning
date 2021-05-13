# Set Directory of Current File
setwd("~/Dropbox/UofT Admin and TA/STAC53/Lectures Files/Pause/Lecture 4/Intro_to_R/data")

# Load and Install Packages
installer <- function(vector) {
  for(i in vector) {
    if(!require(i, character.only = T)) install.packages(i)
    library(i, character.only = T)
  }
}
installer(c("knitr", "kableExtra", "RColorBrewer", "scales", "mgcv"))

library(knitr)
library(kableExtra)
library(RColorBrewer)
library(scales)
library(mgcv)

# Data Types
str(1/7)
str(3+2i)
str(30==30)
str("3.14")

# Mathematical Operations
1+5-2
4*3/2
25^(1/2)
sqrt(25)
6 %% 4
6 %/% 4

3.14+1
"3.14" + 1
TRUE + 3

# Data Structures

# Vector
c(1,FALSE,0.5)
x <- c(1,2,3,4,5, 5, 5)
length(x)

# Matrix
matrix(
  c(1:9),
  nrow=3, ncol=3,
  byrow=F
)

### Cbind and rnbind ##
x <- 1:3
y <- 4:6
z <- c(7,8,"a")
xyz <- rbind(x,y,z)
xyz <- cbind(x,y,z)

# Data Frame
df <- data.frame(
  ALPHABET=letters[1:4],
  INTEGER=c(1:4),
  SQRT=sqrt(c(1:4)),
  GUESSME=c(TRUE,1.2,'0',NA)
)

# List
list.w.vector <- list(
  1.5,TRUE,'a',a=c(1:5))

# Functions

# Built-In
1:4
sum(1:4)
cumsum(1:4)
intersect(1:5, 4:10)
ifelse(1:4 > 2, "Yes", "No")

# User-Defined Functions
logit <- function(p) {log(p/(1 - p))}
logit(c(0.3, 0.5, 0.8))
sapply(c(0.3, 0.5, 0.8), logit)

### For Loop ##
x <- c(0.3, 0.5, 0.8)

for(i in 1:length(x)){
  print(x[i])
}

?intersect

### Generate random numbers ###
# First fix a seed #
set.seed(1002656486)
n <- 10
rnorm(n)
rnorm(n, mean = 1, sd = 2)
rbinom(n, size = 1, prob = 0.5)
rbinom(n, size = 10, prob = 0.5)

## Wirte a function for Exponential

clt.exp <- function(N, seed, sim, rate){
  set.seed(seed)
  Z <- vector()
  for(i in 1:sim){
    X <- rexp(N, rate)
    avg.exp <- sum(X)/N
    Z[i] <- sqrt(N) * ( (avg.exp - (1/rate))/ (sqrt(1/(rate)^2)) )
  }
  hist(Z, breaks = 30, xlab = paste("for rate = ",rate, " and N = ", N,  sep = "" ))
  #  plot(density(Z), main = "Histogram of Z", 
  #      xlab = paste("for rate = ",rate, " and N = ", N,  sep = "" ))
}

pdf("exp.pdf")
par(mfrow = c(3,2), family = 'serif')
clt.exp(N = 5, seed = 123, sim = 1000, rate = 1)
clt.exp(N = 10, seed = 123, sim = 1000, rate = 1)
clt.exp(N = 50, seed = 123, sim = 1000, rate = 1)
clt.exp(N = 100, seed = 123, sim = 1000, rate = 1)
clt.exp(N = 500, seed = 123, sim = 1000, rate = 1)
clt.exp(N = 1000, seed = 123, sim = 1000, rate = 1)
dev.off()

## For Gamma Distribution ##

clt.gam <- function(N, seed, sim, al, bet){
  set.seed(seed)
  Z <- vector()
  for(i in 1:sim){
    X <- rgamma(N, al, bet)
    avg.exp <- sum(X)/N
    Z[i] <- sqrt(N) * ( (avg.exp - (al/bet))/ (sqrt(al/(bet)^2)) )
  }
  hist(Z, breaks = 30, xlab = paste("for alpha = ",al,
                                    " beta= " ,bet,   " and N = ", N,  sep = "" ))
  #  plot(density(Z), main = "Histogram of Z", 
  #      xlab = paste("for rate = ",rate, " and N = ", N,  sep = "" ))
}

#pdf('gamma.pdf')
par(mfrow = c(3,2), family = 'serif')
clt.gam(N = 5, seed = 123, sim = 1000, al = 2, bet = 2)
clt.gam(N = 10, seed = 123, sim = 1000, al = 2, bet = 2)
clt.gam(N = 50, seed = 123, sim = 1000, al = 2, bet = 2)
clt.gam(N = 100, seed = 123, sim = 1000, al = 2, bet = 2)
clt.gam(N = 500, seed = 123, sim = 1000, al = 2, bet = 21)
clt.gam(N = 1000, seed = 123, sim = 1000, al = 2, bet = 2)
#dev.off()

### Calculating probabilities from quantiles ###

## Let you know the calculated z then how can you calculate probabilities ##
z <- -1.96

## CDF of normal distribution ##
pnorm(z)
z <- 1.96
pnorm(z)
1 - pnorm(z)

## Lets say calculated z is 1.89. What is the one and two sided p-values ##
z <- 1.89

## One sided ##
1 - pnorm(z)

## Two sided ##
pnorm(-z) + (1 - pnorm(z))

## Easy Way ##
## For positive z
2 * (1 - pnorm(z))
## For negative z
2 * pnorm(-z)

## For any z positive or negative ##
2 * (1-pnorm(abs(z)))

#### Same thing with t values ##
t.val <- 1.89

2 * (1 - pt(abs(t.val), df = 3))
2 * (1 - pt(abs(t.val), df = 30))
2 * (1 - pt(abs(t.val), df = 300))
2 * (1 - pt(abs(t.val), df = 30000))
2 * (1 - pt(abs(t.val), df = 300000))
2 * (1 - pnorm(abs(z)))

#### T-test ###

t.test(1:10, y = c(7:20))  
t.test(1:10, y = c(7:20), var.equal = T) ## Assuming Equal Variance 
t.test(1:10, y = c(7:20), var.equal = T, alternative = "greater") ## Equal Variance with one sided test

## For real data ##
t.test(extra ~ group, data = sleep, var.equal = T)

## F test for variances ##
var.test(extra ~ group, data = sleep,alternative = "two.sided")
var.test(extra ~ group, data = sleep,alternative = "greater")