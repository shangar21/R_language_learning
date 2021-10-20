library(MASS)

sxx <- function(x){
    return ((x - mean(x))^2)
}

    
syh <- function(x,y,xh){
	fit = lm(y~x)
	MSE = anova(fit)[2,3]
	return MSE*((1/length(x)) + ((xh - mean(x))/sxx(x)))
}

