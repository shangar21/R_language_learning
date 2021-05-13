location_normal_mean_ci <- function(data, sigma, gamma){
	xbar <- mean(data)
	gamma <- (1 + gamma)/2
	n <- length(data)
	ci <- c(xbar - (qnorm(gamma)*sqrt(sigma/n)), xbar + (qnorm(gamma)*sqrt(sigma/n)))
	return ci
}

location_scale_normal_mean_ci <- function(data, gamma){
	xbar <- mean(data)
	S <- sd(data)
	gamma <- (1 + gamma)/2
	n <- length(data)
	ci <- c(xbar - (qt(gamma)*(S/sqrt(n))), xbar + (qt(gamma)*(S/sqrt(n))))
	return ci 
}

location_scale_normal_var_ci <- function(data, gamma){
	S <- sd(data)
	n <- length(data)
	g_l <- (1 + gamma)/2
	g_u <- (1 - gamma)/2
	ci <- c(((n-1)S*S)/(qchisq(g_l, df=n-1)), ((n-1)S*S)/(qchisq(g_u, df=n-1)))
	return ci

}

p_value_location <- function(data, sigma, h_0){
	xbar <- mean(data)
	z <- (xbar - h_0)/(sqrt(sigma/length(data)))
	return 2*(1-pnorm(abs(z)))
}

p_value_location_scale <- function(data, h_0){
	xbar <- mean(data)
	S <- sd(data)
	z <- (xbar - h_0)/((S/sqrt(length(data))))
	return 2*(1-pt(abs(z), df=len(data)-1))
}