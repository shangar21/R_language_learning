require(pacman)
library(pacman)
p_unload(all)

set.seed(101)
x0 <- rexp(100,1)
hist(x0)
EX <- 1
VarX <- 1
s <- 2000
n <- c(5,10,50,100,1000,10000000)
z <- matrix(NA,nrow=s,ncol=length(n))

for(i in 1:s){
	for(j in 1:length(n)){
		samp <- rexp(n=n[j],1)
		M <- mean(samp)
		z[i,j] <- (M-EX)/sqrt(VarX/n[j])
	}
}


#hist(z[,1])
#hist(z[,2])
#hist(z[,3])
#hist(z[,4])
#hist(z[,5])
#hist(z[,6])
plot(z[,6])
