require(pacman)
library(pacman)
p_unload(all)

x <- rnorm(100000)
y = (x^4) + 2*(x^3) - 3

df = data.frame(y)

m=length(df[df$y<2,])-length(df[df$y<=1,])
m <- m/100000

print(m)