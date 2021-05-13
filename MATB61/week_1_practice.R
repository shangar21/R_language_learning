library(ggplot2)

fun1 <- function(x) 32/3 - 5/3*x
fun2 <- function(x) 12 - 2*x
fun3 <- function(x) 0
fun4 <- function(y) 0
p <- function(x) 128/15 - 20/15*x

x1 = seq(3.75,7)

mydf = data.frame(x1, y1=fun1(x1), y2=fun2(x1), y3=fun3(x1), y4=fun4(x1), y5=p(x1))
#mydf <-  transform(mydf, z = pmax(y1,pmin(y2,y3)))

ggplot(mydf, aes(x = x1)) + 
  geom_line(aes(y = y1), colour = 'blue') +
  geom_line(aes(y = y2), colour = 'green') +
  geom_line(aes(y = y3), colour = 'red') 
  #geom_line(aes(y = y5), colour='black')
  #geom_ribbon(aes(ymax=y1, ymin=y2), fill = 'yellow')