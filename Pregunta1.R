#población x<- N(mu, sigma^2)
mu <- 93.5
sigma <- 5.7

curve(dnorm(x,mu,sigma), xlim=c(80,120),col="red")

#Apartado a)
set.seed(123)

Y <- function(i) {sum(rnorm(4,mu,sigma))}
Y(2)

#REPETICIÓN EXPERIMENTO
Y100000 <- sapply(1:500000,Y)
hist(Y100000)

mean(Y100000)

#teoricamente
4*mu


#Apartado b)
Y <- function(i) {sum(rnorm(100,mu,sigma))}
Y500000 <- sapply(1:500000,Y)
var(Y500000)

100*(sigma^2) #aproximación de lo anterior
hist(Y500000, freq=FALSE)
curve(dnorm(x,100*mu,sqrt(100)*sigma),add=TRUE)


#Apartado c) P(x > 103)
1-pnorm(103,mu,sigma)

#Apartado d)
xbar <- function(i) {sum(rnorm(4,mu,sigma))}
xbar500000 <- sapply(1:500000,xbar)

hist(xbar500000, freq=FALSE)

mean(xbar500000>98)
#teoricamente
1- pnorm(98,mu,sigma/sqrt(4))

curve(dnorm(x,mu, sigma/sqrt(4)),add=TRUE,col="red")

#Apartado e) P(S^2 > 32)
Ssq <- function(i) {sum(rnorm(100,mu,sigma))}
Ssq500000 <- sapply(1:500000,Ssq)

hist(Ssq500000, freq=FALSE)

mean(Ssq500000>32)

hist(Ssq500000*(100-1)/sigma^2)
32*(100-1)/sigma^2
curve(dchisq(x,100-1),add=TRUE,col="red")
1- pchisq(32*(100-1)/sigma^2,100-1)
mean(Ssq500000>32)
