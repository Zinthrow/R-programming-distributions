#
#fname <- "/home/alex/Documents/Programming/18.440/Math_scores.csv"

#credit goes to zoonek2.free.fr for distribtuion specific code
# Sample distributions 


#show specifically
distri <- rep(0,17)
distri[4] = 1 # Poisson
distri[5] = 1 # Geometric
distri[8] = 1
#show all

#distri <- 1:50

#1 Binomial dist
if (distri[1] > 0) {
  N <- 500
  n <- 10
  p <- .5
  x <- rbinom(N,n,p)
  hist(x, 
       xlim = c(min(x), max(x)), 
       probability = TRUE, 
       nclass = max(x) - min(x) + 1, 
       col = 'lightblue',
       main = 'Binomial distribution, n=10, p=.5')
  lines(density(x, bw=1), col = 'red', lwd = 3)
}
#2 Binomial dist
if (distri[2] > 0){
  N <- 100000
  n <- 100
  p <- .5
  x <- rbinom(N,n,p)
  hist(x, 
       xlim = c(min(x), max(x)), 
       probability = TRUE, 
       nclass = max(x) - min(x) + 1, 
       col = 'lightblue',
       main = 'Binomial distribution, n=100, p=.5')
  lines(density(x,bw=1), col = 'red', lwd = 3)
}
#3 Hypergeometric
if (distri[3] > 0) {
  N <- 10000
  n <- 5
  x <- rhyper(N, 15, 5, 5)
  hist(x, 
       xlim=c(min(x),max(x)), probability=T, nclass=max(x)-min(x)+1, 
       col='lightblue',
       main='Hypergeometric distribution, n=20, p=.75, k=5')
  lines(density(x,bw=1), col='red', lwd=3)
}

#4 Poisson
if (distri[4] > 0) {
  N <- 10000
  x <- rpois(N, 3)
  hist(x, 
       xlim=c(min(x),max(x)), probability=T, nclass=max(x)-min(x)+1, 
       col='lightblue',
       main='Poisson distribution, lambda=3')
  lines(density(x,bw=1), col='red', lwd=3)
}

#5 Geometric
if (distri[5] > 0) {
  N <- 10000
  x <- rgeom(N, .5)
  hist(x, 
       xlim=c(min(x),max(x)), probability=T, nclass=max(x)-min(x)+1, 
       col='lightblue',
       main='Geometric distribution, p=.5')
  lines(density(x,bw=1), col='red',lwd=3)
}

#6 exponential distribution function
if (distri[6] > 0) {
  curve(dexp(x), xlim=c(0,10), col='red', lwd=3,
        main='Exponential Probability Distribution Function')
}
#7 exponential distribtuion 
if (distri[7] > 0) {
  n <- 1000
  x <- rexp(n)
  hist(x, probability=T,
       col='light blue', main='Exponential Distribution')
  lines(density(x), col='red', lwd=3)
  curve(dexp(x), xlim=c(0,10), col='red', lwd=3, lty=2,
        add=T)
}
#8 CLT and gaussian distribution
if (distri[8] > 0) {
  limite.centrale <- function (r=runif, m=.5, s=1/sqrt(12), n=c(1,3,10,30), N=1000) {
    for (i in n) {
      x <- matrix(r(i*N),nc=i)
      x <- ( apply(x, 1, sum) - i*m )/(sqrt(i)*s)
      hist(x, col='light blue', probability=T, main=paste("n =",i), 
           ylim=c(0,max(.4, density(x)$y)))
      lines(density(x), col='red', lwd=3)
      curve(dnorm(x), col='blue', lwd=3, lty=3, add=T)
      if( N>100 ) {
        rug(sample(x,100))
      } else {
        rug(x)
      }
    }
  }
  op <- par(mfrow=c(2,2))
  limite.centrale()
  par(op)
}
#9 more gaussian
if (distri[9] > 0) {
  curve(pnorm(x), xlim=c(-3,3), col='red', lwd=3)
  title(main='Cumulative gaussian distribution function')
  
  curve(qnorm(x), xlim=c(0,1), col='red', lwd=3)
  title(main='Gaussian quantiles function')
}
#10 chi-squared
if (distri[10]> 0) {
  curve(dchisq(x,1), xlim=c(0,10), ylim=c(0,.6), col='red', lwd=3)
  curve(dchisq(x,2), add=T, col='green', lwd=3)
  curve(dchisq(x,3), add=T, col='blue', lwd=3)
  curve(dchisq(x,5), add=T, col='orange', lwd=3)
  abline(h=0,lty=3)
  abline(v=0,lty=3)
  legend(par('usr')[2], par('usr')[4], xjust=1,
         c('df=1', 'df=2', 'df=3', 'df=5'),
         lwd=3,
         lty=1,
         col=c('red', 'green', 'blue', 'orange')
  )
  title(main='Chi^2 Distributions')
}
#11 students t-test
if (distri[11]>0) {
  curve( dt(x,1), xlim=c(-3,3), ylim=c(0,.4), col='red', lwd=2 )
  curve( dt(x,2), add=T, col='blue', lwd=2 )
  curve( dt(x,5), add=T, col='green', lwd=2 )
  curve( dt(x,10), add=T, col='orange', lwd=2 )
  curve( dnorm(x), add=T, lwd=3, lty=3 )
  title(main="Student T distributions")
  legend(par('usr')[2], par('usr')[4], xjust=1,
         c('df=1', 'df=2', 'df=5', 'df=10', 'Gaussian distribution'),
         lwd=c(2,2,2,2,2), 
         lty=c(1,1,1,1,3),
         col=c('red', 'blue', 'green', 'orange', par("fg")))
}

# 12 Fisher's F
if (distri[12]>0) {
  curve(df(x,1,1), xlim=c(0,2), ylim=c(0,.8), lty=2)
  curve(df(x,3,1), add=T)
  curve(df(x,6,1), add=T, lwd=3)
  curve(df(x,3,3), add=T, col='red')
  curve(df(x,6,3), add=T, lwd=3, col='red')
  curve(df(x,3,6), add=T, col='blue')
  curve(df(x,6,6), add=T, lwd=3, col='blue')
  title(main="Fisher's F")
  legend(par('usr')[2], par('usr')[4], xjust=1,
         c('df=(1,1)', 'df=(3,1)', 'df=(6,1)', 
           'df=(3,3)', 'df=(6,3)', 
           'df=(3,6)', 'df=(6,6)'),
         lwd=c(1,1,3,1,3,1,3),
         lty=c(2,1,1,1,1,1,1),
         col=c(par("fg"), par("fg"), par("fg"), 'red', 'red', 'blue', 'blue'))
}

# 13 Cauchy/bowman's
if (distri[13]) {
  N <- 100                         # Number of arrows
  alpha <- runif(N, -pi/2, pi/2)   # Direction of the arrow
  x <- tan(alpha)                  # Arrow impact
  plot.new()
  plot.window(xlim=c(-5, 5), ylim=c(-1.1, 2))
  segments( 0, -1,     # Position of the Bowman
            x, 0   )   # Impact
  d <- density(x)
  lines(d$x, 5*d$y, col="red", lwd=3 )
  box()
  abline(h=0)
  title(main="The bowman's distribution (Cauchy)")
}
# 14 gaussian mixture
if (distri[14]>0) {
  N <- 10000
  m <- c(-2,0,2)    # Means
  p <- c(.3,.4,.3)  # Probabilities
  s <- c(1, 1, 1)   # Standard deviations
  x <- cbind( rnorm(N, m[1], s[1]), 
              rnorm(N, m[2], s[2]), 
              rnorm(N, m[3], s[3]) )
  a <- sample(1:3, N, prob=p, replace=TRUE)
  y <- x[ 1:N + N*(a-1) ]
  
  qqnorm(y, 
         main="Gaussian QQ-plot of a mixture of gaussians")
  qqline(y, col="red", lwd=3)
}

#15 weibull distribution
if (distri[15]>0){
  curve(dexp(x), xlim=c(0,3), ylim=c(0,2))
  curve(dweibull(x,1), lty=3, lwd=3, add=T)
  curve(dweibull(x,2), col='red', add=T)
  curve(dweibull(x,.8), col='blue', add=T)
  title(main="Weibull Probability Distribution Function")
  legend(par('usr')[2], par('usr')[4], xjust=1,
         c('Exponential', 'Weibull, shape=1',
           'Weibull, shape=2', 'Weibull, shape=.8'),
         lwd=c(1,3,1,1),
         lty=c(1,3,1,1),
         col=c(par("fg"), par("fg"), 'red', 'blue'))
}

#16 Gamma distribution
if (distri[16]>0) {
  curve( dgamma(x,1,1), xlim=c(0,5) )
  curve( dgamma(x,2,1), add=T, col='red' )
  curve( dgamma(x,3,1), add=T, col='green' )
  curve( dgamma(x,4,1), add=T, col='blue' )
  curve( dgamma(x,5,1), add=T, col='orange' )
  title(main="Gamma probability distribution function")
  legend(par('usr')[2], par('usr')[4], xjust=1,
         c('k=1 (Exponential distribution)', 'k=2', 'k=3', 'k=4', 'k=5'),
         lwd=1, lty=1,
         col=c(par('fg'), 'red', 'green', 'blue', 'orange') )
}
#17 Beta Distribution
if (distri[17]>0) {
  curve( dbeta(x,1,1), xlim=c(0,1), ylim=c(0,4) )
  curve( dbeta(x,2,1), add=T, col='red' )
  curve( dbeta(x,3,1), add=T, col='green' )
  curve( dbeta(x,4,1), add=T, col='blue' )
  curve( dbeta(x,2,2), add=T, lty=2, lwd=2, col='red' )
  curve( dbeta(x,3,2), add=T, lty=2, lwd=2, col='green' )
  curve( dbeta(x,4,2), add=T, lty=2, lwd=2, col='blue' )
  curve( dbeta(x,2,3), add=T, lty=3, lwd=3, col='red' )
  curve( dbeta(x,3,3), add=T, lty=3, lwd=3, col='green' )
  curve( dbeta(x,4,3), add=T, lty=3, lwd=3, col='blue' )
  title(main="Beta distribution")
  legend(par('usr')[1], par('usr')[4], xjust=0,
         c('(1,1)', '(2,1)', '(3,1)', '(4,1)', 
           '(2,2)', '(3,2)', '(4,2)',
           '(2,3)', '(3,3)', '(4,3)' ),
         lwd=1, #c(1,1,1,1, 2,2,2, 3,3,3),
         lty=c(1,1,1,1, 2,2,2, 3,3,3),
         col=c(par('fg'), 'red', 'green', 'blue', 
               'red', 'green', 'blue', 
               'red', 'green', 'blue' ))
}
