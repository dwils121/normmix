pnormmix = function(q, mean1, sd1, mean2, sd2){(2/3)*pnorm(q,mean1,sd1) +
    (1/3)*pnorm(q,mean2,sd2)}

dnormmix = function(x, mean1, sd1, mean2, sd2){(2/3)*dnorm(x,mean1,sd1) +
    (1/3)*dnorm(x,mean2,sd2)}

qnormmix = function(p, mean1, sd1, mean2, sd2){
  fun = function(z){pnormmix(z, mean1, sd1, mean2, sd2)-p}
  uniroot(fun, interval = c(-10,10), extendInt = "yes")$root
}
qnormmix = Vectorize(qnormmix)

rnormmix = function(n, mean1, sd1, mean2, sd2) {
  ifelse(rbinom(n=n,size=1,prob=2/3)==1, rnorm(n,mean1,sd1), rnorm(n,mean2,sd2))
}
