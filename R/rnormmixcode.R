#'
#' @param q description
#' @param
#' @param
#' @param
#' @param
#' @param
#' @param
#'
#'
#' @return
#' @export
#'
#' @example

pnormmix = function(q, mean1, sd1, mean2, sd2, mixprop){(1-mixprop)*pnorm(q,mean1,sd1) +
    (mixprop)*pnorm(q,mean2,sd2)}

dnormmix = function(x, mean1, sd1, mean2, sd2, mixprop){(1-mixprop)*dnorm(x,mean1,sd1) +
    (1/3)*dnorm(x,mean2,sd2)}

qnormmix = function(p, mean1, sd1, mean2, sd2, mixprop){
  fun = function(z){pnormmix(z, mean1, sd1, mean2, sd2, mixprop)-p}
  uniroot(fun, interval = c(-10,10), extendInt = "yes")$root
}
qnormmix = Vectorize(qnormmix)

rnormmix = function(n, mean1, sd1, mean2, sd2, mixprop) {
  ifelse(rbinom(n=n,size=1,prob=1-mixprop)==1, rnorm(n,mean1,sd1), rnorm(n,mean2,sd2))
}
