#' Mixture of Normal Distributions
#'
#' @param q A given quantile for the CDF.
#' @param x A given quantile for the PDF.
#' @param p A given probability for the inverse CDF.
#' @param n Number of generated numbers from the distribution.
#' @param mean1 Mean for the first normal distribution.
#' @param mean2 Mean for the second normal distribution.
#' @param sd1 Standard deviation for the first normal distribution.
#' @param sd2 Standard deviation for the second normal distribution.
#' @param mixprop Proportion that the second normal distribution holds in the mixture.
#'
#' @return
#' @export
#'
#' @example

pnormmix = function(q, mean1, sd1, mean2, sd2, mixprop){(1-mixprop)*pnorm(q,mean1,sd1) +
    (mixprop)*pnorm(q,mean2,sd2)}

dnormmix = function(x, mean1, sd1, mean2, sd2, mixprop){(1-mixprop)*dnorm(x,mean1,sd1) +
    mixprop*dnorm(x,mean2,sd2)}

qnormmix = function(p, mean1, sd1, mean2, sd2, mixprop){
  fun = function(z){pnormmix(z, mean1, sd1, mean2, sd2, mixprop)-p}
  uniroot(fun, interval = c(-10,10), extendInt = "yes")$root
}
qnormmix = Vectorize(qnormmix)

rnormmix = function(n, mean1, sd1, mean2, sd2, mixprop) {
  ifelse(rbinom(n=n,size=1,prob=1-mixprop)==1, rnorm(n,mean1,sd1), rnorm(n,mean2,sd2))
}
