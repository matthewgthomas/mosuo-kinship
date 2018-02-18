##
## http://stackoverflow.com/a/21221995/951174
##
confint.gee <- function(object, parm, level = 0.95, ...) {
  cc <- coef(summary(object))
  mult <- qnorm((1+level)/2)
  cc.d <- as.data.frame(cc)
  est <- cc.d$Estimate
  se <- cc.d$"Robust S.E"
  citab <- cbind(lwr=est-mult*se, upr=est+mult*se)
  rownames(citab) <- rownames(cc)
  citab[parm,]
}
confint.geeglm <- function(object, parm, level = 0.95, ...) {
  cc <- coef(summary(object))
  mult <- qnorm((1+level)/2)
  cc.d <- as.data.frame(cc)
  est <- cc.d$Estimate
  se <- cc.d$"Std.err"  #"Robust S.E"
  citab <- cbind(lwr=est-mult*se, upr=est+mult*se)
  rownames(citab) <- rownames(cc)
  #citab[parm,]
  citab
}
# get CIs of standardized GEE model
confint.geeglm.z <- function(object, parm, level = 0.95, ...) {
  cc <- coef(summary(object))
  mult <- qnorm((1+level)/2)
  cc.d <- as.data.frame(cc)
  est <- cc.d$Estimate
  se <- cc.d$"Std.err"
  citab <- cbind(lwr=est-mult*se, upr=est+mult*se)
  #citab <- with(as.data.frame(cc),
  #              cbind(lwr=Estimate-mult*"Robust S.E",
  #                    upr=Estimate+mult*"Robust S.E"))
  rownames(citab) <- rownames(cc)
  citab[parm,]
}

#or_ci = function(x) round(cbind(exp(summary(x)$coef), exp(confint.geeglm(x))), 2)[, c(1,6:7)]  # gee version
#or_ci = function(x, d=3) round(cbind(exp(summary(x)$coef), exp(confint.geeglm(x))), d)[, c(1,5:6)]  # geepack version
#or_ci.z = function(x, d=3) round(cbind(exp(summary(x)$coef), exp(confint.geeglm.z(x))), d)[, c(1,5:6)]

or_ci = function(x, d=3)
{
  est = data.frame( Estimate=round(exp(coef(x)), d) )  # calculate OR for param estimate
  est = cbind( est, round(exp(confint.geeglm(x)), d) )  # append CIs
  
  est = cbind(Parameter=rownames(est), est)  # convert row names into a 'Parameter' variable
  rownames(est) = NULL  # remove row names
  return(est)
}
