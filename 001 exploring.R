
# exploring one sample binomial power March 2024
 
# https://www.mail-archive.com/r-help@r-project.org/msg151422.html
# https://stat.ethz.ch/pipermail/r-help/2011-November/294503.html

# https://stats.stackexchange.com/questions/141769/power-analysis-for-binomial-test-via-simulation?rq=1

# https://stats.stackexchange.com/questions/485577/power-analysis-for-binomial-test

# https://sites.calvin.edu/scofield/courses/m343/F15/handouts/binomialTestPower.pdf

# #https://stats.stackexchange.com/questions/587247/how-to-do-power-analysis-in-r-for-one-sample-binomial-

require(mosaic)

plotDist("binom", params=c(100, .68), col=c("red"),
        # groups=abs(x-50) <= 10, 
        xlim=c(0,100), ylim=c(0,0.1))
plotDist("binom", params=c(100, .68), col="gray60", add=TRUE)


enn = 1:100
critical = qbinom(.025, enn, .5)
beta = pbinom(enn-critical,enn,.68) - pbinom(critical-1,enn,.68)
xyplot(1-beta ~ enn, type="l", lwd=0.5, xlab="n", ylab="power")

min(which(1-beta > 0.8))

#------------------------------------------------------------
# another similar approach
# from r  mailing list
# The possible sample size vector N needs to be selected in such a fashion
# that it covers the possible range of values that include the true
# minima. My example here does with a finite range and makes the 
# plot easier to visualize.
N <- 1:100

Alpha <- 0.025
Pow <- 0.8
p0 <- 0.5
p1 <- 0.68

# Required number of events, given a vector of sample sizes (N)
# to be considered at the null proportion, for the given Alpha
CritVal <- qbinom(p = 1 - Alpha, size = N, prob = p0)

# Get Beta (Type II error) for each N at the alternate hypothesis
# proportion
Beta <- pbinom(CritVal, N, p1)

# Get the Power
Power <- 1 - Beta

# Find the smallest sample size yielding at least the required power
SampSize <- min(which(Power > Pow))

# Get and print the required number of events to reject the null
# given the sample size required
(Res <- paste(CritVal[SampSize] + 1, "out of", N[SampSize]))


# Plot it all 
plot(N, Power, type = "b", las = 1)

title(paste("One Sided Sample Size and Critical Value for H0 =", p0, 
            "versus HA = ", p1, "\n",
            "For Power = ", Pow),
      cex.main = 0.95)

points(N[SampSize], Power[SampSize], col = "red", pch = 19)

text(N[SampSize], Power[SampSize], col = "red",
     label = Res, pos = 3)

abline(h = Pow, lty = "dashed")

#----------------------------------------------------------

# pvalue is the power
binom.test(40, 63, p=0.68, alt="greater", conf.level=0.95)
binom.test(40, 63, p=0.68, alt="two.sided", conf.level=0.9) # show equivalence

sum(dbinom(0:39, 63, 0.68))  # ~20 chance of getting < 40/63 when p=0.68
# power , getting 40 or more
1-sum(dbinom(0:39, 63, 0.68)) # power to reject 0.5 with 63 and p=0.68

1-(pbinom(39, 63, 0.68)) # power to reject 0.5 with 63 and p=0.68

##----------------------------------------
# stack exchange example

#' Sample Size for One-sample Exact Binomial Tests
#'
#' Determines the minimum sample size for a one-sided, one-sample exact
#' binomial test with specified alpha (type I) and beta (type II) errors
#'
#' @details
#' Loops over sample sizes starting with \code{n.min}, determines the critical
#' value for the exact test of size alpha, and calculates the type II error.
#' If the type II error is larger than \code{beta}, increments the sample size
#' by 1 and tries again.
#'
#' \code{\link{print.bin1samp}} is a print method for the output of
#' \code{bin1samp}.
#'
#' @param p0 Null hypothesis response probability
#' @param pa Alternative hypothesis response probability
#' @param alpha Type I error rate
#' @param beta Type II error rate
#' @param n.min Minimum sample size considered
#'
#' @return
#' \code{bin1samp} returns a vector giving the minimum sample size
#' (\code{n}), the critical value \code{r} (reject if outcome is more extreme
#' than \code{r}), the null and alternative response probabilities (\code{p0}
#' and \code{pa}), and the type I and type II errors (\code{size} and
#' \code{type2}).)
#'
#' No value is returned by \code{print.bin1samp}, but it prints the input
#' \code{p0} and \code{pa}, the minimum sample size, the critical value
#' \code{r} for the test that rejects if the number of responses is \code{> r}
#' if \code{pa>p0} or that rejects if the number of responses is \code{< r} if
#' \code{pa<p0}, and the actual type I and type II error rates.
#'
#' @seealso
#' \code{\link{print.bin1samp}}; \code{\link{pickwin}}; \code{\link{rp21}};
#' \code{\link{twostg}}; \code{\link{simon}}
#'
#' @keywords design
#'
#' @examples
#' bin1samp(0.9, 0.95, n.min = 100)
#' bin1samp(0.1, 0.05, n.min = 100)
#'
#' @export

bin1samp <- function(p0, pa, alpha = 0.1, beta = 0.1, n.min = 20L) {
  if (p0 == pa)
    stop('p0 and pa are identical')
  b <- 1
  x <- round(p0 * n.min)
  n <- n.min - 1
  if (pa > p0) {
    while (b > beta) {
      n <- n + 1
      # determine cutoff: reject if X>x
      l <- x:n
      s <- 1 - pbinom(l, n, p0)
      sub <- s <= alpha
      x <- l[sub][1L]
      size <- s[sub][1L]
      b <- pbinom(x, n, pa)
    }
  } else if (pa < p0) {
    while (b > beta) {
      n <- n + 1
      # determine cutoff: reject if X<x
      l <- x:0
      s <- pbinom(l, n, p0)
      sub <- s <= alpha
      x <- l[sub][1L]
      size <- s[sub][1L]
      b <- 1 - pbinom(x, n, pa)
      x <- x + 1
    }
  }
  
  structure(
    c(n = n, r = x, p0 = p0, pa = pa, size = size, type2 = b),
    class = 'bin1samp'
  )
}

#' Print a summary of the output from bin1samp
#'
#' Prints a summary of the output from \code{\link{bin1samp}}.
#'
#' @details
#' Prints operating characteristics and stopping rules of
#' \code{\link{bin1samp}}.
#'
#' @param x An object (vector) of class \code{bin1samp}
#' @param ... Included for compatibility with the generic function
#'
#' @return
#' No value is returned - used for side-effect of printing to console.
#'
#' @seealso
#' \code{\link{bin1samp}}
#'
#' @export

print.bin1samp <- function(x, ...) {
  cat(paste0('             p0 = ', format(x[3L]), '\n'))
  cat(paste0('             pa = ', format(x[4L]), '\n'))
  cat(paste0('min sample size = ', x[1L],'\n\n'))
  if (x[3L] < x[4L])
    cat('reject H0 if # responses >', format(x[2L]), '\n\n')
  else cat('reject H0 if # responses <', format(x[2L]), '\n\n')
  cat(paste0('   type I error = ', format(x[5L]), '\n'))
  cat('  type II error =', format(x[6L]),'\n')
  invisible(NULL)
}

# this does one sided
    #81
bin1samp(p0=.5, pa=.68, alpha = 0.025, beta = 0.2, n.min = 20)

#---------------------------------------------------------

# my simulation!
n <- 63
p <-0.68
s <- rbinom(10000, n, p)
r <- Hmisc::binconf(s, n, alpha = 0.05, method = "wilson", include.x = T, include.n = T)
r <- as.data.frame(r)
r$hw <- (r$Upper - r$Lower) / 2
mean(r$Lower>.5)


#plyr::arrange(unique(r), X)
#Hmisc::binconf(1:7,7, include.x = TRUE, include.n = TRUE)

with(r,plot(hw~X))
#-----------------------------------------------------------------

# stata say you only need 59?
#sampsi .50 .68, onesample power(.80) 

#https://stats.stackexchange.com/questions/587247/how-to-do-power-analysis-in-r-for-one-sample-binomial-test

with(list(mu = 0.5, mu.alt = 0.68, alpha = 0.025, n = 63), 
     (mean(sapply(rbinom(1e4, n, mu.alt), function(x) 
       prop.test(x, n, mu, alternative = "greater")$p.value <= alpha))))

require(gsDesign)

 nBinomial1Sample(
  p0 = 0.5,
  p1 = 0.68,
  alpha = 0.025,
  beta = .2,
  n = 0:100,
  #outtype = 1,
  conservative = TRUE
)

 nBinomial1Sample(
  p0 = 0.5,
  p1 = 0.68,
  alpha = 0.025,
  beta = .2,
  n = 0:100,
  #outtype = 1,
  conservative = FALSE
)


nb1 <- nBinomial1Sample(
  p0 = 0.5,
  p1 = 0.68,
  alpha = 0.025,
  beta = NULL,
  n = 0:100,
  #outtype = 1,
  #conservative = TRUE
)

nb1
library(scales)
ggplot2::ggplot(nb1, ggplot2::aes(x = n, y = Power)) + 
  ggplot2::geom_line() + 
  ggplot2::geom_point() + 
  ggplot2::scale_y_continuous(labels = percent)
 

# Reproduce realized power
stats::pbinom(q = 63, size = 39, prob = .68, lower.tail = FALSE)
1-(pbinom(q=39, size=63, prob=0.68)) # power to reject 0.5 with 63 and p=0.68

 

#----------------------------------------------------------------------------------------------------------------------

# lets try built in functions
MESS::power_binom_test(n = 63, p0 = .5, 
                       pa = .68, sig.level = 0.025, power = NULL, 
                       alternative = c("greater"))

MESS::power_binom_test(n = 63, p0 = .5, 
                       pa = .68, sig.level = 0.05, power = NULL, 
                       alternative = c("two.sided"))

#----------------------------------------------------------------

exactci::powerBinom(n=63,p0=.5,p1=.68, type="standard")
exactci::powerBinom(n=63,p0=.5,p1=.68, type="cilength")  ##?

#----------------------------------------------------------------
 
binGroup::binPower(n=63, delta=.18, p.hyp=0.5, conf.level = 0.95, 
                   alternative = "two.sided", method = "Score")

#----------------------------------------------------------------matching stata

 
x <- EnvStats::propTestPower(n.or.n1 = seq(63, 63, by=1), 
                                   p.or.p1 = 0.68, p0 = 0.5, alpha=0.05, approx=FALSE) 

lapply(x, round, 2) 

# this matches stata
x <- EnvStats::propTestPower(n.or.n1 = seq(59, 59,by=1), 
                             p.or.p1 = 0.68, p0 = 0.5, alpha=0.05, approx=TRUE) 

lapply(x, round, 2) 


n.list <- EnvStats::propTestN(p.or.p1 = 0.68,  p0.or.p2 = 0.5, 
                    power = seq(0.8, 0.9, by = 0.1), approx = FALSE) 

lapply(n.list, round, 3) 

# matches stata!!! so stata is normal approximation!
# sampsi .50 .68, onesample power(.90)
# sampsi .50 .68, onesample power(.80)
n.list <- EnvStats::propTestN(p.or.p1 = 0.68,  p0.or.p2 = 0.5, 
                              power = seq(0.8, 0.9, by = 0.1), approx = TRUE) 

lapply(n.list, round, 3) 

#-------------------------------------------------------

##paper in onedrive simulation, expecting 90% power


## 1.
n <- 81 # sample size
pval <- replicate(5000, { # replications of experiment
  x <- rbinom(1, size = n, # data-generating model with
              prob = 0.5 + 0.18) # minimum relevant effect
  binom.test(x, n = n, p = 0.5)$p.value # p-value of test against H0
})
mean(pval < 0.05) # simulated power at alpha = 0.05



## 2.
n <- 81
pval <- replicate(5000, {
  x <- rbinom(1, size = n, prob = 0.5 + 0.18)
  prop.test(x, n = n, p = 0.5, correct = FALSE)$p.value
})
mean(pval < 0.05)



##paper in onedrive simulation, expecting 80% power, seems a little higher?


## 1.
n <- 23 # sample size
pval <- replicate(9999, { # replications of experiment
  x <- rbinom(1, size = n, # data-generating model with
              prob = 0.37 + 0.31) # minimum relevant effect
  binom.test(x, n = n, p = 0.37)$p.value # p-value of test against H0
})
mean(pval < 0.05) # simulated power at alpha = 0.05



## 2.
n <- 23
pval <- replicate(9999, {
  x <- rbinom(1, size = n, prob = 0.37 + 0.31)
  prop.test(x, n = n, p = 0.37, correct = FALSE)$p.value
})
mean(pval < 0.05)


###-----------------last but not least, great go to approach

# comes from book on group sequential that I adapted 
# https://stats.stackexchange.com/questions/113602/test-if-two-binomial-distributions-are-statistically-different-from-each-other

formatz4 <- function(x){
  sprintf(x, fmt = '%#.4f')  
}


onesamplebinomial=function( u1, u0, n,  alpha1=0.05 ,  nSims=1e5)
{
  
  sigma1=sqrt(u1*(1-u1)) #arm1 se
  
  P1 <- 0 # counter
  
  for (i in 1:nSims) {
    
    y1=rnorm(1, u1, sigma1/sqrt(n))
    y0=u0
    
    z1=(y1-y0)/sqrt(sigma1^2/n) # diff
    
    
    t1=2*(1-pnorm(z1))  # 2 sided p-value
    
    #power
    if(t1<=alpha1/2){P1=P1+1/nSims} # 
    
  }
  
  return (c(  "power=",   formatz4(P1)
              
  ))
}

onesamplebinomial(u0=0.5, u1=0.68, n=81, alpha=.05)  # expect 90%
onesamplebinomial(u0=0.5, u1=0.68, n=63, alpha=.05)   # expect 80%
onesamplebinomial(u0=0.37, u1=0.68, n=28, alpha=.05)  # expect 90%
onesamplebinomial(u0=0.37, u1=0.68, n=23, alpha=.05)   # expect 80%



