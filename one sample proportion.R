

# preparing and researching

n <-30
p <-0.65
s <- rbinom(10000, n, p)
r <- binconf(s, n, alpha = 0.05, method = "wilson", include.x = T, include.n = T)
r <- as.data.frame(r)
r$hw <- (r$Upper - r$Lower) / 2
mean(r$Lower>.35)

 
#plyr::arrange(unique(r), X)
#Hmisc::binconf(1:7,7, include.x = TRUE, include.n = TRUE)

with(r,plot(hw~X))
#-----------------------------------------------------------------

# stata . sampsi .35 .65, onesample

#https://stats.stackexchange.com/questions/587247/how-to-do-power-analysis-in-r-for-one-sample-binomial-test

with(list(mu = 0.35, mu.alt = 0.65, alpha = 0.05, n = 27), 
     (mean(sapply(rbinom(1e4, n, mu.alt), function(x) 
       prop.test(x, n, mu, alternative = "greater")$p.value <= alpha))))

require(gsDesign)

nBinomial1Sample(
  p0 = 0.35,
  p1 = 0.65,
  alpha = 0.05,
  beta = NULL,
  n = 20:27,
  outtype = 1,
  conservative = FALSE
)
 
library(ggplot2)

nb1 <- nBinomial1Sample(p0 = 0.35, p1=0.65,alpha = 0.05, beta=NULL, n = 25:40)
nb1
library(scales)
ggplot2::ggplot(nb1, ggplot2::aes(x = n, y = Power)) + 
  ggplot2::geom_line() + 
  ggplot2::geom_point() + 
  ggplot2::scale_y_continuous(labels = percent)

# simple call with same parameters to get minimum sample size yielding desired power
nBinomial1Sample(p0 = 0.35, p1 = 0.65, alpha = 0.05, beta = .2, n = 23:40)

 

 