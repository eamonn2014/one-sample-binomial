##
# preparing and research


require(Hmisc)
n <-23 #81  #30
p <-0.68
s <- rbinom(1e5, n, p)
r <- binconf(s, n, alpha = 0.05, method = "wilson", include.x = T, include.n = T)
r <- as.data.frame(r)
r$hw <- (r$Upper - r$Lower) / 2
summary(r$hw)
mean(r$Lower>.37)
mean(r$Lower>.50)

#  sampsi .50 .68, onesample 
#  stata says you need 77 for 90% power and 59 for 80% power
# my code 79 and 58 for 

# this matches my code
with(list(mu = 0.5, mu.alt = 0.68, alpha = 0.05, n = 63), 
     (mean(sapply(rbinom(1e4, n, mu.alt), function(x) 
       prop.test(x, n, mu, alternative = "two.sided")$p.value <= alpha))))

with(list(mu = 0.37, mu.alt = 0.68, alpha = 0.05, n = 28), 
     (mean(sapply(rbinom(1e4, n, mu.alt), function(x) 
       prop.test(x, n, mu, alternative = "two.sided")$p.value <= alpha))))

with(list(mu = 0.37, mu.alt = 0.68, alpha = 0.05, n = 23), 
     (mean(sapply(rbinom(1e4, n, mu.alt), function(x) 
       prop.test(x, n, mu, alternative = "two.sided")$p.value <= alpha))))


with(list(mu = 0.5, mu.alt = 0.68, alpha = 0.05, n = 59), 
     (mean(sapply(rbinom(1e4, n, mu.alt), function(x) 
       prop.test(x, n, mu, alternative = "two.sided")$p.value <= alpha))))

 
#binom.test(35, 44, p = 0.68, alternative = c("two.sided"), conf.level = 0.90)



require(gsDesign)

nBinomial1Sample(
  p0 = 0.5,
  p1 = 0.68,
  alpha = 0.025,   # set this to one side for test
  beta = NULL,
  n = 75:81,
  outtype = 1,
  conservative = FALSE
)


nBinomial1Sample(
  p0 = 0.5,
  p1 = 0.68,
  alpha = 0.025,   # set this to one side for test
  beta = NULL,
  n = 60:65,
  outtype = 1,
  conservative = FALSE
)


nBinomial1Sample(
  p0 = 0.37,
  p1 = 0.68,
  alpha = 0.025,   # set this to one side for test
  beta = NULL,
  n = 20:30,
  outtype = 1,
  conservative = FALSE
)


nBinomial1Sample(
  p0 = 0.37,
  p1 = 0.68,
  alpha = 0.025,   # set this to one side for test
  beta = NULL,
  n = 20:30,
  outtype = 1,
  conservative = FALSE
)







library(ggplot2)

nb1 <- nBinomial1Sample(p0 = 0.5, p1=0.68,alpha = 0.05, beta=NULL, n = 50:60)
nb1
library(scales)
ggplot2::ggplot(nb1, ggplot2::aes(x = n, y = Power)) + 
  ggplot2::geom_line() + 
  ggplot2::geom_point() + 
  ggplot2::scale_y_continuous(labels = percent)

# simple call with same parameters to get minimum sample size yielding desired power
nBinomial1Sample(p0 = 0.35, p1 = 0.65, alpha = 0.05, beta = .2, n = 23:40)
