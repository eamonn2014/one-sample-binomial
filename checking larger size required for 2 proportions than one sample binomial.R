


### checking difference in N require for one binomial and two binomial

# Parameters
alpha <- 0.025          # Significance level
power <- 0.8           # Desired power
effect_size <- 0.18    # Effect size

# One-sample binomial test (exact)
pop_prop <- 0.5        # Population proportion (null hypothesis)
sample_size_one_sample_exact <- gsDesign::nBinomial1Sample(
       p0 = pop_prop,
       p1 = pop_prop+effect_size,
       alpha = 0.025,
       beta = 1-power,
       n = 0:100,
      #outtype = 1,
         conservative = FALSE
  )

 
sample_size_two_sample <- sum(Hmisc::bsamsize(p1=pop_prop, p2=pop_prop+effect_size, fraction=.5, alpha=.05, power=power))
  

# Output results
cat("Sample size required for exact one-sample binomial test:",  sample_size_one_sample_exact, "\n")
cat("Sample size required for two-sample test comparing proportions:", sample_size_two_sample, "\n")
