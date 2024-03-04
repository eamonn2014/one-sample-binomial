

#ref https://stats.stackexchange.com/questions/467595/how-do-you-calculate-sample-sizes-for-multiple-treatments

# seems to be in agreement with PASS and literature examples
# three arm binary trial power with bonferroni adjustment
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






# three.arm2(u0=0.2, u1=0.09, u2=0.09, n=192, alpha=.05)
# three.arm2(u0=0.2, u1=0.07, u2=0.07, n=128, alpha=.05)
# three.arm2(u0=0.2, u1=0.05, u2=0.05, n=88, alpha=.05)
# 
# # why not at least one arm significant? 80%
# 
# three.arm2(u0=0.2, u1=0.10, u2=0.10, n=192, alpha=.05)
# three.arm2(u0=0.2, u1=0.09, u2=0.09, n=147, alpha=.05)
# three.arm2(u0=0.2, u1=0.07, u2=0.07, n=101, alpha=.05)
# three.arm2(u0=0.2, u1=0.05, u2=0.05, n=72, alpha=.05)
# 
# 
# 
# # validation of function to pass
# three.arm2(u0=0.2, u1=0.16, u2=0.16, n=1266, alpha=.15) # checking to pass
# three.arm2(u0=0.9, u1=0.7, u2=0.6, n=54, alpha=.15) # ch
# three.arm2(u0=0.2, u1=0.09, u2=0.09, n=140, alpha=.15) # ch
# 
# #--------------------------------------------------------------------------------------
# 
# # lets try and plot
# 
# df_plot1 <- expand_grid(u0=0.2,
#                         u1=c(.1,.09,.07,.05),
#                         n=seq(40,250, 10),
#                         alpha=.05) 
# 
# df_plot1$u2 <- df_plot1$u1
# 
# df_plot1$disjunctive <- df_plot1$arm2 <- df_plot1$arm1 <- NA
# 
# for(i in 1 : dim(df_plot1)[1]) {
#   
#    x <- 
#     three.arm2 (u0=df_plot1$u0[i], 
#                 u1=df_plot1$u1[i],  
#                 u2=df_plot1$u2[i],
#                 n=df_plot1$n[i],
#           alpha=df_plot1$alpha[i],  
#     )
#   
#              df_plot1$arm1[i]   <-    as.numeric(x[2])
#              df_plot1$arm2[i]   <-    as.numeric(x[4])
#              df_plot1$disjunctive[i] <-  as.numeric(x[12]) 
# }
# 
# #--------------------------------------------------------------------------------------
# 
# #-- all in one plot
# 
# df <- df_plot1
# 
# df$u2 <- NULL
# df$alpha <- NULL
# df$arm2 <- NULL
# 
# names(df) <- c("u0","u1", "n", "separate", "disjunctive")
# L <- reshape2::melt(df, id=c("u0", "u1","n"), variable.name="Test")
# 
# library(ggplot2)
# 
# A <- ggplot(data=L, aes(x=n, y=value, linetype=Test, colour=factor(u1  ))) +
#   geom_line()+
#   labs(x=expression("Sample size required in each arm"),
#        y= "Power", colour ="Each active arm AKI rate:") +
#   theme_bw() +
#   theme(legend.position = "bottom")  +
#   scale_y_continuous(breaks = seq(0.1, 1, by = 0.1),
#                      limits = c(0.1, 1)) +
#   scale_x_continuous(breaks = seq(50, 250, 50), limits =   c(40,250)) +
#   theme(plot.caption = element_text(hjust = 0)) + # set the left align here
#   
#    geom_hline(yintercept=c(.8,.9), linetype="dotted", 
#                   color = "black", linewidth=.2) +
#   
#   ggtitle(paste0("Power and sample size, for a three arm RCT study, the placebo AKI rate is 20% and two active treatment arms") ) +
#   labs(caption = paste("- Alpha the type I assertion probability 0.05\n- Bonferroni adjusted z tests with unpooled variance\n- 'Disjunctive' means at least one of the two active arms achieves significance v placebo, 'Separate' means each of the two active arms achieve significance v placebo\n- Final calculations will be performed using PASS/nQuery software" )) 
# 
# ggsave("all.png", width = 20, height = 20, units = "cm")
# 
# #--------------------------------------------------------------------------------------
# 
# eacharm <- ggplot(data=df_plot1, aes(x=n, y=arm1, colour=factor(u2  ))) +
#   geom_line()+
#   labs(x=expression("Sample size in each arm"),
#        y= "Power", colour ="Active arm AKI incidence") +
#   theme_bw() +
#   theme(legend.position = "bottom")  +
#   
#   
#   scale_y_continuous(breaks = seq(0.3, 1, by = 0.1),
#                      limits = c(0.3, 1)) +
#   scale_x_continuous(breaks = seq(50, 250, 50), limits =   c(75,250)) +
#   theme(plot.caption = element_text(hjust = 0)) + # set the left align here
#   
#   geom_hline(yintercept=c(.8,.9), linetype="dotted", 
#              color = "black", linewidth=.2) +
#   
#   ggtitle(paste0("Power for each active arm to reach significance v placebo against arm sample size, \nthree arm study, placebo AKI rate 20%") ) +
#   labs(caption = paste("- Alpha the type I assertion probability 0.05\n- Bonferroni adjusted z tests with unpooled variance" )) 
# 
# ggsave("eacharm.png", width = 20, height = 20, units = "cm")
# 
# #-----------------------------------------------
# 
# anyarm <-ggplot(data=df_plot1, aes(x=n, y=disjunctive, colour=factor(u2  ))) +
#   geom_line()+
#   labs(x=expression("Sample size in each arm"),
#        y= "Power", colour ="Active arm AKI incidence") +
#   theme_bw() +
#   theme(legend.position = "bottom") +
#   scale_y_continuous(breaks = seq(0.3, 1, by = 0.1),
#                      limits = c(0.3, 1)) +
#   scale_x_continuous(breaks = seq(50, 250, 50), limits =   c(75,250)) +
#   theme(plot.caption = element_text(hjust = 0)) + # set the left align here
#   
#   geom_hline(yintercept=c(.8,.9), linetype="dotted", 
#              color = "black", linewidth=.2) +
#   
#   ggtitle(paste0("Disjunctive power (at least one active arm to reach significance v placebo) against arm sample size, \nthree arm study, placebo AKI rate 20%") ) +
#   labs(caption = paste("- Alpha the type I assertion probability 0.05\n- Bonferroni adjusted z tests with unpooled variance" )) 
#   
# ggsave("anyarm.png", width = 20, height = 20, units = "cm")
# 
# #-----------------------------------------------

 




