# Eamonn extended proportions plot to show sample sizes needed for small differences 20Feb2024
# this is used to assess sample size for evaluation against historical controls.
# proportions ----------------------------------------------------------------------------
#https://www.r-bloggers.com/2009/02/r-good-practice-%e2%80%93-adding-footnotes-to-graphics/
#rm(list=ls())
library(rms)
library(ggplot2)
library(grid)

## this version has 0.3 v 0.25 added at customer request


makeFootnote <- function(footnoteText=
                           format(Sys.time(), "%d %b %Y"),
                         size= .7, color= grey(.5))
{
  require(grid)
  pushViewport(viewport())
  grid.text(label= footnoteText ,
            x = unit(1,"npc") - unit(2, "mm"),
            y= unit(2, "mm"),
            just=c("right", "bottom"),
            gp=gpar(cex= size, col=color))
  popViewport()
}


makeFootnoteL <- function(footnoteText=
                            format(Sys.time(), "%d %b %Y"),
                          size= .7, color= grey(.5))
{
  require(grid)
  pushViewport(viewport())
  grid.text(label= footnoteText ,
            x = unit(1,"npc") - unit(250, "mm"),
            y= unit(2, "mm"),
            just=c("left", "bottom"),
            gp=gpar(cex= size, col=color))
  popViewport()
}

N <-1:100
Alpha <- 0.025
p1 <- 0.68

p01 <- 0.37
critical1 <- qbinom(p = 1 - Alpha, size = N, prob = p01)
beta1 <- pbinom(critical1, N, p=p1)
lattice::xyplot(1-beta1 ~ N, type="l", lwd=0.5, xlab="n", ylab="power")

p02 <- 0.5
critical2 <- qbinom(p = 1 - Alpha, size = N, prob = p02)
beta2 <- pbinom(critical2, N, p=p1)
lattice::xyplot(1-beta2 ~ N, type="l", lwd=0.5, xlab="n", ylab="power")


SampSize1 <- min(which(1-beta1 > .90))  # 0.37
SampSize2 <- min(which(1-beta1 > .80))
SampSize3 <- min(which(1-beta2 > .90))  #.5
SampSize4 <- min(which(1-beta2 > .80))
 

# Get and print the required number of events to reject the null
# given the sample size required
(Res1 <- paste(critical1[SampSize1] + 1, "out of", N[SampSize1]))
(Res2 <- paste(critical1[SampSize2] + 1, "out of", N[SampSize2]))
(Res3 <- paste(critical2[SampSize3] + 1, "out of", N[SampSize3]))
(Res4 <- paste(critical2[SampSize4] + 1, "out of", N[SampSize4]))


#----------------------------------------------------------------------------------------------------
##prepare a table of power and N

y<- x<- matrix(NA, 2,4)

p1x <- c(p01, p02)
p2x <- c(p1, p1)


x[1,1]<-p1x[1]
x[2,1]<-p1x[2]
x[1,2]<-p2x[1]
x[2,2]<-p2x[1]

x[1,3]<- SampSize2
x[1,4]<-  SampSize1
x[2,3]<- SampSize4
x[2,4]<-  SampSize3




res <-as.data.frame(x)
names(res) <- c("Prob. under the Null","Prob. under the alternative","N, power 80%", "N, power 90%")

#=============================================================================================

 

power_df <- data.frame(sample_size = N,
                       power_1 = 1-beta1,
                       power_2 = 1-beta2
                       # power_3 = Res1,
                       # power_4 = Res2,
                       # power_5 = Res3,
                       # power_6 = Res3
)

#----------------------------------------------------------------------------------------------




##prepare a dataframe to plot important points 

x<- matrix(NA, 4,2)

x[1,1]<- SampSize2
x[2,1]<-  SampSize1
x[3,1]<- SampSize4
x[4,1]<-  SampSize3

x[1,2]<- power_df$power_1 [critical1[SampSize2]]
x[2,2]<- power_df$power_1 [critical1[SampSize1]]
x[3,2]<- power_df$power_2 [critical2[SampSize4]]
x[4,2]<- power_df$power_2 [critical2[SampSize3]]


x[1,2]<- power_df$power_1 [power_df$sample_size %in% SampSize2 ]
x[2,2]<- power_df$power_1 [power_df$sample_size %in% SampSize1 ]
x[3,2]<- power_df$power_2 [power_df$sample_size %in% SampSize4 ]
x[4,2]<- power_df$power_2 [power_df$sample_size %in% SampSize3 ]

a<-as.data.frame(x)



#----------------------------------------------------------------------------------

 x <- reshape2::melt(power_df, id.vars="sample_size")
  
 x$variable <- as.character(x$variable)
 
 x$variable[x$variable == "power_1"] <- "H0=0.37, H1=0.68"
 x$variable[x$variable == "power_2"] <- "H0=0.50, H1=0.68"
  
 
 
 legend_ord <- c("H0=0.37, H1=0.68",
                 "H0=0.50, H1=0.68" 
 )
 
  
 
all <-ggplot(x, aes(x = sample_size, y = value, #linetype=ratio,
                    #group = interaction(ratio, variable),
                    group = variable ,
                    colour = variable)) +
  geom_line() +
  #geom_point(alpha=0.3) +
  geom_point(data=a, inherit.aes = FALSE,
             aes(x=V1,y=V2), 
             color='black',
             size=3) + 
   
  scale_color_manual(breaks=legend_ord, values= c(
    
     
    "H0=0.37, H1=0.68" ="red", 
    "H0=0.50, H1=0.68" ="blue" 
    
    
  )) + 
  
   
 
  scale_y_continuous(breaks = seq(0, 1, by = 0.1),  limits = c(0,1)) +
  scale_x_continuous(breaks = seq(0,100,10), limits = c(0,100)) +
  theme_bw() +
  #  theme(legend.position="none") +
  theme( 
    plot.title=element_text(size = 12), plot.margin = unit(c(5.5,12,5.5,5.5), "pt"),
    legend.text=element_text(size=10),
    legend.title=element_text(size=10),
    legend.position="right",
    
    legend.key.size = unit(12, "pt"),  # legend horizontal bar length
    
    axis.text.x  = element_text(size=10),
    axis.text.y  = element_text(size=10),
    axis.line.x = element_line(color="black"),
    axis.line.y = element_line(color="black"),
    plot.caption=element_text(hjust = 0, size = 7),
    strip.text.x = element_text(size = 16, colour = "black", angle = 0),
    axis.title.y = element_text(size = rel(1), angle = 90),
    axis.title.x = element_text(size = rel(1), angle = 0 ),
    
    
    strip.background = element_rect(colour = "black", fill = "white"),
    panel.background = element_rect(fill = 'white', colour = 'white'),
    plot.background = element_rect(fill = 'white', colour = 'white')
  ) +
  
  geom_hline(yintercept=c(.8,.9), linetype="dotted", 
             color = "pink", linewidth=.7, alpha =1) +
  
  
  labs(y="Power", x = 'Total Sample Size', color = "Population probabilities", linetype="Ratio p1:p2" )+
  labs(caption = paste("- Alpha the type I assertion probability = 0.05 two sided. 1:1 randomisation.  Binomial exact power (or the sample size to achieve a given power) of one binomial to a hypothesised population value.\n- As the sample size increases, the power of the test generally increases, but it does so in a stepwise manner due to the discrete nature of the binomial distribution, hence power fluctuates as the sample size\n changes, resulting in a sawtooth pattern. Final calculations will be performed using PASS/nQuery. ref: xxxxxxxxxxx.R\n" )) +
  ggtitle(paste0("Power (or sample size) for exact test of a simple null hypothesis in a Bernoulli experiment") ) 


#https://stackoverflow.com/questions/15059093/ggplot2-adjust-the-symbol-size-in-legends
all <- all + guides(color = guide_legend(override.aes = list(linewidth = 3 )))

all



pdf("One sample binomial exact.pdf", width=10, height=6) # open an appropriate graphics device
print(all)
makeFootnote(paste(Sys.Date() )) 
makeFootnoteL("Page 1 of 2") 
dev.off()
 


require(gridExtra)
tab <- tableGrob(res)

tab <- grid.arrange(top="Total Sample Size for scenarios at 80% and 90% power", tableGrob(res))

pdf("power table.pdf", width=10, height=6)
grid.arrange(tab)
makeFootnote(paste(Sys.Date() )) 
makeFootnoteL("Page 2 of 2") 
dev.off()


# 
qpdf::pdf_combine(input = c("One sample binomial exact.pdf" ,
                          "power table.pdf"),
                    output = "20240229_Power_sample_size_request.pdf")  
# 


g <- all + annotation_custom(tableGrob(res, rows=NULL,
                                        theme = ttheme_default(base_size = 8)), 
xmin=2500, xmax=4000, ymin=0, ymax=0.4)


#----------------------------------------------------------------------------------------------------


pdf("test.pdf", width=10, height=6) # open an appropriate graphics device
print(g)
makeFootnote(paste(Sys.Date() )) 
makeFootnoteL("Page 1 of 2") 
dev.off()


g <-   ttheme_default(base_size = 8,
                                      core=list(
                                        fg_params = list(col = c("white","white")),
                                      
                                        bg_params = 
                                  list(fill=c( 
                                              "red","blue"  ))
                                      )
                                      
                                      )

all2 <- all + theme(legend.position = "none")





gg <- all2+ annotation_custom(tableGrob(res, rows=NULL,
                                      theme = g), 
                            xmin=50, xmax=90, ymin=0, ymax=0.2)

pdf("test.pdf", width=10, height=6) # open an appropriate graphics device
print(gg)
makeFootnote(paste(Sys.Date() )) 
makeFootnoteL("Page 1 of 1") 
dev.off()
                    
#----------------------------------------------------------------------------------------------------

                    
            



