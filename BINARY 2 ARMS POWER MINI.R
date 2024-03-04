# Eamonn extended proportions plot to show sample sizes needed for small differences 20Feb2024
# prepare pdf out put for communication

# proportions ----------------------------------------------------------------------------
#https://www.r-bloggers.com/2009/02/r-good-practice-%e2%80%93-adding-footnotes-to-graphics/
rm(list=ls())
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





# Define the parameters of the power calculation
alpha <- 0.05

#ration <- 1/3

# Calculate the power for a range of sample sizes
sample_sizes <- seq(10, 400, by = 10)
power_values_1 <- bpower(n = sample_sizes, p1 = .68, p2 = .37, alpha = alpha)
power_values_2 <- bpower(n = sample_sizes, p1 = .68, p2 = .50, alpha = alpha)

# power_values_5 <- bpower(n = sample_sizes, p1 = .25, p2 = .20, alpha = alpha)
# power_values_6 <- bpower(n = sample_sizes, p1 = .25, p2 = .15, alpha = alpha)
# 
# power_values_3 <- bpower(n = sample_sizes, p1 = .35, p2 = .30, alpha = alpha)
# power_values_4 <- bpower(n = sample_sizes, p1 = .35, p2 = .25,  alpha = alpha)
# 
# power_values_5b <- bpower(n = sample_sizes, p1 = .30, p2 = .25,  alpha = alpha)

# Create a data frame containing the sample sizes and power values
power_df <- data.frame(sample_size = sample_sizes, 
                       power_1 = power_values_1, 
                       power_2 = power_values_2#,
                       # power_3 = power_values_3, 
                       # power_4 = power_values_4,
                       # power_5 = power_values_5, 
                       # power_5b = power_values_5b, 
                       # power_6 = power_values_6
)
 


 

x <- reshape2::melt(power_df, id.vars="sample_size")
 
x$variable <- as.character(x$variable)

x$variable[x$variable == "power_1"] <- "p1=0.68, p2=0.37"
x$variable[x$variable == "power_2"] <- "p1=0.68, p2=0.50"
# x$variable[x$variable == "power_3"] <- "p1=0.35, p2=0.30"
# x$variable[x$variable == "power_4"] <- "p1=0.35, p2=0.25"
# x$variable[x$variable == "power_5"] <- "p1=0.25, p2=0.20" 
# x$variable[x$variable == "power_5b"]<- "p1=0.30  p2=0.25"
# x$variable[x$variable == "power_6"] <- "p1=0.25, p2=0.15" 
#   

legend_ord <- c("p1=0.68, p2=0.37",
                "p1=0.68, p2=0.50"#,
                # "p1=0.35, p2=0.25",
                # "p1=0.20, p2=0.15",
                # "p1=0.25, p2=0.20",
                # "p1=0.30  p2=0.25",
                # "p1=0.35, p2=0.30"
)

 
all <-ggplot(x, aes(x = sample_size, y = value, #linetype=ratio,
                    #group = interaction(ratio, variable),
                    group = variable ,
                    colour = variable)) +
  geom_line() +
   
  scale_color_manual(breaks=legend_ord, values= c(
    
    "p1=0.68, p2=0.37" ="red", 
    "p1=0.68, p2=0.50" ="blue" #,
    # "p1=0.35, p2=0.30" ="black",
    # "p1=0.20, p2=0.15" ="purple",
    # "p1=0.25, p2=0.20" ="springgreen2",      
    # "p1=0.30  p2=0.25" ="red", 
    # "p1=0.35, p2=0.25" ="blue" 
    # 
    
  )) + 
  
   
 
  scale_y_continuous(breaks = seq(0, 1, by = 0.1),  limits = c(0,1)) +
  scale_x_continuous(breaks = seq(0,400,20), limits = c(0,400)) +
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
  labs(caption = paste("- Alpha the type I assertion probability = 0.05 two sided. 1:1 randomisation.\n- Uses method of Fleiss, Tytun, and Ury (but without the continuity correction) to estimate the power (or the sample size to achieve a given power) of a two-sided test for the difference in two proportions. \n- Fleiss JL, Tytun A, Ury HK (1980): A simple approximation for calculating sample sizes for comparing independent proportions. Biometrics 36:343–6.\n- Final calculations will be performed using PASS/nQuery. ref: BINARY 2 ARMS POWER MINI.R" )) +
  ggtitle(paste0("Power (or sample size) for difference in two proportions (p1, p2) for evaluation of treatment effect, binary outcome measure.") ) 


#https://stackoverflow.com/questions/15059093/ggplot2-adjust-the-symbol-size-in-legends
all <- all + guides(color = guide_legend(override.aes = list(linewidth = 3 )))

all



pdf("Mortality reduction proportions2.pdf", width=10, height=6) # open an appropriate graphics device
print(all)
makeFootnote(paste(Sys.Date() )) 
makeFootnoteL("Page 1 of 2") 
dev.off()
 
#----------------------------------------------------------------------------------------------------
##prepare a table of power and N
 
y<- x<- matrix(NA, 2,4)

p1 <- c(0.68,0.68)#, 0.35, 0.2, 0.25, 0.3,0.35)
p2 <- c(0.37,0.50)#, 0.25, 0.15, 0.2, 0.25,0.30)


for (i in 1:length(p1)) {
  
  pow<- 0.8
  
  r <- bsamsize(p1=p1[i], p2=p2[i], fraction=.5, alpha=.05, power=0.8)
  
  x[i,1] <- p1[i] 
  x[i,2] <- p2[i]
  x[i,4] <- ceiling(sum(r))
  x[i,3] <- 0.8
  
  r <- bsamsize(p1=p1[i], p2=p2[i], fraction=.5, alpha=.05, power=0.9)
  
  y[i,1] <- p1[i] 
  y[i,2] <- p2[i]
  y[i,4] <- ceiling(sum(r))
  y[i,3] <- 0.9
  
}

res <-as.data.frame(rbind(x,y))
names(res) <- c("p1","p2","power", "Total sample size")
 
A <- res[res$power == 0.8,]
B <- res[res$power == 0.9,]

names(A) <- c("p1", "p2", "power", "N, power 80%")
A$power <- NULL
names(B) <- c("p1", "p2", "power", "N, power 90%")
B$power <- NULL
 
# maintains order
res <- plyr::join(A,B)



require(gridExtra)
tab <- tableGrob(res)

tab <- grid.arrange(top="Total Sample Size for mortality scenarios at 80% and 90% power", tableGrob(res))

pdf("power table.pdf", width=10, height=6)
grid.arrange(tab)
makeFootnote(paste(Sys.Date() )) 
makeFootnoteL("Page 2 of 2") 
dev.off()


# 
qpdf::pdf_combine(input = c("Mortality reduction proportions2.pdf" ,
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
                                        fg_params = list(col = c("white","white" )),
                                      
                                        bg_params = 
                                  list(fill=c("red","blue" ))
                                      )
                                      
                                      )

all2 <- all + theme(legend.position = "none")

gg <- all2+ annotation_custom(tableGrob(res, rows=NULL,
                                      theme = g), 
                            xmin=250, xmax=400, ymin=0, ymax=0.4)

pdf("test.pdf", width=10, height=6) # open an appropriate graphics device
print(gg)
#makeFootnote(paste(Sys.Date() )) 
#makeFootnoteL("Page 1 of 2") 
dev.off()
                    
#----------------------------------------------------------------------------------------------------

                 