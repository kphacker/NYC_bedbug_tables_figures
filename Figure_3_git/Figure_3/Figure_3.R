#Figure 3

#calculating standarized proportions per year and borough with CI 

setwd("~/Dropbox/BedBugs/NYC_bugs/bed_bugs_codesync/Databases")

x <- read.csv("proportion_CI.csv")

library(binom)

# Get the proportion
x$p_hat = x$count/x$pop * 100000

# Compute the CI

x <- cbind(binom.confint(x$count, x$pop, methods = "logit"), x)

x$mean <- signif(x$mean * 100000, digits = 4)
x$lower <- signif(x$lower * 100000, digits = 4)
x$upper <- signif(x$upper * 100000, digits = 4)

#don't know why it took me so long to think of this

x$CI <- paste(x$lower, x$upper, sep = ", ")
x$CI <- paste("(", x$CI, ")", sep = "")
x$all <- paste(x$mean, x$CI, sep = " ± ")

#write.csv(x, "proportions_CI.csv")

#creating plot 
library(ggplot2)

sub <- subset(x, year == 2014 | year == 2015 |year == 2016 |year == 2017 | year == 2018 |year == 2019 )

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

tiff("figure_3_bb.tiff", units="in", width=4.5, height=4.5, res=1000)

p1 <- ggplot(sub, aes(y = mean, x = year, fill = borough)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper),colour= "#5c5a59", width=.3,position=position_dodge(.9)) +
  labs(title = "Bed Bug Complaints per Borough and Year", 
       y = "Proportion Complaints per Population per 100000", x = "Year")+
  scale_fill_manual(values=cbPalette)+
  labs(fill = "Borough")+
  theme_bw()

p1 <- p1 + theme(plot.title=element_text(hjust = 0.5),  # title
                 axis.text.x=element_text(angle = 0, hjust = 0.5),  # X axis text
                 axis.text.y=element_text(hjust = 1),
                 panel.grid.major = element_blank(),
                 legend.justification = c(0.99,0.99), legend.position = c(0.99, 0.99),
                 legend.key.size = unit(0.5, "cm"))

p1 + theme(text=element_text(size=10))


dev.off()




