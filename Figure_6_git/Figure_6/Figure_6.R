#Preprocessing for this data includes identity information, herein we summarize the total number 
#of compliants and building owner reports aggregated to the NTA level. 

#assessing the concordance betweewn 2018 landlords and complaints 

library(ggpubr)
library(ggplot2)
library(ggrepel)
library(ggpmisc)
library(grid)
library(gridExtra)


bb_2018 <- read.csv("NYC_NTA_2018.csv")

names(bb_2018)[12:13] <- c("raw_landlord","stan_landlord")

bb_2018$Borough <- bb_2018$BoroName


# The palette with black colorblind 

cbp2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

sc <- ggplot(data = bb_2018, aes(raw_landlord, BB_2018, label = NTAName)) +
  geom_point(aes(col = Borough), size = 1) + #this line keeps the entire regression line rather than by boro
  scale_x_continuous(limits = c(0,201), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,205), expand = c(0, 0)) +
  labs(x = "Bed bug infestations reported by building owners", 
       y = "Official bed bug complaints from residents using 311") +
  scale_color_manual(values = cbp2) +
  stat_dens2d_filter(geom = "text_repel", keep.fraction = 0.1, size = 2) +
  geom_smooth(method='lm', col = "black") + 
  theme_bw(base_size = 6) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size = 6)) 


#Modifying theme components 

sc <- sc + theme(plot.title=element_text(size = 6, hjust = 0.5),  # title
                 axis.text.x=element_text(size = 6),  # X axis text
                 axis.text.y=element_text(size = 6, hjust = 1),
                 panel.grid.major = element_blank(),
                 legend.text=element_text(size=6),
                 legend.justification = c(0.05, 0.99), legend.position = c(0.03, 0.99),
                 legend.key.size = unit(0.5, "cm")) +
  ggsave("figure_6_bb.tiff", height=3.42, width=3.42, units='in', dpi=1000)


#faceting by borough

ggplot(data = bb_2018, aes(raw_landlord, BB_2018, label = NTAName))+
  geom_point() +
  labs(x = "Submitted landlord bed bug complaints", 
       y = "Bed bug complaints from residents using 311",
       title = "Bed bug complaints by borough for New York City landlords vs. residents: 2018") +
  stat_dens2d_filter(geom = "text_repel", keep.fraction = 0.19) +
  geom_smooth(method='lm', col = "black") +
  theme_bw() +
  facet_wrap(~Borough,  ncol=1, strip.position = "right")


#individual plots by borough 

ma <- subset(bb_2018, Borough == "Manhattan")
bx <- subset(bb_2018, Borough == "Bronx")
qu <- subset(bb_2018, Borough == "Queens")
br <- subset(bb_2018, Borough == "Brooklyn")
si <- subset(bb_2018, Borough == "Staten Island")

#manhattan 

ma$inside = rep(1,nrow(ma))

lm1 <- lm(BB_2018 ~ raw_landlord, data = ma)
p1 <- predict.lm(lm1,level= 0.95,interval="confidence")
ma$inside[ma$BB_2018 < p1[,2] | ma$BB_2018 > p1[,3]] = 0

ma$inside_lab <- ifelse(ma$inside == 0, as.character(ma$NTAName), "") 


ma_p <- ggplot(data = ma, aes(raw_landlord, BB_2018, label = inside_lab)) +
  geom_point(aes()) + #this line keeps the entire regression line rather than by boro
  labs(x = "Submitted landlord bed bug complaints", 
       y = "Bed bug complaints from residents using 311",
       title = "Bed bug complaints for New York City landlords vs. residents: Manhattan") +
  geom_smooth(method='lm', col = "black", level = 0.95) + 
  geom_text_repel() +
  theme_bw()

#Modifying theme components 

ma_p <- ma_p + theme(plot.title=element_text(size = 10, hjust = 0.5),  # title
                     axis.text.x=element_text(size = 10),  # X axis text
                     axis.text.y=element_text(size = 10, hjust = 1))

#brooklyn 

#subsetting to exclude really high point (will note on graph)
br <- subset(br, br$raw_landlord < 100)
br$inside = rep(1,nrow(br))

lm2 <- lm(BB_2018 ~ raw_landlord, data = br)
p2 <- predict.lm(lm2,level= 0.95,interval="confidence")
br$inside[br$BB_2018 < p2[,2] | br$BB_2018 > p2[,3]] = 0

br$inside_lab <- ifelse(br$inside == 0, as.character(br$NTAName), "") 


br_p <- ggplot(data = br, aes(raw_landlord, BB_2018, label = inside_lab)) +
  geom_point(aes()) + #this line keeps the entire regression line rather than by boro
  labs(x = "Submitted landlord bed bug complaints", 
       y = "Bed bug complaints from residents using 311",
       title = "Bed bug complaints for New York City landlords vs. residents: Brooklyn") +
  geom_smooth(method='lm', col = "black", level = 0.95) + 
  geom_text_repel() +
  theme_bw()

#Modifying theme components 

br_p <- br_p + theme(plot.title=element_text(size = 10, hjust = 0.5),  # title
                     axis.text.x=element_text(size = 10),  # X axis text
                     axis.text.y=element_text(size = 10, hjust = 1))

grob <- grobTree(textGrob("Notable Outlier Points Not Included: \n Prospect Gardens-Windgate (120,149) \n Crown Heights North (148, 200)", 
                          x=0.01,  y=0.95, hjust=0,
                          gp=gpar(fontsize=8)))
# Plot
br_p + annotation_custom(grob)

#queens 

#subsetting to exclude really high point (will note on graph)
qu <- subset(qu, qu$raw_landlord < 56)
qu <- subset(qu, qu$BB_2018 < 60)
qu$inside = rep(1,nrow(qu))

lm2 <- lm(BB_2018 ~ raw_landlord, data = qu)
p2 <- predict.lm(lm2,level= 0.95,interval="confidence")
qu$inside[qu$BB_2018 < p2[,2] | qu$BB_2018 > p2[,3]] = 0

qu$inside_lab <- ifelse(qu$inside == 0, as.character(qu$NTAName), "") 


qu_p <- ggplot(data = qu, aes(raw_landlord, BB_2018, label = inside_lab)) +
  geom_point(aes()) + #this line keeps the entire regression line rather than by boro
  labs(x = "Submitted landlord bed bug complaints", 
       y = "Bed bug complaints from residents using 311",
       title = "Bed bug complaints for New York City landlords vs. residents: Queens") +
  geom_smooth(method='lm', col = "black", level = 0.95) + 
  geom_text_repel() +
  theme_bw()

#Modifying theme components 

qu_p <- qu_p + theme(plot.title=element_text(size = 10, hjust = 0.5),  # title
                     axis.text.x=element_text(size = 10),  # X axis text
                     axis.text.y=element_text(size = 10, hjust = 1))

grob <- grobTree(textGrob("Notable Outlier Points Not Included: \n Woodhaven (8,67) \n Hunters Point-Sunnyside-West Maspeth (84,38)", 
                          x=0.01,  y=0.95, hjust=0,
                          gp=gpar(fontsize=8)))
# Plot
qu_p + annotation_custom(grob)

#bronx 

bx$inside = rep(1,nrow(bx))

lm3 <- lm(BB_2018 ~ raw_landlord, data = bx)
p3 <- predict.lm(lm2,level= 0.95,interval="confidence")
bx$inside[bx$BB_2018 < p3[,2] | bx$BB_2018 > p3[,3]] = 0

bx$inside_lab <- ifelse(bx$inside == 0, as.character(bx$NTAName), "") 


bx_p <- ggplot(data = bx, aes(raw_landlord, BB_2018, label = inside_lab)) +
  geom_point(aes()) + #this line keeps the entire regression line rather than by boro
  labs(x = "Submitted landlord bed bug complaints", 
       y = "Bed bug complaints from residents using 311",
       title = "Bed bug complaints for New York City landlords vs. residents: Bronx") +
  geom_smooth(method='lm', col = "black", level = 0.95) + 
  geom_text_repel() +
  theme_bw()

#Modifying theme components 

bx_p <- bx_p + theme(plot.title=element_text(size = 10, hjust = 0.5),  # title
                     axis.text.x=element_text(size = 10),  # X axis text
                     axis.text.y=element_text(size = 10, hjust = 1))

#staten island 

si$inside = rep(1,nrow(si))

lm4 <- lm(BB_2018 ~ raw_landlord, data = si)
p4 <- predict.lm(lm4,level= 0.95,interval="confidence")
si$inside[si$BB_2018 < p4[,2] | si$BB_2018 > p4[,3]] = 0

si$inside_lab <- ifelse(si$inside == 0, as.character(si$NTAName), "") 


si_p <- ggplot(data = si, aes(raw_landlord, BB_2018, label = inside_lab)) +
  geom_point(aes()) + #this line keeps the entire regression line rather than by boro
  labs(x = "Submitted landlord bed bug complaints", 
       y = "Bed bug complaints from residents using 311",
       title = "Bed bug complaints for New York City landlords vs. residents: Staten Island") +
  geom_smooth(method='lm', col = "black", level = 0.95) + 
  geom_text_repel() +
  theme_bw()

#Modifying theme components 

si_p <- si_p + theme(plot.title=element_text(size = 10, hjust = 0.5),  # title
                     axis.text.x=element_text(size = 10),  # X axis text
                     axis.text.y=element_text(size = 10, hjust = 1))

#arranging plots 

all <- grid.arrange(ma_p, qu_p, br_p, bx_p, si_p, nrow = 2)
