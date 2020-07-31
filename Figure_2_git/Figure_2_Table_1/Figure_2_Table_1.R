#standarized Figure 3 

setwd("~/Dropbox/BedBugs/NYC_bugs/bed_bugs_codesync/Databases")

library(ggplot2)
library(gridExtra)
library(grid)
library(broom)

stan <- read.csv("311_calls_complaints_standardaized.csv")
stan$num <- 1:65

#multiplying by 100,000 to help with decimals 

stan$stan_calls <- stan$stan_calls*100000
stan$r_stan_calls <- stan$r_stan_calls*100000

#sine curve equation resolving 12 months 
xc<-cos(2*pi*stan$num/12)
xs<-sin(2*pi*stan$num/12)

#other variables 
Time_bb <- as.Date(stan$C_Month_Year, format = "%m/%d/%y")
Count1_bb <- stan$stan_calls

fit.lm_bb <- lm(Count1_bb~xc+xs)

#complies confidence intervals 
tidy(fit.lm_bb, conf.int = TRUE)

#creating model fit to examine residuals 
fit_bb <- fitted(fit.lm_bb)  
fit.res_bb = resid(fit.lm_bb)

#complete model 
cos.m_bb <- lm(Count1_bb ~ Time_bb + xc + xs)
tidy(cos.m_bb, conf.int = TRUE)

#fitted complete model 
cos.f_bb <- fitted(cos.m_bb) 

#calculating amplitude and theta 
#https://www.le.ac.uk/users/dsgp1/COURSES/TSERIES/2CYCLES.PDF
#α = ρ cos θ, β = ρ sin θ, and α2 + β2 = ρ2.
#ρ is the amplitude,
#ω is the angular velocity or frequency and
#θ is the phase displacement
#alpha = "cosine variable" - xc 
#beta = "sine variable" - xs 

#calculating amplitude and theta 
#amplitude 

sqrt(1.42^2 + 2.23^2)
acos(1.42/2.64)

##################roaches#################################### 
#starting raw cosinor analysis - not from package 

Time_r <- as.Date(stan$C_Month_Year, format = "%m/%d/%y")
Count1_r <- stan$r_stan_calls

xc<-cos(2*pi*Time_r/12)
xs<-sin(2*pi*Time_r/12)
fit.lm_r <- lm(Count1_r~xc+xs)

##creating model fit to examine residuals 
fit_r <- fitted(fit.lm_r)  
fit.res_r = resid(fit.lm_r)

#now running full model 
cos.m_r <- lm(Count1_r ~ Time_r + xc + xs)

tidy(cos.m_r, conf.int = TRUE)

cos.f_r <- fitted(cos.m_r)  

#creating figure using ggplot 

fit.lm_bb <- lm(Count1_bb~xc+xs)
fit.lm_r <- lm(Count1_r~xc+xs)

stan$fit_bb <- fitted(fit.lm_bb)  
stan$fit.res_bb <- resid(fit.lm_bb)

stan$fit_r <- fitted(fit.lm_r)  
stan$fit.res_r <- resid(fit.lm_r)

#first
p1 <- ggplot(stan, aes(x = Time_bb, y = stan_calls) ) +
  geom_line(data = stan, color = 'red', aes(y = fit_bb)) +
  geom_point() +
  labs(x = "Date", y = "Bed Bug Complaints \n complaints / all inquiries * 100,000") +
  theme_bw(base_size = 10) +
  ggtitle("Monthly Bed Bug Complaints \nwith Cosine Curve") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size = 10))

p2 <- ggplot(stan, aes(x = Time_r, y = r_stan_calls) ) +
  geom_line(data = stan, color = 'red', aes(y = fit_r)) +
  geom_point() +
  labs(x = "Date", y = "Cockroach Complaints \n complaints / all inquiries * 100,000") +
  theme_bw(base_size = 10) +
  ggtitle("Monthly Cockroach Complaints \nwith Cosine Curve") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size = 10))

#second 
p3 <- ggplot(stan, aes(x = Time_bb, y = fit.res_bb) ) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(x = "Date", y = "Bed Bug Complaints \n complaints / allinquiries * 100,000") + 
  theme_bw(base_size = 10) +
  ggtitle("Bed Bug Residual Pattern \nExtracting Cosine Curve") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size = 10))

p4 <- ggplot(stan, aes(x = Time_r, y = fit.res_r) ) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(x = "Date", y = "Cockroach Complaints \n complaints / all inquiries * 100,000") +
  theme_bw(base_size = 10) +
  ggtitle("Cockroach Residual Pattern \nExtracting Cosine Curve") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size = 10))

#third 
cos.m_bb <- lm(Count1_bb ~ Time_bb + xc + xs)
cos.f_bb <- fitted(cos.m_bb)  

cos.m_r <- lm(Count1_r ~ Time_r + xc + xs)
cos.f_r <- fitted(cos.m_r) 

stan$fit_m_r = cos.f_r
stan$fit_m <- cos.f_bb

p5 <- ggplot(stan, aes(x = Time_bb, y = stan_calls) ) +
  geom_line(data = stan, color = 'red', aes(y = fit_m)) +
  geom_point() +
  labs(x = "Date", y = "Bed Bug Complaints \n complaints / all inquiries * 100,000") +
  theme_bw(base_size = 10) +
  ggtitle("Complete Fitted Model: \nBed Bug Complaints") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size = 10))

p6 <- ggplot(stan, aes(x = Time_r, y = r_stan_calls) ) +
  geom_line( color = 'red', aes(y = fit_m_r)) +
  geom_point() +
  labs(x = "Date", y = "Cockroach Complaints \n complaints / all inquiries * 100,000") +
  theme_bw(base_size = 10) +
  ggtitle("Complete Fitted Model: \nCockroach Complaints") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size = 10))

#creating corner labels 

p1 <- arrangeGrob(p1, top = textGrob("A", x = unit(0, "npc")
                                     , y   = unit(1, "npc"), just=c("left","top"),
                                     gp=gpar(col="black", fontsize = 12)))

p3 <- arrangeGrob(p3, top = textGrob("C", x = unit(0, "npc")
                                     , y = unit(1, "npc"), just=c("left","top"),
                                     gp=gpar(col="black", fontsize = 12)))

p5 <- arrangeGrob(p5, top = textGrob("E", x = unit(0, "npc")
                                     , y  = unit(1, "npc"), just=c("left","top"),
                                     gp=gpar(col="black", fontsize = 12)))

p2 <- arrangeGrob(p2, top = textGrob("B", x = unit(0, "npc")
                                     , y = unit(1, "npc"), just=c("left","top"),
                                     gp=gpar(col="black", fontsize = 12)))

p4 <- arrangeGrob(p4, top = textGrob("D", x = unit(0, "npc")
                                     , y = unit(1, "npc"), just=c("left","top"),
                                     gp=gpar(col="black", fontsize = 12)))

p6 <- arrangeGrob(p6, top = textGrob("F", x = unit(0, "npc")
                                     , y = unit(1, "npc"), just=c("left","top"),
                                     gp=gpar(col="black", fontsize = 12)))

tiff("figure_4_bb.tiff", units="in", width=7, height=7, res=1000)

grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)

dev.off()
