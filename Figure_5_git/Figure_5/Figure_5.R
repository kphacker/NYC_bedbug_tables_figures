#going to run cosinor model using a nested tibble approach 
#https://drsimonj.svbtle.com/running-a-model-on-separate-groups
#https://jrnold.github.io/qss-tidy/uncertainty.html#linear-regression-model-with-uncertainty
#https://smu095.github.io/2019/02/16/2019-02-16-tidytuesday-fitting-multiple-time-series-models-using-purrr/

library(tidyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(broom)
library(purrr)
library(Rmisc)
library(modelr)
library(dplyr)

count_all <- read.csv("BB_complaints_borough_year_pop.csv")

time <- as.data.frame(unique(count_all$date))
time$num <- 1:65

xc<-cos(2*pi*time$num/12)
xs<-sin(2*pi*time$num/12)

#standardizing by population 
count_all$stand_count <- (count_all$count/count_all$population)*100000
count_all$date <- as.Date(paste(count_all$C_Day, count_all$C_Month, count_all$C_Year, sep = "/"), format = "%d/%m/%Y")

#looking at regression results - create table 

nested_data <- count_all %>% 
  group_by(borough) %>% 
  nest()

nested_models <- nested_data %>% 
  mutate(model = map(data, ~ lm(stand_count ~ date + xc + xs, data = .x)))

estimates <- nested_models %>% 
  mutate(coefs = map(model, tidy, conf.int = TRUE)) %>% 
  unnest(coefs)

#export estimates for table 

model_e <- estimates[c(1,4:10)]
model_e <- as.data.frame(model_e)

write.csv(model_e, "nested_model_results_n.csv")

#getting r^2

nested_models %>% 
  mutate(metrics = map(model, glance)) %>% 
  unnest(metrics) %>% 
  select(borough:r.squared) %>% 
  arrange(desc(r.squared)) # Arranging by "best" R squared

#only extracting seasonality rather than full model

nested_models_2 <- nested_data %>% 
  mutate(model = map(data, ~ lm(stand_count ~ xc + xs, data = .x)))

nested_models_2 <- nested_models_2 %>% 
  mutate(
    resids = map2(data, model, add_residuals)
  ) 

resids <- unnest(nested_models_2, resids)

#simple residual plot

resids %>% 
  ggplot(aes(date, resid)) +
  geom_point(aes(group = borough), alpha = 1 / 3) + 
  geom_smooth(se = FALSE)

#faceting 

p <- resids %>% 
  ggplot(aes(date, resid, group = borough)) +
  geom_point(alpha = 1 / 3) + 
  facet_grid(borough~ .)

resids$C_Month_Date <- ymd(paste(resids$C_Year, resids$C_Month, resids$C_Day, sep="-"))

resids$C_Month_Date <- as.Date(resids$C_Month_Date, format = "%m/%d/%y")

#extracting geom_smooth equation 

lm_eqn = function(resids){
  m = lm(resid ~ C_Month_Date, resids);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(as.numeric(coef(m)[1], digits = 2)), 
                        b = format(as.numeric(coef(m)[2], digits = 2)), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

#applying linear model with equation per borough

eq <- ddply(resids,.(borough),lm_eqn)

#labled graph of residuals 

p <- ggplot(data = resids, aes(x = C_Month_Date, y = resid)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point()

p1 <- p + geom_text(data=eq,aes(x = as.Date("2019-01-01"), y = 19,label=V1), parse = TRUE, inherit.aes=FALSE) + facet_grid(borough~.)


#putting R^2 and slope on individual facets 
#def a better way of organizing text annotation - but this works

ann_text_1 <- data.frame(stand_count = 19, date = as.Date("2019-06-01"),lab = "R-squared = 0.76",
                         borough = factor("BRONX",levels = c("BRONX","BROOKLYN","MANHATTAN","QUEENS","STATEN ISLAND")))
ann_text_2 <- data.frame(stand_count = 19, date = as.Date("2019-06-01"),lab = "R-squared = 0.82",
                         borough = factor("BROOKLYN",levels = c("BRONX","BROOKLYN","MANHATTAN","QUEENS","STATEN ISLAND")))
ann_text_3 <- data.frame(stand_count = 19, date = as.Date("2019-06-01"),lab = "R-squared = 0.75",
                         borough = factor("MANHATTAN",levels = c("BRONX","BROOKLYN","MANHATTAN","QUEENS","STATEN ISLAND")))
ann_text_4 <- data.frame(stand_count = 19, date = as.Date("2019-06-01"),lab = "R-squared = 0.71",
                         borough = factor("QUEENS",levels = c("BRONX","BROOKLYN","MANHATTAN","QUEENS","STATEN ISLAND")))
ann_text_5 <- data.frame(stand_count = 19, date = as.Date("2019-06-01"),lab = "R-squared = 0.37",
                         borough = factor("STATEN ISLAND",levels = c("BRONX","BROOKLYN","MANHATTAN","QUEENS","STATEN ISLAND")))

ann_text <- rbind(ann_text_1, ann_text_2, ann_text_3, ann_text_4, ann_text_5)

#adding residual slope text 

ann_text1 <- data.frame(stand_count = 15, date = as.Date("2019-06-01"),lab = "slope = -3.4e3",
                        borough = factor("BRONX",levels = c("BRONX","BROOKLYN","MANHATTAN","QUEENS","STATEN ISLAND")))
ann_text2 <- data.frame(stand_count = 15, date = as.Date("2019-06-01"),lab = "slope = -3.5e3",
                        borough = factor("BROOKLYN",levels = c("BRONX","BROOKLYN","MANHATTAN","QUEENS","STATEN ISLAND")))
ann_text3 <- data.frame(stand_count = 15, date = as.Date("2019-06-01"),lab = "slope = -2.9e3",
                        borough = factor("MANHATTAN",levels = c("BRONX","BROOKLYN","MANHATTAN","QUEENS","STATEN ISLAND")))
ann_text4 <- data.frame(stand_count = 15, date = as.Date("2019-06-01"),lab = "slope = -1.2e3",
                        borough = factor("QUEENS",levels = c("BRONX","BROOKLYN","MANHATTAN","QUEENS","STATEN ISLAND")))
ann_text5 <- data.frame(stand_count = 15, date = as.Date("2019-06-01"),lab = "slope = -1.1e3",
                        borough = factor("STATEN ISLAND",levels = c("BRONX","BROOKLYN","MANHATTAN","QUEENS","STATEN ISLAND")))

ann_text_slope <- rbind(ann_text1, ann_text2, ann_text3, ann_text4, ann_text5)

#Figure_5 

tiff("figure_5_bb.tiff", units="in", width=7, height=7, res=1000)

count_all %>% 
  nest(-borough) %>% 
  mutate(borough = factor(borough),
         fit = map(data, ~ lm(stand_count ~ date + xc + xs, data = .)),
         results = map(fit, augment)) %>% 
  unnest(results) %>% 
  ggplot(aes(x = date, y = stand_count)) +
  geom_line(color='red', aes(x = date, y= .fitted )) +
  geom_point() +
  facet_grid(borough ~ .) +
  labs(x = "Date", y = "Bed Bug Complaints \n complaints / borough population  * 100,000") +
  geom_text(data = ann_text,label = ann_text$lab) +
  geom_text(data = ann_text_slope,label = ann_text_slope$lab) +
  theme_bw()

dev.off()


#looking at regression results - create table for Table 3 

nested_data <- count_all %>% 
  group_by(borough) %>% 
  nest()

nested_models <- nested_data %>% 
  mutate(model = map(data, ~ lm(stand_count ~ date + xc + xs, data = .x)))

estimates <- nested_models %>% 
  mutate(coefs = map(model, tidy, conf.int = TRUE)) %>% 
  unnest(coefs)

#export estimates for table 

model_e <- estimates[c(1,4:10)]
model_e <- as.data.frame(model_e)

write.csv(model_e, "nested_model_results_n.csv")

#getting r^2

nested_models %>% 
  mutate(metrics = map(model, glance)) %>% 
  unnest(metrics) %>% 
  select(borough:r.squared) %>% 
  arrange(desc(r.squared)) # Arranging by "best" R squared


