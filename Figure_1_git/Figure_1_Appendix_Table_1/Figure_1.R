#code and databases used to create Figure 1 

library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)

month_311 <- read.csv("month_311_calls_all_2010_2019_RD.csv")
BB_month_311 <- read.csv("month_311_calls_BB_2010_2019_RD.csv")

BB_month_311$type <- "Bed Bug Inquiries"
month_311$type <- "All Inquiries"

stan <- BB_month_311$count/month_311$count

stan <- as.data.frame(stan)
names(stan)[names(stan) == "stan"] <- "count"

stan$type <- "Standardized Bed Bug Inquiries"
stan$C_Month_Year <- as.factor(BB_month_311$C_Month_Year)

BB_month_311 <- BB_month_311[c(5,7,6)]
month_311 <- month_311[c(4,6,5)]

all <- rbind(BB_month_311, month_311, stan)

#struturing Date 

all <- all %>% separate(C_Month_Year, 
                c("Month", "Day", "Year"))
all$Year <- as.numeric(all$Year) 
all$Day <- as.numeric(all$Day) 
all$Month <- as.numeric(all$Month) 
all$Year <- all$Year + 2000 

all <- all %>% mutate(C_Month_Year = as_date(paste(Year, Month, Day, sep="-")))

#plotting 

mycols <- c("#0a8bf5", "#f20a0a", "#3e1475")

#Figure 1 including standardized 
tiff("figure_1_bb.tiff", units="in", width=7, height=7, res=1000)
# insert ggplot code
g <- ggplot(all) +
  geom_line(aes(x = C_Month_Year, y = count, color = type)) +
  scale_color_manual(values = mycols) + 
  geom_vline(xintercept = as.numeric(as.Date("2010-08-31")), linetype=4) +
  geom_vline(xintercept = as.numeric(as.Date("2017-11-6")), linetype=4) +
  facet_grid(type ~ ., scales = "free_y") +
  theme(strip.background = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_text(size = rel(1))) +
  guides(color = FALSE) +
  labs(x = "Date", y = "Count of Inquiries per Month") +
  theme_bw(base_size = 10) +
  ggtitle("All 311 Inquires and Bed Bug Specific 311 Inquiries from 2010 - 2019") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size = 10)) 

ann_text <- data.frame(C_Month_Year = c("2010-07-1", "2017-09-1") , count = c(7e+05, 9e+05),
                       type = factor("All Inquiries",levels = c("All Inquiries", "Bed Bug Inquiries", "Standardized Bed Bug Inquires")),
                       label=c("Disclosure Law 1", "Disclosure Law 2"))

g + geom_text(data = ann_text, aes(x = as.Date(C_Month_Year, "%Y-%m-%d"), y = count), label=ann_text$label,
              angle = 90, hjust = 1, size = 3)

label_text <- data.frame(C_Month_Year = c("2010-07-1", "2017-09-1") , count = c(7e+05, 9e+05),
                         type = factor("All Inquiries",levels = c("All Inquiries", "Bed Bug Inquiries")),
                         label=c("Disclosure Law 1", "Disclosure Law 2"))

dev.off()
