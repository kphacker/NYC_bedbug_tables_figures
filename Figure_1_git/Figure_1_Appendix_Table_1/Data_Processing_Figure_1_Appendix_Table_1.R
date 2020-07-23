#Call center inquiries data formating. Call center inquires in its raw data format 
#can be downloaded from: https://data.cityofnewyork.us/City-Government/311-Call-Center-Inquiry/tdd6-3ysr
#We used all data from the beginning of the dataset through the end of 2019 - herein called "311_Call_Center_Inquiry.csv" 
#the raw data was too large to include on github but can be dowloaded directly from NYC Open Data

#reloading 311 inquiries 

library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)

filepath <- "/Dropbox/BedBugs/NYC_bugs/bed_bugs_codesync/Databases"
inquries <- read_csv(paste0(filepath, "311_Call_Center_Inquiry.csv"))

inquries <- read_csv("311_Call_Center_Inquiry.csv")
saveRDS(inquries,"NYC_311_2010_2020.rds")

DB_311 <- inquries %>%
  mutate(C_Date = lubridate::mdy(DATE)) %>% 
  mutate(C_Year = lubridate::year(C_Date)) %>% 
  mutate(C_Month = lubridate::month(C_Date)) %>% 
  mutate(C_Month_Date = paste(C_Month, C_Year, sep="-"))

DB_311$dv <- "TRUE"
month_311 <- DB_311 %>% group_by(C_Year, C_Month, dv) %>% summarise(count=n())
month_311 <- month_311 %>% mutate(C_Month_Year = as.Date(paste(C_Year,C_Month, "01", sep="-")))
month_311 <- as.data.frame(month_311)
head(month_311)

write.csv(month_311, "month_311_calls_all_2010_2019_RD.csv")

bb_terms <- c("bed bug", 
              "bed bugs", 
              "bedbug",
              "bedbugs",
              "Bed bug",
              "Bed bugs",
              "Bedbug",
              "Bedbugs")

DB_311$bb <- sapply(DB_311$BRIEF_DESCRIPTION, function(x) any(str_detect(x, pattern = bb_terms)))
head(DB_311)

table(DB_311$bb)

NYC_BB <- subset(DB_311, bb == "TRUE")

#saveRDS(NYC_BB,"NYC_311_bb_2010_2020.rds")
#write.csv(NYC_BB, "NYC_311_bb_2010_2020.csv")

#need to subset by date 

#information for table 

BB_311 <- read.csv("NYC_311_bb_2010_2020.csv")

table(BB_311$AGENCY, BB_311$BRIEF_DESCRIPTION) #results are for Table 1

BB_311 <- BB_311 %>%
  mutate(C_Date = lubridate::mdy(DATE)) %>% 
  mutate(C_Year = lubridate::year(C_Date)) %>% 
  mutate(C_Month = lubridate::month(C_Date)) %>% 
  mutate(C_Month_Date = paste(C_Month, C_Year, sep="-"))

BB_311$dv <- "TRUE"

BB_month_311 <- BB_311 %>% group_by(C_Year, C_Month, dv) %>% summarise(count=n())
BB_month_311 <- BB_month_311 %>% mutate(C_Month_Year = as.Date(paste(C_Year,C_Month, "01", sep="-")))

BB_month_311 <- as.data.frame(BB_month_311)
BB_month_311 <- BB_month_311[-c(110), ]

write.csv(BB_month_311, "month_311_calls_BB_2010_2019_RD.csv")

