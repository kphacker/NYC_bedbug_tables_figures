#creating dataset looking at the total number of bed bugs and cockroaches for each year standardized 
#by the total number of 311 calls for as well as a secondary variable which is standardized for the population

library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)

#repeat of data formating used for Figure One - the goal is to get the total number of 311 calls for
#each year to use as a standardization

filepath <- "/Dropbox/BedBugs/NYC_bugs/bed_bugs_codesync/Databases" #can be downloaded from: https://data.cityofnewyork.us/City-Government/311-Call-Center-Inquiry/tdd6-3ysr
inquries <- read_csv(paste0(filepath, "311_Call_Center_Inquiry.csv"))

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

#write.csv(month_311, "month_311_calls_all_2010_2019_RD.csv")

#subsetting to the timeline of the complaint data

DB_311 <- read.csv("month_311_calls_all_2010_2019_RD.csv")

DB_311$C_Month_Year <- as.Date(DB_311$C_Month_Year, format = '%Y-%m-%d')

DB_311 <- subset(DB_311, C_Month_Year >= as.Date("2014-07-01"))

#Getting bed bug and cockroach complaint data

com <- read.csv("Housing_Maintenance_Code_Complaints.csv") #avilible for download at:
#https://data.cityofnewyork.us/Housing-Development/Housing-Maintenance-Code-Complaints/uwyv-629c

cp <- read.csv("Complaint_Problems.csv") #availible for download at:
#https://data.cityofnewyork.us/Housing-Development/Complaint-Problems/a2nx-4u46

#subsetting for bedbugs specific complaints 
cp_BB <- cp %>% 
  filter(CodeID == 2517 | CodeID == 2818)

#adding georeferenced information from the Complaint_Problems.csv 
bb_com <- left_join(cp_BB, com, by = "ComplaintID")
bb_com_s <- bb_com[c(2,4,6,10,12,14,16,17,21:32)]
#write.csv(bb_com_s, "NYC_complaint_georef_short_2014_2019.csv")

bb_com_s <- read.csv("NYC_complaint_georef_short_2014_2019.csv")
bb_com_s$year <- substring(bb_com_s$ReceivedDate, 7, 10) 

#setting up timeseries data for BB complaints 

bb_com_s$first <- 1 
bb_com_s <- bb_com_s %>%
  mutate(ReceivedDate = lubridate::mdy(ReceivedDate)) %>% 
  mutate(C_Year = lubridate::year(ReceivedDate)) %>% 
  mutate(C_Month = lubridate::month(ReceivedDate)) %>% 
  mutate(C_Day = lubridate::day(ReceivedDate)) %>%
  mutate(C_Month_Date1 = ymd(paste(C_Year, C_Month, first, sep="-"))) %>%
  mutate(C_Month_Date = ymd(paste(C_Year, C_Month, C_Day, sep="-"))) %>%
  filter(C_Year > 2013)


bb_com_s$count <- 1
com_s <- bb_com_s[c(24, 25, 29)]


count_bb <- com_s %>%
  group_by(C_Month, C_Year) %>%
  summarise(com_bb = sum(count))

count_bb <- as.data.frame(count_bb)
names(count_bb) <- c("month", "year", "count")

count_bb$day <- 1
count_bb$date <- as.Date(paste(count_bb$day, count_bb$month, count_bb$year, sep = "/"), format = "%d/%m/%Y")

#ordering by date 
count_bb <- count_bb[order(as.Date(count_bb$date, format="%d/%m/%Y")),]

#subset start in July 2014

count_bb <- subset(count_bb, date >= as.Date("2014-07-01"))

#bringing in population data for standarization 
#2019 is estimated 
pop_nyc <- c(8398748, 8443713, 8560072, 8461961, 8426743, 8354889)
year <- 2019:2014
nyc_popy <- data.frame("pop" = pop_nyc, "year"= year)

count_bb <- merge(count_bb, nyc_popy, by = "year")

count_bb$stan_count <- count_bb$count/count_bb$pop*100000

#write.csv(count_bb, "standardized_count_bb_timeseries_clean.csv")

#cockroaches
 
#subsetting for roaches 
cp_roach <- cp %>% 
  filter(CodeID == 2514 | CodeID == 2823)

#adding georeferenced information 
roach_com <- left_join(cp_roach, com, by = "ComplaintID")
roach_com_s <- roach_com[c(2,4,6,10,12,14,16,17,21:32)]
#write.csv(roach_com_s, "NYC_complaint_georef_roaches_short.csv")

roach_com_s <- read.csv("NYC_complaint_georef_roaches_short.csv")
roach_com_s$year <- substring(roach_com_s$ReceivedDate, 7, 10) 

#setting up timeseries data for BB complaints 
roach_com_s$first <- 1 
roach_com_s <- roach_com_s %>%
  mutate(ReceivedDate = lubridate::mdy(ReceivedDate)) %>% 
  mutate(C_Year = lubridate::year(ReceivedDate)) %>% 
  mutate(C_Month = lubridate::month(ReceivedDate)) %>% 
  mutate(C_Day = lubridate::day(ReceivedDate)) %>%
  mutate(C_Month_Date1 = ymd(paste(C_Year, C_Month, first, sep="-"))) %>%
  mutate(C_Month_Date = ymd(paste(C_Year, C_Month, C_Day, sep="-"))) %>%
  filter(C_Year > 2013)


roach_com_s$count <- 1
com_s <- roach_com_s[c(1,25, 27,9)]

com_s <- com_s %>%
  mutate(month = month(C_Month_Date1)) %>%
  mutate(year = year(C_Month_Date1))

com_s$count_n <- 1

count_roaches <- com_s %>%
  group_by(month, year) %>%
  summarise(com_bb = sum(count_n))

count_roaches <- as.data.frame(count_roaches)
names(count_roaches) <- c("month", "year", "count")

count_roaches$day <- 1
count_roaches$date <- as.Date(paste(count_roaches$day, count_roaches$month, count_roaches$year, sep = "/"), format = "%d/%m/%Y")

#ordering by date 
count_roaches <- count_roaches[order(as.Date(count_roaches$date, format="%d/%m/%Y")),]

count_roaches <- subset(count_roaches, date >= as.Date("2014-07-01"))

#bringing in population data for standarization 
#2019 is estimated 
pop_nyc <- c(8398748, 8443713, 8560072, 8461961, 8426743, 8354889)
year <- 2019:2014
nyc_popy <- data.frame("pop" = pop_nyc, "year"= year)

count_roaches <- merge(count_roaches, nyc_popy, by = "year")

count_roaches$stan_count <- count_roaches$count/count_roaches$pop*100000

#write.csv(count_bb, "standardized_count_roaches_timeseries_clean.csv")

#merging counts into single database

#renaming variables for merge 

DB_311$call_count <- DB_311$count

DB_311 <- DB_311[c(2,3,6,7)]

count_bb$bb_raw <- count_bb$count

count_bb$bb_stan_count <- count_bb$stan_count

count_bb <- count_bb[c(10, 7, 11)]

count_roaches$r_raw <- count_roaches$count

count_roaches$r_stan_count <- count_roaches$stan_count

count_roaches <- count_roaches[c(10, 7, 11)]

all_counts <- cbind(DB_311, count_bb, count_roaches)

#standarizing by number of 311 calls 

all_counts$bb_stan_calls <- all_counts$bb_raw/all_counts$call_count

all_counts$r_stan_calls <- all_counts$r_raw/all_counts$call_count

#write.csv(all_count, "311_calls_complaints_standardized.csv")




