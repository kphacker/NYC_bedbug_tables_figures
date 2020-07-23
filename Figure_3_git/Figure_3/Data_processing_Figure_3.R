#Data processing that was used to estimate proportion of bed bug complaints per borough - this was used 
#to create figure 3 and appendix table 2 

#packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(data.table)


com <- read.csv("Housing_Maintenance_Code_Complaints.csv") #avilible for download at:
#https://data.cityofnewyork.us/Housing-Development/Housing-Maintenance-Code-Complaints/uwyv-629c

cp <- read.csv("Complaint_Problems.csv") #availible for download at:
#https://data.cityofnewyork.us/Housing-Development/Complaint-Problems/a2nx-4u46

#subsetting for bedbugs specific complaints 
cp_BB <- cp %>% 
  filter(CodeID == 2517 | CodeID == 2818)

#adding georeferenced information from the Complaint_Problems.csv 
bb_com <- left_join(cp_BB, com, by = "ComplaintID")
#write.csv(bb_com, "NYC_complaint_georef_2014_2019.csv")

bb_com_s <- read.csv("NYC_complaint_georef_2014_2019.csv")

#maintain by borough through data processing 

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

#trying to get cleaner code as one 

count_all <- bb_com_s %>%
  group_by(Borough, C_Month_Date1) %>%
  summarise(com_bb = sum(count))

count_all <- as.data.frame(count_all)
names(count_all) <- c("borough", "date", "count")

count_all <- subset(count_all, date >= as.Date("2014-07-01"))

count_all <- count_all %>%
  mutate(C_Year = lubridate::year(date)) %>% 
  mutate(C_Month = lubridate::month(date)) %>% 
  mutate(C_Day = lubridate::day(date))

count_all$b_year <- paste(count_all$borough, count_all$C_Year, sep = "_")
count_all$b_year <- str_replace_all(count_all$b_year, " ", "_")

#population size estimates per borough per year 
#https://data.census.gov/cedsci/table?q=&d=ACS%205-Year%20Estimates%20Data%20Profiles&table=DP05&tid=ACSDP5Y2014.DP05&g=0400000US36_1600000US3651000_0500000US36061,36005,36081,36047,36085&lastDisplayedRow=25
#https://www.census.gov/quickfacts/fact/table/newyorkcitynewyork,bronxcountybronxboroughnewyork,kingscountybrooklynboroughnewyork,newyorkcountymanhattanboroughnewyork,queenscountyqueensboroughnewyork,richmondcountystatenislandboroughnewyork/PST045219

#formating population table <- there is def a better way to do this 
#note on 2019 totals which are not yet released we used the most current estimates from 2018 from a variety of compounded surveys
#the other numbers 2018 - 2014 were from the american community survey 

bronx <- c(1432132, 1437872, 1455846, 1436785, 1428357, 1413566) 
brooklyn <- c(2582830, 2600747, 2635121, 2606852, 2595259, 2570801)
manhattan <- c(1628701,1632480, 1653877, 1634989, 1629507, 1618389)
queens <- c(2278906,2298513, 2339280, 2310011, 2301139, 2280602)
staten_island <- c(476179, 474101, 475948, 473324, 472481, 471522) 

pop <- data.frame("year" = c(2019:2014), "bronx" = bronx, 
                  "brooklyn" = brooklyn, "manhattan" = manhattan,
                  "queens" = queens, "staten_island" = staten_island, stringsAsFactors = FALSE)

#transposing and adjusting 
pop_t <- as.data.frame(t(as.matrix(pop)))
pop_t <- pop_t[-c(1),] 
colnames(pop_t) <- c(2019:2014)
pop_t$borough <- rownames(pop_t)

pop_t <- melt(pop_t, id=c("borough"))
colnames(pop_t) <- c("borough", "year", "population")
pop_t$borough <- str_to_upper(pop_t$borough)
pop_t$b_year <- paste(pop_t$borough, pop_t$year, sep = "_")
pop_t <- pop_t[c(3:4)]

#merging population table with bed bug counts 

count_all <- merge(count_all, pop_t, by = "b_year")
count_all <- count_all[order(as.Date(count_all$date, format="%d/%m/%Y")),]

#count_all was then exported 
#in excel the data was processed to include sum totals for each of the boroughs and shortened to only
#include the columns listed in proprotion_CI.csv


