#creating geocoded database of bed bug complaints to create a maps of total number of bed bug complaints
#per NTA area.Prepossessing completed in R and then exported to QGIS for data visualization 

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

#shapefile of community districts in NYC 

shp_com <- 'nycd.shp' #availible for download at:
#https://data.cityofnewyork.us/City-Government/Neighborhood-Tabulation-Areas-NTA-/cpf4-rkhq

nyc_com <- st_read(shp_com,
                   stringsAsFactors = FALSE)

#aggregating by community level 

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
com_s <- bb_com_s[c(2, 3, 10:18, 22:29)]

com_s <- read.csv("NYC_complaints_cleaned_address.csv")

#setting up for geo-coding to get API from google 
#method used https://www.storybench.org/geocode-csv-addresses-r/

com_s$city <- "New York City, NY"

com_s$addresses_short <- paste(com_s$HouseNumber, com_s$StreetName, sep = " ")
com_s$addresses <- paste(com_s$addresses_short, com_s$city, sep = ", ")

#write.csv(com_s, "NYC_complaints_cleaned_address.csv")

##################################################################
#start here for geocoding addresses 
##################################################################

#Get the latest Install and update packages

if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup", force=TRUE)

#Load the library
library("ggmap")

#Set your API Key
#ok finally got that working 

ggmap::register_google(key = "AIzaSyASPDSWKvYuzqpKeIOe3keumzr_NZr__NQ")


# Read in the CSV data and store it in a variable 
origAddress <- read.csv("NYC_complaints_cleaned_address.csv", stringsAsFactors = FALSE)

# Initialize the data frame
geocoded <- data.frame(stringsAsFactors = FALSE)

# Loop through the addresses to get the latitude and longitude of each address and add it to the
# origAddress data frame in new columns lat and lon
for(i in 1:nrow(origAddress))
{
  # Print("Working...")
  result <- geocode(origAddress$addresses[i], output = "latlona", source = "google")
  origAddress$lon[i] <- as.numeric(result[1])
  origAddress$lat[i] <- as.numeric(result[2])
  origAddress$geoAddress[i] <- as.character(result[3])
}
# Write a CSV file containing origAddress to the working directory
write.csv(origAddress, "geocoded.csv", row.names=FALSE)