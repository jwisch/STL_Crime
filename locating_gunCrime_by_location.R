#Using Chris Prener's package to pull STL crime data: https://github.com/slu-openGIS/compstatr
#It pulls metro STL crime data from here: http://www.slmpd.org/Crimereports.shtml

library(compstatr)
library(data.table)
library(stringr)
library(proj4)

# create index


crime_files <- list.files(path = "./RawCrimeFiles/")
crime_file_list = lapply(paste0( "./RawCrimeFiles/", crime_files), read.csv)
crime_files <- rbindlist(crime_file_list, fill = TRUE)
rm(crime_file_list)

#Gun-related ones:
#HOMICIDE
#WEAPONS-CITY VIOL/DISCHRGING IN CITY
#WEAPONS-STATE VIOL/UNLWFL USE/DISCHARGING
#contains word "FIREARM"

#gun related crimes:
gun_crimes <- unique(crime_files$Description)
gun_crimes <- str_subset(gun_crimes, "FIREARM")
gun_crimes <- c(gun_crimes, "HOMICIDE", "WEAPONS-CITY VIOL/DISCHRGING IN CITY",
                "WEAPONS-STATE VIOL/UNLWFL USE/DISCHARGING")


crime_files <- crime_files[crime_files$Description %in% gun_crimes,]
crime_files$Date <- as.Date(crime_files$DateOccured, format = "%m/%d/%Y")
crime_files[is.na(crime_files$Date),]$Date <- as.Date(crime_files[is.na(crime_files$Date),]$DateOccur, format = "%m/%d/%Y")


geocode_frames <- rbind(crime_files[!is.na(crime_files$XCoord) & !(crime_files$XCoord == 0), c("XCoord", "YCoord")],
                        setNames(crime_files[!is.na(crime_files$X.Coord) & !(crime_files$X.Coord == 0), c("X.Coord", "Y.Coord")], c("XCoord", "YCoord"))
                        
                        )
geocode_frames <- geocode_frames[!duplicated(geocode_frames)]

proj4string <- "+proj=tmerc +lat_0=35.83333333333334 +lon_0=-90.5 +k=0.9999333333333333 +x_0=250000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs"

# Source data
xy <- data.frame(x=geocode_frames$XCoord, y=geocode_frames$YCoord)

# Transformed data
pj <- project(xy, proj4string, inverse=TRUE)
latlon <- data.frame(lat=pj$y, lon=pj$x)


geocode_frames$lat <- latlon$lat
geocode_frames$lon <- latlon$lon


crime_files$x <- ifelse(!is.na(crime_files$XCoord), crime_files$XCoord, crime_files$X.Coord)
crime_files$y <- ifelse(!is.na(crime_files$YCoord), crime_files$YCoord, crime_files$Y.Coord)

crime_files$Date <- as.Date(crime_files$Date, format = "%Y-%m-%d")
crime_files$DateOccured <- as.Date(crime_files$DateOccured, format = "%m/%d/%Y")
crime_files$Date.Occur <- as.Date(crime_files$Date.Occur, format = "%m/%d/%Y")
crime_files$DateOccur <- as.Date(crime_files$DateOccur, format = "%m/%d/%Y")

crime_files$Date_Occur <- data.table::fifelse(!is.na(crime_files$DateOccured),
                                 crime_files$DateOccured,
                                 data.table::fifelse(!is.na(crime_files$Date.Occur), crime_files$Date.Occur,
                                                     data.table::fifelse(!is.na(crime_files$DateOccur), crime_files$DateOccur, crime_files$Date)))

crime_files <- crime_files[, c("Date_Occur", "Crime", "Description", "District", 
                               "ILEADSAddress", "ILEADSStreet", "Neighborhood", "x", "y")]
#Want to convert the x and y's to lat longs where i can, then geocode the rest
proj4string <- "+proj=tmerc +lat_0=35.83333333333334 +lon_0=-90.5 +k=0.9999333333333333 +x_0=250000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs"

# Source data
xy <- data.frame(x=crime_files$x, y=crime_files$y)

# Transformed data
pj <- project(xy, proj4string, inverse=TRUE)
latlon <- data.frame(lat=pj$y, lon=pj$x)

crime_files$lat <- latlon$lat
crime_files$lon <- latlon$lon

crime_files[crime_files$x == 0, c("lat", "lon")] <- NA

saveRDS(crime_files, "./Processed_Data/Crime_Site_Addresses.RDS")
