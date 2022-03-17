library(osmar) #https://gis.stackexchange.com/questions/218072/how-can-i-use-roads-from-osm-data-in-r/218306
library(tmaptools)
library(osmdata) #https://rforjournalists.com/2020/12/15/how-to-access-open-street-map-in-r/
library(sf)
library(pracma) #necessary for haversine funciton
library(gridExtra)
library(osrm) #https://github.com/riatelab/osrm
library(sf)
library(maptiles)
library(mapsf)
options(osrm.server = "http://127.0.0.1:5000/", osrm.profile = "car")

address_df <- readRDS("./Processed_Data/Crime_Site_Addresses.RDS")

address_df$fullSearch <- paste0(address_df$ILEADSAddress, " ", address_df$ILEADSStreet, ", ", address_df$CITY, ", ", address_df$STATE)
#geocoding. 
#when address doesn't work for lat long, 
#using street name without number (apparently a common bug relating to street length in osm)
#when neither of the above options work, using 9 digit zip
latLongs_address <- geocode_OSM(address_df$fullSearch)

saveRDS(latLongs_address, "./Processed_Data/Geocoded_Crime_Site_Addresses.RDS")

missing_values <- address_df[!(address_df$fullSearch %in% latLongs_address$query),]

write.csv(missing_values, "./Processed_Data/Missings_geocodes.csv", row.names = FALSE)


library(proj4)
#The XCoord and YCoord fields are X- and Y-coordinates of the I/Leads incident
# location and in State Plane North American Datum 1983 (NAD83) format. This
# is a standard coordinate system for displaying regional/local geographic data in
# any desktop mapping application. 

proj4string <- "+proj=tmerc +lat_0=35.83333333333334 +lon_0=-90.5 +k=0.9999333333333333 +x_0=250000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs"
# Source data
xy <- data.frame(x=902041.0, y= 1021716)
coordinates(xy) <- c('x', 'y')
# Transformed data
pj <- project(xy, inverse = TRUE)
latlon <- data.frame(lat=pj$y, lon=pj$x)
print(latlon)


proj4string <- "+proj=tmerc +lat_0=35.83333333333334 +lon_0=-90.5 +k=0.9999333333333333 +x_0=250000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs"

# Source data
xy <- data.frame(x=902041, y=1021716)

# Transformed data
pj <- project(xy, proj4string, inverse=TRUE)
latlon <- data.frame(lat=pj$y, lon=pj$x)
print(latlon)




library(rgdal)
nad83_coords <- data.frame(x=902041.0, y= 1021716)
coordinates(nad83_coords) <- c('x', 'y')
proj4string(nad83_coords)=CRS("+init=epsg:2804")
spTransform(nad83_coords,CRS("+init=epsg:4326"))


library(compstatr)
library(mapview)
# project data
tmp_sf <- crime_files[1:10,c("ILEADSAddress", "ILEADSStreet", "XCoord", "YCoord")]
tmp_sf <- cs_projectXY(tmp_sf, varX = XCoord, varY = YCoord)

# preview data
mapview(tmp_sf)
library(ggplot2)
p <- ggplot() + 
  geom_sf(data = tmp_sf, color = "red", fill = NA, size = 2)
