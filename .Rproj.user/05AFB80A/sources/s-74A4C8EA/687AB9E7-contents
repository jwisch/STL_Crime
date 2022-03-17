#working through: https://hughst.github.io/week-4/
#inspired by: https://link.springer.com/article/10.1007/s11524-018-0244-8

#Viz Libraries
library(rgdal)
library(raster)
library(ggplot2)
library(spatstat)
library(plotrix)
library(fields)
library(leaflet)
library(maptools)
library(RColorBrewer)
library(lattice)
library(geoR)
library(plotrix) 
library(car)  # contains a function for logistic transformation (log odds) to make more normal
library(tidycensus)
library(tidyverse)
library(sf)
library(osmdata)
library(data.table)
library(tigris)
library(acs)
library(censusr)


#Spatial Management
# Moran's I and spatial dependencies
library(spdep) # Spatial Dependence: Weighting Schemes, Statistics and Models
library(ape) # Analyses of Phylogenetics and Evolution
library(pgirmess) # Data Analysis in Ecology

# Libraries for point processes
library(spatstat)
library(splancs) # K-function
library(smacpod) # Spatial scanning statistic



df <- readRDS("./Processed_Data/Crime_Site_Addresses.RDS")

#Going to pull down population from census bureau so that then we can get shooting prevelance?

tmp <- df[df$Date_Occur >= as.Date("2020-01-01", format = "%Y-%m-%d"),]


census_api_key("25f4f15b896d92ddbb4b02fdc8c281b9304e001b", overwrite = TRUE, install = TRUE)

vars <- load_variables(year = 2017,
                       dataset = "acs5",
                       cache = TRUE)

stl_value <- get_acs(geography = "tract"
                     , state = c("MO")
                     ,
                     county = c( "St. Louis city")
                     ,
                     variables = "B19013_001"
                     ,
                     geometry = TRUE)


census_block <- list()
for(i in 1:length(tmp$lat)){
  print(i)
  census_block[[i]] <- tryCatch(
    {tigris::call_geolocator_latlon(lat = tmp$lat[i],
                                    lon = tmp$lon[i])
    },
    error = function(cond){
      return(NA)
    })
}

census_block <- as.data.frame(do.call(rbind, census_block))
tmp$census_block <- census_block$V1
tmp$GEOID <- substr(tmp$census_block, start = 1, stop = 11)

plot_df <- merge(stl_value, tmp, 
                 by = "GEOID", all.x = TRUE, all.y = FALSE)
ggplot(plot_df) + geom_sf() + 
  geom_point(aes(x = lon, y = lat), colour = "red", shape = 3, size = 0.6) + 
  xlim(c(-90.32, -90.18)) + ylim(c(38.53, 38.76)) + theme_bw()




#Establishing the box to analyze
#https://cran.r-project.org/web/packages/spatstat/vignettes/getstart.pdf
mypattern <- ppp(plot_df$lon, plot_df$lat, c(-90.32, -90.18), c(38.53, 38.76))
plot(mypattern)
plot(Kest(mypattern)) #Ripley's K function

#Interpreting a K plot:
#It estimates how many points you expect to see wtihin the distance r given on the horizontal radius
#So in this case we are seeing way more shootings (actual given with black) than one would
#expect based on a poisson distribution (red = homogeneous poisson point process)...meaning there are hot spots/clusters for crime. 

K <- Kest(mypattern, correction = "iso")
g <- pcf(mypattern, correction = "iso") #pair correlation function
plot(anylist(g, K), main = "", main.panel = "", nrows = 2)

saveRDS(plot_df, "./Processed_Data/2020_gunCrime_matched_to_census_tract.RDS")

BF_Adm_1 <- raster::getData("GADM", country="USA", level=1)
BF_Adm_1 <- subset(BF_Adm_1, NAME_1 == "US.MO")
proj4string(BF_Adm_1) <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ')

pal = colorNumeric("Oranges", BF_malaria_data$prevalence)
leaflet(BF_malaria_data) %>% addTiles() %>% addCircleMarkers(~longitude, ~latitude, fillOpacity=1,
                                                             fillColor= ~pal(prevalence), radius=~prevalence*10, stroke=TRUE, weight=1) %>% 
  addLegend(pal = pal, values = ~prevalence)

