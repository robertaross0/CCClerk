#Rob Ross 2018-09-10
#This file builds a basic demographic file and shapefile for Cook County voting precincts
# very helpful for mapping 
  #https://www.computerworld.com/article/3038270/data-analytics/create-maps-in-r-in-10-fairly-easy-steps.html


#install.packages("shinyjs")
#install.packages("rgdal")
#install.packages("raster")
#install.packages("rgeos")
#install.packages("dplyr")
#install.packages("sqldf")
#install.packages('C:/Users/rross/Downloads/sqldf_0.4-11.zip', lib='C:/Program Files/R/R-3.5.1/library',repos = NULL)
#install.packages('C:/Users/rross/Downloads/gsubfn_0.7.zip', lib='C:/Program Files/R/R-3.5.1/library',repos = NULL)
#install.packages('C:/Users/rross/Downloads/proto_1.0.0.zip', lib='C:/Program Files/R/R-3.5.1/library',repos = NULL)
#install.packages('C:/Users/rross/Downloads/RSQLite_2.1.1.zip', lib='C:/Program Files/R/R-3.5.1/library',repos = NULL)
#install.packages('C:/Users/rross/Downloads/DBI_1.0.0.zip', lib='C:/Program Files/R/R-3.5.1/library',repos = NULL)
#install.packages('C:/Users/rross/Downloads/chron_2.3-53.zip', lib='C:/Program Files/R/R-3.5.1/library',repos = NULL)
#install.packages("devtools")
#devtools::install_github("ggrothendieck/sqldf")
#install.packages(c("ggplot2", "devtools", "stringr"))
#install.packages(c("maps", "mapdata"))
#devtools::install_github("dkahle/ggmap")
#install.packages("tmap")
#install.packages("tmaptools")
#install.packages("sf")
#install.packages("downloader")
#devtools::install_github("rstudio/leaflet")
#install.packages("scales")
#install.packages("tidyverse")
#install.packages("htmlwidgets")

##load libraries
library(magrittr)
library(downloader)
library(shinyjs)
library(scales)
library(tmap)
library(tmaptools)
library(ggmap)
library(maps)
library(mapdata)
library(sf)
library(raster)
library(rgeos)
library(rgdal)
library(sqldf)
library(ggplot2)
library(stringr)
library(dplyr)
library(leaflet)

rm(list=ls())

#Download this file ftp://ftp2.census.gov/geo/tiger/TIGER2012/VTD/tl_2012_17_vtd10.zip
#unzip(zipfile="C:/Users/rross/Downloads/tl_2012_17_vtd10.zip", exdir="U:/Election Data Unit/Tableau/Data/Spatial")
precincts_map <- subset(readOGR('U:/Election Data Unit/Tableau/Data/Spatial/Census Precincts/tl_2012_17_vtd10.shp'), COUNTYFP10=="031" & NAME10!="Lake Michigan")
names(precincts_map)[7] <- 'NAME'
precincts_map$NAME=as.character(precincts_map$NAME)
qtm(precincts_map)

##Demographic data from 2012 
demographic <- 
subset(read.csv("U:/Election Data Unit/Tableau/Data/Data tables/nhgis0024_ds171_2010_votedist.csv"), STATE=="Illinois" & COUNTY=="Cook County")
demographic$VTDA <- as.numeric(demographic$VTDA)
demographic$hispanic <- demographic$H7R002
demographic$white <- demographic$H7R005
demographic$black <- demographic$H7R006
demographic$others <-demographic$H7R001-demographic$hispanic-demographic$white-demographic$black
demographic$total<-demographic$H7R001
demographic$GEOID10<-demographic$GISJOIN
demographic$NAME <-as.character(demographic$NAME)
myvars <- c("NAME", "VTDA", "GEOID10", "total", "hispanic", "white","black","others") 
demographic <- demographic[myvars]

#Merge 
str(demographic$NAME)
str(precincts_map$NAME)
county_precincts<-merge(precincts_map, demographic, by="NAME")
rm(demographic, precincts_map)

#basic map for pdf
county_precincts$pct_white <- county_precincts$white/county_precincts$total
qtm(county_precincts, "pct_white")


#Save
class(county_precincts)
writeOGR(obj=county_precincts, dsn="U:/Election Data Unit/Tableau/Data/Spatial/CC_2012_VTDS", layer="voting_dists", driver="ESRI Shapefile")

#MAPPING IN R
#leaflet map
#Color palette
pct_white_pallette <- 
  colorQuantile(palette = "Blues"
    , n=4
    , domain=county_precincts$pct_white
    , na.color = "#808080"
)
quantile(county_precincts$pct_white, c(.25, .5, .75, 1), na.rm=TRUE)
bins <- c(0, .057, .485, .774, .978, Inf)
pal <- colorBin("Blues"
                , domain = county_precincts$pct_white
                , bins = bins)

popup <- paste("Voting Disctrict:", county_precincts$NAME, "- Percent White:", percent(county_precincts$pct_white))
legend_pallete <- colorQuantile(palette = "Blues", n=4, domain=county_precincts$pct_white)
leaflet(county_precincts) %>%
  addTiles() %>% 
  addPolygons(stroke=TRUE
  , smoothFactor = .2
  , weight =1
  , popup=popup
  , fillOpacity = .75
  , color= ~pal(pct_white)
) %>% 
  addLegend(pal = pal
            , values = ~pct_white
            , opacity = 0.7
            , title = "% of Residents who are White"
            , position = "bottomleft")

#clear
rm(list=ls())