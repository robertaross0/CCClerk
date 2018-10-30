#Rob Ross 2018-09-10
#This file builds a basic demographic file and shapefile for Cook County voting precincts
# very helpful for mapping 
  #https://www.computerworld.com/article/3038270/data-analytics/create-maps-in-r-in-10-fairly-easy-steps.html

rm(list=ls())

# Libraries ---------------------------------------------------------------
libs <- c("magrittr", "shinyjs", "downloader", "scales", "tmap", "tmaptools", "maps"
          , "mapdata", "sf", "raster", "rgeos", "rgdal", "sqldf", "ggplot2", "stringr", "dplyr"
          , "leaflet", "tidyverse", "spData", "fields", "raster")
# Check to see if packages are installed. Install them if they are not, then load them into the R session.
check.packages <- function(libs){
  new.libs <- libs[!(libs %in% installed.packages()[, "Package"])]
  if (length(new.libs)) 
    install.packages(new.libs, dependencies = TRUE)
  sapply(libs, require, character.only = TRUE)
}
for (iter in 1:length(libs))
  check.packages(libs[iter]);

# Designate file paths ----------------------------------------------------

local_path <- "U:/Election Data Unit/Tableau/"

census_tract_path <- paste(local_path,"Data/Spatial/Census Precincts/", sep="")
CCsuburbs_tract_path <- paste(local_path,"Data/Spatial/County Precincts CC Open Data/", sep="")
data_tables_path <- paste(local_path,"Data/Data tables/", sep="" )
CCCity_file <- paste(local_path,"Data/Spatial/City Precincts Chicago Open Data/", sep="" )

census_file <- paste(census_tract_path, "tl_2012_17_vtd10.shp", sep="")
demo_file <- paste(data_tables_path, "nhgis0024_ds171_2010_votedist.csv", sep="")
CCsuburbs_file <- paste(CCsuburbs_tract_path, "Precinct_Current.shp", sep="")
CCCity_file <- paste(CCCity_file, "PRECINCTS_2012.shp", sep="")

out_census_file <- "U:/Election Data Unit/Tableau/Data/Spatial/CC_2012_VTDS"
# Compile Census TIGER lines and demographic data -------------------------

#Download this file ftp://ftp2.census.gov/geo/tiger/TIGER2012/VTD/tl_2012_17_vtd10.zip
#unzip(zipfile="C:/Users/rross/Downloads/tl_2012_17_vtd10.zip", exdir="U:/Election Data Unit/Tableau/Data/Spatial")
precincts_map <- subset(readOGR(census_file), COUNTYFP10=="031" & NAME10!="Lake Michigan")
names(precincts_map)[7] <- 'NAME'
precincts_map$NAME=as.character(precincts_map$NAME)
qtm(precincts_map)

##Demographic data from 2012 
demographic <- 
subset(read.csv(demo_file), STATE=="Illinois" & COUNTY=="Cook County")
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
writeOGR(obj=county_precincts, dsn=out_census_file, layer="voting_dists", driver="ESRI Shapefile",overwrite_layer=TRUE)

# Re-format Cook County Precincts from Open Data Portal ----------------

ccsuburbs <- readOGR(CCsuburbs_file);
qtm(ccsuburbs)
ccsuburbs <- spTransform(ccproj,CRS("+proj=longlat"));
ccsuburbs$Shape_area <- round(ccsuburbs$Shape_area, digits=4)
writeOGR(obj=ccsuburbs, dsn=out_census_file, layer="suburban_voting_dists", driver="ESRI Shapefile",overwrite_layer=TRUE)

# Generate crosswalk from Census IDs to local precinct IDs ----------------

# Have 2 different spatial files. Want 1 spatial file with data appended as attributes.

# Census files have demographic information and Census identifiers - VTDA codes
# County files hve IDPCT codes necessary to link to election results

# Read in raw files
cpproj <- county_precints <- readOGR("U:/Election Data Unit/Tableau/Data/Spatial/CC_2012_VTDS/voting_dists.shp");
ccproj <- ccsuburbs <- readOGR("U:/Election Data Unit/Tableau/Data/Spatial/County Precincts CC Open Data");

# Reproject to the same X/Y
cp <- spTransform(cpproj,CRS("+proj=longlat"));	# Minor formatting only.  Was already x/y
cc <- spTransform(ccproj,CRS("+proj=longlat"));

# WARNING: THESE SHAPEFILES ARE PROJECTED DIFFERENTLY
par(mfrow=c(1,2));plot(cp,axes=TRUE,main=proj4string(cp));plot(ccproj,axes=TRUE,main=proj4string(ccproj));

# Create a result table for matching algorithm
remap <- data.frame(VTDA=cp$VTDA,closestIX=NA,areaMatchAmount=NA,stringsAsFactors=FALSE);
# Algorithm: intersected poly has at least 5% overlap
# Choose possible matches only from candidate list where centroids are reasonably close
# Extract Centroids
cp_xy <- coordinates(cp);
cc_xy <- coordinates(cc);
# Calculate distance from all centroids, and keep 10 closest ones
dists <- rdist.earth(cp_xy,cc_xy);
IX <- t(apply(dists,1,function(vec) which(vec<=sort(vec)[10])));
# Loop through each and do intersection test
for (iter in 1:nrow(remap)) {
  if (iter%%100==0) { cat(iter,"of",nrow(remap),"\n");flush.console(); }
  this <- cp[iter,];
  that <- cc[IX[iter,],];	# candidate to match
  
  # Intersect the census-polygon with candidate precinct-polygons
  those <- lapply(1:10,function(iter2) {
    out <- try(gIntersection(this,that[iter2,]),silent=TRUE);	# one of the polygon breaks...not sure
    if (inherits(out,"try-error")) return(NULL);
    return(out);
  });
  
  # Calculate areas
  area_that <- sapply(1:10,function(iter2) area(that[iter2,]));
  area_those <- sapply(1:10,function(iter2) ifelse(!inherits(those[[iter2]],"SpatialPolygons"),0,area(those[[iter2]])));
  ratio <- area_those/area_that;
  
  # Choose maximum intersect, that is non-zero
  ix <- setdiff(which.max(ifelse(ratio>.05,ratio,0)),which(ratio==0));
  if (length(ix)>1) {
    stop("Multi match");
  }
  if (length(ix)==0) next;	# No match
  # Otherwise, assign
  remap$closestIX[iter] <- IX[iter,][ix];
  remap$areaMatchAmount[ix] <- ratio[ix];
}

# THIS SECTION IS TO MANAULLY HACK/HARDCODE THE REMAP OBJECT IF THE MATCH DIDN'T WORK
# e.g. (nonsense example) remap$closestIX[match(20511,remap$VTDA)] <- match(7000003,cc$Idpct);

# Join
out <- cp;
out@data <- cbind(out@data,cc@data[remap$closestIX,]);
dat <- out@data;

# Perform check by plotting everything, sourcing the original data
pdf("temp.pdf",width=11,height=11);
# Plot all no matches
those <- out[which(is.na(out$Idpct)),];
plot(those,axes=TRUE,col='#E41A1C44',main="pink = NO MATCH (census), brown = no match, green = MATCH (precincts)");
plot(cc,add=TRUE,col='#4DAF4A44');

par(mfrow=c(3,3),mar=c(1,1,4,1),bg="black",col.main="white",col.axis='white',col.lab='white');
# Plot matches
for (iter in 1:nrow(out)) {
  if (iter%%100==0) { cat(iter,"of",nrow(out),"\n");flush.console(); }
  this <- cp[iter,];
  flag_nomatch <- is.na(out$Idpct[iter]);
  if (flag_nomatch) {
    that <- cc[IX[iter,],];
  } else {
    that <- cc[match(out$Idpct[iter],cc$Idpct),];
  }
  
  # Where it overlaps, it'll turn brown
  bb1 <- bbox(this);
  bb2 <- bbox(that);
  bb <- rbind(range(c(bb1[1,],bb2[1,])),range(c(bb1[2,],bb2[2,])));
  plot(that,main=paste0("cp_index=",iter," | VTDA=",this$VTDA),xlim=bb[1,],ylim=bb[2,],
       col=ifelse(flag_nomatch,"#E41A1C",'#E41A1C66'));
  mtext(ifelse(flag_nomatch,"",paste0("Idpct=",that$Idpct)),3,col='white');
  plot(this,add=TRUE,col=ifelse(flag_nomatch,"#4DAF4A","#4DAF4A66"),border="#4DAF4A");
  if (flag_nomatch) text(mean(bb[1,]),mean(bb[2,]),"no-match",cex=3,col='darkgray');
}
dev.off();

# Leaflet map -------------------------------------------------------------

#MAPPING IN R
#leaflet map
#Color palette
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