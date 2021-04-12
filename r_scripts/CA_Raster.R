###############################################################################
# Creating random data and a correlated response                              # 
# Author, 2020                                                                # 
###############################################################################
# year classification
###############################################################################
# 1. Call needed packages 
library(sf)
library(lubridate)
library(dplyr)
library(GISTools)
library(raster)
library(rgeos)
library(tmap)
###############################################################################
# 2. Establish breaks and scores for catchment area
n<-seq(0.025,1,by=0.05)
max(n)
feed.in_l<-c(-Inf,0,0,n[1],n[2],5,n[2],n[3],10,
             n[3],n[4],15,n[4],n[5],20,
             n[5],n[6],25,n[6],n[7],30,n[7],
             n[8],35,n[8],n[9],40,
             n[9],n[10],45,n[10],n[11],50,
             n[11],n[12],55,n[12],n[13],60,
             n[13],n[14],65,n[14],n[15],70,
             n[15],n[16],75,n[16],n[17],80,
             n[17],n[18],85,n[18],n[19],90,
             n[19],n[20],95,n[20],Inf,100)
###############################################################################
# 3. Load catchment area rasters
setwd("Raster")
years <- list("2008",
              "2009",
              "2010",
              "2011",
              "2012",
              "2013",
              "2014",
              "2015",
              "2016",
              "2017",
              "2018")
raster.list <- list()
feed.inre <- list() 
# create list with catchment area rasters created in ArcGIS
for (i in 1:length(years)){
  raster.list[[i]]<-raster(paste0(years[[i]],".tif"))
  raster.list[[i]]@data@names<-""
  feed.inre[[i]]<-reclassify(raster.list[[i]],feed.in_l)
}
###############################################################################
# 4. Save Reclassification
setwd("..")
save(feed.inre, file = "rda/reclas_raster.rda")
###############################################################################
# 5. Rasters to polygons
poly_WEC<-list()
Pol_WEC_sf<-list()
# Transform Raster to polygons and save them in a list
for(i in 1:length(feed.inre)){
  poly_WEC[[i]]<-rasterToPolygons(feed.inre[[i]],dissolve=T)
  Pol_WEC_sf[[i]]<-st_as_sf(poly_WEC[[i]])
  Pol_WEC_sf[[i]]$layer<-as.integer(Pol_WEC_sf[[i]]$layer)
}
###############################################################################
#6. Save the data as backup
save(Pol_WEC_sf, file = "rda/Pol_WEC_sf.rda")
###############################################################################
# 7. Join every new polygon to one polygon by score.
# After the conversion 1000 more polygons of score 80 appeared, thus here 
# they are joined again to just have one polygon for each score.
n<-seq(0,100,by=5)
d_Pol_WEC<-list()
dis_Pol_WEC<-list()
for (j in 1:length(feed.inre)){for(i in 1:length(n)){
  dis_Pol_WEC[[i]]<-as.data.frame(st_union(filter(Pol_WEC_sf[[j]],layer==n[[i]])))
  dis_Pol_WEC[[i]]$cat<-n[[i]]
  d_Pol_WEC[[j]]<-do.call("rbind",dis_Pol_WEC)
  d_Pol_WEC[[j]]<-st_as_sf(d_Pol_WEC[[j]])
}}
###############################################################################
# 8. Save polygons
save(d_Pol_WEC, file = "rda/d_Pol_WEC.rda")
load("rda/d_Pol_WEC.rda")
###############################################################################
# 9. Testing and plotting 
tmap_mode('view')
tm_shape(d_Pol_WEC[[1]]) +
  tm_polygons("cat",palette="Reds")
###############################################################################
# 10. Testing and plotting 
tmap_mode('view')
tm_shape(feed.inre[[1]]) +
  tm_raster("layer",palette="Reds")+tm_shape(feed.inre[[11]]) +
  tm_raster("layer",palette="Blues",alpha=0.5)
