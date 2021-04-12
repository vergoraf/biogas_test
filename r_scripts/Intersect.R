###############################################################################
# Creating random data and a correlated response                              # 
# Author, 2020                                                                # 
###############################################################################
# intersecting
###############################################################################
# 1. Call needed packages 
library(sf)
library(tmap)
library(plyr)
library(dplyr)
library(tmaptools)
library(GISTools)
library(grid)
library(classInt)
library(rgeos)
###############################################################################
# 2. Import silage maize data
setwd("Spatial_Data/InVeKoS/")
years_tot <- list("2005",
                  "2006",
                  "2007",
                  "2008",
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
s_maize <- list()
# Select Silage Maize categories for yearly data analysis.
# 177 <- Mais mit Blüh- und/oder Bejagungsschneisen (from 2012)
# 176 <- Mais mit Bejagungsschneise in gutem landwirtschaftlichen und 
# oekologischen Zustand (2012-2014)
# 411 <- Silomais or Silomais (als Hauptfutter) (from 2005)
# 172 <- Mais für Biogas (from 2015)
Smaize_cat < -c(177,
                176,
                411,
                172)
Smaize_cat_bis2015 <- c(177,
                        176,
                        411)
# read data from the source and then filter it, to decrease size
for( i in 1:length(years_tot)){
  s_maize[[i]] <- st_read(paste0("Inv_KPD_",
                                 years_tot[[i]],"
                                 _C.shp"))
  s_maize[[i]] <- s_maize[[i]][, c("K_ART",
                                   "BNR_ZD",
                                   "FB_FLIK",
                                   "FLIK_SC")]
  # Filter data for silage maize type
  s_maize[[i]] <- if(i >= 11){filter(s_maize[[i]],
                                     K_ART %in% Smaize_cat)}
  else{filter(s_maize[[i]],
              K_ART %in% Smaize_cat_bis2015)}
  s_maize[[i]]$AYEAR <- as.integer(years_tot[[i]])
}
###############################################################################
# 3. Save data silage maize
setwd("../../")
save(s_maize, 
     file = "rda/s_maize.rda")
# load(file = "rda/s_maize.rda")#only if it is necessary
###############################################################################
# 4. Subset from 2008 to 2018
s_maize_post2007 <- s_maize[4:14]
# This is for post-join in this script.
# create id field for each silage maize plot for each year
for(i in 1:length(s_maize_post2007)){
  s_maize_post2007[[i]] <- mutate(s_maize_post2007[[i]], 
                                  id = rownames(s_maize_post2007[[i]]))
}
###############################################################################
# 5. Save silage maize for 2008-2018
save(s_maize_post2007, file = "rda/s_maize_post2007.rda")
load(file = "rda/s_maize_post2007.rda")#only if it is necessary.
###############################################################################
# 6. Intersection 
load("rda/d_Pol_WEC.rda")
# Intersection of Silage maize with the Catchment area polygons.
# Adjusting projections
# assign projection WGS 84 / UTM zone 33N ->32633 to each component
#of the silage maize plots
for( i in 1:11){
  s_maize_post2007[[i]] <- st_transform(s_maize_post2007[[i]],
                                        32633)
}
# Intersection
s_maize_intersect_b <- list()
# intersect silage maize plots for each year according to each catchment area score polygon.
for (i in 1:length(s_maize_post2007)) {
  s_maize_intersect_b[[i]] <- st_intersection(s_maize_post2007[[i]],
                                              d_Pol_WEC[[i]])
}
###############################################################################
# 7. Save Intersection
save(s_maize_intersect_b, 
     file = "rda/s_maize_intersect_b.rda")
# load(file = "rda/s_maize_intersect_b.rda")# only if it is necessary.
###############################################################################
# 8. Area calculation
# calculate area in square meters
for (i in 1:length(s_maize_intersect_b)) {
  s_maize_intersect_b[[i]]$area <- st_area(s_maize_intersect_b[[i]])
}
# Aggregate id.
selected_s_maize_aggr <- list()
s.maize_list <- list()
for (j in 1:length(s_maize_intersect_b)) {
  s.maize_list[[j]] <- as.data.frame(s_maize_intersect_b[[j]][, c("id",
                                                                  "cat",
                                                                  "area")])
  selected_s_maize_aggr[[j]] <- aggregate(x=s.maize_list[[j]]["area"],
                                          by=s.maize_list[[j]][c("id",
                                                                 "cat")],
                                          FUN=sum)
}
# calculate the area of all the silage maize plots in square meters
for (i in 1:length(s_maize_post2007)) {
  s_maize_post2007[[i]]$area_or <- st_area(s_maize_post2007[[i]])
}
###############################################################################
# 9. Join tables and calculate proportion
s_proportion <- list()
for (i in 1:length(s_maize_post2007)){
  s_proportion[[i]] <- left_join(s_maize_post2007[[i]],
                                 selected_s_maize_aggr[[i]],
                                 by="id")
  s_proportion[[i]]$Proportion <- as.numeric(s_proportion[[i]]$area/s_proportion[[i]]$area_or)
  s_proportion[[i]]$cat_CA_B <- as.numeric(s_proportion[[i]]$cat*s_proportion[[i]]$Proportion)
  s_proportion[[i]] <- as.data.frame(s_proportion[[i]])
  s_proportion[[i]] <- aggregate(x = s_proportion[[i]]["cat_CA_B"],
                                 by=s_proportion[[i]]["id"],
                                 FUN=sum)
}
###############################################################################
# 10. Save Data
save(s_proportion,
     file="rda/s_proportion.rda")
###############################################################################
# 11. Tables and calculate proportion
silage_maize_cat<-list()
for (i in 1:length(s_maize_post2007)){
  silage_maize_cat[[i]] < -left_join(s_maize_post2007[[i]],
                                     s_proportion[[i]],
                                     by="id")
}
###############################################################################
# 12. Join the cat_CA_B to the selected silage maize 
load("rda/silage_maize_cat.rda")
for (i in 1:length(silage_maize_cat)){
  silage_maize_cat[[i]] <- left_join(silage_maize_cat[[i]],
                                     s_proportion[[i]],
                                     by="id")
}
###############################################################################
# 13. Save silage maize data including the cat WEC.
save(silage_maize_cat, 
     file = "rda/silage_maize_cat.rda")
# only if it is necessary.
# load(file = "rda/silage_maize_cat.rda")
###############################################################################
# 14. Testing and plotting 
tmap_mode('view')
tm_shape(silage_maize_cat[[6]]) +
  tm_polygons("cat_CA",
              palette = "Spectral",
              style = "equal",
              border.col = NA) +
  tm_shape(d_Pol_WEC[[6]]) +
  tm_polygons(col = "green", 
              alpha=0.5)+
  tm_layout(frame = T,
            title="2008")
