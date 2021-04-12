###############################################################################
# Creating random data and a correlated response                              # 
# Author, 2020                                                                # 
###############################################################################
# livestock
###############################################################################
# 1. Call needed packages 
library(readxl)
library(dplyr)
library(sf)
###############################################################################
# 2. Import livestock data tables and combine tehm in one file.
livestock_files <- list.files("Tables/LELF_2005 - 2018/", 
                              pattern = ".xlsx", 
                              full.names = TRUE)
livestock.list <- list()
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
for (file_index in seq_along(livestock_files)) {
  livestock.list[[file_index]] <-  read_excel(livestock_files[[file_index]])
  livestock.list[[file_index]]$year <- years_tot[[file_index]] 
}
# 2.2. Save data 
save(livestock.list,
     file = "rda/livestock.list.rda")
# load("rda/livestock.list.rda")

# merge list elements in one dataframe  vector of 14 years.
livestock_tot <- do.call("rbind",
                         livestock.list)
###############################################################################
# 3. Load silage maize data
load("rda/s_maize_post2007.rda")
###############################################################################
# 4. Create Farm area inside buffers
# 4.1. Calculate area in square meters
for (i in 1:length(s_maize_post2007)) {
  s_maize_post2007[[i]]$area <- st_area(s_maize_post2007[[i]])
}
# 4.2. Adjust projections
for( i in 1:11){
  s_maize_post2007[[i]] <- st_transform(s_maize_post2007[[i]],
                                        32633)
}
# 4.3. Intersection
# create only the area where the polygons intersect
load("rda/buffer_list.rda")
s_maize_BNR_ZD_intersect <- list()
for (i in 1:length(s_maize_post2007)) {
  s_maize_BNR_ZD_intersect[[i]] <- st_intersection(s_maize_post2007[[i]],
                                                   buffer.list[[i]])
}
# 4.4. Save data
save(s_maize_BNR_ZD_intersect,
     file = "rda/s_maize_BNR_ZD_intersect.rda")
# load("rda/s_maize_BNR_ZD_intersect.rda")
# 4.5. Aggregate BNR_ZD
s_maize_BNR_ZD_aggr <- list()
s_maize_BNR_ZDlist <- list()
for (j in 1:length(s_maize_post2007)) {
  s_maize_BNR_ZDlist[[j]] <- as.data.frame(s_maize_BNR_ZD_intersect[[j]][, c("BNR_ZD",
                                                                             "area")])
  s_maize_BNR_ZD_aggr[[j]]<- aggregate(x = s_maize_BNR_ZDlist[[j]]["area"],
                                       by = s_maize_BNR_ZDlist[[j]]["BNR_ZD"],
                                       FUN = sum)
}
# 4.6. Save data
save(s_maize_BNR_ZD_aggr,
     file = "rda/s_maize_BNR_ZD_aggr.rda")
# load("rda/s_maize_BNR_ZD_aggr.rda")
###############################################################################
# 5. Create Area Farm
# 5.1. Aggregate BNR_ZD with total area
s_maize_post2007_aggr <- list()
s.maize_post2007list <- list()
for (j in 1:length(s_maize_post2007)) {
  s.maize_post2007list[[j]] <- as.data.frame(s_maize_post2007[[j]][,c("BNR_ZD",
                                                                      "area")])
  s_maize_post2007_aggr[[j]] <- aggregate(x = s.maize_post2007list[[j]]["area"],
                                          by = s.maize_post2007list[[j]]["BNR_ZD"],
                                          FUN = sum)
}
# 5.2. Save data
save(s_maize_post2007_aggr,
     file = "rda/s_maize_post2007_aggr.rda")
# load("rda/s_maize_post2007_aggr.rda")
###############################################################################
# 6. Join both, areas and Proportion
livestock_BNR_ZD <- list()
for (i in 1:length(s_maize_post2007_aggr)){
  livestock_BNR_ZD[[i]] <- left_join(s_maize_post2007_aggr[[i]],
                                     s_maize_BNR_ZD_aggr[[i]],
                                     by="BNR_ZD")
  names(livestock_BNR_ZD[[i]]) <- c("BNR_ZD",
                                    "area_Farm",
                                    "area_buffer")
  livestock_BNR_ZD[[i]]$Proportion <- as.numeric(livestock_BNR_ZD[[i]]$area_buffer/livestock_BNR_ZD[[i]]$area_Farm)*100
}
###############################################################################
# 7. Import biogas_data
lst <- lapply(1:5, 
              function(i) read_excel("Tables/Rinder_anzahl.xlsx", 
                                     sheet = i))
consumption_factor <- as.data.frame(lst[5])
###############################################################################
# 8. Consumption per cow
# 8.1. Define the livestock years
livestock_post2007 <- livestock.list[4:14]
# 8.2 Join table and calculate score (cat_liv)
for (i in 1:length(livestock_post2007)){
  livestock_post2007[[i]]$`BNR-ZD` <- as.factor(livestock_post2007[[i]]$`BNR-ZD`)
  livestock_BNR_ZD[[i]] <- left_join(livestock_post2007[[i]],
                                     livestock_BNR_ZD[[i]],
                                     by=c("BNR-ZD"="BNR_ZD"))
  # in hectares
  livestock_BNR_ZD[[i]]$Consumptionarea <- livestock_BNR_ZD[[i]]$Rinder*consumption_factor$Consumption.factor[[i]]
  livestock_BNR_ZD[[i]]$area_Farm <- as.numeric(livestock_BNR_ZD[[i]]$area_Farm/10000)
  livestock_BNR_ZD[[i]]$cat_liv <- ifelse(livestock_BNR_ZD[[i]]$Schweine>0,100,0)
  livestock_BNR_ZD[[i]]$cat_liv <- ifelse(is.na(livestock_BNR_ZD[[i]]$Schweine),0,livestock_BNR_ZD[[i]]$cat_liv)
  livestock_BNR_ZD[[i]]$cal <- 100-(livestock_BNR_ZD[[i]]$Consumptionarea/livestock_BNR_ZD[[i]]$area_Farm)*100
  livestock_BNR_ZD[[i]]$cat_liv <- ifelse(is.na(livestock_BNR_ZD[[i]]$cal),
                                          livestock_BNR_ZD[[i]]$cat_liv,
                                          livestock_BNR_ZD[[i]]$cal)
  livestock_BNR_ZD[[i]]$cat_liv <- ifelse(livestock_BNR_ZD[[i]]$cat_liv<0,0,livestock_BNR_ZD[[i]]$cat_liv)
  livestock_BNR_ZD[[i]] <- livestock_BNR_ZD[[i]] %>% 
    select("BNR-ZD","cat_liv")
  livestock_BNR_ZD[[i]]$`BNR-ZD` <- as.factor(livestock_BNR_ZD[[i]]$`BNR-ZD`)
}
###############################################################################
# 8.3 Save 
save(livestock_BNR_ZD, 
     file = "rda/livestock_BNR_ZD.rda")
# load("rda/livestock_BNR_ZD.rda")
###############################################################################
# 9. Load silage maize data
load("rda/silage_maize_cat.rda")
###############################################################################
# 10. Join the cat_livs to the silage maize selected
for (i in 1:length(silage_maize_cat)){
  silage_maize_cat[[i]] <- left_join(silage_maize_cat[[i]],
                                     livestock_BNR_ZD[[i]],
                                     by=c("BNR_ZD"="BNR-ZD"))
}
###############################################################################
# 11. Save data
save(silage_maize_cat,file = "rda/silage_maize_cat.rda")