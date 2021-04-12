###############################################################################
# Creating random data and a correlated response                              # 
# Author, 2020                                                                # 
###############################################################################
# Energy Maize Final categorization
###############################################################################
# 1. Call needed packages 
library(dplyr)
library(sf)
###############################################################################
# 2.Import silage maize data
load("rda/silage_maize_cat.rda")
###############################################################################
# 3. Weighting values
weight.ca <- 0.527
weight.livestock <- 0.217
weight.persistency <- 0.040
weight.sizefarmy <- 0.217  
###############################################################################
# 4. Formula of final score
for(i in 1:length(silage_maize_cat)){
  silage_maize_cat[[i]]$cat_total <- 0 
  silage_maize_cat[[i]]$cat_total <- ((ifelse(!is.na(silage_maize_cat[[i]]$cat_CA_B),
                                              (silage_maize_cat[[i]]$cat_CA_B*weight.ca),
                                              0)) +
                                        (ifelse(!is.na(silage_maize_cat[[i]]$cat_liv),
                                                (silage_maize_cat[[i]]$cat_liv*weight.livestock),
                                                0)) +
                                        (ifelse(!is.na(silage_maize_cat[[i]]$cat_pers),
                                                (silage_maize_cat[[i]]$cat_pers*weight.persistency),
                                                0)) +
                                        (ifelse(!is.na(silage_maize_cat[[i]]$cat_size),
                                                (silage_maize_cat[[i]]$cat_size*weight.sizefarmy),
                                                0)))
}
###############################################################################
# 5. Summarize data in one file and establish categorization values
outcome_smaize <- do.call("rbind",
                          silage_maize_cat)
outcome_smaize$area <- st_area(outcome_smaize)
outcome_smaize$cat_label <- ifelse(outcome_smaize$cat_total >= 55,
                                   "Likely",
                                   "Unlikely")
###############################################################################
# 6. Export final results of silage maize categories as one shapefile
st_write(outcome_smaize,
         ds="Spatial_Data/outcome_smaize_c.shp",
         layer="outcome_smaize_c.shp",
         driver="ESRI Shapefile",
         delete_layer=T)
###############################################################################
# 7. Save final data
save(outcome_smaize, 
     file = "rda/outcome_smaize.rda") 
# loop export final results of silage maize categories for each year   
for(i in 1:length(Pol_WEC_sf)){
  st_write(silage_maize_cat[[i]],
           ds=paste0("Spatial_Data/outcome_smaize",
                     years[[i]],
                     ".shp"),
           layer=paste0("outcome_smaize",
                        years[[i]],
                        ".shp"),
           driver="ESRI Shapefile",
           delete_layer=T)
}