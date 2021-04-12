###############################################################################
# Creating random data and a correlated response                              # 
# Author, 2020                                                                # 
###############################################################################
# persistency
###############################################################################
# 1. Call needed packages 
library(sf)
library(dplyr)
library(sjmisc)
library(plyr)
###############################################################################
# 2. Import silage maize data from 2008-2018
load("rda/s_maize_post2007.rda")
###############################################################################
# 3. Select silage maize fields suited to analyse persistency.
#to minimize moemory processing
# usable columns
columns.sel <- c("FLIK_SC",
                 "K_ART",
                 "AYEAR",
                 "geometry")
s_maize_pers <- list()
for( i in 1:length(s_maize_post2007)){
  s_maize_pers[[i]] <- s_maize_post2007[[i]]
  s_maize_pers[[i]] <- s_maize_pers[[i]][,columns.sel]
  s_maize_pers[[i]]$FLIK_SC <- as.character(s_maize_pers[[i]]$FLIK_SC)
  s_maize_pers[[i]]$K_ART <- as.character(s_maize_pers[[i]]$K_ART)
  # Create an "id" for step 8.
  s_maize_pers[[i]] <- mutate(s_maize_pers[[i]], 
                              id = rownames(s_maize_pers[[i]]))
  s_maize_pers[[i]]$area<-st_area(s_maize_pers[[i]])
}
###############################################################################
# 4. Remove s_maize_post2007 to reduce memory use
rm(s_maize_post2007)
###############################################################################
# 5. Import all plot data specifically for persistency
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
rounds <- list(0,1,2,3,4,5,6,7,8,9,10,11,12,13)
pers_table <- list()
# it was done for every 3 years, due to hardware capabilities
# if loaded more data the model stops
for (j in 1:11){
  all_K_Art <- list()  
  for( i in 1:3){
    all_K_Art[[i]] <- st_read(paste0("Inv_KPD_",
                                     years_tot[[i+rounds[[j]]]],
                                     "_C.shp"))
    all_K_Art[[i]] <- all_K_Art[[i]][,c("K_ART",
                                        "FLIK_SC")]
    all_K_Art[[i]]$AYEAR <- as.integer(years_tot[[i+rounds[[j]]]])
  }
}
###############################################################################
# 6. Intersection between silage maize at initial year and 
# total agricultural area
s_maize_inter<-list()
for( i in 1:3){
  # intersect
  s_maize_inter[[i]] <- st_intersection(s_maize_pers[[j]],
                                        all_K_Art[[i]])
  s_maize_inter[[i]]$K_ART.1 <- as.character(s_maize_inter[[i]]$K_ART.1)
  # calculate area
  s_maize_inter[[i]]$area_i <- st_area(s_maize_inter[[i]])
  # round the proportion
  s_maize_inter[[i]]$apror <- round((s_maize_inter[[i]]$area_i/s_maize_inter[[i]]$area),2)
}
###############################################################################
# 7. Aggregate Kultur_ART by ID
s_maize_inter_aggr<-list()
for(i in 1:3){
  s_maize_inter_aggr[[i]] <- as.data.frame(s_maize_inter[[i]][,c("id","K_ART.1","apror")])
  s_maize_inter_aggr[[i]] <- aggregate(x = s_maize_inter_aggr[[i]]["apror"],
                                       by = s_maize_inter_aggr[[i]][c("id","K_ART.1")],
                                       FUN = sum)
}
###############################################################################
# 8. Identify silage maize category and its proportional area
pers_years <- list()
cat_match< - c("411",
               "176",
               "177")
cat_match_p_15 <- c("411",
                    "176",
                    "177",
                    "172")
for (i in 1:3){
  ifelse(j >= 8,
         pers_years[[i]] <- ddply(s_maize_inter_aggr[[i]],
                                  ~id,
                                  function(x){x[match(cat_match_p_15,
                                                      x$K_ART.1),]}),
         pers_years[[i]] <- ddply(s_maize_inter_aggr[[i]],
                                  ~id,
                                  function(x){x[match(cat_match,
                                                      x$K_ART.1),]}))
  colnames(pers_years[[i]]) <- c("id",
                                 "K_ART.1",
                                 paste0("apror_",
                                        years_tot[[i+rounds[[j]]]]))
  pers_years[[i]] <- pers_years[[i]][,c("id",
                                        paste0("apror_",
                                               years_tot[[i+rounds[[j]]]]))]
}
###############################################################################
# 9. Join tables
Year_ana<-list()
pers_join<-list()
Year_ana[[j]] <- as.data.frame(s_maize_pers[[j]])
Year_ana[[j]] <- Year_ana[[j]][,c("id", 
                                  "FLIK_SC",
                                  "K_ART",
                                  "AYEAR")]
pers_join[[j]] <- list(Year_ana[[j]],
                       pers_years[[3]],
                       pers_years[[2]],
                       pers_years[[1]])
pers_table[[j]] <- join_all(pers_join[[j]],
                            by='id',
                            type='left')
# eliminate duplicates
pers_table[[j]] <- pers_table[[j]][!duplicated(pers_table[[j]][,
                                                               "id"]), ]
###############################################################################
# 10. Sum values
pers_table[[j]]$TOTAL <- rowSums(pers_table[[j]][c(5:7)],
                                 na.rm=TRUE)
###############################################################################
# 11. Save data
setwd("../..")
save(pers_table,
     file = "rda/pers_table.rda")
# load("rda/pers_table.rda")
###############################################################################
# 12. Calculation by the exponential growth based on natural value.
cat_pers_values<-pers_table
for(i in 1:length(pers_table)){
  cat_pers_values[[i]]$cat_pers <- 100*round(1*(1-exp(-cat_pers_values[[i]]$TOTAL)),2) 
  # select just the necessary columns
  cat_pers_values[[i]] <- cat_pers_values[[i]][,c("FLIK_SC",
                                                  "cat_pers")]
}
# load("rda/cat_pers_values.rda")
save(cat_pers_values,
     file = "rda/cat_pers_values.rda")
###############################################################################
# 13. Load silage maize data
load(file = "rda/silage_maize_cat.rda")
###############################################################################
# 14. Join cat_pers to the selected silage maize 
for (i in 1:length(silage_maize_cat)){
  silage_maize_cat[[i]] <- left_join(silage_maize_cat[[i]],
                                     cat_pers_values[[i]],
                                     by="FLIK_SC")
  
  #eliminate duplicates
  silage_maize_cat[[i]] <- silage_maize_cat[[i]][!duplicated(silage_maize_cat[[i]][,"id"]), ]
}
###############################################################################
# 15. Save data
save(silage_maize_cat,
     file = "rda/silage_maize_cat.rda")