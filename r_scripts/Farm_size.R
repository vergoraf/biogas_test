###############################################################################
# Creating random data and a correlated response                              # 
# Author, 2020                                                                # 
###############################################################################
# year classification
###############################################################################
# 1. Call needed packages 
library(dplyr)
library(foreign)
###############################################################################
# 2. Import area data of each plot from dbf
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
setwd("Spatial_Data/InVeKoS/")

# create list with area data for each year 
plots.list <- list()
for( i in 1:length(years)){
  # import data dbf
  plots.list[[i]] <- read.dbf(paste0("Inv_KPD_", years[[i]], "_C.dbf"),
                              as.is = FALSE)
  # select only the necessary data
  plots.list[[i]] <- plots.list[[i]][, c("K_ART","BNR_ZD", "Area_H")]
  # convert year data to integer
  plots.list[[i]]$AYEAR <- as.integer(years[[i]])
}
###############################################################################
# 3. Set working directory and save data as back up
setwd("../..")
save(plots.list, file = "rda/plots.list.rda")
# load("rda/plots.list.rda") 
# just if it is necessary
###############################################################################
# 4. Aggregate plots to the corresponding farm for each year
size_farms<-list()
size_farms_aggr<-list()
# Set the score of farms size 
breaks_values <- c(350, 
                   600, 
                   1250, 
                   1550)
scores_values <- c(0,
                   25,
                   50,
                   75,
                   100)
# aggregate and assign the break values to the farms for each year.
for (i in 1:length(plots.list)) {
  size_farms[[i]] <- plots.list[[i]]
  size_farms_aggr[[i]] <- aggregate(x = size_farms[[i]]["Area_H"],
                                    by = size_farms[[i]]["BNR_ZD"],
                                    FUN=sum)
  size_farms_aggr[[i]]$cat_size <- ifelse(size_farms_aggr[[i]]$Area_H >= breaks_values[1] & size_farms_aggr[[i]]$Area_H < breaks_values[2],
                                          scores_values[2], 
                                          ifelse(size_farms_aggr[[i]]$Area_H >= breaks_values[2] & size_farms_aggr[[i]]$Area_H < breaks_values[3],
                                                 scores_values[3],
                                                 ifelse(size_farms_aggr[[i]]$Area_H >= breaks_values[3] & size_farms_aggr[[i]]$Area_H < breaks_values[4],
                                                        scores_values[4], 
                                                        ifelse(size_farms_aggr[[i]]$Area_H < breaks_values[1],
                                                               scores_values[1],
                                                               scores_values[5]))))
  # select only The ID_owners and cat size
  size_farms_aggr[[i]] <- size_farms_aggr[[i]][, c("BNR_ZD","cat_size")]
}
# Save data
save(size_farms_aggr,
     file = "rda/size_farms_aggr.rda")
# load("rda/size_farms_aggr.rda") 
# just if it is necessary,
###############################################################################
# 5. Import silage maize data and join the cat_size according to the 
# silage maize selected
load("rda/silage_maize_cat.rda")
# write down here what the for-loop is doing!!!
for (i in 1:length(silage_maize_cat)){
  silage_maize_cat[[i]] <- left_join(silage_maize_cat[[i]],
                                     size_farms_aggr[[i]],
                                     by="BNR_ZD")
}
# Save data
save(silage_maize_cat,
     file = "rda/silage_maize_cat.rda")