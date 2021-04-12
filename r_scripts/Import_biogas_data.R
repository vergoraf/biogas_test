###############################################################################
# Creating random data and a correlated response                              # 
# Author, 2020                                                                # 
###############################################################################
# Import biogas data which previously was joined EKS with LfU by 
# equal Biogasanlagen ID and saved. 
###############################################################################
# 1. Call needed packages
library(sf)
library(tmap)
library(tidyverse)
library(readxl)
###############################################################################
# 2. Import Biogas data
# locale(encoding = "ISO-8859-1"),
biogas <- read_xls("Tables/biogas_list.xls", 
                   col_names = TRUE, 
                   col_types=NULL)
###############################################################################
# 3. Convert data to sf using coordinates from biogas data and set CRS
# Project to ETRS89 / UTM zone 33N
biogas_c <- st_as_sf(x = biogas,
                     coords = c("Ostwert",
                                "Nordwrt"),
                     crs = 32633,
                     remove = F)
b_pl_years <- biogas_c %>% 
  st_transform(32633)
rm(biogas_c, biogas)
# Date "betrieb" from unofficial database is used.t looks updated in compare to 
#from LfU
###############################################################################
# 4. Save as R data
save(b_pl_years, 
     file = "rda/b_pl_years.rda")
###############################################################################
# 5. - Check data with viewer
tmap_mode("view") 
tm_shape(biogas_pl_years) +
  tm_dots(size=0.02,col="green")
# close viewer
tmap_mode("plot")