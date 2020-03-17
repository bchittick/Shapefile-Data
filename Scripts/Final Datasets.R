########################################
# Combining Tables for Qubole 
########################################
library(dplyr)
library(plyr)


setwd("~/Shapefile Data")

########################################
# 1) Final Shapefile Datasets
########################################

# get list of all tables in folder 
filelist = list.files("~/Shapefile Data/Data/Outfiles/Outfiles Dataframes")

filepath = "~/Shapefile Data/Data/Outfiles/Outfiles Dataframes/"
outfile.shapedata = data.frame()

for(file in filelist) {
tempdf = read.csv(paste0(filepath, file),  header = T) %>% dplyr::select(-X)
tempdf = as.data.frame(tempdf)
outfile.shapedata = rbind.fill(outfile.shapedata, tempdf)
}


write.csv(outfile.shapedata, "~/Shapefile Data/Data/Qubole Tables/shapefile_dataset.csv", row.names = F)

########################################
# 2) Final Neighboring Files
########################################

# get list of all tables in folder 
filelist = list.files("~/Shapefile Data/Data/Outfiles", pattern = ".csv")

filepath = "~/Shapefile Data/Data/Outfiles/"
outfile.neighbordata = data.frame()

for(file in filelist) {
  tempdf = read.csv(paste0(filepath, file),  header = T) %>% dplyr::select(-X)
  tempdf = as.data.frame(tempdf)
  outfile.neighbordata = rbind.fill(outfile.neighbordata, tempdf)
}


write.csv(outfile.neighbordata, "~/Shapefile Data/Data/Qubole Tables/neighbooring_dataset.csv", row.names = F)
