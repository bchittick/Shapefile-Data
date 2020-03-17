##################################################
#  Shapefile Data Ingestions
##################################################

# need to convert shapefile into geojson file and simplfy 
# https://blog.exploratory.io/creating-geojson-out-of-shapefile-in-r-40bc0005857d

library(sp)
library(raster)
library(ggplot2)
library(spbabel)
library(maptools)
library(plotly)
library(geojsonio) # doesent work 
library(rgeos) # dosent work 
library(rgdal) # dosent work 
library(sf) # dosent work 
library(dplyr)
library (plyr)
library(ggplot2)
library(modeest)


# very easy to find intersection with sf https://github.com/r-spatial/sf/issues/234
# or g-Touches in Rgeos package

setwd("~/Shapefile Data")


##########################################
#           CITY
##########################################

# read in polygon data, SF format - fix names (NAME_2 seems to be what we want)
UK_CITY <- readRDS("~/Shapefile Data/Data/SF Files/UK_CITY.rds")
UK_CITY$NAME_2 <- toupper(gsub(" ", "_",gsub(".*?(\\b[A-Za-z0-9 ]+\\b).*","\\1", UK_CITY$NAME_2)))
UK_CITY$NAME_3 <- toupper(gsub(" ", "_",gsub(".*?(\\b[A-Za-z0-9 ]+\\b).*","\\1", UK_CITY$NAME_3)))
UK_CITY$NAME_2 = recode(UK_CITY$NAME_2,
                          `GREATER_LONDON` = 'LONDON',
                          `ABERDEEN` = "ABERDEEN_CITY",
                          `ANGLESEY` = "ISLE_OF_ANGLESEY",
                          `DERRY_AND_STRABANE` = "DERRY_CITY_AND_STRABANE",
                          `DUNDEE` = "DUNDEE_CITY",
                          `GLASGOW` = "GLASGOW_CITY",
                          `NEWRY` = "NEWRY_AND_MOURNE",
                          `NORTH_DOWN_AND_ARDS` = "ARDS_AND_NORTH_DOWN",
                          `PERTHSHIRE_AND_KINROSS` = "PERTH_AND_KINROSS",
                          `SAINT_HELENS` = "ST_HELENS",
                          `SOUTHEND` = "SOUTHEND_ON_SEA",
                          `STOCKTON` = "STOCKTON_ON_TEES",
                          `STOKE` = "STOKE_ON_TRENT")


# Match with HCOM naming conventions, add DMA Bespoke
UK_DIM = read.csv("~/Shapefile Data/Data/Bespoke Dim Table/UK CITY Lookup.csv") %>% mutate_if(is.factor, as.character)
UK_REGION_DIM = UK_DIM %>% mutate_if(is.factor, as.character) %>% select(adju_geo_region_name, geo_dma_bespoke) %>% unique()


# Check which cities match
datamart_names = UK_DIM %>% select(region = adju_geo_region_name, city = adju_geo_city, geo_dma_bespoke) %>% unique() %>% mutate_if(is.factor, as.character) %>% mutate(`in_datamart` = 1)
shapefile_names = UK_CITY %>% st_set_geometry(NULL) %>% select(region = NAME_2, city = NAME_3) %>% unique() %>% mutate(`in_shapefile` = 1)
t = left_join(shapefile_names, datamart_names)

# can find a match for every shapefile field but 406 Shapefile names vs 11K cities
sum(t$in_datamart, na.rm = T) / nrow(t)

# Get intersections and rename
spare_mtx <- st_intersects(UK_CITY, UK_CITY) %>% as.list()
names(spare_mtx) = UK_CITY$NAME_3 
names.lookup = data.frame("name" = UK_CITY$NAME_3) %>% dplyr::mutate(number = as.character(row_number()))

# Create mapping table
map = setNames(names.lookup$name, names.lookup$number)

new.list = list()
for(i in 1:length(spare_mtx)) {
  print(spare_mtx[i])
  new.list[[i]] = as.character(map[spare_mtx[[i]]])
}

names(new.list) = names(spare_mtx)

# Convert to DF
df <- ldply (new.list, data.frame)
names(df) = c("region", "neighbors")
df$neighbors = as.character(df$neighbors)
df = df %>% mutate(posa = 'GBR', geo_grain = 'adju_geo_city')
write.csv(df, "~/Shapefile Data/Data/Outfiles/UK_CITY_Pairs.csv")


# City 
# Plot - one Geo and borders 
ggplot(UK_CITY) +
  aes(fill = NAME_3) +
  scale_fill_viridis_d() +
  geom_sf() +
  coord_sf(crs = 4326) +
  guides(fill = FALSE) +
  ggtitle("CITY") +
  theme(title = element_text(size = 16),
        plot.margin = unit(c(0,0.1,0,0.25), "inches"))


##########################################
#           Regions
##########################################

# read in polygon data, SF format - fix names (NAME_2 seems to be what we want)
UK_REGION <- readRDS("~/Shapefile Data/Data/SF Files/UK_REGIONS.rds")
UK_REGION$NAME_2 <- toupper(gsub(" ", "_",gsub(".*?(\\b[A-Za-z0-9 ]+\\b).*","\\1", UK_REGION$NAME_2)))
UK_REGION$NAME_2 = recode(UK_REGION$NAME_2,
                        `GREATER_LONDON` = 'LONDON',
                         `ARMAGH` = "ARMAGH_CITY_BANBRIDGE_AND_CRAIGAVON",
                        `ABERDEEN` = "ABERDEEN_CITY",
                        `ANGLESEY` = "ISLE_OF_ANGLESEY",
                        `DERRY_AND_STRABANE` = "DERRY_CITY_AND_STRABANE",
                        `DUNDEE` = "DUNDEE_CITY",
                        `GLASGOW` = "GLASGOW_CITY",
                        `NEWRY` = "NEWRY_MOURNE_AND_DOWN",
                        `NORTH_DOWN_AND_ARDS` = "ARDS_AND_NORTH_DOWN",
                        `PERTHSHIRE_AND_KINROSS` = "PERTH_AND_KINROSS",
                        `SAINT_HELENS` = "ST_HELENS",
                        `SOUTHEND` = "SOUTHEND_ON_SEA",
                        `STOCKTON` = "STOCKTON_ON_TEES",
                        `STOKE` = "STOKE_ON_TRENT")

UK_REGION = UK_REGION %>% mutate(adju_geo_region = NAME_2)

# Match with HCOM naming conventions, add DMA Bespoke
UK_DIM = read.csv("~/Shapefile Data/Data/Bespoke Dim Table/UK CITY Lookup.csv") %>% mutate_if(is.factor, as.character)


# function to get Asssign one Region to each DMA
g <- function(number) {
  ux <- unique(number)
  ux[which.max(tabulate(match(number, ux)))]
}

UK_Dim_Region_DMA = aggregate(geo_dma_bespoke~adju_geo_region_name, data=UK_DIM, g) %>%
  mutate(geo_dma_bespoke = case_when(adju_geo_region_name == "ROTHERHAM" ~ "ITV_YORKSHIRE",
                                     adju_geo_region_name == "LONDON" ~ "ITV_LONDON",
                                     adju_geo_region_name == "BUCKINGHAMSHIRE" ~ "ITV_LONDON",
                                     adju_geo_region_name == "NEWRY_MOURNE_AND_DOWN" ~ "UTV",
                                     adju_geo_region_name == "ARMAGH_CITY_BANBRIDGE_AND_CRAIGAVON" ~ "UTV",
                                     adju_geo_region_name == "HERTFORDSHIRE" ~ "ITV_LONDON",
                                     adju_geo_region_name == "ISLES_OF_SCILLY" ~ "ITV_WESTCOUNTRY",
                                     TRUE ~ geo_dma_bespoke))

# How many match? only 169 (182 with manual adjustments) or 241, though some may be smaller - will manually map a couple
datamart_names = data.frame("region" = UK_Dim_Region_DMA$adju_geo_region_name) %>% mutate_if(is.factor, as.character) %>% mutate(`in_datamart` = 1)
shapefile_names = UK_REGION %>% st_set_geometry(NULL) %>% select(region = NAME_2) %>% mutate(`in_shapefile` = 1)
t = left_join(shapefile_names, datamart_names)

# can find a match for every shapefile field but 406 Shapefile names vs 11K cities
sum(t$in_datamart, na.rm = T) / nrow(t)

# Get intersections and rename
spare_mtx <- st_intersects(UK_REGION, UK_REGION) %>% as.list()
names(spare_mtx) = UK_REGION$NAME_2 
names.lookup = data.frame("name" = UK_REGION$NAME_2) %>% dplyr::mutate(number = as.character(row_number()))

# Create mapping table
map = setNames(names.lookup$name, names.lookup$number)

new.list = list()
for(i in 1:length(spare_mtx)) {
  print(spare_mtx[i])
  new.list[[i]] = as.character(map[spare_mtx[[i]]])
}

names(new.list) = names(spare_mtx)

# Convert to DF
df <- ldply (new.list, data.frame)
names(df) = c("region", "neighbors")
df$neighbors = as.character(df$neighbors)
df = df %>% mutate(posa = 'GBR', geo_grain = 'adju_geo_region_name')
write.csv(df, "~/Shapefile Data/Data/Outfiles/UK_Regions_Pairs.csv")


# Plot - have to plot the polygons (some DMA's not one polygon)
plotdata = UK_REGION %>% filter(!NAME_2 %in% c('ALASKA', "HAWAII"))

ggplot(UK_REGION) +
  aes(fill = NAME_2) +
  scale_fill_viridis_d() +
  geom_sf() +
  coord_sf(crs = 4326) +
  guides(fill = FALSE) +
  ggtitle("Regions") +
  theme(text = element_text(color = '#000000')
        ,title = element_text(size = 16)
        #,plot.margin = unit(c(0,0.1,0,0.25), "inches")
        ,plot.subtitle = element_text(size = 12)
        ,axis.ticks = element_blank()
        ,axis.text = element_blank()
        ,panel.grid = element_blank()
        ,panel.background = element_rect(fill = '#ffffff')
        ,plot.background = element_rect(fill = '#ffffff')
        ,legend.title=element_text(size=8)
        ,legend.text=element_text(size=6)
  )


ggplotly(t)


# Save SF Object in case needed
output_sf = UK_REGION %>% mutate(adju_geo_country = 'GBR', filename = "GBR_REGION", updated = as.Date(Sys.time()))
saveRDS(output_sf, "~/Shapefile Data/Data/Outfiles/Outfiles SF/GBR_REGION_SF.rds")

# Convert to DF and Save for Qubole 
output_df = SfToDataframe(UK_REGION, idcol = 'adju_geo_region', savecols = c("NAME_1"))
output_df = output_df %>% mutate(adju_geo_country = 'GBR', filename = "GBR_REGION", updated = as.Date(Sys.time())) %>% select(-NAME_1)
write.csv(output_df, "~/Shapefile Data/Data/Outfiles/Outfiles Dataframes/GBR_REGION.csv")



##########################################
#           BESPOKE DMA 
##########################################
# Will need to aggregate up from Regions 
library(data.table)

UK_DMA <- 
  UK_REGION %>% 
  left_join(UK_Dim_Region_DMA %>%select(NAME_2 = adju_geo_region_name, geo_dma_bespoke)) %>%
  dplyr::mutate(geo_dma_bespoke = ifelse(is.na(geo_dma_bespoke), "NA", geo_dma_bespoke)) %>%
  group_by(geo_dma_bespoke) %>%
  dplyr::summarise() %>%
  ungroup() %>% 
  st_cast(., 'MULTIPOLYGON')

# Get intersections and rename
spare_mtx <- st_intersects(UK_DMA, UK_DMA) %>% as.list()
names(spare_mtx) = UK_DMA$geo_dma_bespoke
names.lookup = data.frame("name" = UK_DMA$geo_dma_bespoke) %>% dplyr::mutate(number = as.character(row_number()))

# Create mapping table
map = setNames(names.lookup$name, names.lookup$number)

new.list = list()
for(i in 1:length(spare_mtx)) {
  print(spare_mtx[i])
  new.list[[i]] = as.character(map[spare_mtx[[i]]])
}

names(new.list) = names(spare_mtx)

# Convert to DF
df <- ldply (new.list, data.frame)
names(df) = c("region", "neighbors")
df$neighbors = as.character(df$neighbors)
df = df %>% mutate(posa = 'GBR', geo_grain = 'geo_dma_bespoke')
write.csv(df, "~/Shapefile Data/Data/Outfiles/UK_DMA_Pairs.csv")


# Plot - have to plot the polygons (some DMA's not one polygon)
plotdata = UK_REGION %>% filter(!NAME_2 %in% c('ALASKA', "HAWAII"))

ggplot(UK_DMA) +
  aes(fill = geo_dma_bespoke) +
  scale_fill_viridis_d() +
  geom_sf() +
  coord_sf(crs = 4326) +
  guides(fill = FALSE) +
  ggtitle("Bespoke DMA") +
  theme(text = element_text(color = '#000000')
        ,title = element_text(size = 16)
        #,plot.margin = unit(c(0,0.1,0,0.25), "inches")
        ,plot.subtitle = element_text(size = 12)
        ,axis.ticks = element_blank()
        ,axis.text = element_blank()
        ,panel.grid = element_blank()
        ,panel.background = element_rect(fill = '#ffffff')
        ,plot.background = element_rect(fill = '#ffffff')
        ,legend.title=element_text(size=8)
        ,legend.text=element_text(size=6)
  )


ggplotly(t)


# Save SF Object in case needed
output_sf = UK_DMA %>% mutate(adju_geo_country = 'GBR', filename = "GBR_DMA", updated = as.Date(Sys.time()))
saveRDS(output_sf, "~/Shapefile Data/Data/Outfiles/Outfiles SF/GBR_DMA_SF.rds")

# Convert to DF and Save for Qubole 
output_df = SfToDataframe(UK_DMA, idcol = 'geo_dma_bespoke')
output_df = output_df %>% mutate(adju_geo_country = 'GBR', filename = "GBR_DMA", updated = as.Date(Sys.time()))
write.csv(output_df, "~/Shapefile Data/Data/Outfiles/Outfiles Dataframes/GBR_DMA.csv")


