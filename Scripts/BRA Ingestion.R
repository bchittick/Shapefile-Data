##################################################
#  Shapefile Data Ingestion
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
library(stringr)
library(stringi)
library(plotly)
library(randomcoloR)
library(rmapshaper)


# very easy to find intersection with sf https://github.com/r-spatial/sf/issues/234
# or g-Touches in Rgeos package

setwd("~/Shapefile Data")


# read in polygon data, SF format - fix names (NAME_2 seems to be what we want) - all regions match
BRA_CITY <- readRDS("~/Shapefile Data/Data/SF Files/BRA_CITY.rds")
BRA_CITY$NAME_1 = stri_trans_general(BRA_CITY$NAME_1, "Latin-ASCII")
BRA_CITY$NAME_2 = stri_trans_general(BRA_CITY$NAME_2, "Latin-ASCII")
BRA_CITY$NAME_3 = stri_trans_general(BRA_CITY$NAME_3, "Latin-ASCII")
BRA_CITY$NAME_1 <- toupper(gsub(" ", "_",gsub(".*?(\\b[A-Za-z0-9 ]+\\b).*","\\1", BRA_CITY$NAME_1)))
BRA_CITY$NAME_2 <- toupper(gsub(" ", "_",gsub(".*?(\\b[A-Za-z0-9 ]+\\b).*","\\1", BRA_CITY$NAME_2)))
BRA_CITY$NAME_3 <- toupper(gsub(" ", "_",gsub(".*?(\\b[A-Za-z0-9 ]+\\b).*","\\1", BRA_CITY$NAME_3)))

# Match with HCOM naming conventions, add DMA Bespoke
bra_city_DMA_dim = read.csv("~/Shapefile Data/Data/Bespoke Dim Table/BRA City Lookup.csv")
bra_city_DMA_dim = bra_city_DMA_dim %>% mutate_if(is.factor, as.character)

# All Regions Match
datamart_names = data.frame("region" = bra_city_DMA_dim$adju_geo_region_name) %>% unique() %>% mutate_if(is.factor, as.character)
shapefile_names = BRA_CITY %>% st_set_geometry(NULL) %>% select(region = NAME_1) %>% unique() %>% mutate(`in_shapefile` = 1)
t = left_join(datamart_names, shapefile_names)


##########################################
#           Region
##########################################

BRA_REGION <- 
  BRA_CITY %>% 
  group_by(NAME_1) %>%
  dplyr::summarise() %>%
  ungroup() %>% 
  st_cast(., 'MULTIPOLYGON')

# Get intersections and rename
spare_mtx <- st_intersects(BRA_REGION, BRA_REGION) %>% as.list()
names(spare_mtx) = BRA_REGION$NAME_1 
names.lookup = data.frame("name" = BRA_REGION$NAME_1 ) %>% dplyr::mutate(number = as.character(row_number()))

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
df = df %>% mutate(posa = 'BRA', geo_grain = 'adju_geo_region_name')
write.csv(df, "~/Shapefile Data/Data/Outfiles/BRA_Regions_Pairs.csv")

# Test Plot
BRA_REGION = BRA_REGION %>% mutate(color = case_when(NAME_1 %in% unlist(new.list['SAO_PAULO']) & NAME_1 != 'SAO_PAULO' ~ 'Red',
                                                     NAME_1 == 'SAO_PAULO' ~ 'purple',
                                                     TRUE ~ 'grey'))


ggplot(BRA_REGION) +
  aes(fill = NAME_1) +
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
output_sf = BRA_REGION %>% mutate(adju_geo_country = 'BRA', filename = "BRA_REGION", updated = as.Date(Sys.time())) %>%
  mutate(adju_geo_region_name = NAME_1)
saveRDS(output_sf, "~/Shapefile Data/Data/Outfiles/Outfiles SF/BRA_REGION_SF.rds")

# Convert to DF and Save for Qubole 
output_df = SfToDataframe(BRA_REGION, idcol = 'NAME_1')
output_df = output_df %>% mutate(adju_geo_country = 'BRA', filename = "BRA_REGION", updated = as.Date(Sys.time())) %>% select(adju_geo_region_name = NAME_1, everything())
write.csv(output_df, "~/Shapefile Data/Data/Outfiles/Outfiles Dataframes/BRA_REGION.csv")

##########################################
#           City 
##########################################
# 6k cities in the Adobe Tables - distrcit seems to be better option as it will join to large cities at least
# cities match at 95% with NAME_3 (municipality - 10k Plus) but no Sao Paulo City, match with 78% at NAME_2 (district - 5k plus)

# Check which cities match
datamart_names = bra_city_DMA_dim %>% select(region = adju_geo_region_name, city = adju_geo_city) %>% unique() %>% mutate_if(is.factor, as.character)
shapefile_names = BRA_CITY %>% st_set_geometry(NULL) %>% select(region = NAME_1, city = NAME_2) %>% unique() %>% mutate(`in_shapefile` = 1)
t = left_join(datamart_names, shapefile_names)

sum(t$in_shapefile, na.rm = T) / nrow(t)

# Concatenate city and region so can create unique inde
BRA_CITY <- BRA_CITY %>% mutate(REGION_CITY = paste0(NAME_1, ".", NAME_2))

# Get intersections and rename
spare_mtx <- st_intersects(BRA_CITY, BRA_CITY) %>% as.list()
names(spare_mtx) = BRA_CITY$REGION_CITY
names.lookup = data.frame("name" = BRA_CITY$REGION_CITY ) %>% dplyr::mutate(number = as.character(row_number()))

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
df = df %>% mutate(posa = 'BRA', geo_grain = 'adju_geo_region_city')
write.csv(df, "~/Shapefile Data/Data/Outfiles/BRA_City_Pairs.csv")


# Create Simplfied GGPlot
BRA_CITY = BRA_CITY %>% 
  left_join(bra_city_DMA_dim %>% mutate(REGION_CITY = paste0(adju_geo_region_name, ".", adju_geo_city))) 



# Plot - have to plot the polygons (some DMA's not one polygon)
plotdata = BRA_CITY %>% filter(! NAME_2 %in% c("LAS_PALMAS", "SANTA_CRUZ_DE_TENERIFE"))

ggplot(BRA_CITY ) +
  aes(fill = NAME_1) +
  scale_fill_viridis_d() +
  geom_sf() +
  coord_sf(crs = 4326) +
  guides(fill = FALSE) +
  ggtitle("City") +
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
output_sf = BRA_CITY %>% mutate(adju_geo_country = 'BRA', filename = "BRA_CITY", updated = as.Date(Sys.time()))
saveRDS(output_sf, "~/Shapefile Data/Data/Outfiles/Outfiles SF/BRA_CITY_SF.rds")

# Convert to DF and Save for Qubole 
output_df = SfToDataframe(BRA_CITY, idcol = 'REGION_CITY', savecols = c("geo_dma_bespoke", "adju_geo_region_name", "adju_geo_city"))
output_df = output_df %>% mutate(adju_geo_country = 'BRA', filename = "BRA_CITY", updated = as.Date(Sys.time())) 
write.csv(output_df, "~/Shapefile Data/Data/Outfiles/Outfiles Dataframes/BRA_CITY.csv")

 
##########################################
#           BESPOKE DMA 
##########################################
# Will need to aggregate up from Regions 

BRA_DMA <- 
  BRA_CITY %>% 
  left_join(bra_city_DMA_dim %>% mutate(REGION_CITY = paste0(adju_geo_region_name, ".", adju_geo_city))) %>%
  group_by(geo_dma_bespoke) %>%
  dplyr::summarise() %>%
  ungroup() %>% 
  st_cast(., 'MULTIPOLYGON')

# Get intersections and rename
spare_mtx <- st_intersects(BRA_DMA, BRA_DMA) %>% as.list()
names(spare_mtx) = BRA_DMA$geo_dma_bespoke 
names.lookup = data.frame("name" = BRA_DMA$geo_dma_bespoke) %>% dplyr::mutate(number = as.character(row_number()))

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
df = df %>% mutate(posa = 'BRA', geo_grain = 'geo_dma_bespoke')
write.csv(df, "~/Shapefile Data/Data/Outfiles/BRA_DMA_Pairs.csv")


# Plot - have to plot the polygons (some DMA's not one polygon)
plotdata = BRA_DMA %>% mutate(geo_dma_bespoke = ifelse(geo_dma_bespoke == "UNCLASSIFIED", NA, geo_dma_bespoke))

unique(BRA_DMA$geo_dma_bespoke)

t = ggplot(plotdata) +
  aes(fill = geo_dma_bespoke) +
  scale_fill_viridis_d(na.value = 'white') +
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
output_sf = BRA_DMA %>% mutate(adju_geo_country = 'BRA', filename = "BRA_DMA", updated = as.Date(Sys.time()))
saveRDS(output_sf, "~/Shapefile Data/Data/Outfiles/Outfiles SF/BRA_DMA_SF.rds")

# Convert to DF and Save for Qubole 
output_df = SfToDataframe(BRA_DMA, idcol = 'geo_dma_bespoke')
output_df = output_df %>% mutate(adju_geo_country = 'BRA', filename = "BRA_DMA", updated = as.Date(Sys.time())) 
write.csv(output_df, "~/Shapefile Data/Data/Outfiles/Outfiles Dataframes/BRA_DMA.csv")



