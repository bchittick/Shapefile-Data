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
library(stringr)
library(stringi)
library(plotly)
library(randomcoloR)
library(rmapshaper)


# very easy to find intersection with sf https://github.com/r-spatial/sf/issues/234
# or g-Touches in Rgeos package

setwd("~/Shapefile Data")
US_DMA = read_sf("~/Shapefile Data/Data/Json files/US_DMAS")
US_DMA$dma_name <- toupper(gsub(" ", "_",gsub(".*?(\\b[A-Za-z0-9 ]+\\b).*","\\1", US_DMA$dma_name)))

# Match with HCOM naming conventions, add DMA Bespoke
US_DMA_DIM = read.csv("~/Shapefile Data/Data/Bespoke Dim Table/US_DMA_LOOKUP.csv")
US_DMA_DIM = US_DMA_DIM %>% mutate_if(is.factor, as.character) %>% select(dma_code = orig_geo_dma, everything())


# Check which DMAs match - we don't have Alaska or Hawai coming through
datamart_names = US_DMA_DIM %>% mutate(`in_datamart` = 1)
shapefile_names = US_DMA %>% st_set_geometry(NULL) %>% unique() %>% mutate(`in_shapefile` = 1)
t = left_join(shapefile_names, datamart_names)

sum(t$in_shapefile, na.rm = T) / nrow(t)

##########################################
#           DMA - from Github
##########################################

US_DMA <- 
  US_DMA %>% 
  left_join(US_DMA_DIM) %>%
  mutate(orig_geo_dma_name = coalesce(orig_geo_dma_name, dma_name))
  

# Get intersections and rename
spare_mtx <- st_intersects(US_DMA, US_DMA) %>% as.list()
names(spare_mtx) = US_DMA$orig_geo_dma_name
names.lookup = data.frame("name" = US_DMA$orig_geo_dma_name ) %>% dplyr::mutate(number = as.character(row_number()))

# Create mapping table
map = setNames(names.lookup$name, names.lookup$number)

new.list = list()
for(i in 1:length(spare_mtx)) {
  print(spare_mtx[i])
  new.list[[i]] = as.character(map[spare_mtx[[i]]])
}

names(new.list) = names(spare_mtx)
new.list = new.list[!is.na(names(new.list))]

# Convert to DF
df <- ldply (new.list, data.frame)
names(df) = c("region", "neighbors")
df$neighbors = as.character(df$neighbors)
df = df %>% mutate(posa = 'USA', geo_grain = 'geo_dma_bespoke')
write.csv(df, "~/Shapefile Data/Data/Outfiles/USA_DMA_Pairs.csv")


# Plot - have to plot the polygons (some DMA's not one polygon)
plotdata = US_DMA %>% filter(!dma_name %in% c('JUNEAU', "FAIRBANKS", "ANCHORAGE","HONOLULU","ALASKA"))

t = ggplot(plotdata) +
  aes(fill = orig_geo_dma_name) +
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
output_sf = US_DMA %>% mutate(adju_geo_country = 'USA', filename = "USA_DMA", updated = as.Date(Sys.time()))
saveRDS(output_sf, "~/Shapefile Data/Data/Outfiles/Outfiles SF/USA_DMA_SF.rds")

# Convert to DF and Save for Qubole 
output_df = SfToDataframe(US_DMA, idcol = 'orig_geo_dma_name', simp.per = .75)
output_df = output_df %>% mutate(adju_geo_country = 'USA', filename = "USA_DMA", updated = as.Date(Sys.time())) %>%
  select(geo_dma_bespoke = orig_geo_dma_name, everything())
write.csv(output_df, "~/Shapefile Data/Data/Outfiles/Outfiles Dataframes/USA_DMA.csv")


##########################################
#           Region - from GADM
##########################################

setwd("~/Shapefile Data")
US_REGIONS = readRDS("~/Shapefile Data/Data/SF Files/US_REGIONS.rds")
US_REGIONS$NAME_1 <- toupper(gsub(" ", "_",gsub(".*?(\\b[A-Za-z0-9 ]+\\b).*","\\1", US_REGIONS$NAME_1)))

# Match with HCOM naming conventions, add DMA Bespoke
US_REGION_DIM = read.csv("~/Shapefile Data/Data/Bespoke Dim Table/US_REGION_LOOKUP.csv")
US_REGION_DIM = US_REGION_DIM %>% mutate_if(is.factor, as.character) %>% select(NAME_1 = orig_geo_region_name, everything())


# Check which cities match - we don't have Alaska or Hawai coming through
datamart_names = US_REGION_DIM %>% mutate(`in_datamart` = 1)
shapefile_names = US_REGIONS  %>% st_set_geometry(NULL) %>% select(NAME_1) %>% unique() %>% mutate(`in_shapefile` = 1)
t = left_join(shapefile_names, datamart_names)

sum(t$in_shapefile, na.rm = T) / nrow(t)

#names match - combine
US_REGIONS <- 
  US_REGIONS %>% 
  left_join(US_REGION_DIM) %>%
  st_cast("MULTIPOLYGON")

# Get intersections and rename
spare_mtx <- st_intersects(US_REGIONS, US_REGIONS) %>% as.list()
names(spare_mtx) = US_REGIONS$NAME_1
names.lookup = data.frame("name" = US_REGIONS$NAME_1 ) %>% dplyr::mutate(number = as.character(row_number()))

# Create mapping table
map = setNames(names.lookup$name, names.lookup$number)

new.list = list()
for(i in 1:length(spare_mtx)) {
  print(spare_mtx[i])
  new.list[[i]] = as.character(map[spare_mtx[[i]]])
}

names(new.list) = names(spare_mtx)
new.list = new.list[!is.na(names(new.list))]

# Convert to DF
df <- ldply (new.list, data.frame)
names(df) = c("region", "neighbors")
df$neighbors = as.character(df$neighbors)
df = df %>% mutate(posa = 'USA', geo_grain = 'adju_geo_region_name')
write.csv(df, "~/Shapefile Data/Data/Outfiles/USA_Regions_Pairs.csv")


# Plot - have to plot the polygons (some DMA's not one polygon)
plotdata = US_REGIONS %>% filter(!NAME_1 %in% c('ALASKA', "HAWAII"))

ggplot(plotdata) +
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
output_sf = US_REGIONS %>% mutate(adju_geo_country = 'USA', filename = "USA_REGION", updated = as.Date(Sys.time()))
saveRDS(output_sf, "~/Shapefile Data/Data/Outfiles/Outfiles SF/USA_REGION_SF.rds")

# Convert to DF and Save for Qubole 
output_df = SfToDataframe(US_REGIONS, idcol = 'NAME_1')
output_df = output_df %>% mutate(adju_geo_country = 'USA', filename = "USA_REGION", updated = as.Date(Sys.time())) %>% 
  select(adju_geo_region_name = NAME_1, everything())
write.csv(output_df, "~/Shapefile Data/Data/Outfiles/Outfiles Dataframes/USA_REGION.csv")


