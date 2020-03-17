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


# cities only match at 57%, so not going to try and build city list 

setwd("~/Shapefile Data")
ESP_CITY = readRDS("~/Shapefile Data/Data/SF Files/ESP_City.rds")
ESP_CITY$NAME_1 = stri_trans_general(ESP_CITY$NAME_1, "Latin-ASCII")
ESP_CITY$NAME_2 = stri_trans_general(ESP_CITY$NAME_2, "Latin-ASCII")
ESP_CITY$NAME_4 = stri_trans_general(ESP_CITY$NAME_4, "Latin-ASCII")
ESP_CITY$NAME_1 <- toupper(gsub(" ", "_",gsub(".*?(\\b[A-Za-z0-9 ]+\\b).*","\\1", ESP_CITY$NAME_1)))
ESP_CITY$NAME_2 <- toupper(gsub(" ", "_",gsub(".*?(\\b[A-Za-z0-9 ]+\\b).*","\\1", ESP_CITY$NAME_2)))
ESP_CITY$NAME_4 <- toupper(gsub(" ", "_",gsub(".*?(\\b[A-Za-z0-9 ]+\\b).*","\\1", ESP_CITY$NAME_4)))

# clean up some naming differnces at DMA and Region levels 
ESP_CITY$NAME_1 = recode(ESP_CITY$NAME_1, 
                "PRINCIPADO_DE_ASTURIAS" = "ASTURIAS",
                "ISLAS_CANARIAS" = "CANARIAS",
                "CATALUNA" = "CATALUNA_",
                "COMUNIDAD_DE_MADRID" = "MADRID",
                "REGION_DE_MURCIA" = "MURCIA",
                "COMUNIDAD_FORAL_DE_NAVARRA" = "NAVARRA",
                "COMUNIDAD_VALENCIANA" = "VALENCIA",
                "CASTILLA" = "CASTILLA_LA_MANCHA")

ESP_CITY$NAME_2 = recode(ESP_CITY$NAME_2, 
                         "A_CORUNA" = "LA_CORUNA",
                         "OURENSE" = "ORENSE",
                         "LLEIDA" = "LERIDA",
                         "BALEARES" = "ISLAS_BALEARES")



sort(unique(paste0(shapefile_names$NAME_1,".",shapefile_names$NAME_2)))
sort(unique(paste0(datamart_names$NAME_1, ".", datamart_names$NAME_2)))


# Match with HCOM naming conventions, add DMA Bespoke
ESP_DIM = read.csv("~/Shapefile Data/Data/Bespoke Dim Table/ESP_CITY_DMA_LOOKUP.csv")
ESP_DIM = ESP_DIM %>% mutate_if(is.factor, as.character) %>% select(NAME_1 = geo_dma_bespoke, NAME_2 = adju_geo_region_name, NAME_4 = adju_geo_city)


# Check which cities match - we don't have Alaska or Hawai coming through
datamart_names = ESP_DIM %>% select(NAME_1) %>% unique() %>% mutate(`in_datamart` = 1)
shapefile_names = ESP_CITY %>% st_set_geometry(NULL) %>% select(NAME_1) %>% unique() %>% mutate(`in_shapefile` = 1)
t = left_join(datamart_names, shapefile_names)

sum(t$in_shapefile, na.rm = T) / nrow(t)

##########################################
#           Region
##########################################

ESP_REGION <- 
  ESP_CITY %>% 
  group_by(NAME_2) %>%
  dplyr::summarise() %>%
  ungroup()

# turns in multipolygon
ESP_REGION = ESP_REGION %>% st_cast("MULTIPOLYGON")

  
# Get intersections and rename
spare_mtx <- st_intersects(ESP_REGION, ESP_REGION) %>% as.list()
names(spare_mtx) = ESP_REGION$NAME_2
names.lookup = data.frame("name" = ESP_REGION$NAME_2 ) %>% dplyr::mutate(number = as.character(row_number()))

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
df = df %>% mutate(posa = 'ESP', geo_grain = 'adju_geo_region_name')
write.csv(df, "~/Shapefile Data/Data/Outfiles/ESP_Regions_Pairs.csv")


# Plot - have to plot the polygons (some DMA's not one polygon)
plotdata = ESP_REGION %>% filter(! NAME_2 %in% c("LAS_PALMAS", "SANTA_CRUZ_DE_TENERIFE"))

ggplot(plotdata) +
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
output_sf = ESP_REGION %>% mutate(adju_geo_country = 'ESP', filename = "ESP_REGION", updated = as.Date(Sys.time())) %>%
  mutate(adju_geo_region_name = NAME_2)
saveRDS(output_sf, "~/Shapefile Data/Data/Outfiles/Outfiles SF/ESP_REGION_SF.rds")

# Convert to DF and Save for Qubole 
output_df = SfToDataframe(ESP_REGION, idcol = 'NAME_2')
output_df = output_df %>% mutate(adju_geo_country = 'ESP', filename = "ESP_REGION", updated = as.Date(Sys.time())) %>% select(adju_geo_region_name = NAME_2, everything())
write.csv(output_df, "~/Shapefile Data/Data/Outfiles/Outfiles Dataframes/ESP_REGION.csv")


##########################################
#           DMA
##########################################

ESP_DMA <- 
  ESP_CITY %>% 
  group_by(NAME_1) %>%
  dplyr::summarise() %>%
  ungroup()

# turns in multipolygon
ESP_DMA = ESP_DMA %>% st_cast("MULTIPOLYGON")


# Get intersections and rename
spare_mtx <- st_intersects(ESP_DMA, ESP_DMA) %>% as.list()
names(spare_mtx) = ESP_DMA$NAME_1
names.lookup = data.frame("name" = ESP_DMA$NAME_1 ) %>% dplyr::mutate(number = as.character(row_number()))

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
df = df %>% mutate(posa = 'ESP', geo_grain = 'geo_dma_bespoke')
write.csv(df, "~/Shapefile Data/Data/Outfiles/ESP_DMA_Pairs.csv")


# Plot - have to plot the polygons (some DMA's not one polygon)
plotdata = ESP_DMA %>% filter(! NAME_1 %in% c("CANARIAS"))

ggplot(plotdata) +
  aes(fill = NAME_1) +
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
output_sf = ESP_DMA %>% mutate(adju_geo_country = 'ESP', filename = "ESP_DMA", updated = as.Date(Sys.time())) %>%
  mutate(geo_dma_bespoke = NAME_1)
saveRDS(output_sf, "~/Shapefile Data/Data/Outfiles/Outfiles SF/ESP_DMA_SF.rds")

# Convert to DF and Save for Qubole 
output_df = SfToDataframe(ESP_DMA, idcol = 'NAME_1')
output_df = output_df %>% mutate(adju_geo_country = 'ESP', filename = "ESP_DMA", updated = as.Date(Sys.time())) %>% select(geo_dma_bespoke = NAME_1, everything())
write.csv(output_df, "~/Shapefile Data/Data/Outfiles/Outfiles Dataframes/ESP_DMA.csv")
