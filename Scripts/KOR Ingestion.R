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

# read in polygon data, SF format - fix names (NAME_2 seems to be what we want) - all regions match
KOR_DMA <- readRDS("~/Shapefile Data/Data/SF Files/KOR_REGIONS.rds")
KOR_DMA$NAME_1 <- toupper(gsub(" ", "_",gsub(".*?(\\b[A-Za-z0-9 ]+\\b).*","\\1", KOR_DMA$NAME_1)))
KOR_DMA$NAME_2 <- toupper(gsub(" ", "_",gsub(".*?(\\b[A-Za-z0-9 ]+\\b).*","\\1", KOR_DMA$NAME_2)))
KOR_DMA$NAME_1 = recode(KOR_DMA$NAME_1, 
                        "CHUNGCHEONGBUK" = "CHUNGCHEONGBUG",
                        "GANGWON" = "GANGWEON",
                        "GYEONGSANGBUK" = "GYEONGSANGBUG",
                        "JEOLLABUK" = "JEONRABUG",
                        "JEOLLANAM" = "JEONRANAM")
KOR_DMA$region.dma = paste0(KOR_DMA$NAME_1, ".", KOR_DMA$NAME_2)
KOR_DMA$region.dma = recode(KOR_DMA$region.dma,
                            "BUSAN.DONGNAE" = "BUSAN.DONGRAE",
                            "BUSAN.SEO" = "BUSAN.JUNG",
                            "CHUNGCHEONGBUG.CHEONGJU" = "CHUNGCHEONGBUG.HEUNGDEOK",
                            "CHUNGCHEONGBUG.CHEONGWON" = "CHUNGCHEONGBUG.SANGDANG",
                            "CHUNGCHEONGNAM.CHEONAN" = "CHUNGCHEONGNAM.SEOBUK",
                            "GANGWEON.GANGNEUNG" = "GANGWEON.GANGREUNG",
                            "GYEONGGI.ANSOENG" = "GYEONGGI.ANSEONG",
                            "GYEONGGI.BUCHEON" = "GYEONGGI.SOSA",
                            "GYEONGGI.ANYANG" = "GYEONGGI.MANAN",
                            "GYEONGGI.ANSAN" = "GYEONGGI.DANWON",
                            "GYEONGGI.GOYANG" = "GYEONGGI.DEOGYANG",
                            "GYEONGGI.SEONGNAM" = "GYEONGGI.SUJEONG",
                            "GYEONGGI.SUWON" = "GYEONGGI.PALDAL",
                            "GYEONGGI.YONGIN" = "GYEONGGI.SUJI",
                            "GYEONGSANGBUG.POHANG" = "GYEONGSANGBUG.BUK",
                            "JEONRABUG.JEONJU" = "JEONRABUG.WANSAN",
                            "SEOUL.GANDONG" = "SEOUL.GANGDONG",
                            "SEOUL.GEUM" = "SEOUL.GEUMCHEON",
                            "SEOUL.EUN" = "SEOUL.EUNPYEONG",
                            "SEOUL.DONG" = "SEOUL.DONGDAEMUN",
                            "SEOUL.GWANG" = "SEOUL.GWANGJIN",
                            "SEOUL.JUNGNANG" = "SEOUL.JUNGRANG")

sort(unique(paste0(KOR_DMA$NAME_1, ".", KOR_DMA$NAME_2)))

# Match with HCOM naming conventions, add DMA Bespoke
KOR_DMA_dim = read.csv("~/Shapefile Data/Data/Bespoke Dim Table/KOR_DMA_LOOKUP.csv")
KOR_DMA_dim = KOR_DMA_dim %>% mutate_if(is.factor, as.character) %>% 
              mutate(short_region = str_extract(adju_geo_region_name, "[^_]+"),
                     short_dma = str_extract(geo_dma_bespoke, "[^_]+")) %>%
              mutate(region.dma = paste0(short_region, ".", short_dma ))


# All Regions Match
datamart_names = data.frame("region" = KOR_DMA_dim$region.dma) %>% unique() %>% mutate_if(is.factor, as.character) %>% mutate(`in_datamart` = 1)
shapefile_names = KOR_DMA %>% st_set_geometry(NULL) %>% select(region = region.dma) %>% unique() %>% mutate(`in_shapefile` = 1)
t = left_join(datamart_names, shapefile_names)


sum(t$in_shapefile, na.rm = T) / nrow(t)

# There are unmapped names on both sides

# Investigating DMA names and renaming above 
print("Unmatched Shapefile Names:") 
print(paste0(setdiff(shapefile_names$region, datamart_names$region)))

print("Unmatched Shapefile Names:") 
print(paste0(setdiff(datamart_names$region, shapefile_names$region)))



##########################################
#           Region
##########################################

# Group up DMA Data and cast as multiploygon at end or run into erros later 
KOR_REGION <- 
  KOR_DMA %>% 
  left_join(KOR_DMA_dim %>% select(adju_geo_region_name, geo_dma_bespoke, region.dma) %>% unique()) %>%
  mutate(adju_geo_region_name = coalesce(adju_geo_region_name,NAME_1)) %>%
  group_by(adju_geo_region_name) %>%
  dplyr::summarise() %>%
  ungroup() %>% 
  st_cast(., 'MULTIPOLYGON')

# Get intersections and rename
spare_mtx <- st_intersects(KOR_REGION, KOR_REGION) %>% as.list()
names(spare_mtx) = KOR_REGION$adju_geo_region_name 
names.lookup = data.frame("name" = KOR_REGION$adju_geo_region_name ) %>% dplyr::mutate(number = as.character(row_number()))

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
df = df %>% mutate(posa = 'KOR', geo_grain = 'adju_geo_region_name')
write.csv(df, "~/Shapefile Data/Data/Outfiles/KOR_Regions_Pairs.csv")

# Test Plot
plotdata = KOR_REGION %>% mutate(color = case_when(adju_geo_region_name %in% unlist(new.list['DAEGU']) & adju_geo_region_name != 'DAEGU' ~ 'Red',
                                                   adju_geo_region_name == 'DAEGU' ~ 'purple',
                                                   TRUE ~ 'grey')) 
  #lwgeom::st_make_valid() %>%
  #sf::st_as_sf()


# Plot - have to plot the polygons (some DMA's not one polygon)
t = ggplot(plotdata) +
  aes(fill = adju_geo_region_name) +
  scale_fill_viridis_d() +
  geom_sf() +
  coord_sf(crs = 4326) +
  guides(fill = FALSE) +
  ggtitle("Region") +
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
output_sf = KOR_REGION %>% mutate(adju_geo_country = 'KOR', filename = "KOR_REGION", updated = as.Date(Sys.time()))
saveRDS(output_sf, "~/Shapefile Data/Data/Outfiles/Outfiles SF/KOR_REGION_SF.rds")

# Convert to DF and Save for Qubole 
output_df = SfToDataframe(KOR_REGION, idcol = 'adju_geo_region_name')
output_df = output_df %>% mutate(adju_geo_country = 'KOR', filename = "KOR_REGION", updated = as.Date(Sys.time()))
write.csv(output_df, "~/Shapefile Data/Data/Outfiles/Outfiles Dataframes/KOR_REGION.csv")



##########################################
#           DMA
##########################################
# see above about mismatches - 92% of Adobe Regions *now* have a match after manual intervention

KOR_DMA_JOINED <- 
  KOR_DMA %>% 
  left_join(KOR_DMA_dim %>% select(adju_geo_region_name, geo_dma_bespoke, region.dma) %>% unique()) %>%
  mutate(geo_dma_bespoke = coalesce(geo_dma_bespoke,NAME_2))

# Get intersections and rename
spare_mtx <- st_intersects(KOR_DMA_JOINED, KOR_DMA_JOINED) %>% as.list()
names(spare_mtx) = KOR_DMA_JOINED$geo_dma_bespoke 
names.lookup = data.frame("name" = KOR_DMA_JOINED$geo_dma_bespoke ) %>% dplyr::mutate(number = as.character(row_number()))

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
df = df %>% mutate(posa = 'KOR', geo_grain = 'geo_dma_bespoke')
write.csv(df, "~/Shapefile Data/Data/Outfiles/KOR_DMA_Pairs.csv")

# Test Plot

plotdata = KOR_DMA_JOINED %>% mutate(color = case_when(geo_dma_bespoke %in% unlist(new.list['YEONGJU_SI']) & geo_dma_bespoke != 'YEONGJU_SI' ~ 'Red',
                                                       geo_dma_bespoke == 'YEONGJU_SI' ~ 'purple',
                                                       TRUE ~ 'grey'))

# Plot - have to plot the polygons (some DMA's not one polygon)
t = ggplot(plotdata) +
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
output_sf = KOR_DMA_JOINED %>% mutate(adju_geo_country = 'KOR', filename = "KOR_DMA", updated = as.Date(Sys.time()))
saveRDS(output_sf, "~/Shapefile Data/Data/Outfiles/Outfiles SF/KOR_DMA_SF.rds")

# Convert to DF and Save for Qubole 
output_df = SfToDataframe(KOR_DMA_JOINED, idcol = 'geo_dma_bespoke', savecols = c("adju_geo_region_name"))
output_df = output_df %>% mutate(adju_geo_country = 'KOR', filename = "KOR_DMA", updated = as.Date(Sys.time()))
write.csv(output_df, "~/Shapefile Data/Data/Outfiles/Outfiles Dataframes/KOR_DMA.csv")

