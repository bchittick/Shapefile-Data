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
library(ggplot2)


# very easy to find intersection with sf https://github.com/r-spatial/sf/issues/234
# or g-Touches in Rgeos package

setwd("~/Shapefile Data")

# read in polygon data and convert to df 
# readOGR() need some help from it 
polydata = readRDS("~/Shapefile Data/Data/gadm36_USA_1_sp.rds") # these files are much bigger
UK_COUNTRY <- readRDS("~/Shapefile Data/Data/SF Files/gadm36_GBR_2_sf.rds")


# Get intersections and rename
spare_mtx <- st_intersects(UK_COUNTRY, UK_COUNTRY) %>% as.list()
names(spare_mtx) = UK_COUNTRY$NAME_2 
names.lookup = as.data.frame(UK_COUNTRY$NAME_2) %>% mutate(number = as.character(row_number()))


# Mappping
map = setNames(names.lookup$`UK_COUNTRY$NAME_2`, names.lookup$number)

new.list = list()
for(i in 1:length(spare_mtx)) {
  print(spare_mtx[i])
  new.list[[i]] = as.character(map[spare_mtx[[i]]])
}

names(new.list) = names(spare_mtx)


# Convert List to DF
library(data.table)
df = as.data.frame(new.list)
as.data.frame(t(as.data.frame(new.list)))

cbind(region = names(spare_mtx),
      new.list)

df <- data.frame(matrix(unlist(new.list), byrow=T),stringsAsFactors=FALSE)

df = do.call(rbind.data.frame, new.list)

library (plyr)
df <- ldply (new.list, data.frame)
names(df) = c("region", "neighboors")
df$neighboors = as.character(df$neighboors)

sort(unique(df$region))

# Final Table with Neighbooring Regions
new.list[1][-c("Barnsley")]
t = unlist(new.list[1])[t != "Barnsley"]


# get into a table 
df <- cbind 
colnames(df) <- c("Company", "MPE")

# Some plots 
UK_COUNTRY$type = ifelse(UK_COUNTRY$NAME_2 %in% unlist(new.list[1]), 'red', 'grey')
ggplot(UK_COUNTRY) +
  aes(fill = ifelse(NAME_2 %in% unlist(new.list[1])[t != "Barnsley"],'red',
                    ifelse(NAME_2 == "Barnsley", 'purple', 'grey'))) +
  #scale_fill_viridis_d() +
  geom_sf() +
  coord_sf(crs = 4326) +
  guides(fill = FALSE) +
  ggtitle("Barnsley") +
  theme(title = element_text(size = 16),
        plot.margin = unit(c(0,0.1,0,0.25), "inches"))


spare_mtx[[1]] = replace(spare_mtx[[1]], names.lookup$`UK_COUNTRY$NAME_2`)
as.character(map[match(spare_mtx[[1]], map), "replace"])        
        
repl <- as.character(spare_mtx[[1]])
names(repl) <- match(replnames.lookup$`UK_COUNTRY$NAME_2`)
animal2 <- revalue(animal, repl)



Touching_List <- gTouches(polydata, byid = TRUE, returnDense=FALSE)
unlist(Touching_List)

spare_mtx <- st_intersects(UK_COUNTRY, UK_COUNTRY)


# getting regions which border each other 
philly_ctr_sp <- SpatialPoints(coords, proj4string = prj) #

# Other Stuff
polydata@data$id <-rownames(polydata@data)
dpts <- ggplot2::fortify(polydata) 
ff = merge(dpts, polydata@data, by.x = "id", by.y = "id") # this replicates a lot of info
lines = SpatialLines(polydata@polygons)


# Plot - have to plot the polygons (some DMA's not one polygon)
p = ggplot(data = ff,aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = NAME_1), color = "white") + 
  #scale_fill_manual(values=c('#CB2C30', '#BDB9B7','#7B1FA2'))  +
  coord_fixed(1.3) +
  guides(fill=FALSE) +  # do this to leave off the color legend
  labs(x = NULL
       ,y = NULL) +
  theme(text = element_text(family = 'Gill Sans', color = '#000000')
        ,plot.title = element_text(size = 28)
        ,plot.subtitle = element_text(size = 14)
        ,axis.ticks = element_blank()
        ,axis.text = element_blank()
        ,panel.grid = element_blank()
        ,panel.background = element_rect(fill = '#ffffff')
        ,plot.background = element_rect(fill = '#ffffff')
        ,legend.position = c(.12,.36)
        ,legend.background = element_blank()
        ,legend.key = element_blank()
  ) 

p = ggplotly(p, tooltip = c("text"))



# Plot - have to plot the polygons (some DMA's not one polygon)
p = ggplot(data = usmapdata,aes(x = longitude, y = latitude, group = dma.number)) + 
  geom_polygon(aes(), color = "white") + 
  #scale_fill_manual(values=c('#CB2C30', '#BDB9B7','#7B1FA2'))  +
  coord_fixed(1.3) +
  guides(fill=FALSE) +  # do this to leave off the color legend
  labs(x = NULL
       ,y = NULL) +
  theme(text = element_text(family = 'Gill Sans', color = '#000000')
        ,plot.title = element_text(size = 28)
        ,plot.subtitle = element_text(size = 14)
        ,axis.ticks = element_blank()
        ,axis.text = element_blank()
        ,panel.grid = element_blank()
        ,panel.background = element_rect(fill = '#ffffff')
        ,plot.background = element_rect(fill = '#ffffff')
        ,legend.position = c(.12,.36)
        ,legend.background = element_blank()
        ,legend.key = element_blank()
  ) 

p = ggplotly(p, tooltip = c("text"))

library(dplyr) polydata
usmapdata %>% group_by(dma.name) %>% summarise(n = n())
ff %>% group_by(NAME_1) %>% summarise(n = n())
