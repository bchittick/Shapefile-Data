###################################
# US CORONOVIRUS OUTBREAK BY DMA
###################################
library(readr)
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
library(gganimate)
library(scales)
library(color)
library(lwgeom)

setwd("~/Shapefile Data")

#read in shapefile data & dim table
US_DMA = read_sf("~/Shapefile Data/Data/Json files/US_DMAS")
US_DMA$dma_name <- toupper(gsub(" ", "_",gsub(".*?(\\b[A-Za-z0-9 ]+\\b).*","\\1", US_DMA$dma_name)))

# Match with HCOM naming conventions, add DMA Bespoke
US_DMA_DIM = read.csv("~/Shapefile Data/Data/Bespoke Dim Table/US_DMA_LOOKUP.csv")
US_DMA_DIM = US_DMA_DIM %>% mutate_if(is.factor, as.character) %>% select(dma_code = orig_geo_dma, everything())


# Check which cities match - we don't have Alaska or Hawai coming through
datamart_names = US_DMA_DIM %>% mutate(`in_datamart` = 1)
shapefile_names = US_DMA %>% st_set_geometry(NULL) %>% unique() %>% mutate(`in_shapefile` = 1)
t = left_join(shapefile_names, datamart_names)

sum(t$in_shapefile, na.rm = T) / nrow(t)


US_DMA <- 
  US_DMA %>% 
  left_join(US_DMA_DIM) %>%
  mutate(orig_geo_dma_name = coalesce(orig_geo_dma_name, dma_name))


# read in cancellations Data 
US_CANCELATIONS<- read_csv("Data/US_CANCELATIONS_DMA.csv", 
                  col_types = cols(gmt_trans_date = col_date(format = "%m/%d/%Y")))

US_CANCELATIONS = as.data.frame(US_CANCELATIONS) %>% select(dma_code = geo_dma, everything())


t = aggdata %>% group_by(gmt_trans_date) %>%
  dplyr::summarise_at(c("purchase", "cancel"), sum, na.rm= T) %>%
  mutate(cancel.rate = cancel/purchase) %>% ungroup()

t %>%
plot_ly(x = ~gmt_trans_date, y = ~cancel.rate, type = 'scatter', mode = 'lines')


########################
# Maps and Animation 
########################
# aggregated data 
aggdata = US_CANCELATIONS %>% group_by(dma_code, gmt_trans_date) %>% dplyr::summarise_at(c("purchase", "cancel"), sum, na.rm= T) %>%
  mutate(cancel.rate = cancel/purchase) %>%  filter(gmt_trans_date >= as.Date('2020-02-12')) %>% 
  mutate(cancel.rate = coalesce(cancel.rate,1))

# Cancel rate and bucket for plot
aggdata$cancel.rate.quan <-cut(aggdata$cancel.rate, breaks=quantile(aggdata$cancel.rate, seq(0,1, length.out=8)), include.lowest=T, labels=F)
aggdata = aggdata %>%
  left_join(as.data.frame(cbind("ntile" = quantile(aggdata$cancel.rate, seq(0,1, length.out=8))[-8], "cancel.rate.quan" = seq(1,7, length.out=7))))

aggdata$CancelRateBucket = as.character(paste0(round(aggdata$ntile, 3) * 100, "+ %"))


# Plot - have to plot the polygons (some DMA's not one polygon)
plotdata2 = US_DMA %>% filter(!dma_name %in% c('JUNEAU', "FAIRBANKS", "ANCHORAGE","HONOLULU","ALASKA")) %>% 
  merge(aggdata) %>% lwgeom::st_make_valid() %>% sf::st_as_sf()

class(plotdata)
class(plotdata2)

# color function 
colfunc <- colorRampPalette(c("#b7df89", "red"))
cols = colfunc(7)
breaks = quantile(plotdata$cancel.rate, seq(0,1, length.out=11))[-11]


p = ggplot(plotdata) +
  geom_sf(aes(fill = cancel.rate)) +
  #scale_fill_manual(breaks=breaks, values=cols) +
  scale_fill_gradient(low = "green", high = "red", rescaler = scales::rescale,  n.breaks = 10, labels = percent, name = 'Cancelation Rate',
                      limits = c(breaks["10%"], breaks["90%"]), oob = scales::squish) + 
  #scale_fill_gradient2(low = "#b7df89", high = "red", midpoint = quantile(aggdata$cancel.rate,.5), na.value = NA, labels = percent) +
  #scale_fill_gradientn(colours =  colfunc(10)
  #                     ,labels = percent
  #                     ,values = scales::rescale(breaks)
  #                     ,breaks = breaks) +
  coord_sf(crs = 4326) +
  guides(fill = guide_legend(reverse = T)) +
  #ggtitle("HCOM Cancelations Rate Last 30 Days by DMA") +
  #labs(x = NULL,y = NULL) +
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
        #,legend.position = c(.12,.12)
        #,legend.background = element_blank()
        #,legend.key = element_blank()
  ) 


anim = p + transition_time(gmt_trans_date) +
  labs(title = "HCOM Cancelation Rate Last 30 Days by DMA \n Date: {frame_time}")


start.time = Sys.time()
gganimate::animate(anim, fps = 1, nframes = length(unique(plotdata$gmt_trans_date)), renderer = gifski_renderer("gganim.gif"))
start.time - Sys.time()
anima
######### Examples 
library(gapminder)
library(gifski)
library(transformr)

head(gapminder)
p <- ggplot(
  gapminder, 
  aes(x = gdpPercap, y=lifeExp, size = pop, colour = country)
) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectancy")
p


anim = p + transition_time(year) +
  labs(title = "Year: {frame_time}")

animate(anim, nframes = 20, renderer = gifski_renderer("gganim.gif"))