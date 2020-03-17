###########################################
# Examples 
###########################################
library(devtools)
library(tidyr)
#install_github("RamiKrispin/coronavirus")
library(coronavirus)
library(ggplot2)
library(dplyr)
library(sf)
library(readr)
library(dplyr)
library(rmapshaper)
library(gganimate)
library(scales)
library(lwgeom)
library(gifski)
library(transformr)

##############################################
# 1) US Coronavirus cases vs Cancelation Rate 
###############################################

setwd("~/Shapefile Data")

# load coronavirus dataset & US DMA SF 
# Match with HCOM naming conventions, add DMA Bespoke
US_REGION_DIM = read.csv("~/Shapefile Data/Data/Bespoke Dim Table/US_REGION_LOOKUP.csv")
US_REGION_DIM = US_REGION_DIM %>% mutate_if(is.factor, as.character) %>% select(NAME_1 = orig_geo_region_name, everything())


US_REGION = readRDS("~/Shapefile Data/Data/Outfiles/Outfiles SF/USA_REGION_SF.rds")
US_REGION = ms_simplify(US_REGION,  keep=.01)
US_Cancelations = read.csv("~/Shapefile Data/Scripts/Examples/US_Region_Cancellations.csv") %>%
  mutate_if(is.factor, as.character) %>% mutate(date = as.Date(gmt_trans_date, format = '%m/%d/%y')) %>%
  mutate(orig_geo_region = toupper(geo_region)) %>%
  left_join(US_REGION_DIM) %>% mutate(state= NAME_1)

# coronovirus dataset 
data("coronavirus")
us_coronavirus = coronavirus %>% filter(Country.Region == 'US') %>%
  mutate(state = toupper(gsub(" ", "_",gsub(".*?(\\b[A-Za-z0-9 ]+\\b).*","\\1", Province.State)))) %>%
  dplyr::group_by(state, date, type) %>% dplyr::summarise(cases = sum(cases, na.rm = T)) %>%
  ungroup() %>%
  spread(key = type, value = cases) %>%
  replace(is.na(.),0) %>% group_by(state) %>% arrange(date) %>%
  mutate_at(c("confirmed", "death", "recovered"), cumsum) %>% 
  ungroup()


# See if names join 
US_Joined = US_Cancelations %>%
  merge(us_coronavirus) %>% 
  filter(orig_geo_region != '?') %>% replace(is.na(.),0) %>%
  mutate(cancel.rate = cancel / purchase )


# plot data 
plotdata = US_REGION %>% filter(!NAME_1 %in% c('ALASKA', "HAWAII")) %>%
  merge(US_Joined %>% filter(date >= as.Date('2020-02-14'))) %>%
  mutate(confirmed = ifelse(confirmed == 0, NA, confirmed)) %>%
  lwgeom::st_make_valid() %>% sf::st_as_sf() 
 
# Breaks

breaks = c(0, quantile(plotdata$cancel.rate,probs = c(0.05, .10, 0.2, 0.4, .50, 0.6, 0.8, .90, 0.95 ,0.99), type = 1))
names(breaks) = c("<5%", paste0(names(breaks[2:10]), "+"), paste0(names(breaks[length(breaks)]), "+"))

plotdata$breaks <- as.character(cut(plotdata$cancel.rate, 
                       breaks=c(breaks,Inf),
                       labels=names(breaks),
                       include.lowest = T))

    
# plot 
p = ggplot(plotdata) +
  geom_sf(aes(fill = cancel.rate)) +
  #scale_fill_gradient(low = "green", high = "red", rescaler = scales::rescale, n.breaks = 10, 
  #                    labels = percent, name = 'Cancelation Rate',
  #                    limits = c(breaks["10%+"], breaks["90%+"]), oob = scales::squish) + 
  scale_fill_gradient(low = "green", high = "red", trans = "log", n.breaks = 9, labels = percent) +
  geom_point(aes(size =confirmed, geometry = geometry), color = 'darkred', stat = "sf_coordinates") + 
  geom_text(aes(geometry = geometry,label = ifelse(NAME_1 %in% c("CALIFORNIA", "NEW_YORK", "WASHINGTON") & !is.na(confirmed),
                                                   paste0("  " ,orig_geo_region, ": \n", confirmed), " ")),
            stat = "sf_coordinates", colour = "black", nudge_x = .9, nudge_y = -.9) +
  scale_size(range = c(3,10)) + 
  #geom_point(aes(size = cancel.rate, geometry = geometry)) + 
  coord_sf(crs = 4326) +
  guides(fill = guide_legend(title = "Cancel Rate", reverse = T), size = guide_legend(title = "COVID-19 Cases")) +
  #ggtitle("Regions") +
  labs(x = NULL,y = NULL) +
  theme(text = element_text(color = '#000000')
        ,title = element_text(size = 18, color='darkred', face = 'bold' )
        ,plot.margin = unit(c(0,0.1,0,0.25), "inches")
        ,plot.subtitle = element_text(size = 12)
        ,axis.ticks = element_blank()
        ,axis.text = element_blank()
        ,panel.grid = element_blank()
        ,panel.background = element_rect(fill = '#ffffff')
        ,plot.background = element_rect(fill = '#ffffff')
        ,legend.title=element_text(size=9)
        ,legend.text=element_text(size=8)
        #,legend.position = c(.12,.12)
        #,legend.background = element_blank()
        #,legend.key = element_blank()
  )

anim = p + transition_time(date) +
  labs(title = "HCOM Cancelation Rate Last 30 Days by State",
       subtitle = "\n Date: {frame_time}")

options(gganimate.dev_args = list(width = 900, height = 700))
start.time = Sys.time()
gganimate::animate(anim, fps = 1, nframes = length(unique(plotdata$date)), renderer = gifski_renderer("gganim.gif"))
start.time - Sys.time()


sc

?animate

