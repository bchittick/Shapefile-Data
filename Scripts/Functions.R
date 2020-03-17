

#############
# Simplify and turn to df 
#############
# convert to df - simplyfy first (1/5 of the datapoints)
#a = as(plotdata, "Spatial")
#regions_df <- a@data
#regions_gSimplify <- gSimplify(a, tol = 0.1, topologyPreserve = TRUE)
#regions_gSimplify <- sp::SpatialPolygonsDataFrame(regions_gSimplify, regions_df)

#regions_gSimplify@data$id <-rownames(regions_gSimplify@data)
#dpts <- ggplot2::fortify(regions_gSimplify) 
#ff = merge(dpts, regions_gSimplify@data, by.x = "id", by.y = "id") # this replicates a lot of info


# function to simplfy sf object and convert to DF
SfToDataframe = function(sfobject = NULL, idcol = NULL, savecols = NULL, simp.per = .04) {
  
  a = sfobject
  a = ms_simplify(a,  keep=simp.per)
  tempsp <- as_Spatial(a, IDs=a[[idcol]])
  tempmap <- fortify(tempsp)
  
  temp <- a %>%
    st_drop_geometry() %>%
    dplyr::mutate(id=as.character(dplyr::row_number()))
  
  ff <- tempmap %>%
    left_join(temp, by="id")
  
  ff = ff[, names(ff) %in% c("long","lat","order","hole","piece","id", "group",paste0(idcol), paste0(savecols))]
  
  return(ff)
  
}

df = SfToDataframe(plotdata, idcol = 'adju_geo_region_name')

################
# GGPlotly
################


# Plot - have to plot the polygons (some DMA's not one polygon)
p = ggplot(data = output_df,aes(x = long, y = lat, group = group,
                           text = paste(orig_geo_dma_name))) + 
  geom_polygon(aes(fill = orig_geo_dma_name), color = "white") + 
  #scale_fill_manual(values=c('#CB2C30', '#BDB9B7','#7B1FA2'))  +
  #coord_fixed(1.3) +
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

plot_ly(t)


################
# More tries at Plots 
################

# only way to plot and see cities with big files (BRA City)
g = list(
  scope = 'south america',
  scale = 1,
  showframe = FALSE,
  bgcolor = 'white',
  projection = list(type = 'Mercator'),
  lonaxis = list(
    range = c(20,0)
  ),
  lataxis = list(
    range = c(-50, 0)
  )
)



ff %>%
  filter(!is.na(group)) %>%
  group_by(group) %>%
  plot_geo() %>%
  add_trace(x = ~long, y = ~lat, 
            showlegend = T,
            text = ~NAME_2, 
            color = ~geo_dma_bespoke,
            mode = "lines",
            hoverinfo = 'text') %>%
  #add_polygons(color = ~geo_dma_bespoke) %>%
  #add_polygons(
  #  fillcolor = 'transparent',
  #  line = list(color = 'black', width = 0.5),
  #  showlegend = FALSE, hoverinfo = 'none'
  #) %>%
  layout(title = "US Counties by Number of Vertices",
         geo = g)

sort(unique(bra_city_DMA_dim$adju_geo_region_name))

