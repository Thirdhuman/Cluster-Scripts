# https://walkerke.github.io/tidycensus/articles/basic-usage.html
 
# exporting the map
# https://gis.stackexchange.com/questions/171827/generate-html-file-with-r-using-leaflet
 
# 3D map
# https://www.r-bloggers.com/create-3d-county-maps-using-density-as-z-axis/

library(tidycensus)
library(tidyverse)
library(viridis)
library(mapview)
library(sf)
library(rgdal)    # for readOGR and others
library(leaflet)  # for interactive maps (NOT leafletR here)
library(dplyr)    # for working with data frames
library(ggplot2)  # for plotting
library(maptools)
library(htmlwidgets)
library(haven)
options(tigris_use_cache = TRUE)
# census <- load_variables(dataset = 'sf1', year = 2010, cache = TRUE)
# ACS <- load_variables(dataset = 'acs5', year = 2017, cache = TRUE)
# us_pov <- get_acs(geography = "county"  , variables = c(pov = 'B06012_002') #, pov_ratio  =pov/total
#                             , summary_var = 'B06012_001',geometry = TRUE)
# us_pov$ratio = (us_pov$estimate/ us_pov$summary_est) * 100
# us_pov_map=mapview(us_pov, zcol = "ratio", legend = TRUE)
# us_pov_map
# st_write(us_pov, "us_poverty.shp", update=TRUE)

# Load shape files
# poverty=sf::st_read("poverty_rate.shp")
# opp_z=sf::st_read("/Users/rorr/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/Web_Map/Cluster_OZ_Final.shp")
# geo_china=sf::st_read('geo_china.shp')
opp_z<-readOGR(dsn="~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/Web_Map/Final_Shapefile",layer="Cluster_OZ_Final")


# geo_china = geo_china[with(geo_china, as.logical(ave(l_shn__, FUN=function(x) quantile(x, .75, na.rm = T) < x))),]
# geo_china = geo_china[with(geo_china, as.logical(ave(d_pct_m, FUN=function(x) quantile(x, .20, na.rm = T) > x))),]
# poverty = poverty[with(poverty, as.logical(ave(ratio, FUN=function(x) quantile(x, 0.80, na.rm = T) < x))),]
names(opp_z)
# opp_z=within(opp_z, rm(STATENAME, QOZ, GEOID10, COUNTYNAME))


# Initialsation
m <- leaflet(padding = 0)
### Create an asymmetric color range
# ## Make vector of colors for values smaller than 0 (20 colors)
# rc1 <- colorRampPalette(colors = c("green", "white"), space = "Lab")(18)
# ## Make vector of colors for values larger than 0 (180 colors)
# rc2 <- colorRampPalette(colors = c("white", "red"), space = "Lab")(55)
# ## Combine the two color palettes
# rampcols <- c(rc1, rc2)
# mypal <- colorNumeric(palette = rampcols, domain = poverty$ratio)
# ## If you want to preview the color range, run the following code
# previewColors(colorNumeric(palette = rampcols, domain = NULL), values = 1:100)
# 
# summary(geo_china$d_pct_m)
# ### Create an asymmetric color range 2
# ## Make vector of colors for values smaller than 0 (20 colors)
# rc3 <- colorRampPalette(colors = c("orange", "white"), space = "Lab")(15)
# ## Make vector of colors for values larger than 0 (180 colors)
# rc4 <- colorRampPalette(colors = c("white", "purple"), space = "Lab")(50)
# ## Combine the two color palettes
# rampcols2 <- c(rc3, rc4)
# mypal2 <- colorNumeric(palette = rampcols2, domain = geo_china$d_pct_m)
# ## If you want to preview the color range, run the following code
# previewColors(colorNumeric(palette = rampcols2, domain = NULL), values = 1:100)

library(viridis) # My favorite palette for maps
OZpal <- colorFactor(viridis(17), opp_z$data$cluster_abbvr) 

# Add country
m <- addPolygons(map = m, data = opp_z, opacity = 1, 
                 color = "grey", smoothFactor = 0.8, 
                 weight = 0.5,popup = NULL,
                 options = list(clickable = FALSE), 
                 fill = T, 
                 fillColor = ~OZpal(opp_z$data$cluster_abbvr),
                 fillOpacity = .85, group = "Tracts")

# m <- addPolygons(map = m, data = poverty, color = "grey", opacity = 1, 
#                 weight = 0.25, smoothFactor = 0.8, 
#                 options = list(clickable = FALSE), 
# 		popup = ~paste0("Location: ",poverty$NAME, "<br>",
# 			"Poverty Rate: ",round(poverty$ratio, 1), "% <br>"),
# 		fillColor = ('darkred'),         
# 		fill = T, 
#                 fillOpacity = .6, group = "High Poverty Rate")

# m <- addPolygons(map = m, data = geo_china, color = "grey", opacity = 1, 
#                 weight = 0.25, smoothFactor = 0.8, 
#                 options = list(clickable = FALSE), 
# 		popup = ~paste0("Change in manufacturing employment in working-age population: ",
# 			round(geo_china$d_pct_m, 1), "% <br>",
# 			"Change in import exposure per worker: ",round(geo_china$d_trds_, 1), "% <br>",
# 			"CBP Manufacturing Employment Share: ", 100* round(geo_china$l_shn__, 1), "%"),
# 		fillColor = ('yellow'),         
# 		fill = T, 
#                 fillOpacity = .3, group = "Large Manufacturing Decline")
# # Dimention of the map
m$width <- 850
m$height <- 650
m <- addTiles(map = m, group = "OSM (default)") %>%
	# addLayersControl(
	# baseGroups = c("OSM (default)"),
	# overlayGroups = c("Opportunity Zones"),
	# options = layersControlOptions(collapsed = FALSE))%>% 
	setView(-105.5, 44, zoom = 4)
m

# Export as HTML file
# saveWidget(m, '/Users/rorr/Google Drive - Niskanen/srp-web-hosting/map_oppz_final_Stripped_SELF.html', selfcontained = T)

#### Trade Shock
## Articles
# Nice map: https://www.wsj.com/articles/how-the-china-shock-deep-and-swift-spurred-the-rise-of-trump-1470929543
## Papers
# https://pubs.aeaweb.org/doi/pdfplus/10.1257/aer.103.6.2121
# https://www.dropbox.com/s/vacprfieznxzc9b/bisbee_jmp.pdf?dl=0
## Commuting Zones
# https://sites.psu.edu/psucz/data/




