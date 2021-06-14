# read library
library(ggplot2)
library(dplyr)
require(maps)
require(viridis)
theme_set(
  theme_void()
)

#
polyphonic <- FALSE
ggplot.info.file <- "songs_geoinfo.csv"
outputdir <- "../../output/figures/"
outputfilename <- "samplingmap.png"

MANUAL_JITTERING <- 1.1

LEGEND_TEXTSIZE <- 13
pointsize <- 8
pointalpha <- 0.9
fighei = 5 * 1.2
figwid = 8.09 * 2

# 
lonlat <- read.csv(ggplot.info.file)
if (!polyphonic) {
  lonlat <- lonlat[lonlat$style != "Polyphonic", ]
}

#
lonlat[lonlat$song == "T5427R45m", ]$lat <- lonlat[lonlat$song == "T5427R45m", ]$lat + MANUAL_JITTERING
lonlat[lonlat$song == "T5427R45m", ]$long <- lonlat[lonlat$song == "T5427R45m", ]$long + MANUAL_JITTERING
lonlat[lonlat$song == "NHSDiscography-094", ]$lat <- lonlat[lonlat$song == "NHSDiscography-094", ]$lat - MANUAL_JITTERING
lonlat[lonlat$song == "NHSDiscography-094", ]$long <- lonlat[lonlat$song == "NHSDiscography-094", ]$long - MANUAL_JITTERING

lonlat[lonlat$song == "T5404R84", ]$lat <- lonlat[lonlat$song == "T5404R84", ]$lat + MANUAL_JITTERING
lonlat[lonlat$song == "T5404R84", ]$long <- lonlat[lonlat$song == "T5404R84", ]$long + MANUAL_JITTERING
lonlat[lonlat$song == "NHSDiscography-054", ]$lat <- lonlat[lonlat$song == "NHSDiscography-054", ]$lat - MANUAL_JITTERING
lonlat[lonlat$song == "NHSDiscography-054", ]$long <- lonlat[lonlat$song == "NHSDiscography-054", ]$long - MANUAL_JITTERING

#
world_map <- map_data("world")
mymap <- ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightgray", color = "black", size = 0.1)

mymap <- mymap +
  geom_point(data = lonlat, aes(x = long, y = lat, shape = source, colour = style)
              , size = pointsize, alpha = pointalpha) +
  scale_color_manual(values = c("#1e90ff", "#ff4500"))

mymap <- mymap +
  theme(legend.position = "top", legend.title = element_text(size = 16), legend.text = element_text(size = LEGEND_TEXTSIZE)) +
  labs(colour = "Accompaniment", shape = "Source") + 
  guides(colour = guide_legend(nrow = 2, byrow = TRUE), shape = guide_legend(nrow = 2, byrow = TRUE))
  

#
ggsave(paste(outputdir, outputfilename, sep = ""),
       plot = mymap, width = figwid, height = fighei)