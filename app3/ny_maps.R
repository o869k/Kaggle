library(sp)
library(rgdal)
library(rgeos)
library(ggplot2)
library(ggthemes)
library(RgoogleMaps)
library(ggmap)
library(zipcode)

nyc <- readOGR("C:\\Accidents\\ZIP_CODE_040114\\ZIP_CODE_040114.shp", ogrListLayers("C:\\Accidents\\ZIP_CODE_040114\\ZIP_CODE_040114.shp")[1], stringsAsFactors=FALSE)

# base
plot(nyc, lwd=0.5, asp=1)

# ggplot2

# simplifying the polygons speeds up ggplot2 a bit
nyc_map <- fortify(gSimplify(nyc, 0.05))

gg <- ggplot()
gg <- gg + geom_map(data=nyc_map, map=nyc_map,
                    aes(x=long, y=lat, map_id=id),
                    color="black", fill="white", size=0.25)
gg <- gg + coord_equal() 
gg <- gg + theme_map()
gg

