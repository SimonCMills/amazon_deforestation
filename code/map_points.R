
# devtools::install_github("ropensci/rnaturalearthdata")
# devtools::install_github("ropensci/rnaturalearthhires")

# housekeeping
library(sf); library(rnaturalearthhires); library(raster); library(rgeos)
UTM <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs" 
# read in points file 
pts_full <- readRDS("data/formatted_points.rds") 
pts_UTM <- pts_full %>%
    st_transform(crs=UTM)

pts_bbox <- st_bbox(pts_UTM) + c(-1, -1, 1, 1)*c(2.5e5, 1.5e5, 2.5e5, 1.5e5)
sp_bbox <- st_as_sfc(pts_bbox)
pts_bbox <- extent(pts_UTM) + c(-1, 1, -1, 1)*c(2.5e5, 2.5e5, 1.5e5, 1.5e5) 

# read in forest cover raster
tc <- raster("../../tc_layer_300.tif")
# tc_crop <- crop(tc, pts_bbox)
# plot(tc_crop)
# tc_UTM <- projectRaster(tc_crop, res=5000, crs=UTM)
tc_df <- rasterToPoints(tc) %>%
    as_tibble() %>%
    filter(tc_layer_300 > 30)

# read in country shapefiles
countries <- ne_countries(country = c("Colombia", "Peru", "Ecuador", "Brazil"), 
                          returnclass = "sf", scale = "large") %>%
    st_transform(crs=UTM)

# clip countries to points
countries_clip <- countries %>%
    st_crop(pts_bbox)

ggplot(countries_clip) + 
    geom_raster(data=tc_df, aes(x, y, fill=tc_layer_300)) +
    geom_sf(fill=NA, col="black", size=1) +
    geom_sf(data=pts_UTM, pch=21, size=2.5, fill="grey30") +
    scale_x_continuous(expand=c(0,0), lim = pts_bbox[c(1,2)]) +
    scale_y_continuous(expand=c(0,0), lim = pts_bbox[c(3,4)]) +
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.title = element_blank(), 
          axis.text = element_text(colour="black")) +
    scale_fill_gradient(low = "white", high = "#3F704D") +
    guides(fill=F)
ggsave("figures/map_points.png")
