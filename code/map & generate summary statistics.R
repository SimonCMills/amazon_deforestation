# initial exploration of Peru and Colombia datasets: map points across 
# the two datasets & generate summary statistics

library(dplyr); library(sf)
# read Jacob's Peru data
dat <- read.csv("doi_10.5061_dryad.nh17p1n__v2/birds.csv") %>%
    as_tibble()

pts <- read.csv("doi_10.5061_dryad.nh17p1n__v2/Census_Points.csv", as.is=T) %>%
    as_tibble

# function to convert degree:minute:seconds to decimal
dms_to_dec <- function(x, type) {
    x_clean <- gsub("^ *", "", x) %>%
        gsub("  ", " ", .) %>%
        gsub("deg", "", .) %>%
        gsub("\\'", " ", .) %>%
        gsub("\\\"[A-Z]", "", .)
    x_tab <- strsplit(x_clean, " ") %>%
        unlist %>%
        as.numeric %>%
        matrix(., ncol = 3, byrow = T)
    x_tab
    x_tab[,2] <- x_tab[,2]/60
    x_tab[,3] <- x_tab[,3]/3600
    x_dec <- rowSums(x_tab)
    x_dec * -1
}


pts_clean <- pts %>%
    mutate(lat_dec = dms_to_dec(Lat, type="Lat"), 
           lon_dec = dms_to_dec(Long, type = "Long"))


pts_sf <- st_as_sf(pts_clean, coords = c("lon_dec", "lat_dec"), crs = "+init=epsg:4326")
                   # crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    
library(mapview)
mapview(pts_sf)


# get CO points
pts_CO <- read.csv("Birds_CO_21Apr2020/socolar_points_v1.csv") %>%
    filter(grepl("SG|PL|PS", name)) %>%
    mutate(habitat = c("forest", "pasture")[forest+1]) %>%
    st_as_sf(., coords=c("lon", "lat"), 
             crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") %>%
    select(point = name, habitat)

mapview(pts_CO, zcol= "habitat")

pts_P <- pts_sf %>% 
    select(point = Transect, habitat = Habitat)

pts_all <- rbind(pts_CO, pts_P)
mapview(pts_all)
