# Create dataset for Amazon by joining Amazon-specific point counts from 
# Colombia (CO) and Jacob's previous work in NE Peru (PE)
#
# Formatting and joining is done, but need to check taxonomy

# Housekeeping ----
library(dplyr); library(ggplot2); library(sf)

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
    x_tab[,2] <- x_tab[,2]/60
    x_tab[,3] <- x_tab[,3]/3600
    x_dec <- rowSums(x_tab)
    x_dec * -1
}

# function to unroll
unroll <- function(col) {
    col <- as_NA(col)
    ind <- !is.na(col)
    headers <- col[ind]
    headers[cumsum(ind)]
}
first_element <- function(x) x[1]
as_NA <- function(x) ifelse(x=="", NA, x)


# Sort out point and point-count data ----
# * Colombia dataset ----
# Points
pts_CO <- read.csv("Birds_CO_21Apr2020/socolar_points_v1.csv", as.is=T) %>%
    as_tibble %>%
    filter(grepl("SG|PL|PS", name)) %>%
    mutate(Group = cluster, 
           Habitat = c("pasture","forest")[forest+1], 
           Site = substr(name, 1, 2)) %>%
    st_as_sf(., coords=c("lon", "lat"), 
             crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") %>%
    select(Group, Point = name, Habitat)

# Bird dataset
cols_to_unroll <- c("Site", "Point", "Take", "Date", "Time")
birds_CO_raw <- read.csv("Birds_CO_21Apr2020/Jacob_data_v1.1.csv", as.is=TRUE) %>%
    as_tibble %>% 
    mutate_at(cols_to_unroll, unroll) %>%
    rename(Visit = Take)

# get unique point:visit combinations (to check none are dropped in filtering)
uniqueVisits <- birds_CO_raw[,c("Point", "Visit")] %>% unique
    
birds_CO <- birds_CO_raw %>%
    filter(Dist != "D", grepl("SG|PL|PS", Point), 
           !grepl('_sp$|Visu|Sono', Species), 
           is.na(FO)) %>%
    mutate(Country = "CO") %>%
    #mutate(Count = as.numeric(Count > 0)) %>%
    select(Site,
           Point, 
           Visit,  # note: renamed from 'Take' in original df
           Date, 
           Time,
           Point_time, 
           Species,
           Count, 
           Dist, 
           ID_method, 
           FO, 
           FK, 
           Country)

# all points have birds (i.e. no points are being discarded due to having no 
# birds once band D etc are being filtered out)
anti_join(birds_CO, uniqueVisits)

# join
full_CO <- full_join(pts_CO, birds_CO, by="Point")

# * Peru dataset ----
# Points
pts_PE <- read.csv("doi_10.5061_dryad.nh17p1n__v2/Census_Points.csv", as.is=T) %>%
    as_tibble() %>%
    mutate(lat_dec = dms_to_dec(Lat, type="Lat"), 
           lon_dec = dms_to_dec(Long, type = "Long"), 
           id_transect_point = paste0(Transect, "_", Point), 
           habitat_actual = case_when(Disturbance == "U" ~ "forest", 
                                      Disturbance == "D" ~ "smallholder")) %>%
    st_as_sf(coords = c("lon_dec", "lat_dec"), crs = "+init=epsg:4326") %>%
    select(Site = Location, 
           Group = Transect,
           Point = id_transect_point, 
           Habitat = habitat_actual)

# bird dataset
birds_PE_raw <- read.csv("doi_10.5061_dryad.nh17p1n__v2/birds.csv", as.is=T) %>%
    as_tibble() %>%
    mutate(Point = paste0(Transect, "_", Point), 
           Dist = LETTERS[1:4][cut(Distance, c(-1, 25, 50, 100, Inf))])


birds_PE <- birds_PE_raw %>%
    filter(Dist != "D") %>%
    mutate(FO = NA, 
           Country = "PE") %>%
    select(# Site = Location,               Site is in point df
        Point, # note: combined from Transect and Point in the orignal df 
        Visit, 
        Date, 
        Time,
        Point_time = Pt.time, 
        Species,
        Count, 
        Dist, 
        ID_method = ID.Type, 
        FO, # hmmm, in CO, FO implies not flyover, wheras here impl needs to be diff.  
        FK = Flock, 
        Country) 

uniqueVisits_PE <- birds_PE_raw[,c("Point", "Visit")] %>% unique

# all points have birds (i.e. no points are being discarded due to having no 
# birds once band D etc are being filtered out)
anti_join(birds_PE, uniqueVisits_PE)

full_PE <- full_join(pts_PE, birds_PE, by="Point") %>%
    select(Site, everything())

# Join PE & CO ----
# there are 5 points in the point file that do not have bird data, filtering 
# on Country removes these
df_full <- rbind(full_CO, full_PE) %>%
    select(-geometry, everything(), geometry) %>%
    select(Site, everything()) %>%
    filter(!is.na(Country)) 

n_visits <- df_full %>%
    select(Point, Visit) %>%
    unique %>%
    group_by(Point) %>%
    summarise(n_visit = n())


# Dataset summary ----
df_full %>% select(Point) %>% unique %>% nrow()
df_full %>% select(Country, Point) %>% unique %>% group_by(Country) %>%
    summarise(n_pts = n())
# 329 points in total, 94:235 Colombia:Peru

df_full %>% select(Point, Visit) %>% unique %>% nrow()
df_full %>% select(Country, Point, Visit) %>% unique %>% group_by(Country) %>%
    summarise(n_pts = n())
# 1290 point counts total, 359:931 Colombia:Peru

# A bunch of points have 3 visits (confirm these with Jacob)
n_visits %>% filter(n_visit != 4) %>% View

# Jacob mentioned that in each dataset there were a couple of points that could
# be more properly designated as pasture/smallholder- get these. 

# Write
saveRDS(df_full, "data/formatted_dataset.rds")

pts_full <- df_full %>% 
    select(Site, Group, Point, Habitat) %>%
    unique
saveRDS(pts_full, "data/formatted_points.rds")
