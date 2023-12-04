rm(list=ls(all=TRUE))

library(tidyverse)
library(sf) #spatial data
library(tigris) #geojoin
library(magrittr)
library(raster)
library(rmapshaper)
library(tmap)
library(glue)

setwd("C:/Users/samue/Committee of 100, Inc/Research - Reports/Alien land laws/landlaw_app")

# make states_shapes df ####
# read in state abbreviations for map and matching
state_codes <- read_csv("statecodes.csv")

# read in US states shapefile data
states_geo <- read_sf(dsn = "C:/Users/samue/OneDrive - Committee of 100, Inc/C100/mapping and shiny/shiny/landlaw-app/ne_10m_admin_1_states_provinces_lakes", layer = "ne_10m_admin_1_states_provinces_lakes")
states_geo %<>% filter(admin == "United States of America") %>%
        subset(select = c(name, latitude, longitude, geometry))

states <- full_join(state_codes, states_geo, states, by = c("state_name" = "name"))

## make boxes for small east coast states ####

## adjust lat-lon for labels ####
states %<>%  mutate(latitude = case_when(
        state_abb == "VT" ~ 45.6858928919431,
        state_abb == "NH" ~ 46.65966415854359,
        state_abb == "MA" ~ 40.72199786562894 + 2,
        state_abb == "RI" ~ 40.534073831616985,
        state_abb == "CT" ~ 40.534073831616985 - 1.5,
        state_abb == "NJ" ~ 40.534073831616985 - 3,
        state_abb == "DE" ~ 40.534073831616985 - 4.5,
        state_abb == "MD" ~ 40.534073831616985 - 6,
        state_abb == "DC" ~ 40.534073831616985 - 7.5,
        state_abb == "HI" ~ 24.39898,
        state_abb == "AK" ~ 26.39898,
        TRUE ~ latitude)) %>% 
        mutate(longitude = case_when(
        state_abb == "VT" ~ -72.70731416934166,
        state_abb == "NH" ~ -71.3889548745072,
        state_abb == "MA" ~ -69.7832838495052,
        state_abb == "RI" ~ -69.7832838495052,
        state_abb == "CT" ~ -69.7832838495052,
        state_abb == "NJ" ~ -69.7832838495052,
        state_abb == "DE" ~ -69.7832838495052,
        state_abb == "MD" ~ -69.7832838495052,
        state_abb == "DC" ~ -69.7832838495052,
        state_abb == "HI" ~ -102,
        state_abb == "AK" ~ -115.61515,
        state_abb == "MN" ~ -95.1859,
        TRUE ~ longitude))

# make bbox latitudes and longitudes
states %<>% mutate(latitude_min = latitude - .6) %>%
        mutate(latitude_max = latitude + .6) %>%
        mutate(longitude_min = longitude - .7) %>%
        mutate(longitude_max = longitude + .7)

## make boxes ####
box_states <- c("VT", "NH", "MA", "RI", "CT", "NJ", "DE", "MD", "DC")

for (i in box_states) {
        # assign longitude and latitude box for state
        lon = c(states$longitude_min[states$state_abb == i], states$longitude_max[states$state_abb == i])
        lat = c(states$latitude_min[states$state_abb == i], states$latitude_max[states$state_abb == i])
        
        # make box into geometry
        state_box = data.frame(lon, lat) %>%
                st_as_sf(coords = c("lon", "lat"), 
                         crs = 4326) %>% 
                st_bbox() %>%
                st_as_sfc() %>% 
                st_transform('+proj=longlat +datum=WGS84')
        
        # isolate state geometry
        state_iso <- states %>% 
                filter(state_abb == i) %>% 
                subset(select = c(geometry)) %>%
                st_as_sf(crs = 4326) %>% 
                st_transform('+proj=longlat +datum=WGS84')
        
        # merge box and state geometry 
        merge_geo <- st_union(state_box, state_iso) %>% 
                as_Spatial() %>% 
                st_as_sf()
        
        # update df geometry
        states$geometry[states$state_abb == i] = merge_geo$geometry
        
}

# subset to relevant columns
states %<>% subset(select = c(state_name, 
                              state_abb, 
                              latitude, 
                              longitude, 
                              geometry))

save(states, file = "map_states.rdata")

# make into SF df ####
# only perform once merged with substantive df
states_sf <- states %>% 
        st_as_sf(crs = 4326) %>% 
        shift_geometry() %>%
        st_transform('+proj=longlat +datum=WGS84')


