library(p3k14c)
library(tidyverse)
library(terra)
library(rnaturalearth)
library(magrittr)

# US SOUTHWEST ----
SW_subregions <- readr::read_csv(here::here("data/data-raw/SW_subregions.csv"))

SWcounties10 <- sf::read_sf(here::here("data/data-raw/ne_10m_admin_2_counties/ne_10m_admin_2_counties.shp")) %>% 
  dplyr::filter(REGION %in% c("CO", "NM", "UT", "AZ", "NV")) %>% 
  dplyr::rename(state = REGION, county = NAME) %>% 
  dplyr::select(state, county) %>% 
  dplyr::left_join(SW_subregions, by = c("state" = "State", "county" = "County")) %>% 
  dplyr::mutate(subregion = factor(subregion)) %>% 
  dplyr::filter(!is.na(subregion),
                subregion != "Mogollon")

#get the two mexico states
statesMX  <- rnaturalearth::ne_states(country = "Mexico") %>%
  sf::st_as_sf() %>%
  sf::st_make_valid() %>%
  dplyr::select( name)  %>% 
  dplyr::rename(state  = name) %>% 
  dplyr::filter(state  %in% c("Sonora") ) %>% 
  dplyr::mutate(subregion = "Desert")

#combine
SW_subs <- bind_rows(SWcounties10, statesMX) %>% 
  terra::vect() 
  
SWcounties10 %>% terra::plot()

# load in 14C and create subregions

SW_c14  <- p3k14c::p3k14c_data %>% 
  dplyr::filter(Country %in% c("USA", "Mexico"))  %>% 
  terra::vect(geom = c("Long", "Lat"), crs = "EPSG: 4326") %>% 
  terra::mask(SW_subs%>% 
                terra::aggregate(by = "subregion")) %>% 
  terra::intersect(SW_subs%>% 
                     terra::aggregate(by = "subregion")) %>% 
  terra::as.data.frame(geom = c("XY")) %>% 
  dplyr::select(-state, -county) #%>% 
  # dplyr::mutate(isMaize = stringr::str_detect(Taxa, "mays"),
  #               isMaize = tidyr::replace_na(isMaize))

SW_c14 %>% readr::write_csv(here::here("data/data-derived/c14/SW_c14.csv"))


# Midwest -----
Milner_cemeteries <- readr::read_csv(here::here("data/data-raw/fert_rates/Milner_TableS2_ageProfiles.csv")) %>% 
  dplyr::mutate(County = ifelse(stringr::str_detect(County, "40BY"), "Bradley", County),
                County = ifelse(stringr::str_detect(County, "40MN"), "McMinn", County)) %>% 
  dplyr::group_by(County, State) %>% 
  dplyr::summarise(nCem = dplyr::n()) %>% 
  dplyr::ungroup()

Milner_archaeobot <- readr::read_csv(here::here("data/data-raw/archaeobot/Milner/Milner_TableS3_archaeobot.csv")) %>% 
  dplyr::group_by(County, State) %>% 
  dplyr::summarise(nBot = dplyr::n()) %>% 
  dplyr::ungroup()

Milner <-full_join(Milner_archaeobot, Milner_cemeteries, by = c("State" = "State", "County" = "County"))

MW_Milner_counties <- 
  sf::read_sf(here::here("data/data-raw/ne_10m_admin_2_counties/ne_10m_admin_2_counties.shp")) %>% 
  dplyr::select(REGION, NAME, ADM2_CODE) %>% 
  dplyr::rename(state = REGION, county = NAME) %>% 
  dplyr::left_join(., Milner, by =c("state" = "State", "county" = "County")) %>% 
  dplyr::mutate(inMilner = ifelse(!is.na(nBot|nCem), TRUE, FALSE)) %>% 
  dplyr::filter(state  %in% c("IL", "IA", "IN", "MO", "AR", "MS", "KY", "TN", "OH", "WV", "AL"))

MW_Milner_extent <- MW_Milner_counties %>% dplyr::filter(inMilner == TRUE) %>%  terra::vect() %>% terra::ext()

MW_Milner <-  MW_Milner_counties %>% terra::vect() %>% 
  terra::intersect(y = MW_Milner_extent)

MW_Milner_subregion <- MW_Milner_counties %>% dplyr::filter(ADM2_CODE  %in% MW_Milner$ADM2_CODE ) %>% terra::vect() 

library(tidyterra)

MW_Milner_subregion %>% 
  dplyr::mutate(
    nCem = ifelse(is.na(nCem), 0, nCem),
    nBot = ifelse(is.na(nBot), 0, nBot),
    nSample = nCem + nBot) %>% 
  ggplot(aes(fill = nSample), alpha = 0.5)+
  geom_spatvector()+
  scale_fill_gradient(low = "white", high = "red")+
  # geom_spatvector(data = MW_Milner_subregion, aes(fill = nCem), alpha = 0.5)+
  theme_minimal()+
  theme(legend.position = "top") + 
  guides(fill=guide_legend(title="MW"))
  

#extract c14
MW_c14  <- p3k14c::p3k14c_data %>% 
  dplyr::filter(Country %in% c("USA", "Mexico"))  %>% 
  terra::vect(geom = c("Long", "Lat"), crs = "EPSG: 4326") %>% 
  terra::mask(MW_Milner_subregion) %>% 
  terra::as.data.frame(geom = c("XY")) %>% 
  dplyr::bind_rows(p3k14c_data %>% 
                     dplyr::filter(Country == "USA",
                       LocAccuracy == 0, 
                                   Province %in% c("Illinois", "Indiana", "Kentucky", "Tennessee"))) %>% 
  dplyr::mutate(subregion = "MW")

MW_c14 %>% readr::write_csv(here::here("data/data-derived/c14/MW_c14.csv"))

# US Map -----

MW_Milner_subregion$subregion <- "Midwest"

SW <- SW_subs['subregion'] 
MW <- MW_Milner_subregion['subregion'] 

studyareas <- rbind(SW, MW )

studyareas %>% terra::plot()

studyareas$subregion <- factor(studyareas$subregion, levels = c("Desert", "Plateau", "Midwest"),
                               labels = c("Desert", "Upland", "Midwest"))

studyareas_extent <- terra::ext(studyareas %>% terra::buffer(width = 80000))

studyareas_extent <- terra::ext(-120, -80, 25, 45)

NorthAm <- rnaturalearth::ne_states(country = c("Mexico", "United States of America")) %>% 
  terra::vect() %>% 
  terra::crop(studyareas_extent)

studyareas %>% saveRDS("data/data-derived/subregions.rds")

library(tidyterra)

Fig1map <- ggplot()+
  geom_spatvector(data = studyareas, aes(fill = subregion))+
  geom_spatvector(data = NorthAm, color = "black", fill = "NA")+
  theme_minimal()+
  theme(legend.position = "top") + 
  guides(fill=guide_legend(title="Subregions"))

Fig1map


png(filename = "figures/Fig1map.png", width = 6, height = 4, units = "in", res = 300)
Fig1map
dev.off()

