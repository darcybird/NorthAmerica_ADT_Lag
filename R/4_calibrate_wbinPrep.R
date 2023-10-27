library(tidyverse)
library(magrittr)
library(rcarbon)
library(purrr)

# create SPD -----
c14 <- 
  readr::read_csv(here::here("data/data-derived/c14/study_c14.csv")) %>% 
    dplyr::mutate(calib = "intcal20",
                  site = stringr::str_c(
                    ifelse(!is.na(SiteID), SiteID, 0), 
                    ifelse(!is.na(SiteName), SiteName, 0), 
                    ifelse(!is.na(subregion), subregion, 0), 
                    ifelse(!is.na(Province), Province, 0), 
                    ifelse(!is.na(x), round(x,1), 0), 
                    ifelse(!is.na(y), round(y,1), 0))) %>% 
    dplyr::select(Age, Error, calib, site, studyarea, subregion) %>% 
  dplyr::filter(studyarea != "EU", Age > 2*Error) 
  
c14 %>% dplyr::group_by(subregion) %>% dplyr::select(site) %>% dplyr::distinct()

# #calculate growth rates ----
plateau <- c14 %>%   dplyr::filter(subregion == "Plateau") %>% dplyr::filter(Age < 6000)
desert <- c14 %>%   dplyr::filter(subregion == "Desert") %>% dplyr::filter(Age < 6000) 
MW <- c14 %>%   dplyr::filter(subregion == "MW") %>% dplyr::filter(Age < 6000)

# calculate spds and their growth rates

#desert
desert_cal <- rcarbon::calibrate(x = desert$Age, errors = desert$Error,calCurves = desert$calib,verbose = FALSE, normalised=FALSE, calMatrix=TRUE)
desert_bins <- rcarbon::binPrep(sites = desert$site, ages = desert$Age, h = 100)
desert_spd <- rcarbon::spd(x=desert_cal,bins = desert_bins, timeRange=c(5500,100), spdnormalised = TRUE,verbose = FALSE)
desert_spd_200 <- rcarbon::spd(x=desert_cal, bins = desert_bins, timeRange=c(5500,100), spdnormalised = TRUE,runm = 200,verbose = FALSE)
desert.gg <- spd2rc(desert_spd_200,breaks=seq(5400,300,-10))
desert.gg.100 <- spd2rc(desert_spd_200,breaks=seq(5400,300,-100))
plot(desert.gg.100)

desert_spd_grid <- desert_spd$grid
desert_spd_200_grid <- desert_spd_200$grid

remove(desert_cal)
remove(desert_spd)
remove(desert_spd_200)
gc()

#plateau
plateau_cal <- rcarbon::calibrate(x = plateau$Age, errors = plateau$Error,calCurves = plateau$calib,verbose = FALSE, normalised=FALSE, calMatrix=TRUE)
plateau_bins <- rcarbon::binPrep(sites = plateau$site, ages = plateau$Age, h = 100)
plateau_spd <- rcarbon::spd(x=plateau_cal, bins = plateau_bins, timeRange=c(5500,100), spdnormalised = TRUE,verbose = FALSE)
plateau_spd_200 <- rcarbon::spd(x=plateau_cal,bins = plateau_bins,timeRange=c(5500,100), spdnormalised = TRUE,runm = 200,verbose = FALSE)
plateau.gg <- spd2rc(plateau_spd_200,breaks=seq(5400,300,-10))
plateau.gg.100 <- spd2rc(plateau_spd_200,breaks=seq(5400,300,-100))
plot(plateau.gg)

plateau_spd_grid <- plateau_spd$grid
plateau_spd_200_grid <- plateau_spd_200$grid

remove(plateau_cal)
remove(plateau_spd)
remove(plateau_spd_200)
gc()


#MW
MW_cal <- rcarbon::calibrate(x = MW$Age, errors = MW$Error,calCurves = MW$calib,verbose = FALSE, normalised=FALSE, calMatrix=TRUE)
MW_bins <- rcarbon::binPrep(sites = MW$site, ages = MW$Age, h = 100)
MW_spd <- rcarbon::spd(x=MW_cal,bins = MW_bins, timeRange=c(5500,100), spdnormalised = TRUE,verbose = FALSE)
MW_spd_200 <- rcarbon::spd(x=MW_cal,bins = MW_bins, timeRange=c(5500,100), spdnormalised = TRUE,runm = 200,verbose = FALSE)
MW.gg <- spd2rc(MW_spd_200,breaks=seq(5400,300,-10))
MW.gg.100 <- spd2rc(MW_spd_200,breaks=seq(5400,300,-100))
plot(MW.gg)

MW_spd_grid <- MW_spd$grid
MW_spd_200_grid <- MW_spd_200$grid

remove(MW_cal)
remove(MW_spd)
remove(MW_spd_200)
gc()

#make growth rate dataframe
gg <- bind_cols(desert.gg$breaks[2:510], desert.gg$roca, plateau.gg$roca, MW.gg$roca  ) 

names(gg) <- c("breaks", "desert", "plateau", "MW")

ggplot(gg, aes(x = breaks))+
  geom_line(aes(y=desert), color = "red") +
  geom_line(aes(y=plateau), color = "blue")+
  geom_line(aes(y=MW), color = "orange")+
  scale_x_reverse()

gg %>% write_csv(here::here("data/data-derived/c14/spd_growth_rates.csv"))

gg.100 <- bind_cols(desert.gg.100$breaks[2:51], desert.gg.100$roca, plateau.gg.100$roca, MW.gg.100$roca  ) 

names(gg.100) <- c("breaks", "desert", "plateau", "MW")

ggplot(gg.100, aes(x = breaks))+
  geom_line(aes(y=desert), color = "red") +
  geom_line(aes(y=plateau), color = "blue")+
  geom_line(aes(y=MW), color = "orange")+
  scale_x_reverse()

gg.100 %>% write_csv(here::here("data/data-derived/c14/spd_growth_rates_100.csv"))

#combine for SPD results
desert_spd_grid$subregion <- "desert"
desert_spd_200_grid$subregion <- "desert"
plateau_spd_grid$subregion <- "plateau"
plateau_spd_200_grid$subregion <- "plateau"
MW_spd_grid$subregion <- "MW"
MW_spd_200_grid$subregion <- "MW"

desert_spd_grid$runm <- "0"
desert_spd_200_grid$runm <- "200"
plateau_spd_grid$runm <- "0"
plateau_spd_200_grid$runm <- "200"
MW_spd_grid$runm <- "0"
MW_spd_200_grid$runm <- "200"

SPDs <- bind_rows(desert_spd_grid,desert_spd_200_grid,
                  plateau_spd_grid,plateau_spd_200_grid,  
                  MW_spd_grid, MW_spd_200_grid)

SPDs%>% write_csv(here::here("data/data-derived/c14/spds.csv"))
