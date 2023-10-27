library(magrittr)
library(terra)
library(tidyverse)
library(rcarbon)

options(scipen = 999)

# Midwest archaeobot ----
MW_bot <- readr::read_csv(here::here("data/data-raw/archaeobot/Milner/Milner_TableS3_archaeobot.csv")) %>% 
  dplyr::mutate(
    MNI = Nuts + EAC + IP,
    cultigens_rel_MNI = (EAC + IP)/MNI,
    IP_rel_cultigens = IP/(EAC + IP),
    EAC_rel_MNI = EAC / MNI,
    studyarea = "MW"
  ) %>% 
  dplyr::filter(MNI > 20) %>% 
  dplyr::select(-Source)

MW_bot %>% dplyr::arrange(MNI)

MW_bot %>% 
  ggplot(aes(x = BP, y = MNI))+
  geom_point()+
  scale_x_reverse()+
  scale_y_log10()

MW_bot %>% 
  ggplot(aes(x = BP, y = cultigens_rel_MNI))+
  geom_point(aes(size = MNI),  shape=1 )+
  geom_smooth(aes(weight= MNI),se=FALSE,
              method="loess",span=0.5,fullrange=TRUE,
              linewidth = 0.5, color = "black"
  )+
  scale_x_reverse()

MW_bot %>% 
  ggplot(aes(x = BP, y = IP_rel_cultigens))+
  geom_point(aes(size = MNI),  shape=1 )+
  geom_smooth(aes(weight= MNI),se=FALSE ,
              method="loess",span=0.6,fullrange=TRUE,
              linewidth = 0.5, color = "black"
  )+
  scale_x_reverse()

MW_bot %>% readr::write_csv(here::here("data/data-derived/archaeobot/mw_archaeobot.csv"))
