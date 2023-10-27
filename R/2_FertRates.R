library(tidyverse)


### SOUTHWEST -----
SW <- readr::read_csv(here::here("data/data-raw/fert_rates/kohler_reese2014_ndt_calculated.csv")) %>% 
  dplyr::rename(site = Sample,
                Date = Date_AD,
                MNI = N_GE_5,
                N5_19 = N519,
                N20 = N_GT_19,
                domDate = Early_Maize,
                agDate = Eff_Maize
                ) %>% 
  dplyr::mutate(studyarea = "SW",
                country = "USA",
                dateBP = abs(Date - 1950 ),
                N0_4 = NA, 
                MNI  = (N5_19 + N20),
                delta_dom = DT_Early,
                delta_ag = DT_Int,
                JI = N5_19 / (N5_19 + N20),
                CBR = (JI ^ 0.89074 ) * 0.15334 + 0.00375,
                GR = (JI ^ 0.47788) * 0.12555 - 0.05389,
                subregion = factor(ifelse(Region %in% c("TON", "HOH" ), "Desert", ifelse(Region == "MOG", "Mogollon", "Plateau")))) %>% 
  rownames_to_column(var = "ID") %>% 
  dplyr::select(country, studyarea, subregion, site, MNI, N0_4, N5_19, N20, JI, CBR, GR, dateBP, domDate, delta_dom, agDate, delta_ag )


SW %>% dplyr::filter(MNI > 10,
                     subregion != "Mogollon") %>% 
  ggplot(aes(x = delta_ag, y = CBR))+
  geom_point(aes(size = MNI),  shape=1 )+
  geom_smooth(aes(weight= MNI),
              method="loess",span=0.5,fullrange=TRUE,se=FALSE,
              linewidth = 0.5, color = "black"
  )

MW <- readr::read_csv(here::here("data/data-raw/fert_rates/Milner_TableS2_ageProfiles.csv")) %>% 
  dplyr::rename(
    site = Site,
    dateBP = BP,
    N0_4 = `0-4Yrs`,
    N5_19 = `5-19Yrs`,
    N20 = `20+Yrs`,
    MNI = N,
    JI = Estimate
  ) %>% 
  dplyr::mutate(country = "USA",
                studyarea = "MW",
                subregion = "MW",
                JI2 =  N5_19 / (N5_19 + N20),
                CBR = (JI ^ 0.89074 ) * 0.15334 + 0.00375,
                GR = (JI ^ 0.47788) * 0.12555 - 0.05389
  ) %>% 
  dplyr::select(country, studyarea, subregion, site, MNI, N0_4, N5_19, N20, JI, CBR, GR, dateBP )

MW %>% dplyr::filter(MNI > 10) %>% 
  ggplot(aes(x = dateBP, y = CBR))+
  geom_point(aes(size = MNI),  shape=1 )+
  geom_smooth(aes(weight= MNI),
              method="loess",span=0.5,fullrange=TRUE,se=FALSE,
              linewidth = 0.5, color = "black"
  )

cemeteries  <- bind_rows(SW, MW)


cemeteries %>% dplyr::filter(MNI > 10) %>% 
  ggplot(aes(x = dateBP, y = CBR))+
  geom_point(aes(size = MNI),  shape=1 )+
  geom_smooth(aes(weight= MNI),
              method="loess",span=0.5,fullrange=TRUE,se=FALSE,
              linewidth = 0.5, color = "black"
  )+
  scale_x_reverse()+
  facet_wrap(vars(subregion), nrow = 3, scale = "free_x")

cemeteries %>% dplyr::filter(MNI > 10,
                             subregion != "Mogollon") %>% write_csv(here::here("data/data-derived/cemetery_data.csv"))
