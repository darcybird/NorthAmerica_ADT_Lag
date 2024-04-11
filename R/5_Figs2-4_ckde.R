library(magrittr)
library(tidyverse)
library(patchwork)
library(stats)

options(scipen=999)
#load in data ----
#load in SPDs

ckdes <- readRDS(here::here("data/data-derived/c14/ckde_out_100.rds"))%>% 
  pivot_longer(starts_with('V'), names_to = 'rep') %>% 
  group_by(subregion, age) %>% 
  arrange(value) %>% 
  dplyr::summarise(meanVal =  mean(value),
                   CI_95 = quantile(value, 0.975, na.rm = TRUE),
                   CI_5 = quantile(value, 0.025, na.rm = TRUE)
                   ) %>% 
  filter(age >140) 

#load in growth rates
c14_growth_rates <- readRDS(here::here("data/data-derived/c14/c14_ckde_growth_rates_100.rds"))

#load in MW plants
MW_bot <- readr::read_csv(here::here("data/data-derived/archaeobot/mw_archaeobot.csv"))

#load in cemeteries
cemeteries <-  readr::read_csv(here::here("data/data-derived/cemetery_data.csv"))
# Midwest -----

# find important places in growth rates:
# MW_d <-
c14_growth_rates %>% 
  dplyr::filter(subregion == "MW",
                breaks >500) %>%
  ggplot(aes(x = breaks, y = meanVal, color = scale))+
  geom_line() +
  scale_x_reverse(limits = c(5000, 0), name = "Calibrated Date BP")+
  theme_minimal()


c14_growth_rates %>% 
  dplyr::filter(subregion == "MW", breaks %in% c(900:600) 
  ) %>% 
  ggplot(aes(x = breaks, y = meanVal, color = scale))+
  geom_line()+scale_x_reverse()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


MW_regimes <- tibble::tibble(
  start = c(5000, 3700, 2250, 1500, 700 ),
  end = c(3700, 2250, 1500, 700, 400),
  name = c("I", "II", "III", "IV", "V")
) %>% 
  dplyr::mutate(name = factor(name, levels = c("I", "II", "III", "IV", "V")),
                mid = (start+end)/2,
                `BC/AD` = 1950-end)

  


MW_bot_long <-   MW_bot %>%
  dplyr::select(BP, Nuts, EAC, IP, MNI) %>% 
  dplyr::mutate(maize_prop = IP / MNI,
                EAC_prop = (EAC / MNI),
                cultigens_prop = ((IP + EAC) / MNI )) %>% 
  dplyr::select(BP, MNI, maize_prop, EAC_prop, cultigens_prop) %>% 
  tidyr::pivot_longer(maize_prop:cultigens_prop, names_to = "assemblage") %>%
  dplyr::mutate(assemblage=
                factor(assemblage,
                       levels = c("cultigens_prop", "EAC_prop", "maize_prop"),
                       labels = c("All Cultigens", "EAC", "Introduced Plants"))) 

MW_a <-
  ggplot(MW_bot_long , aes(x = BP, y = value, color = assemblage)) +
  stat_smooth(aes(weight= MNI),
              geom= "smooth", method="loess", 
              span=0.55,
              fullrange=TRUE,
              se=FALSE , linewidth = 0.7 ) +
  # geom_point()+
  scale_color_manual(values = c("#F57600", "#5BA300", "#8BABF1"))+
  scale_x_reverse(limits = c(5000, 0), breaks = c(MW_regimes$end, 5000, 3000)) +
  scale_y_continuous(name = "Proportion", breaks = seq(0,0.8, 0.1))+
  ggtitle("US Midwest")+
  geom_vline(data = MW_regimes, aes(xintercept = end), linetype = "dashed", inherit.aes = FALSE)+
  geom_text(data = MW_regimes, aes(x = mid, label = name ), y = 0.85, inherit.aes = FALSE)+
  coord_cartesian(ylim = c(0,0.85)) +
  theme_minimal()+
  theme(    legend.justification = "left",
            legend.position  = c(0.05, 0.5),
        legend.title = element_text(size=8),
        legend.text = element_text(size=6),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.spacing.y = unit(1, 'scaledpts'),
        legend.box.background=element_rect(fill = "white")
  ) +
  guides(color=guide_legend(title="Proportion of\nArchaeobotanical Assemblage", byrow = TRUE))

MW_b <-
  ckdes %>% 
  dplyr::filter(subregion == "MW") %>% 
  ggplot()+
  geom_ribbon(aes(x = age, ymin = CI_5, ymax= CI_95), fill = "gray80")+
  geom_line(aes(x = age, y = meanVal), color = "black", linewidth = 0.7)+
  scale_x_reverse(limits = c(5000, 0),  breaks = c(MW_regimes$end, 5000, 3000)) +
  scale_y_continuous(name = "cKDE (Relative\nEnergy Throughput)") +
  geom_vline(data = MW_regimes, aes(xintercept = end), linetype = "dashed")+
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    legend.justification = "left",
    legend.position  = c(0.05, 0.8),
    legend.title = element_text(size=8),
    legend.text = element_text(size=6),
    legend.spacing.x = unit(0.1, 'mm'),
    legend.direction="horizontal",
    legend.box.background=element_rect(fill = "white"))


MW_b2 <- c14_growth_rates %>% 
  dplyr::filter(subregion == "MW"
  ) %>% 
  ggplot(aes(x = breaks, y = meanVal, color = scale, linewidth = scale )) + 
  geom_line()+
  scale_linewidth_manual(values = c(0.3, 0.7))+  
  scale_color_manual(values = c( "gray50", "black")) +
  scale_y_continuous(name = "cKDE Growth Rates (%)"  ) +
  scale_x_reverse(limits =c(5000, 0), name = "Calibrated Date BP",  
                  breaks = c(MW_regimes$end, 5000, 3000)) +
  geom_vline(data = MW_regimes, aes(xintercept = end), linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.justification = "left",
        legend.position  = c(0.05, 0.3),
        legend.title = element_text(size=8),
        legend.text = element_text(size=6),
        legend.spacing.x = unit(0.1, 'mm'),
        legend.direction="horizontal",
        legend.box.background=element_rect(fill = "white"))+
  guides(color=guide_legend(title="Scale of Comparison"),
         linewidth=guide_legend(title="Scale of Comparison"))


MW_c <- cemeteries %>% 
  dplyr::filter(studyarea == "MW") %>% 
  ggplot(aes(x = dateBP, y = JI))+
  geom_point(aes(size = MNI), shape = 1, color = "gray50")+
  stat_smooth(aes(weight= MNI),
              geom= "smooth", method="loess", 
              span=0.55,
              fullrange=TRUE, 
              se=FALSE , linewidth = 0.5, color = "black") +
  scale_x_reverse(
    limits = c(5000, 0), 
    name = "\nCalibrated Date BP\nYear BC/AD",
    breaks = c(MW_regimes$end, 5000, 3000)) +
  annotate(geom = "text", x = c(5000, 3000, MW_regimes$end), 
           y = 0.03, label = c(-3050, -950,MW_regimes$`BC/AD`), size = 2.5) +
  coord_cartesian(ylim = c(0.1,0.5), xlim = c(5250,-250), expand = FALSE, clip = "off") +
  
  scale_y_continuous(name = "Juvenility Index") +
  scale_size_continuous(breaks = c(33, 200, 500, 1289))+
  geom_vline(data = MW_regimes, aes(xintercept = end), linetype = "dashed", inherit.aes = FALSE)+
  theme_minimal()+
  theme(        legend.justification = "left",
                legend.position  = c(0.05, 0.8),
        legend.title = element_text(size=8),
        legend.text = element_text(size=6),
        legend.spacing.x = unit(0.1, 'mm'),
        legend.direction="horizontal",
        legend.box.background=element_rect(fill = "white"))


  
library(patchwork)
(MW_a / MW_b / MW_b2 / MW_c) 

png(filename = "figures/FigS2_MW_100.png", width = 6, height = 8, units = "in", res = 300)
(MW_a / MW_b / MW_b2 / MW_c) + plot_annotation(tag_levels = 'A')
dev.off()


# Southwest ----
## Plateau ----
# find important places in growth rates:

c14_growth_rates %>% 
  dplyr::filter(subregion == "plateau") %>% 
  ggplot(aes(x = breaks, y = value, color = scale))+
  geom_line() +
  scale_x_reverse(limits = c(5000, 0), name = "Calibrated Date BP")+
  theme_minimal()


c14_growth_rates %>% 
  dplyr::filter(subregion == "plateau", breaks %in% c(2300:2600) 
  ) %>% 
  ggplot(aes(x = breaks, y = value, color = scale))+
  geom_line()+scale_x_reverse()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


Plateau_regimes <- tibble::tibble(
  start = c(5000, 3460, 2490, 1500, 830 ),
  end = c(3460, 2490, 1500, 830, 400),
  name = c("I", "II", "III", "IV", "V")
) %>% 
  dplyr::mutate(name = factor(name, levels = c("I", "II", "III", "IV", "V")),
                mid = (start+end)/2,
                `BC/AD` = 1950-end)


#make the graphs
Plateau_b <-  ckdes %>% 
  dplyr::filter(subregion == "Plateau") %>% 
  ggplot()+
  geom_ribbon(aes(x = age, ymin = CI_5, ymax= CI_95), fill = "gray80")+
  geom_line(aes(x = age, y = meanVal), color = "black", linewidth = 0.7)+
  ggtitle("US Upland Southwest")+
  scale_x_reverse(limits = c(5000, 0),  breaks = c(Plateau_regimes$end, 5000, 4000 ,3000, 2000)) +
  scale_y_continuous(name = "cKDE (Relative\nEnergy Throughput)", breaks = c(0,0.0002,0.0004,0.0006)) +
  geom_vline(data = Plateau_regimes, aes(xintercept = end), linetype = "dashed", inherit.aes = FALSE)+
  geom_text(data = Plateau_regimes, aes(x = mid, label = name ), y = 0.0007, inherit.aes = FALSE)+
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    legend.justification = "left",
    legend.position  = c(0.05, 0.6),
    legend.title = element_text(size=8),
    legend.text = element_text(size=6),
    legend.spacing.x = unit(0.1, 'mm'),
    legend.direction="horizontal",
    legend.box.background=element_rect(fill = "white"))


Plateau_b2 <- c14_growth_rates %>% 
  dplyr::filter(subregion == "Plateau" ) %>% 
  ggplot(aes(x = breaks, y = meanVal, color = scale, linewidth = scale )) + 
  geom_line()+
  scale_linewidth_manual(values = c(0.3, 0.7))+  
  scale_color_manual(values = c( "gray50", "black")) +
  scale_y_continuous(name = "cKDE Growth Rates (%)" ) +
  scale_x_reverse(limits = c(5000, 0),  breaks = c(Plateau_regimes$end, 5000, 4000 ,3000, 2000)) +
  geom_vline(data = Plateau_regimes, aes(xintercept = end), linetype = "dashed", inherit.aes = FALSE)+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.justification = "left",
        legend.position  = c(0.05, 0.3),
        legend.title = element_text(size=8),
        legend.text = element_text(size=6),
        legend.spacing.x = unit(0.1, 'mm'),
        legend.direction="horizontal",
        legend.box.background=element_rect(fill = "white"))+
  guides(color=guide_legend(title="Scale of Comparison"),
         linewidth=guide_legend(title="Scale of Comparison"))

cemeteries %>% 
  dplyr::filter(subregion == "Plateau",
                dateBP %in% c(2450:1400)) %>% 
  dplyr::arrange(-JI)


Plateau_c <- cemeteries %>% 
  dplyr::filter(subregion == "Plateau") %>% 
  ggplot(aes(x = dateBP, y = JI))+
  geom_point(aes(size = MNI), shape = 1, color = "gray50")+
  stat_smooth(aes(weight= MNI),
              geom= "smooth", method="loess", 
              span=0.55,
              fullrange=TRUE, 
              se=FALSE , linewidth = 0.5, color = "black") +
  scale_x_reverse( name = "\nCalibrated Date BP\nYear BC/AD",
                  limits = c(5000, 0), 
                  breaks = c(Plateau_regimes$end, 5000, 4000 ,3000, 2000)) +
  
  annotate(geom = "text", x = c(5000, 4000, 3000, 2000,  Plateau_regimes$end), 
           y = -0.075, label = c(-3050, -1950,-950,  -50, Plateau_regimes$`BC/AD`), size = 2.5) +
  coord_cartesian(ylim = c(0.03,0.55), xlim = c(5250,-250), expand = FALSE, clip = "off") +
  
  scale_y_continuous(name = "Juvenility Index") +
  scale_size_continuous(breaks = c(11, 100, 200, 305))+
  geom_vline(data = Plateau_regimes, aes(xintercept = end), linetype = "dashed", inherit.aes = FALSE)+
  theme_minimal()+
  theme(   
    legend.justification = "left",
            legend.position  = c(0.05, 0.8),
        legend.text.align = 0,
        legend.title = element_text(size=8),
        legend.text = element_text(size=6),
        legend.spacing.x = unit(0.1, 'mm'),
        legend.direction="horizontal",
        legend.box.background=element_rect(fill = "white"))

Plateau_b / Plateau_b2 / Plateau_c

png(filename = "figures/FigS4_Plateau_100.png", width = 6, height = 6, units = "in", res = 300)
(Plateau_b / Plateau_b2 / Plateau_c) + plot_annotation(tag_levels = 'A')
dev.off()

## desert ----
c14_growth_rates %>% 
  dplyr::filter(subregion == "desert") %>% 
  ggplot(aes(x = breaks, y = value, color = scale))+
  geom_line() +
  scale_x_reverse(limits = c(5000, 0), name = "Calibrated Date BP")+
  theme_minimal()


c14_growth_rates %>% 
  dplyr::filter(subregion == "desert", breaks %in% c(3700:4000) 
  ) %>% 
  ggplot(aes(x = breaks, y = value, color = scale))+
  geom_line()+scale_x_reverse()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


Desert_regimes <- tibble::tibble(
  start = c(5000, 3850, 2900, 2000, 850 ),
  end = c(3850, 2900, 2000, 850, 400),
  name = c("I", "II", "III", "IV", "V")
) %>% 
  dplyr::mutate(name = factor(name, levels = c("I", "II", "III", "IV", "V")),
                mid = (start+end)/2,
                `BC/AD` = 1950-end)
#make the graphs

Desert_b <-  ckdes %>% 
  dplyr::filter(subregion == "Desert") %>% 
  ggplot()+
  geom_ribbon(aes(x = age, ymin = CI_5, ymax= CI_95), fill = "gray80")+
  geom_line(aes(x = age, y = meanVal), color = "black", linewidth = 0.7)+
  scale_x_reverse(limits = c(5000, 0), breaks = c(Desert_regimes$end,  5000))+
  scale_y_continuous(
    name = "cKDE (Relative\nEnergy Throughput)", breaks = c(0,0.0002,0.0004,0.0006,0.0008,0.0009,0.001)) +
  geom_vline(data = Desert_regimes, aes(xintercept = end), linetype = "dashed", inherit.aes = FALSE)+
  geom_text(data = Desert_regimes, aes(x = mid, label = name ), y = 0.0008, inherit.aes = FALSE)+
  ggtitle("US Desert Southwest")+
  theme_minimal()+
  theme(
    legend.justification = "left",
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    legend.position  = c(0.05, 0.6),
    legend.title = element_text(size=8),
    legend.text = element_text(size=6),
    legend.spacing.x = unit(0.1, 'mm'),
    legend.direction="horizontal",
    legend.box.background=element_rect(fill = "white"))


Desert_b2 <- c14_growth_rates %>% 
  dplyr::filter(subregion == "Desert"
  ) %>% 
  ggplot(aes(x = breaks, y = meanVal, color = scale, linewidth = scale )) + 
  geom_line()+
  scale_linewidth_manual(values = c(0.3, 0.7))+  
  scale_color_manual(values = c( "gray50", "black")) +
  scale_y_continuous(name = "cKDE Growth Rates (%)" ,# limits = c(-0.9,0.6), 
                     breaks = c(-0.015, -.01, -.005, 0, .005, .01) ) +
  scale_x_reverse(limits = c(5000, 0), breaks = c(Desert_regimes$end,  5000))+
  geom_vline(data = Desert_regimes, aes(xintercept = end), linetype = "dashed", inherit.aes = FALSE)+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()+
  theme(
    legend.justification = "left",
    axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position  = c(0.05, 0.1),
        legend.title = element_text(size=8),
        legend.text = element_text(size=6),
        legend.spacing.x = unit(0.1, 'mm'),
        legend.direction="horizontal",
        legend.box.background=element_rect(fill = "white"))+
  guides(color=guide_legend(title="Scale of Comparison"),
         linewidth=guide_legend(title="Scale of Comparison"))

Desert_c <- cemeteries %>% 
  dplyr::filter(subregion == "Desert") %>% 
  ggplot(aes(x = dateBP, y = JI))+
  geom_point(aes(size = MNI), shape = 1, color = "gray50")+
  stat_smooth(aes(weight= MNI),
              geom= "smooth", method="loess", 
              span=0.55,
              fullrange=TRUE, 
              se=FALSE , linewidth = 0.5, color = "black") +
  scale_x_reverse( name = "\nCalibrated Date BP\nYear BC/AD",
                  limits = c(5000, 0), breaks = c(Desert_regimes$end,  5000))+
  annotate(geom = "text", x = c( 5000, Desert_regimes$end),
           y = -0.25, label = c(-3050, Desert_regimes$`BC/AD`), size = 2.5) +
  coord_cartesian(ylim = c(-0.03,1), xlim = c(5250,-250), expand = FALSE, clip = "off") +
  scale_y_continuous(name = "Juvenility Index") +
  scale_size_continuous(breaks = c(11, 100, 200, 250))+
  geom_vline(data = Desert_regimes, aes(xintercept = end), linetype = "dashed", inherit.aes = FALSE)+
  theme_minimal()+
  theme(
    legend.justification = "left",
    legend.position  = c(0.05, 0.8),
        legend.title = element_text(size=8),
        legend.text = element_text(size=6),
        legend.spacing.x = unit(0.1, 'mm'),
        legend.direction="horizontal",
        legend.box.background=element_rect(fill = "white"))

Desert_b / Desert_b2 / Desert_c


# combine figs

png(filename = "figures/FigS3_desert_100.png", width = 6, height = 6, units = "in", res = 300)
(Desert_b / Desert_b2 / Desert_c) + plot_annotation(tag_levels = 'A')
dev.off()

# massive figure ----

# png(filename = "all.png", width = 10, height = 6, units = "in", res = 300)
# ( plot_spacer() / Desert_b / Desert_b2 / Desert_c) | (Plateau_a / Plateau_b / Plateau_b2 / Plateau_c) | (MW_a / MW_b / MW_b2 / MW_c) + 
#   plot_annotation(tag_levels = 'A')
# dev.off()

# export regimes ----

allregimes <- bind_rows(
  MW_regimes %>% dplyr::mutate(subregion = "MW"),
  Plateau_regimes %>% dplyr::mutate(subregion = "plateau"),
  Desert_regimes %>% dplyr::mutate(subregion = "desert")
) %>% 
  dplyr::rename(regime = name)

allregimes %>% 
  write_csv(here::here("data/data-derived/regimes.csv"))

# Table 2 ----
allregimes <- read_csv(here::here("data/data-derived/regimes.csv"))

allregimes_long <- allregimes  %>% 
  dplyr::select(-mid, -`BC/AD`) %>% 

  dplyr::rowwise() %>% 
  dplyr::mutate(calBP = list(start:end)) %>% 
  tidyr::unnest_longer(col = c(calBP)) %>% 
  dplyr::select(-start, -end) 
  

subgrowth <- c14_growth_rates %>% 
  dplyr::left_join(allregimes_long, by = c("breaks" = "calBP", "subregion" = "subregion")) %>% 
  dplyr::group_by(subregion, regime, scale) %>% 
  dplyr::summarise(mean = mean(value),
                   SD = sd(value)) %>%
  dplyr::mutate(mean = round(mean *100, digits =1),
                SD = round(SD * 100, digits = 1)) %>% 
  dplyr::mutate(GR = stringr::str_c(mean, " + ", SD)) %>% 
  dplyr::select(subregion, scale, regime, GR) %>% 
  tidyr::pivot_wider(names_from = subregion, values_from = GR) %>% 
  dplyr::arrange(scale)

all_growth <- c14_growth_rates %>% 
  dplyr::left_join(allregimes_long, by = c("breaks" = "calBP", "subregion" = "subregion")) %>% 
  dplyr::group_by(regime, scale) %>% 
  dplyr::summarise(mean = mean(meanVal),
                   SD = sd(meanVal)) %>%
  dplyr::mutate(mean = round(mean *100, digits =1),
                SD = round(SD * 100, digits = 1)) %>% 
  dplyr::mutate(GR = stringr::str_c(mean, " + ", SD),
                subregion = "all subregions") %>% 
  dplyr::select(subregion, scale, regime, GR) %>% 
  tidyr::pivot_wider(names_from = subregion, values_from = GR)%>% 
  dplyr::arrange(scale) %>% 
  dplyr::left_join(subgrowth, by = c("regime" = "regime", "scale" = "scale"))

# all_growth %>% write_csv("data/data-derived/GR_summary.csv")

# ji output

cem_long <- cemeteries %>% 
  dplyr::select(subregion, MNI, JI, dateBP) %>% 
  dplyr::mutate(MNI = floor(MNI)) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(individual = list(1:MNI)) %>% 
  tidyr::unnest_longer(col = c(individual)) %>% 
  dplyr::mutate(subregion = ifelse(subregion == "MW", subregion, stringr::str_to_lower(subregion))) %>% 
  dplyr::left_join(allregimes_long, by = c("dateBP" = "calBP", "subregion" = "subregion")) 

subcem <- cem_long %>% 
  dplyr::group_by(subregion, regime) %>% 
  dplyr::summarise(mean = round(mean(JI ), digits = 2),
                   SD = round(sd(JI ), digits = 2))  %>% 
  dplyr::mutate(GR = stringr::str_c(mean, " + ", SD)) %>% 
  dplyr::select(subregion, regime, GR) %>% 
  tidyr::pivot_wider(names_from = subregion, values_from = GR) 
#weighted byMNI
#  cem_long %>% 
#   dplyr::group_by(regime) %>% 
#   dplyr::summarise(sub = round(mean(JI ), digits = 2),
#                    SD = round(sd(JI ), 2))  

#not weihted by MNI
allcem <- cem_long %>% 
  dplyr::select(-individual) %>% 
  dplyr::distinct() %>% 
  dplyr::group_by(regime) %>% 
  dplyr::summarise(mean = round(mean(JI ), 2),
                   SD = round(sd(JI ), 2)) %>% 
  dplyr::mutate(GR = stringr::str_c(mean, " + ", SD),
                subregion = "all subregions") %>% 
  dplyr::select(subregion, regime, GR) %>% 
  tidyr::pivot_wider(names_from = subregion, values_from = GR) %>% 
  dplyr::left_join(subcem, by = c("regime" = "regime"))
# allcem

# allcem %>% write_csv("data/data-derived/cem_summary.csv")


Table2 <- all_growth %>% dplyr::filter(scale == "Centennial") %>% 
  bind_rows(allcem) %>% 
  dplyr::filter(!is.na(regime)) %>% 
  dplyr::select(-scale)

Table2 %>% 
  write_csv("data/data-derived/Table2.csv")


##### 30-year and 50-year figs -----

ckdes_30 <- readRDS(here::here("data/data-derived/c14/ckde_out_30.rds"))%>% 
  pivot_longer(starts_with('V'), names_to = 'rep') %>% 
  group_by(subregion, age) %>% 
  arrange(value) %>% 
  dplyr::summarise(meanVal =  mean(value),
                   CI_95 = quantile(value, 0.975, na.rm = TRUE),
                   CI_5 = quantile(value, 0.025, na.rm = TRUE)
  ) %>% 
  filter(age >140) 


MW_b_30 <-
  ckdes_30 %>% 
  dplyr::filter(subregion == "MW") %>% 
  ggplot()+
  geom_ribbon(aes(x = age, ymin = CI_5, ymax= CI_95), fill = "gray80")+
  geom_line(aes(x = age, y = meanVal), color = "black", linewidth = 0.7)+
  scale_x_reverse(limits = c(5000, 0),  breaks = c(MW_regimes$end, 5000, 3000)) +
  scale_y_continuous(name = "cKDE (Relative\nEnergy Throughput)") +
  geom_vline(data = MW_regimes, aes(xintercept = end), linetype = "dashed")+
  geom_text(data = MW_regimes, aes(x = mid, label = name ), y = 0.001, inherit.aes = FALSE)+
  ggtitle("US Midwest")+
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    legend.justification = "left",
    legend.position  = c(0.05, 0.8),
    legend.title = element_text(size=8),
    legend.text = element_text(size=6),
    legend.spacing.x = unit(0.1, 'mm'),
    legend.direction="horizontal",
    legend.box.background=element_rect(fill = "white"))

Desert_b_30 <-  ckdes_30 %>% 
  dplyr::filter(subregion == "Desert") %>% 
  ggplot()+
  geom_ribbon(aes(x = age, ymin = CI_5, ymax= CI_95), fill = "gray80")+
  geom_line(aes(x = age, y = meanVal), color = "black", linewidth = 0.7)+
  scale_x_reverse(limits = c(5000, 0), breaks = c(Desert_regimes$end,  5000))+
  scale_y_continuous(
    name = "cKDE (Relative\nEnergy Throughput)", breaks = c(0,0.0002,0.0004,0.0006,0.0008,0.0009,0.001)) +
  geom_vline(data = Desert_regimes, aes(xintercept = end), linetype = "dashed", inherit.aes = FALSE)+
  geom_text(data = Desert_regimes, aes(x = mid, label = name ), y = 0.0009, inherit.aes = FALSE)+
  ggtitle("US Desert Southwest")+
  theme_minimal()+
  theme(
    legend.justification = "left",
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    legend.position  = c(0.05, 0.6),
    legend.title = element_text(size=8),
    legend.text = element_text(size=6),
    legend.spacing.x = unit(0.1, 'mm'),
    legend.direction="horizontal",
    legend.box.background=element_rect(fill = "white"))

Plateau_b_30 <-  ckdes_30 %>% 
  dplyr::filter(subregion == "Plateau") %>% 
  ggplot()+
  geom_ribbon(aes(x = age, ymin = CI_5, ymax= CI_95), fill = "gray80")+
  geom_line(aes(x = age, y = meanVal), color = "black", linewidth = 0.7)+
  ggtitle("US Upland Southwest")+
  scale_x_reverse(limits = c(5000, 0),  breaks = c(Plateau_regimes$end, 5000, 4000 ,3000, 2000)) +
  scale_y_continuous(name = "cKDE (Relative\nEnergy Throughput)", breaks = c(0,0.0002,0.0004,0.0006)) +
  geom_vline(data = Plateau_regimes, aes(xintercept = end), linetype = "dashed", inherit.aes = FALSE)+
  geom_text(data = Plateau_regimes, aes(x = mid, label = name ), y = 0.0007, inherit.aes = FALSE)+
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    legend.justification = "left",
    legend.position  = c(0.05, 0.6),
    legend.title = element_text(size=8),
    legend.text = element_text(size=6),
    legend.spacing.x = unit(0.1, 'mm'),
    legend.direction="horizontal",
    legend.box.background=element_rect(fill = "white"))

png(filename = "figures/FigS5_ckde_30.png", width = 6, height = 6, units = "in", res = 300)
(MW_b_30 / Desert_b_30 / Plateau_b_30) + plot_annotation(tag_levels = 'A', title = 'cKDEs with 30-year bin width')
dev.off()





ckdes_50 <- readRDS(here::here("data/data-derived/c14/ckde_out_50.rds"))%>% 
  pivot_longer(starts_with('V'), names_to = 'rep') %>% 
  group_by(subregion, age) %>% 
  arrange(value) %>% 
  dplyr::summarise(meanVal =  mean(value),
                   CI_95 = quantile(value, 0.975, na.rm = TRUE),
                   CI_5 = quantile(value, 0.025, na.rm = TRUE)
  ) %>% 
  filter(age >140) 


MW_b_50 <-
  ckdes_50 %>% 
  dplyr::filter(subregion == "MW") %>% 
  ggplot()+
  geom_ribbon(aes(x = age, ymin = CI_5, ymax= CI_95), fill = "gray80")+
  geom_line(aes(x = age, y = meanVal), color = "black", linewidth = 0.7)+
  scale_x_reverse(limits = c(5000, 0),  breaks = c(MW_regimes$end, 5000, 3000)) +
  scale_y_continuous(name = "cKDE (Relative\nEnergy Throughput)") +
  geom_vline(data = MW_regimes, aes(xintercept = end), linetype = "dashed")+
  geom_text(data = MW_regimes, aes(x = mid, label = name ), y = 0.0009, inherit.aes = FALSE)+
  ggtitle("US Midwest")+
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    legend.justification = "left",
    legend.position  = c(0.05, 0.8),
    legend.title = element_text(size=8),
    legend.text = element_text(size=6),
    legend.spacing.x = unit(0.1, 'mm'),
    legend.direction="horizontal",
    legend.box.background=element_rect(fill = "white"))

Desert_b_50 <-  ckdes_50 %>% 
  dplyr::filter(subregion == "Desert") %>% 
  ggplot()+
  geom_ribbon(aes(x = age, ymin = CI_5, ymax= CI_95), fill = "gray80")+
  geom_line(aes(x = age, y = meanVal), color = "black", linewidth = 0.7)+
  scale_x_reverse(limits = c(5000, 0), breaks = c(Desert_regimes$end,  5000))+
  scale_y_continuous(
    name = "cKDE (Relative\nEnergy Throughput)", breaks = c(0,0.0002,0.0004,0.0006,0.0008,0.0009,0.001)) +
  geom_vline(data = Desert_regimes, aes(xintercept = end), linetype = "dashed", inherit.aes = FALSE)+
  geom_text(data = Desert_regimes, aes(x = mid, label = name ), y = 0.0008, inherit.aes = FALSE)+
  ggtitle("US Desert Southwest")+
  theme_minimal()+
  theme(
    legend.justification = "left",
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    legend.position  = c(0.05, 0.6),
    legend.title = element_text(size=8),
    legend.text = element_text(size=6),
    legend.spacing.x = unit(0.1, 'mm'),
    legend.direction="horizontal",
    legend.box.background=element_rect(fill = "white"))

Plateau_b_50 <-  ckdes_50 %>% 
  dplyr::filter(subregion == "Plateau") %>% 
  ggplot()+
  geom_ribbon(aes(x = age, ymin = CI_5, ymax= CI_95), fill = "gray80")+
  geom_line(aes(x = age, y = meanVal), color = "black", linewidth = 0.7)+
  ggtitle("US Upland Southwest")+
  scale_x_reverse(limits = c(5000, 0),  breaks = c(Plateau_regimes$end, 5000, 4000 ,3000, 2000)) +
  scale_y_continuous(name = "cKDE (Relative\nEnergy Throughput)", breaks = c(0,0.0002,0.0004,0.0006)) +
  geom_vline(data = Plateau_regimes, aes(xintercept = end), linetype = "dashed", inherit.aes = FALSE)+
  geom_text(data = Plateau_regimes, aes(x = mid, label = name ), y = 0.0007, inherit.aes = FALSE)+
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    legend.justification = "left",
    legend.position  = c(0.05, 0.6),
    legend.title = element_text(size=8),
    legend.text = element_text(size=6),
    legend.spacing.x = unit(0.1, 'mm'),
    legend.direction="horizontal",
    legend.box.background=element_rect(fill = "white"))

png(filename = "figures/FigS6_ckde_50.png", width = 6, height = 6, units = "in", res = 300)
(MW_b_50 / Desert_b_50 / Plateau_b_50) + plot_annotation(tag_levels = 'A', title = 'cKDEs with 50-year bin width')
dev.off()

