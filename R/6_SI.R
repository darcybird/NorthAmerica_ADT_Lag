library(tidyverse)
library(magrittr)
library(rcarbon)

#Tree Rings -----
dendro <- read_csv(here::here("data/data-raw/treerings/diss3_TR.csv")) %>% 
  dplyr::mutate(#subregion = factor(subregion),
                `Study Area` = factor(`Study Area`),
                Date_BP = 1950 - `Year AD`,
                SD = 0 ### Fiddle with this later
  ) %>% 
  dplyr::filter(subregion == "Plateau") %>% 
  dplyr::filter(Type %in% c("Near-cutting", "Cutting")) %>% 
  dplyr::select(subregion, Type, Date_BP, SD) 

empty <- tibble::tibble(Date_BP = seq(0, 2000, 1))


dendro_summed <- dendro %>% 
  dplyr::group_by(Type, Date_BP) %>% 
  dplyr::summarize(n = dplyr::n()) %>% 
  dplyr::right_join(empty, by = c("Date_BP" = "Date_BP")) 

movingAverage <- function(x,n){ 
  stats::filter(x = x, 
                filter = rep(1/n, n), 
                method = "convolution", 
                sides = 1, 
                circular = FALSE)
}

#spread probability of near-cutting dates over this year and prior 3 years
dendro_near <- dendro_summed %>% 
  dplyr::filter(Type == "Near-cutting")%>% 
  dplyr::ungroup() %>% 
  dplyr::select(-Type) %>% 
  dplyr::right_join(empty, by = c("Date_BP" = "Date_BP"))  %>% 
  dplyr::arrange(Date_BP) %>% 
  dplyr::mutate(n = ifelse(is.na(n), 0, n),
                n_3 = movingAverage(n, 4),
                n_3 = ifelse(is.na(n_3), 0, n_3))

# join the probabilities for cutting and near-cutting dates together
dendro_spd <- dendro_summed %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(Type == "Cutting") %>% 
  dplyr::mutate(n = ifelse(is.na(n), 0, n)) %>% 
  dplyr::right_join(dendro_near %>% dplyr::select(-n), by = c("Date_BP" = "Date_BP")) %>%
  dplyr::mutate(prob = n + n_3) %>% 
  dplyr::select(-n, -n_3) %>% 
  #normalize and create running means 
  dplyr::mutate(norm_sum = prob / nrow(dendro),
                norm_sum =  ifelse(is.na(norm_sum), 0, norm_sum),
                norm_sum21 = rcarbon::runMean(norm_sum, 21, edge = "fill"),
                norm_sum51 = rcarbon::runMean(norm_sum, 51, edge = "fill")) %>% 
  dplyr::arrange(-Date_BP) 
  
dendro_spd %>% ggplot(aes(x = Date_BP))+
  geom_line(aes(y = norm_sum), color = "gray80")+
  geom_line(aes(y = norm_sum21), color = "gray30", linewidth = 0.7) +
  geom_line(aes(y = norm_sum51), color = "black", linewidth = 0.7) +
  scale_x_reverse()

dendro_spd %>% write_csv(here::here("data/data-derived/dendro_spd.csv"))

#dendro growth
simple_growth <- function(spd, breaks){
  nBreaks = length(breaks) - 1
  timeSequence = 2000:400
  changexpr = expression((t1/t0)^(1/d) -  1)
  if (!is.null(breaks)) {
    type = "blocks"
    obs = numeric(length = nBreaks)
    for (x in 1:nBreaks) {
      index = which(timeSequence <= breaks[x] & timeSequence > 
                      breaks[x + 1])
      obs[x] = sum(spd$norm_sum51[index])
    }
    res = numeric(length = nBreaks - 1)
    for (i in 1:(nBreaks - 1)) {
      d = abs(breaks[i + 1] - breaks[i])
      t0 = obs[i]
      t1 = obs[i + 1]
      res[i] = eval(changexpr)
      if (t1 == 0 | t0 == 0) {
        res[i] = NA
      }
    }
  }
  res = list(sumblock = obs, roca = res, breaks = breaks, 
             timeSequence = timeSequence, type = type)
  class(res) <- append(class(res), "spdRC")
  return(res)
}

# rcarbon::spd2rc()

dendro_gg <- simple_growth(dendro_spd, breaks = seq(2000,400,-10))
dendro_gg %>% plot()

dendro_growth <- bind_cols(dendro_gg$breaks[3:161], dendro_gg$roca) 


names(dendro_growth) <- c("Date_BP", "GR")

dendro_growth %>% 
  dplyr::mutate(GR = ifelse(is.na(GR), 0, GR)) %>% 
  write_csv(here::here("data/data-derived/dendro_growth.csv"))

rm(list = ls())
gc()
# compare Tree rings with 14C ----

#load in dendro
dendro_spd <- read_csv(here::here("data/data-derived/dendro_spd.csv")) %>% 
  dplyr::select(-norm_sum21, -prob) %>% 
  tidyr::pivot_longer(cols = c(norm_sum:norm_sum51), names_to = "SPD", values_to = "PrDens") %>% 
  dplyr::mutate(SPD = factor(SPD, levels = c("norm_sum",  "norm_sum51"),
                             labels = c("Annual",  "51-year mean")))

dendro_growth <- read_csv(here::here("data/data-derived/dendro_growth.csv"))
dendro_growth %>% ggplot(aes(x = Date_BP, y = GR))+
  geom_line()+
  scale_x_reverse()

#load in c14
SPDs <- read_csv(here::here("data/data-derived/spds.csv")) %>% 
  dplyr::mutate(runm =ifelse(runm == "200", "Dens200", "Dens1"),
                runm = factor(runm, levels = c("Dens1", "Dens200"), 
                              labels = c("Annual", "200-year mean"))) %>% 
  dplyr::rename(SPD = runm)

# load in growth rates
c14_growth_rates <- read_csv(here::here("data/data-derived/c14_growth_rates.csv"))

Plateau_regimes_short <- read_csv(here::here("data/data-derived/regimes.csv")) %>% 
  dplyr::filter(subregion == "plateau") %>% 
  dplyr::filter(end < 2000)

dendro_spd %>% filter(SPD == "51-year mean") %>% dplyr::arrange(-PrDens)
#make figs

TR_a <- dendro_spd %>% 
  ggplot(aes(x = Date_BP))+
  geom_line(aes(y = PrDens, color = SPD, linewidth = SPD))+
  scale_color_manual(values = c("gray80",  "black"))+
  scale_x_reverse(limits = c( 1500, 400),breaks = c(2000, 1400, 830, 400), name = "calBP") +
  scale_linewidth_manual(values = c(0.3, 0.7))+
  scale_y_continuous(name = "Tree-Ring\nTime-series", breaks = c(0, 0.002, 0.004, 0.006, 0.008, 0.01, 0.012, 0.014)) +
  ggtitle("US SW Colorado Plateau")+
  geom_vline(data = Plateau_regimes_short, aes(xintercept = end), linetype = "dashed", inherit.aes = FALSE)+
  geom_text(data = Plateau_regimes_short, aes(x = mid, label = regime ), y = 0.012, inherit.aes = FALSE)+
  theme_minimal()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.justification = "left",
        legend.position  = c(0.05, 0.5),
        legend.title = element_blank(),
        legend.text = element_text(size=6),
        legend.direction="horizontal",
        legend.spacing.x = unit(0.1, 'mm'),
        legend.box.background=element_rect(fill = "white"))

TR_a2 <- dendro_growth %>%
  ggplot(aes(x = Date_BP    , y = GR )) +
  geom_line() +
  scale_y_continuous(name = "Decadal Dendro\nGrowth Rates (%)"  ) +
  scale_x_reverse(limits = c( 1500, 400), breaks = c(2000, 1400, 830, 400), name = "calBP") +
  geom_vline(data = Plateau_regimes_short, aes(xintercept = end), linetype = "dashed", inherit.aes = FALSE)+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )


Plateau_b <-  SPDs %>% 
  dplyr::filter(subregion == "plateau") %>% 
  ggplot(aes(x = calBP, y = PrDens, color = SPD, linewidth = SPD))+
  geom_line()+
  scale_color_manual(values = c( "gray50", "black")) +
  scale_linewidth_manual(values = c(0.3, 0.7))+
  # ggtitle("US SW Colorado Plateau")+
  scale_x_reverse(limits = c( 1500, 400),breaks = c(2000, 1400, 830, 400), name = "calBP") +
  scale_y_continuous(name = "SPD (Relative\nEnergy Consumption)", breaks = c(0,0.0002,0.0004,0.0006)) +
  geom_vline(data = Plateau_regimes_short, aes(xintercept = end), linetype = "dashed", inherit.aes = FALSE)+
  # geom_text(data = Plateau_regimes_short, aes(x = mid, label = name ), y = 0.0007, inherit.aes = FALSE)+
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    legend.justification = "left",
    legend.position  = c(0.05, 0.2),
    legend.title = element_text(size=8),
    legend.text = element_text(size=6),
    legend.spacing.x = unit(0.1, 'mm'),
    legend.direction="horizontal",
    legend.box.background=element_rect(fill = "white"))


Plateau_b2 <- c14_growth_rates %>%
  dplyr::filter(subregion == "plateau"#, breaks %in% c(3600:5000)
  ) %>%
  ggplot(aes(x = breaks, y = value, color = scale, linewidth = scale  )) +
  geom_line()+
  scale_color_manual(values = c( "gray50", "black")) +
  scale_linewidth_manual(values = c(0.3, 0.7))+
  scale_y_continuous(name = "Decadal SPD\nGrowth Rates (%)" ,
                     breaks = c(-0.8,  -0.6, -0.4, -0.2, 0,  0.2, 0.4)
  ) +
  scale_x_reverse(limits = c( 1500, 400),breaks = c(2000, 1400, 830, 400), name = "calBP") +
  geom_vline(data = Plateau_regimes_short, aes(xintercept = end), linetype = "dashed", inherit.aes = FALSE)+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()+
  theme(
    legend.justification = "left",
    legend.position  = c(0.05, 0.2),
    legend.title = element_text(size=8),
    legend.text = element_text(size=6),
    legend.spacing.x = unit(0.1, 'mm'),
    legend.direction="horizontal",
    legend.box.background=element_rect(fill = "white"))

library(patchwork)
TR_a / TR_a2 / Plateau_b / Plateau_b2

png(filename = "figures/Fig_S1.png", width = 6, height = 7, units = "in", res = 300)
(TR_a / TR_a2 / Plateau_b / Plateau_b2) + plot_annotation(tag_levels = 'A')
dev.off()

# Make Tables S1 and S2 ----

cemeteries <-  readr::read_csv(here::here("data/data-derived/cemetery_data.csv"))

allregimes_long <- read_csv(here::here("data/data-derived/regimes.csv")) %>% 
  dplyr::select(-mid, -`BC/AD`) %>% 
  
  dplyr::rowwise() %>% 
  dplyr::mutate(calBP = list(start:end)) %>% 
  tidyr::unnest_longer(col = c(calBP)) %>% 
  dplyr::select(-start, -end) 


cem_long <- cemeteries %>% 
  dplyr::select(subregion, MNI, CBR, GR, dateBP) %>% 
  dplyr::mutate(MNI = floor(MNI),
                CBR = 100 * CBR,
                GR = 100 * GR) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(individual = list(1:MNI)) %>% 
  tidyr::unnest_longer(col = c(individual)) %>% 
  dplyr::mutate(subregion = ifelse(subregion == "MW", subregion, stringr::str_to_lower(subregion))) %>% 
  dplyr::left_join(allregimes_long, by = c("dateBP" = "calBP", "subregion" = "subregion")) 

subcem <- cem_long %>% 
  dplyr::group_by(subregion, regime) %>% 
  dplyr::summarise(mean_CBR = round(mean(CBR ), digits = 2),
                   SD_CBR = round(sd(CBR ), digits = 2),
                   mean_GR = round(mean(GR ), digits = 2),
                   SD_GR = round(sd(GR ), digits = 2),
                   )  %>% 
  dplyr::mutate(CBR = stringr::str_c(mean_CBR, " + ", SD_CBR),
                GR = stringr::str_c(mean_GR, " + ", SD_GR)) %>% 
  dplyr::select(subregion, regime, CBR, GR) %>% 
  tidyr::pivot_wider(names_from = subregion, values_from = c(CBR:GR)) 

subcem %>% write_csv("data/data-derived/TableS2.csv")
