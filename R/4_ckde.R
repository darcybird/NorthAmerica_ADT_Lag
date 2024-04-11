library(tidyverse)
library(magrittr)
library(rcarbon)
library(purrr)


# load data -----

c14_ckde <- 
  readr::read_csv(here::here("data/data-derived/c14/study_c14.csv")) %>% 
  dplyr::mutate(calib = "intcal20") %>% 
  dplyr::filter(studyarea != "EU") %>% 
  dplyr::select(Age, Error, calib, subregion) %>% 
  dplyr::filter(Age < 6000) %>% 
  dplyr::group_by(subregion)%>% 
  nest(data = -subregion)

nested_function <- function(data){
  cptcal <- rcarbon::calibrate(x = data$Age,  
                               errors = data$Error,
                               calCurves = data$calib, 
                               verbose = FALSE) 
  sampled <- rcarbon::sampleDates(cptcal,
                                  nsim=1000,
                                  boot= TRUE,
                                  verbose=FALSE)
  kde <- rcarbon::ckde(x = sampled,
                       timeRange = c(6000, 100),
                       bw = 50,
                       normalised = TRUE)
  return(kde)
}

tidy_ckde <- function(dat) {
  tr <- dat$timeRange
  as_tibble(dat$res.matrix) |>
    mutate(age = tr[1]:tr[2], .before = 1)
}

# ckde_out_30 <- c14_ckde |>
#   mutate(kde_results = map(data, nested_function),
#          tidied = map(kde_results, tidy_ckde)) |>
#   select(subregion, tidied) |>
#   unnest(tidied) #|>
  # pivot_longer(starts_with('V'), names_to = 'rep')

ckde_out_50 <- c14_ckde |>
  mutate(kde_results = map(data, nested_function),
         tidied = map(kde_results, tidy_ckde)) |>
  select(subregion, tidied) |>
  unnest(tidied) 

ckde_out_50 %>% 
  mutate(across(everything(), replace_na, 0)) |>
  saveRDS("data/data-derived/c14/ckde_out_50.rds")

ckde_out_50 <- readRDS("data/data-derived/c14/ckde_out_50.rds")
# 
# 
# ckde_out_30 %>% 
#   pivot_longer(starts_with('V'), names_to = 'rep') %>% 
#   ggplot(aes(age, value, group = rep)) +
#   geom_line(alpha = .1) +
#   facet_wrap(~subregion, nrow =3) +
#   scale_x_reverse(breaks = seq(500, 6000, 300)) +
#   labs(x = 'Years BP')

# calculate growth rate ----
#modify the spd2rc function slightly so it works with the above ckde 


ckde2rc <- function(data, breaks, changexpr = expression((t1/t0)^(1/d) - 1)){
  nBreaks = length(breaks) - 1
  timeSequence = max(breaks):min(breaks)
  
  out <- purrr::map(
    #select the KDE row 
    .x = 3:ncol(data),
    .f = function(whichKDE){
      thisKDE <- data[,(whichKDE)]
      obs = numeric(length = nBreaks)
      
      for (x in 1:nBreaks) {
        index = which(timeSequence <= breaks[x] & timeSequence > 
                        breaks[x + 1])
        obs[x] = sum(thisKDE[index,])
      }
      
      res = numeric(length = nBreaks - 1)
      for (i in 1:(nBreaks - 1)) {
        d = abs(breaks[i+ 1 ] - breaks[i ])
        t0 = obs[i ]
        t1 = obs[i + 1 ]
        res[i] = eval(changexpr)
        if (t1 == 0 | t0 == 0) { res[i] = NA} 
      } 
      
      return(res)
    }) %>% 
    bind_cols(subregion = unique(data$subregion), 
              scale = as.character((breaks[1] - breaks[2])),
              breaks = breaks[2:(length(breaks)- 1)], 
              ., 
              .name_repair = c("unique_quiet")) 

  return(out)
}

desert.gg <- ckde2rc(data = ckde_out_50 %>%  filter(subregion == "Desert", age <= 5400, age > 99), 
          breaks=seq(5400,100,-10)) 

desert.gg.100 <- ckde2rc(ckde_out_50 %>%  filter(subregion == "Desert", age <= 5400, age >99),
                         breaks=seq(5400,100,-100))


plateau.gg <- ckde2rc(ckde_out_50 %>%  filter(subregion == "Plateau", age <= 5400, age >99),
                      breaks=seq(5400,100,-10))
plateau.gg.100 <- ckde2rc(ckde_out_50 %>%  filter(subregion == "Plateau", age <= 5400, age >99),
                          breaks=seq(5400,100,-100))

MW.gg <- ckde2rc(ckde_out_50 %>%  filter(subregion == "MW", age <= 5400, age >100),
                 breaks=seq(5400,100,-10))
MW.gg.100 <- ckde2rc(ckde_out_50 %>%  filter(subregion == "MW", age <= 5400, age >100),
                     breaks=seq(5400,100,-100))

gg_av <- bind_rows(desert.gg,desert.gg.100,plateau.gg, plateau.gg.100, MW.gg, MW.gg.100) |>
  pivot_longer(starts_with('...'), names_to = 'rep') %>% 
  mutate(subregion = as.factor(subregion),
         scale = factor(scale, levels = c("10", "100"), 
                           labels = c("Decadal", "Centennial")),
         ) %>% 
  filter(!is.na(value)) %>% 
  group_by(subregion, scale, breaks) %>% 
  dplyr::summarise( meanVal =  mean(value),
                   CI_95 = quantile(value, 0.975, na.rm = TRUE),
                   CI_5 = quantile(value, 0.025, na.rm = TRUE) ) %>% 
  filter(breaks >140)
  
gg_av %>% ggplot(aes(x = breaks, y = meanVal, color = scale))+
  geom_line() +
  facet_wrap(~subregion, nrow = 3)+
  scale_x_reverse()

# gg_av %>% 
#   saveRDS(here::here("data/data-derived/c14/c14_ckde_growth_rates.rds"))

gg_av %>% 
  saveRDS(here::here("data/data-derived/c14/c14_ckde_growth_rates_50.rds"))

