rm(list = ls())
library(openxlsx)
library(tidyverse)
library(ggplot2)
library(ggpubr)

hyphae_summary <- read.csv("data/meta_data_202207.csv") %>%
  mutate(Inoculum_name = recode_factor(Inoculum_name, 
                                       "GigMar"= "GigMar", 
                                       "ScuCal" = "ScuCal",
                                       "GloMos" = "GloMos", 
                                       "RI_DAOM10" = "RI_DAOM10",
                                       "RI_VAM23" = "RI_VAM23"))

pixel <- read.csv("data/pixel_across_time_202207.csv") %>%
  mutate(Inoculum_name = recode_factor(Inoculum_name, "NonAMF" = "NonAMF",
                                       "GigMar"= "GigMar", 
                                       "ScuCal" = "ScuCal",
                                       "GloMos" = "GloMos", 
                                       "RI_DAOM10" = "RI_DAOM10",
                                       "RI_VAM23" = "RI_VAM23"))

dat <- full_join(hyphae_summary %>% 
                   filter(Harvest_time == "2021-07-01") %>% 
                   select(PotID, Inoculum_name, Shoot_weight), 
                 pixel %>% 
                   filter(Harvest_time == "28/06/2021") %>% 
                   select(PotID, Inoculum_name, Pixel_frac))

ggplot(dat, aes(x=Shoot_weight, y=Pixel_frac, colour=Inoculum_name)) + 
  geom_point() + 
  geom_smooth(method='lm')

mod <- lm(Pixel_frac ~ Shoot_weight * Inoculum_name, data=dat)
car::Anova(mod)
summary(mod)

dat %>% 
  group_by(Inoculum_name) %>% 
  summarise(sd_shootmass = sd(Shoot_weight, na.rm=TRUE), 
            sd_pixel = sd(Pixel_frac, na.rm=TRUE))
