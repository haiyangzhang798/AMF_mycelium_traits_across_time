rm(list = ls())
library(openxlsx)
library(tidyverse)


pixel <- read.csv("data/pixel_across_time_202207.csv") %>%
  mutate(Inoculum_name = recode_factor(Inoculum_name, "NonAMF" = "NonAMF",
                                       "GigMar"= "GigMar", 
                                       "ScuCal" = "ScuCal",
                                       "GloMos" = "GloMos", 
                                       "RI_DAOM10" = "RI_DAOM10",
                                       "RI_VAM23" = "RI_VAM23"))


pixel_summary <- pixel %>%
  dplyr::select(Harvest_time, Inoculum_name, PotID,Pixel_frac)%>%
  group_by(Harvest_time, Inoculum_name)  %>%
  summarise(mean_pixel = mean(Pixel_frac, na.rm = T),
            sd_pixel = sd(Pixel_frac, na.rm = T),
            n = n()) %>%
  mutate(se_pixel = sd_pixel/sqrt(n)) %>%
  droplevels() %>% 
  mutate(Harvest_time = as.Date(Harvest_time, format = "%d/%m/%Y"))
#############################################################
library(ggpubr)
library(scales)
hue_pal()(5)
cols <- c("grey", "#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3")

fig1 <- ggplot(pixel_summary, aes(x=as.Date(Harvest_time), y=mean_pixel, color=Inoculum_name)) + 
  geom_errorbar(position=position_dodge(width = 5),aes(ymin=mean_pixel-se_pixel, 
                                                       ymax=mean_pixel+se_pixel), width=0.3)+
  geom_point(position=position_dodge(width = 5), size =5)+
  ylab("Pixel fraction (%)") +xlab("")+
  theme_pubr()+scale_color_manual(values = cols)+
  theme(legend.position = c(0.8, 0.2), 
        legend.title = element_blank(),
        legend.background =element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# ggplot(hyphae_summary, aes(x=Inoculum_name, y=mean_weight)) + 
#   geom_boxplot() + ylab("Mycelium")
png("output/Fig16_plant_pixel.png", width = 8, height = 6, units = "in", res = 350)
library(gridExtra)
grid.arrange(fig1, ncol = 1) 
dev.off()

#################################
## mean of control for 05-17 0.3257936
## mean of control for 06-06 0.4140690
## mean of control for 07-01 0.3372123

pixel_select <- pixel %>% filter(!(Inoculum_name == "NonAMF")) %>%
  mutate(Control_pixel = case_when(Harvest_time == "16/05/2021" ~ 0.3258,
                                   Harvest_time == "6/06/2021" ~ 0.4141,
                                   Harvest_time == "28/06/2021" ~ 0.3372)) %>%
  mutate(Pixel_RR = log10(Pixel_frac/Control_pixel)) %>%
  dplyr::select(PotID, Harvest_time, Inoculum_name , Pixel_RR) %>%
  drop_na() %>% 
  mutate(Harvest_time = recode_factor(Harvest_time, 
                                      "16/05/2021" = "2021-05-17", 
                                      "6/06/2021" = "2021-06-08", 
                                      "28/06/2021" = "2021-07-01"))


hyphae_traits_pixel <- read.csv("data/meta_data_202207.csv") %>%
  left_join(pixel_select) %>%
  dplyr::select(PotID:Mycelium_CP, -Mycelium_Si, Pixel_RR) %>% 
  dplyr::select(PotID,Harvest_time, Inoculum_name, 
                Pixel_RR, everything()) %>% 
  pivot_longer(!(PotID:Pixel_RR),
               names_to = "Traits", values_to = "Value")


fig2 <- ggplot(hyphae_traits_pixel) + 
  aes(x= Pixel_RR, y = Value, 
      color = Inoculum_name, shape = Harvest_time) +
  geom_point( size = 3) + 
  facet_wrap(~Traits*Inoculum_name, scales = "free")

png("output/Fig7_pixel_ResRatio.png", width = 16, height = 12, units = "in", res = 350)
library(gridExtra)
grid.arrange(fig2, ncol = 1) 
dev.off()

