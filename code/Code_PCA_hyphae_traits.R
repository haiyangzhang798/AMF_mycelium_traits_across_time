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



hyphae_traits <- hyphae_summary %>%
  dplyr::select(Inoculum_name,Harvest_time, 
                Mycelium_weight, Mycelium_C, Mycelium_N, 
                Mycelium_P, Mycelium_CN, Mycelium_NP, 
                Mycelium_CP, Mycelium_Diameter) %>% 
  drop_na()

library("factoextra")
res.pca <- prcomp(hyphae_traits[, 3:10],  scale = TRUE)
#cols <- hue_pal()(5)

p1 <- fviz_pca_biplot(res.pca, 
                      geom.ind = "point",
                      pointshape = 21,
                      pointsize = 2.5,
                      fill.ind = hyphae_traits$Inoculum_name,
                      legend.title = list(fill = "Species", color = "Clusters"),
                      repel = TRUE)+theme_pubr()


png("output/fig5_pca_hyphae.png", width = 6, height = 6, units = "in", res = 350)
library(gridExtra)
grid.arrange(p1, ncol = 1) 
dev.off()




#####################
hyphae_traits_1 <-hyphae_traits %>% filter(Harvest_time == "2021-05-17")
res.pca1 <- prcomp(hyphae_traits_1[, 3:10],  scale = TRUE)
p2 <- fviz_pca_biplot(res.pca1, 
                      geom.ind = "point",
                      pointshape = 21,
                      pointsize = 2.5,
                      fill.ind = hyphae_traits_1$Inoculum_name,
                      legend.title = list(fill = "Species", color = "Clusters"),
                      repel = TRUE        # Avoid label overplotting
)+labs(title ="2021-05-17") + theme_pubr()


hyphae_traits_2 <-hyphae_traits %>% filter(Harvest_time == "2021-06-08")
res.pca2 <- prcomp(hyphae_traits_2[, 3:10],  scale = TRUE)
p3 <- fviz_pca_biplot(res.pca2, 
                      geom.ind = "point",
                      pointshape = 22,
                      pointsize = 2.5,
                      fill.ind = hyphae_traits_2$Inoculum_name,
                      legend.title = list(fill = "Species", color = "Clusters"),
                      repel = TRUE        # Avoid label overplotting
)+labs(title ="2021-06-08")+theme_pubr()



hyphae_traits_3 <-hyphae_traits %>% filter(Harvest_time == "2021-07-01")
res.pca3 <- prcomp(hyphae_traits_3[, 3:10],  scale = TRUE)
p4 <- fviz_pca_biplot(res.pca3, 
                      geom.ind = "point",
                      pointshape = 24,
                      pointsize = 2.5,
                      fill.ind = hyphae_traits_3$Inoculum_name,
                      legend.title = list(fill = "Species", color = "Clusters"),
                      repel = TRUE        # Avoid label overplotting
)+labs(title ="2021-07-01")+theme_pubr()

png("output/figS2_pca_hyphae.png", width = 18, height = 5, units = "in", res = 350)
library(gridExtra)
grid.arrange(p2,p3, p4, ncol = 3) 
dev.off()

