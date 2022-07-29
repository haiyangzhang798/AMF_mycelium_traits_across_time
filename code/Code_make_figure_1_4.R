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

fig1 <- ggplot(hyphae_summary, aes(x=Harvest_time, y=Mycelium_weight, color = Inoculum_name)) + 
  geom_boxplot(outlier.shape = NA)+ 
  geom_jitter(color = "black", alpha = 0.3)+
  xlab("") + ylab("Mycelium biomass (mg)")+
  facet_wrap(~Inoculum_name, ncol = 6) + theme_pubr()+
  theme(legend.position = "none",axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

hyphae_summary_myco <- hyphae_summary %>%
                       filter(Harvest_time == "2021-07-01")

fig2 <- ggplot(hyphae_summary_myco, aes(x=Mycelium_weight, y=Colonization, color = Inoculum_name)) + 
  geom_point(size = 5)+ #geom_smooth() +
  geom_abline(slope = 19.64, intercept = 8.44, lwd = 1)+
  #stat_ellipse()+
  xlab("Mycelium biomass (mg)") + ylab("Mycorrhizal colonization (%)")+
  theme_pubr()+
  theme(legend.position = "none",axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


############################################################################

png("output/Fig1_myco_parameters.png", width = 8, height = 8, units = "in", res = 350)
library(gridExtra)
grid.arrange(fig1, fig2, ncol = 1) 
dev.off()

############################################################################

fig3 <- ggplot(hyphae_summary, aes(x=Harvest_time, y=Mycelium_C, color = Inoculum_name)) + 
  geom_boxplot(outlier.shape = NA)+ 
  geom_jitter(color = "black", alpha = 0.3)+
  xlab("") + ylab("Mycelium C conc. (%)")+
  facet_wrap(~Inoculum_name, ncol = 6) + theme_pubr()+
  theme(legend.position = "none",axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

fig4 <- ggplot(hyphae_summary, aes(x=Harvest_time, y=Mycelium_N, color = Inoculum_name)) + 
  geom_boxplot(outlier.shape = NA)+ 
  geom_jitter(color = "black", alpha = 0.3)+
  xlab("") + ylab("Mycelium N conc. (%)")+
  facet_wrap(~Inoculum_name, ncol = 6) + theme_pubr()+
  theme(legend.position = "none",axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

fig5 <- ggplot(hyphae_summary, aes(x=Harvest_time, y=Mycelium_P, color = Inoculum_name)) + 
  geom_boxplot(outlier.shape = NA)+ 
  geom_jitter(color = "black", alpha = 0.3)+
  xlab("") + ylab("Mycelium P conc. (%)")+
  facet_wrap(~Inoculum_name, ncol = 6) + theme_pubr()+
  theme(legend.position = "none",axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

png("output/Fig2_Mycelium_CNP_conc.png", width = 8, height = 8, units = "in", res = 350)
library(gridExtra)
grid.arrange(fig3,fig4, fig5, ncol = 1) 
dev.off()


############################################################################

fig6 <- ggplot(hyphae_summary, aes(x=Harvest_time, y=Mycelium_Si, color = Inoculum_name)) + 
  geom_boxplot(outlier.shape = NA)+ 
  geom_jitter(color = "black", alpha = 0.3)+
  xlab("") + ylab("Mycelium Si conc. (%)")+
  facet_wrap(~Inoculum_name, ncol = 6) + theme_pubr()+
  theme(legend.position = "none",axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

summary(lm(Mycelium_C~Mycelium_Si, data = hyphae_summary))

fig6_1 <- ggplot(hyphae_summary, aes(x=Mycelium_C, y=Mycelium_Si, color = Inoculum_name, shape = Harvest_time)) + 
  geom_point(size = 5)+ 
  xlab("Mycelium C conc.(%)") + 
  ylab("Mycelium Si conc. (%)")+
  theme_pubr()+
  theme(legend.position = "bottom",legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


png("output/FigS1_Mycelium_C_Si.png", width = 8, height = 8, units = "in", res = 350)
library(gridExtra)
grid.arrange(fig6,fig6_1, ncol = 1) 
dev.off()

fig7 <- ggplot(hyphae_summary, aes(x=Harvest_time, y=Mycelium_CN, color = Inoculum_name)) + 
  geom_boxplot(outlier.shape = NA)+ 
  geom_jitter(color = "black", alpha = 0.3)+
  xlab("") + ylab("Mycelium C:N (mass ratio)")+
  facet_wrap(~Inoculum_name, ncol = 6) + theme_pubr()+
  theme(legend.position = "none",axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

fig8 <- ggplot(hyphae_summary, aes(x=Harvest_time, y=Mycelium_CP, color = Inoculum_name)) + 
  geom_boxplot(outlier.shape = NA)+ 
  geom_jitter(color = "black", alpha = 0.3)+
  xlab("") + ylab("Mycelium C:P (mass ratio)")+
  facet_wrap(~Inoculum_name, ncol = 6) + theme_pubr()+
  theme(legend.position = "none",axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

fig9 <- ggplot(hyphae_summary, aes(x=Harvest_time, y=Mycelium_NP, color = Inoculum_name)) + 
  geom_boxplot(outlier.shape = NA)+ 
  geom_jitter(color = "black", alpha = 0.3)+
  xlab("") + ylab("Mycelium N:P (mass ratio)")+
  facet_wrap(~Inoculum_name, ncol = 6) + theme_pubr()+
  theme(legend.position = "none",axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

png("output/Fig3_Mycelium_stoi.png", width = 8, height = 8, units = "in", res = 350)
library(gridExtra)
grid.arrange(fig7,fig8, fig9, ncol = 1) 
dev.off()


fig10 <- ggplot(hyphae_summary, aes(x=Harvest_time, y=Mycelium_Diameter, color = Inoculum_name)) + 
  geom_boxplot(outlier.shape = NA)+ 
  geom_jitter(color = "black", alpha = 0.3)+
  xlab("") + ylab("Mycelium diameter (um)")+
  facet_wrap(~Inoculum_name, ncol = 6) + theme_pubr()+
  theme(legend.position = "none",axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

png("output/Fig4_hyphae_dia.png", width = 8, height = 4, units = "in", res = 350)
library(gridExtra)
grid.arrange(fig10, ncol = 1) 
dev.off()
