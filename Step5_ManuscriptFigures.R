# Authors: Jesse Wolf 
# Purpose: Generate final plots for manuscript
# Input: Oreamnos/odocoileus home range, proportional habitat use, and microbiome data
# Output: Final manuscript plots in .pdf and .tiff 
# Title: Step5_ManuscriptFigures


# Packages and environment setup ----

libs <- c('dplyr', 
          'tidyverse', 
          'ggplot2', 
          'patchwork')
       #   'png',
      #    'extrafont',)

lapply(libs, require, character.only = TRUE)

# Colours for plots
mycol <- c('#E1BE6A', '#40B0A6')

species_mb_hr <- read_csv("Microbiome_and_homerange_data.csv")
species_mb_hs <- read_csv ("Microbiome_and_use_data.csv")

# Figure 2----
### Firm_Bact
mg_FB_coresummer <- ggplot (data=subset(species_mb_hr, 
                                         !is.na(Species)),
                            aes(x = Firm_Bact, 
                                y= core_summer,
                                color = ageclass,
                                shape = sex)) +
  geom_point(size=5)+
  geom_smooth(data = subset(species_mb_hr, Species =="O. virginianus"),aes(group = 1),method = glm, formula = y~x, color ='black') +
  geom_smooth(data = subset(species_mb_hr, Species =="O. americanus"),aes(group = 1),method = glm, formula = y~x, color ='darkgrey') +
  xlab ("Firmicutes to Bacteroidetes ratio") +
  ylab ("Core range (km^2)")+
  scale_color_manual(values=mycol) +
  guides (color=FALSE) +
  guides (shape =FALSE)+
  theme(axis.title=element_text(family="Times", face="bold", size=17),
        axis.text.x = element_text(family = "Times",face = "bold", size=15),
        axis.text.y=element_text(family = "Times", face = "bold", size=15))+scale_x_continuous(
          labels = scales::number_format(accuracy = 0.1,
                                         decimal.mark = '.'))

### Pielou
mg_p_coresummer <- ggplot (data=subset(species_mb_hr, 
                                      !is.na(Species)),
                            aes(x = pielou, 
                                y= core_summer,
                                color = ageclass,
                                shape = sex)) +
  geom_point(size=5)+
  geom_smooth(aes(group = 1),method = glm, formula = y~x, color ='black') +
  xlab ("Pielou's evenness") +
  ylab ("Core range (km^2)")+
  scale_color_manual(values=mycol) +
  guides (color=FALSE) +
  guides (shape =FALSE)+
  theme(axis.title=element_text(family="Times", face="bold", size=17),
        axis.text.x = element_text(family = "Times",face = "bold", size=15),
        axis.text.y=element_text(family = "Times", face = "bold", size=15))+scale_x_continuous(
          labels = scales::number_format(accuracy = 0.001,
                                         decimal.mark = '.'))

# Facet wrap for core range plots

facet_FB_core<- mg_FB_coresummer+
  facet_grid(cols = vars(Species),scales = "free") +
  theme_bw()+
  theme(strip.text.x = element_text(size = 13, colour = "black"),
        axis.title=element_text(family="Times", face="bold", size=17),
  axis.text.x = element_text(family = "Times",face = "bold", size=11),
axis.text.y=element_text(family = "Times", face = "bold", size=11))

facet_pielou_core <- mg_p_coresummer +
  facet_grid (cols = vars(Species), scales = "free")+
  theme_bw()+
  theme(strip.text.x = element_text(size = 13, colour = "black"),
        axis.title=element_text(family="Times", face="bold", size=17),
        axis.text.x = element_text(family = "Times",face = "bold", size=11),
        axis.text.y=element_text(family = "Times", face = "bold", size=11))

# Combining both facets
facets_core<- facet_FB_core+
  facet_pielou_core

# Combine using patchwork
facets_core+
  plot_layout(ncol=1, nrow=2)

# Export
ggsave("Fig2.tiff", dpi=600, compression = "lzw")
dev.off()
ggsave("Fig2.pdf", dpi=600)
dev.off()

# Figure 3----

# Firm_Bact

mg_FB_proportion <- species_mb_hs %>%
  filter(Species =="O. americanus")%>%
  ggplot (aes(x = Firm_Bact, 
                                y= proportion,
                                color = age_class,
                                shape = sex)) +
  geom_point(size=5)+
  geom_smooth(data = subset(species_mb_hs, proportion_type =="Escape Terrain"),aes(group = 1),method = glm, formula = y~x, color ='darkgrey') +
  geom_smooth(data = subset(species_mb_hs, proportion_type =="Treed"),aes(group = 1),method = glm, formula = y~x, color ='darkgrey') +
  geom_smooth(data = subset(species_mb_hs, proportion_type =="Heat Load Index"),aes(group = 1),method = glm, formula = y~x, color ='darkgrey') +
  xlab ("Firmicutes to Bacteroidetes ratio") +
  ylab ("Proportional Habitat Use")+
  scale_color_manual(values=mycol) +
  guides (color=FALSE) +
  guides (shape =FALSE)+
  theme(axis.title=element_text(family="Times", face="bold", size=17),
        axis.text.x = element_text(family = "Times",face = "bold", size=15),
        axis.text.y=element_text(family = "Times", face = "bold", size=15))

# Pielou

mg_pielou_proportion <- species_mb_hs %>%
  filter(Species =="O. americanus")%>%
  ggplot (aes(x = pielou, 
              y= proportion,
              color = age_class,
              shape = sex)) +
  geom_point(size=5)+
  geom_smooth(data = subset(species_mb_hs, proportion_type =="Escape Terrain"),aes(group = 1),method = glm, formula = y~x, color ='black') +
  geom_smooth(data = subset(species_mb_hs, proportion_type =="Treed"),aes(group = 1),method = glm, formula = y~x, color ='black') +
  geom_smooth(data = subset(species_mb_hs, proportion_type =="Heat Load Index"),aes(group = 1),method = glm, formula = y~x, color ='darkgrey') +
  xlab ("Pielou's evenness") +
  ylab ("Proportional Habitat Use")+
  scale_color_manual(values=mycol) +
  guides (color=FALSE) +
  guides (shape =FALSE)+
  theme(axis.title=element_text(family="Times", face="bold", size=17),
        axis.text.x = element_text(family = "Times",face = "bold", size=15),
        axis.text.y=element_text(family = "Times", face = "bold", size=15))

# Facet wrap for proportion plots

facet_FB_proportion_mg<- mg_FB_proportion+
  facet_grid(cols = vars(proportion_type),scales = "free") +
  theme_bw()+
  theme(strip.text.x = element_text(size = 13, colour = "black"),
        axis.title=element_text(family="Times", face="bold", size=17),
        axis.text.x = element_text(family = "Times",face = "bold", size=11),
        axis.text.y=element_text(family = "Times", face = "bold", size=11))
 
facet_pielou_proportion_mg<- mg_pielou_proportion +
  facet_grid (cols = vars(proportion_type), scales = "free")+
  theme_bw()+
  theme(strip.text.x = element_text(size = 13, colour = "black"),
        axis.title=element_text(family="Times", face="bold", size=17),
        axis.text.x = element_text(family = "Times",face = "bold", size=11),
        axis.text.y=element_text(family = "Times", face = "bold", size=11))

# Combine both facets
facets_proportion_mg<- facet_FB_proportion_mg+
  facet_pielou_proportion_mg

# Organize using patchwork
facets_proportion_mg+
  plot_layout(ncol=1, nrow=2)

# Export

ggsave("Fig3.tiff", dpi=600, compression = "lzw")
dev.off()
ggsave("Fig3.pdf", dpi=600)
dev.off()
