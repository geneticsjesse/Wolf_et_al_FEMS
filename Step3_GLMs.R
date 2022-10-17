# Authors: Jesse Wolf ----
# Purpose: Generate Generalized Linear Models for core range size and proportional habitat use
# Input: Oreamnos/odocoileus home range, proportional habitat use, and microbiome data
# output: Summary tables of GLM data
# Title: Step3_GLMs

# Packages and environment preparation 

libs <- c('dplyr', 'tidyverse', 'sjPlot')
lapply(libs, require, character.only = TRUE)


# Read in data

oreamnos <- read_csv("MountainGoatData.csv")
odocoileus <- read_csv("WhiteTailedDeerData.csv")

# Mountain goat core range size GLMs

model1a<- glm(core_summer ~ Firm_Bact + age_class, 
                      data = oreamnos)
model1b <- glm (core_summer ~ pielou_e + age_class,
                       data = oreamnos)

# Output document using tab_model
tab_model(model1a, model1b,
          title = "Mountain goat",
          dv.labels = c("Core range size", "Core range size"),
          pred.labels = c("(Intercept)", "F:B Ratio", "Age class (Subadult)", "Pielou's evenness"),
          file = "CoreRangeSize_Oreamnos.doc",
          show.obs = TRUE)

# Mountain goat proportional habitat use GLMs

model2a<- glm(Selection_Esc_Summer50~Firm_Bact + age_class,
                    data=oreamnos)
model2b<- glm (Selection_Esc_Summer50 ~ pielou_e + age_class,
                    data=oreamnos)
model2c<- glm(Selection_Treed_Summer50~Firm_Bact + age_class,
              data=oreamnos)
model2d<- glm (Selection_Treed_Summer50 ~ pielou_e + age_class,
               data=oreamnos)
model2e<- glm(Selection_HLI_Summer50~Firm_Bact + age_class,
              data=oreamnos)
model2f<- glm (Selection_HLI_Summer50 ~ pielou_e + age_class,
               data=oreamnos)

# Output document using tab_model
tab_model(model2a, model2b,
          title = "Mountain goat",
          dv.labels = c("Escape terrain proportional habitat use", "Escape terrain proportional habitat use"),
          pred.labels = c("(Intercept)", "F:B Ratio", "Age class (Subadult)", "Pielou's evenness"),
          file = "EscapeUse_Oreamnos.doc",
          show.obs = FALSE)

# White-tailed deer core range size GLMs

model3a <- glm(summer_core ~Firm_Bact + ageclass,
                       data=odocoileus)
model3b <-glm(summer_core~pielou +ageclass,
                      data=odocoileus)

# Output document using tab_model
tab_model(model3a, model3b, 
          title = "White-tailed deer",
          dv.labels = c("Core range size", "Core range size"),
          pred.labels = c("(Intercept)", "F:B Ratio", "Age class (Subadult)", "Pielou's evenness"),
          file = "CoreRangeSize_Odocoileus.doc",
          show.obs = FALSE)

# White-tailed deer proportional habitat use GLMs

model4a <- glm(Selection_Forest_Summer50 ~ Firm_Bact +ageclass,
                         data=odocoileus)
model4b <- glm(Selection_Forest_Summer50 ~pielou +ageclass,
                         data=odocoileus)
model4c <- glm(Selection_ThicketSwamp_Summer50 ~ Firm_Bact +ageclass,
               data=odocoileus)
model4d <- glm(Selection_ThicketSwamp_Summer50 ~pielou +ageclass,
               data=odocoileus)
model4e <- glm(Selection_TreedSwamp_Summer50 ~ Firm_Bact +ageclass,
               data=odocoileus)
model4f <- glm(Selection_TreedSwamp_Summer50 ~pielou +ageclass,
               data=odocoileus)

# Output document using tab_model
tab_model(model4a, model4b,
          title = "White-tailed deer",
          dv.labels = c("Forest proportional habitat use", "Forest habitat proportional habitat use"),
          pred.labels = c("(Intercept)", "F:B Ratio", "Age class (Subadult)", "Pielou's evenness"),
          file = "ForestedSelection_Odocoileus.doc",
          show.obs = FALSE)
