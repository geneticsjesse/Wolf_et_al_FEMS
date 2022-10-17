# Authors: Jesse Wolf 
# Purpose: Use Caret to cross validate GLMs
# Input: Oreamnos/odocoileus home range, proportional habitat use, and microbiome data
# Output: RMSE and SI values, best parameters
# Title: Step4_ModelCrossValidation

# Packages and environment preparation
libs <- c('dplyr', 'caret')
lapply(libs, require, character.only = TRUE)

# Read in data
mg_cv<-read_csv("mg_cv.csv")
mg_selection_cv<-read_csv("mg_selection_cv.csv")
wtd_cv<-read_csv("wtd_cv.csv")
wtd_selection_cv<-read_csv("wtd_selection_cv.csv")

# Set seed
set.seed(50)

# Setting up training function for 5 fold cross validation
data_ctrl<- trainControl (method = "cv", number = 5)

# General process is as follows: create model using train function for a given response (core_summer or Selection_***) and predictors
# (either pielou_e or Firm_Bact, and age_class) using the glm method and passing over any NA values
# Model is then printed to view relevant data (e.x. RMSE, number of samples used, etc.) and $finalmodel is accessed to determine
# the best model parameters

#pielou - MG
model_caret_pielouMG <- train (core_summer ~ pielou_e + age_class, 
                               data = mg_cv,
                      trControl= data_ctrl,
                      method="glm",
                      na.action=na.pass)
model_caret_pielouMG
model_caret_pielouMG$finalModel

#FB - MG
model_caret_FBMG<- train(core_summer ~ Firm_Bact+age_class,
                         data=mg_cv,
                         trControl=data_ctrl,
                         method="glm",
                         na.action=na.pass)
model_caret_FBMG
model_caret_FBMG$finalModel

#pielou - WTD
model_caret_pielouWTD <- train (core_summer ~ pielou + age_class, 
                               data = wtd_cv,
                               trControl= data_ctrl,
                               method="glm",
                               na.action=na.pass)
model_caret_pielouWTD
model_caret_pielouWTD$finalModel

#FB - WTD
model_caret_FBWTD <- train (core_summer ~ Firm_Bact + age_class, 
                                data = wtd_cv,
                                trControl= data_ctrl,
                                method="glm",
                                na.action=na.pass)
model_caret_FBWTD
model_caret_FBWTD$finalModel

#MG - esc habitat selection

model_ET_FB<- train(Selection_Esc_Summer50~Firm_Bact+age_class,
                    data=mg_selection_cv,
                    trControl=data_ctrl,
                    method="glm",
                    na.action=na.pass)
model_ET_FB
model_ET_FB$finalModel


model_ET_pielou <- train(Selection_Esc_Summer50~pielou_e+age_class,
                         data=mg_selection_cv,
                         trControl=data_ctrl,
                         method="glm",
                         na.action=na.pass)
model_ET_pielou
model_ET_pielou$finalModel

#MG - treed habitat selection
model_tree_FB<- train(Selection_Treed_Summer50~Firm_Bact+age_class,
                    data=mg_selection_cv,
                    trControl=data_ctrl,
                    method="glm",
                    na.action=na.pass)
model_tree_FB
model_tree_FB$finalModel

model_tree_pielou <- train(Selection_Treed_Summer50~pielou_e+age_class,
                         data=mg_selection_cv,
                         trControl=data_ctrl,
                         method="glm",
                         na.action=na.pass)
model_tree_pielou
model_tree_pielou$finalModel

# MG - HLI habitat selection

model_HLI_FB<- train(Selection_HLI_Summer50~Firm_Bact+age_class,
                      data=mg_selection_cv_HLI,
                      trControl=data_ctrl,
                      method="glm",
                      na.action=na.pass)
model_HLI_FB
model_HLI_FB$finalModel

model_HLI_pielou <- train(Selection_HLI_Summer50~pielou_e+age_class,
                           data=mg_selection_cv_HLI,
                           trControl=data_ctrl,
                           method="glm",
                           na.action=na.pass)
model_HLI_pielou
model_HLI_pielou$finalModel

# WTD - forest selection
model_forest_FB<- train(Selection_Forest_Summer50~Firm_Bact+age_class,
                     data=wtd_selection_cv,
                     trControl=data_ctrl,
                     method="glm",
                     na.action=na.pass)
model_forest_FB
model_forest_FB$finalModel

model_forest_pielou<- train(Selection_Forest_Summer50~pielou+age_class,
                        data=wtd_selection_cv,
                        trControl=data_ctrl,
                        method="glm",
                        na.action=na.pass)
model_forest_pielou
model_forest_pielou$finalModel

# WTD - thicket swamp selection
model_thicket_FB<- train(selection_thicketswamp_summer50~Firm_Bact+age_class,
                        data=wtd_selection_cv,
                        trControl=data_ctrl,
                        method="glm",
                        na.action=na.pass)
model_thicket_FB
model_thicket_FB$finalModel

model_thicket_pielou<- train(selection_thicketswamp_summer50~pielou+age_class,
                            data=wtd_selection_cv,
                            trControl=data_ctrl,
                            method="glm",
                            na.action=na.pass)
model_thicket_pielou
model_thicket_pielou$finalModel

# WTD - treed swamp selection
model_treeswamp_FB<- train(Selection_TreedSwamp_Summer50~Firm_Bact+age_class,
                        data=wtd_selection_cv,
                        trControl=data_ctrl,
                        method="glm",
                        na.action=na.pass)
model_treeswamp_FB
model_treeswamp_FB$finalModel

model_treeswamp_pielou<- train(Selection_TreedSwamp_Summer50~pielou+age_class,
                            data=wtd_selection_cv,
                            trControl=data_ctrl,
                            method="glm",
                            na.action=na.pass)
model_treeswamp_pielou
model_treeswamp_pielou$finalModel