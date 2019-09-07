#----LOAD DATA----
library(assertthat)
library(mediation)
set.seed(7117)

load("/Users/cjleschak/Documents/UCLA/Research/NSHAP Smell/251/NSHAP/Wave1/ICPSR_20541/DS0001/20541-0001-Data-REST.rda")
load("/Users/cjleschak/Documents/UCLA/Research/NSHAP Smell/251/NSHAP/Wave2/ICPSR_34921/DS0002-Death/34921-0002-Data-REST.rda")
NSHAP_Wave1 <- da20541.0001 #Wave 1 data
NSHAP_Wave2 <- da34921.0002 #Wave 2 5-year mortality data
rm(da20541.0001)
rm(da34921.0002)

#Merge W1 and W2 files together
NSHAP_data <- merge(NSHAP_Wave1,NSHAP_Wave2,by="CASEID")

#----Create non-factor variables------

NSHAP_data$DECEASED_num <- as.numeric(NSHAP_data$DECEASED)-1

#Social Network Size Variables
NSHAP_data$FRAMT_num <- as.numeric(NSHAP_data$FRAMT)-1
NSHAP_data$CLSREL_num <- as.numeric(NSHAP_data$CLSREL)-1

#Physical Closeness Variables
NSHAP_data$TOUCHPET_num <- as.numeric(NSHAP_data$TOUCHPET)-1
NSHAP_data$EMBRACE_num <- as.numeric(NSHAP_data$EMBRACE)-1
NSHAP_data$PLAYCHLD_num <- as.numeric(NSHAP_data$PLAYCHLD)-1
NSHAP_data$HUGPTNR_num <- as.numeric(NSHAP_data$HUGPTNR)-1
NSHAP_data$HUGHOLD_num <- as.numeric(NSHAP_data$HUGHOLD)-1

NSHAP_data$SOCIAL_num <- as.numeric(NSHAP_data$SOCIAL)-1

#Emotional Closeness Variables
NSHAP_data$SPOPEN_num <- as.numeric(NSHAP_data$SPOPEN)-1
NSHAP_data$SPRELY_num <- as.numeric(NSHAP_data$SPRELY)-1
NSHAP_data$SPDEMAND_num <- as.numeric(NSHAP_data$SPDEMAND)-1
NSHAP_data$SPCRITZE_num <- as.numeric(NSHAP_data$SPCRITZE)-1
NSHAP_data$FAMOPEN_num <- as.numeric(NSHAP_data$FAMOPEN)-1
NSHAP_data$FAMRELY_num <- as.numeric(NSHAP_data$FAMRELY)-1
NSHAP_data$FAMDEMAN_num <- as.numeric(NSHAP_data$FAMDEMAN)-1
NSHAP_data$FAMCRITZ_num <- as.numeric(NSHAP_data$FAMCRITZ)-1
NSHAP_data$FROPEN_num <- as.numeric(NSHAP_data$FROPEN)-1
NSHAP_data$FRRELY_num <- as.numeric(NSHAP_data$FRRELY)-1
NSHAP_data$FRDEMN_num <- as.numeric(NSHAP_data$FRDEMN)-1
NSHAP_data$FRCRITZ_num <- as.numeric(NSHAP_data$FRCRITZ)-1

NSHAP_data$COMPANION_num <- as.numeric(NSHAP_data$COMPANION)-1
NSHAP_data$LEFTOUT_num <- as.numeric(NSHAP_data$LEFTOUT)-1
NSHAP_data$ISOLATED_num <- as.numeric(NSHAP_data$ISOLATED)-1

##Covariates
#Age already non-factor
NSHAP_data$GENDER_num <- as.numeric(NSHAP_data$GENDER)-1 #male=0, female=1
NSHAP_data$GENDER_numR <- 1-as.numeric(NSHAP_data$GENDER_num) #female=0, male=1
NSHAP_data$DEGREE_CODED_num <- as.numeric(NSHAP_data$DEGREE_CODED)-1
NSHAP_data$HRTATK_num <- as.numeric(NSHAP_data$HRTPROB)-1 #0=no, 1=yes
NSHAP_data$HRTFAIL_num <- as.numeric(NSHAP_data$HRTFAIL)-1 #0=no, 1=yes
NSHAP_data$STROKE_num <- as.numeric(NSHAP_data$CONDITNS_5)-1 #0=no, 1=yes
NSHAP_data$DIABETES_num <- as.numeric(NSHAP_data$CONDITNS_7)-1 #0=no, 1=yes
NSHAP_data$HYPERT_num <- as.numeric(NSHAP_data$CONDITNS_6)-1 #0=no, 1=yes
NSHAP_data$COPD_num <- as.numeric(NSHAP_data$CONDITNS_3)-1 #0=no, 1=yes
NSHAP_data$LIVERDAM_num <- as.numeric(NSHAP_data$CONDITNS_9)-1 #0=no, 1=yes
NSHAP_data$CANCER_num <- as.numeric(NSHAP_data$CONDITNS_14)-1 #0=no, 1=yes, excluding skin cancer (asked seprately)
#Create dummy variables for ethnicity
NSHAP_data$ethgrp_d1 <- ifelse(as.character(NSHAP_data$ETHGRP) == "(2) black", c(1), c(0))
NSHAP_data$ethgrp_d2 <- ifelse(as.character(NSHAP_data$ETHGRP) == "(3) hispanic, non-black", c(1), c(0))
NSHAP_data$ethgrp_d3 <- ifelse(as.character(NSHAP_data$ETHGRP) == "(4) other", c(1), c(0))

#----Code olfactory test items as correct/incorrect------

#Create dichotomous variable for each pen of the smell test - correct (1)/incorrect (0)
#-1 = refused, -2 = don't know -> both coded as incorrect (following past work). 
#Otherwise (e.g. -6 = missing in error & -3 = not applicable) reated as true missing.

#case ID #s with -6 or -3 (as coded in SPSS file). To be replaced with NA when computing BPX_corr variables.
BP_trueMissing <- c(757, 2237, 41, 74, 75, 86, 175, 181, 183, 200, 381, 386, 551, 567, 612, 755, 796, 804, 864, 
                    921, 941, 946, 971, 1034, 1043, 1149, 1261, 1283, 1328, 1336, 1503, 1516, 1576, 1649, 1675, 
                    1727, 1731, 1805, 1821, 1825, 1847, 1883, 1997, 2050, 2134, 2160, 2259, 2390, 2410, 2424, 
                    2426, 2444, 2466, 2511, 2563, 2571, 2597, 2788, 2826, 2831, 2842, 2858, 2874, 2884, 2976)

# Incorrect (coded as 0) for all values other than the correct answer (including missing [to be replaced next]).
NSHAP_data$BP1_corr <- ifelse(as.character(NSHAP_data$BLUEPEN_1) != "(3) rose" | is.na(NSHAP_data$BLUEPEN_1), c(0), c(1))
NSHAP_data$BP2_corr <- ifelse(as.character(NSHAP_data$BLUEPEN_2) != "(3) leather" | is.na(NSHAP_data$BLUEPEN_2), c(0), c(1))
NSHAP_data$BP3_corr <- ifelse(as.character(NSHAP_data$BLUEPEN_3) != "(1) orange" | is.na(NSHAP_data$BLUEPEN_3), c(0), c(1))
NSHAP_data$BP4_corr <- ifelse(as.character(NSHAP_data$BLUEPEN_4) != "(2) fish" | is.na(NSHAP_data$BLUEPEN_4), c(0), c(1))
NSHAP_data$BP5_corr <- ifelse(as.character(NSHAP_data$BLUEPEN_5) != "(2) peppermint" | is.na(NSHAP_data$BLUEPEN_5), c(0), c(1))

#Recode values coded as incorrect in above as NA if actually true missing (rather than 'refused' or 'don't know')
for (i in 1:3005) {
  if (is.na(NSHAP_data$BLUEPEN_1[i]) & is.element(NSHAP_data$CASEID[i],BP_trueMissing)) {
    NSHAP_data$BP1_corr[i] <- NA
  }
  if (is.na(NSHAP_data$BLUEPEN_2[i]) & is.element(NSHAP_data$CASEID[i],BP_trueMissing)) {
    NSHAP_data$BP2_corr[i] <- NA
  }
  if (is.na(NSHAP_data$BLUEPEN_3[i]) & is.element(NSHAP_data$CASEID[i],BP_trueMissing)) {
    NSHAP_data$BP3_corr[i] <- NA
  }
  if (is.na(NSHAP_data$BLUEPEN_4[i]) & is.element(NSHAP_data$CASEID[i],BP_trueMissing)) {
    NSHAP_data$BP4_corr[i] <- NA
  }
  if (is.na(NSHAP_data$BLUEPEN_5[i]) & is.element(NSHAP_data$CASEID[i],BP_trueMissing)) {
    NSHAP_data$BP5_corr[i] <- NA
  }}

#----CREATE (NON-Z) COMPOSITES----
#--OLFACTORY DYSFUNCTION--#
#Create composite variable of # of incorrect answers on smell test
NSHAP_data$olf_dys <- 5-(NSHAP_data$BP1_corr + NSHAP_data$BP2_corr + NSHAP_data$BP3_corr + 
                           NSHAP_data$BP4_corr + NSHAP_data$BP5_corr)

#--PHYSICAL CLOSENESS--#
#Create average of frequency of physical contact scale (5 items)
phys_cols <- c(which(colnames(NSHAP_data)=="TOUCHPET_num"),which(colnames(NSHAP_data)=="EMBRACE_num"),
               which(colnames(NSHAP_data)=="PLAYCHLD_num"),which(colnames(NSHAP_data)=="HUGPTNR_num"),
               which(colnames(NSHAP_data)=="HUGHOLD_num"))
NSHAP_data$freq_contact <- rowMeans(NSHAP_data[,phys_cols], na.rm=TRUE)

#--EMOTIONAL CLOSENESS--#
#Reverse score negative items (6 items)
NSHAP_data$SPDEMAND_numR <- 2-NSHAP_data$SPDEMAND_num
NSHAP_data$SPCRITZE_numR <- 2-NSHAP_data$SPCRITZE_num
NSHAP_data$FAMDEMAN_numR <- 2-NSHAP_data$FAMDEMAN_num
NSHAP_data$FAMCRITZ_numR <- 2-NSHAP_data$FAMCRITZ_num
NSHAP_data$FRDEMN_numR <- 2-NSHAP_data$FRDEMN_num
NSHAP_data$FRCRITZ_numR <- 2-NSHAP_data$FRCRITZ_num

#create average of social support from partner/spouse (if applicable), family, friends (12 items)
support_cols <- c(which(colnames(NSHAP_data)=="SPOPEN_num"),which(colnames(NSHAP_data)=="SPRELY_num"),
                  which(colnames(NSHAP_data)=="SPDEMAND_numR"),which(colnames(NSHAP_data)=="SPCRITZE_numR"),
                  which(colnames(NSHAP_data)=="FAMOPEN_num"),which(colnames(NSHAP_data)=="FAMRELY_num"),
                  which(colnames(NSHAP_data)=="FAMDEMAN_numR"),which(colnames(NSHAP_data)=="FAMCRITZ_numR"),
                  which(colnames(NSHAP_data)=="FROPEN_num"),which(colnames(NSHAP_data)=="FRRELY_num"),
                  which(colnames(NSHAP_data)=="FRDEMN_numR"),which(colnames(NSHAP_data)=="FRCRITZ_numR"))
NSHAP_data$support <- rowMeans(NSHAP_data[,support_cols], na.rm=TRUE)

#Create average of UCLA loneliness items (3 items)
lonely_cols <- c(which(colnames(NSHAP_data)=="COMPANION_num"),which(colnames(NSHAP_data)=="LEFTOUT_num"), 
                 which(colnames(NSHAP_data)=="ISOLATED_num"))
NSHAP_data$lonely <- rowMeans(NSHAP_data[,lonely_cols], na.rm=TRUE)

#----Create reduced dataset----
#Check who has full data on all variables
NSHAP_data$fullDataFilter <- ifelse(is.na(NSHAP_data$DECEASED_num) | is.na(NSHAP_data$olf_dys)      #Main outcome & main predictor
                                    | is.na(NSHAP_data$CLSREL_num) | is.na(NSHAP_data$FRAMT_num)    #Social network size variables
                                    | is.na(NSHAP_data$support) | is.na(NSHAP_data$lonely)          #Emotional closeness variables
                                    | is.na(NSHAP_data$freq_contact) | is.na(NSHAP_data$SOCIAL_num) #Physical closeness variables
                                    | is.na(NSHAP_data$GENDER_num)                                  #Proposed moderator
                                    | is.na(NSHAP_data$ethgrp_d1) | is.na(NSHAP_data$ethgrp_d2) | is.na(NSHAP_data$ethgrp_d3)   #covariates
                                    | is.na(NSHAP_data$DEGREE_CODED_num) | is.na(NSHAP_data$HRTATK_num)
                                    | is.na(NSHAP_data$HRTFAIL_num) | is.na(NSHAP_data$STROKE_num)
                                    | is.na(NSHAP_data$DIABETES_num) | is.na(NSHAP_data$HYPERT_num)
                                    | is.na(NSHAP_data$COPD_num) | is.na(NSHAP_data$LIVERDAM_num)
                                    | is.na(NSHAP_data$CANCER_num),c(0), c(1))

#Final Sample size = 2264
NSHAP_fullData <- NSHAP_data[ which(NSHAP_data$fullDataFilter==1), ]
nrow(NSHAP_fullData)


#----Z-TRANSFORM & CREATE FINAL COMPOSITES----

#--SOCIAL NETWORK SIZE--#
#Z-transform the Social Network Size variables
NSHAP_fullData$z_FRAMT<-scale(NSHAP_fullData$FRAMT_num, center = TRUE, scale = TRUE)
NSHAP_fullData$z_CLSREL<-scale(NSHAP_fullData$CLSREL_num, center = TRUE, scale = TRUE)

#Create composite variable of z-transformed social network size variables
#Must have z-score for both (z_FRAMT & z_CLSREL) to be included in computation.
NSHAP_fullData$network_size <- NSHAP_fullData$z_FRAMT + NSHAP_fullData$z_CLSREL


#--PHYSICAL CLOSENESS--#
#Z-transform the physical closeness variables
NSHAP_fullData$z_freq_contact <- scale(NSHAP_fullData$freq_contact, center = TRUE, scale = TRUE)
NSHAP_fullData$z_inperson_soc <- scale(NSHAP_fullData$SOCIAL_num, center = TRUE, scale = TRUE)

#Create composite variable of z-transformed social network size variables
#Must have z-score for both (z_freq_contact & z_inperson_soc) to be included in computation.
NSHAP_fullData$phys_closeness <- NSHAP_fullData$z_freq_contact + NSHAP_fullData$z_inperson_soc


#--EMOTIONAL CLOSENESS--#
#Z-transform the emotional closeness variables
NSHAP_fullData$z_support <- scale(NSHAP_fullData$support, center = TRUE, scale = TRUE) #z-transform the variable.
NSHAP_fullData$z_lonely <- scale(NSHAP_fullData$lonely, center = TRUE, scale = TRUE) #z-transform the variable.

#Create composite variable of emotional closeness variables
#Must have z-score for both (z_lonely & z_support) to be included in computation.
NSHAP_fullData$emo_closeness <- NSHAP_fullData$z_support + NSHAP_fullData$z_lonely



#----Social Network Size as a Mediator, with Gender as a moderator----

  #a-path, predicting network size from olfactory dysfunction (interacting with gender)
model.m.network_size <- lm(network_size ~ GENDER_num*olf_dys + AGE + DEGREE_CODED_num + ethgrp_d1 + ethgrp_d2 + ethgrp_d3 + HRTATK_num + HRTFAIL_num + STROKE_num + DIABETES_num + HYPERT_num + COPD_num + LIVERDAM_num + CANCER_num, data=NSHAP_fullData)
model.m.network_size_R <- lm(network_size ~ GENDER_numR*olf_dys + AGE + DEGREE_CODED_num + ethgrp_d1 + ethgrp_d2 + ethgrp_d3 + HRTATK_num + HRTFAIL_num + STROKE_num + DIABETES_num + HYPERT_num + COPD_num + LIVERDAM_num + CANCER_num, data=NSHAP_fullData)

  #b-path, predicting 5-year mortality from network size
model.y.network_size <- glm(DECEASED_num ~ network_size + GENDER_num + olf_dys + AGE + DEGREE_CODED_num + ethgrp_d1 + ethgrp_d2 + ethgrp_d3 + HRTATK_num + HRTFAIL_num + STROKE_num + DIABETES_num + HYPERT_num + COPD_num + LIVERDAM_num + CANCER_num, family="binomial", data=NSHAP_fullData)

  #calculating indirect effect for males/females separately
med.eff.network_size.male <- mediate(model.m.network_size, model.y.network_size, treat="olf_dys", mediator="network_size", covariates = list(GENDER_num = 0), sims = 10000)
med.eff.network_size.female <- mediate(model.m.network_size, model.y.network_size, treat="olf_dys", mediator="network_size", covariates = list(GENDER_num = 1), sims = 10000)

  #calculating average indirect effect (across males/females)
med.eff.network_size <- mediate(model.m.network_size, model.y.network_size, treat="olf_dys", mediator="network_size", sims = 10000)

  #testing difference of indirect effect for males/females
test.modmed(med.eff.network_size, covariates.1 = list(GENDER_num=0), covariates.2 = list(GENDER_num=1), sims=10000)

summary(model.m.network_size) #a path coefficients (where olf_dys coeff. = a path for males)
summary(model.m.network_size_R) #a path coefficients (where olf_dys coeff. = a path for females)
summary(model.y.network_size) #b path coefficients
summary(med.eff.network_size.male) #Is the indirect effect significant for males?
summary(med.eff.network_size.female) #Is the indirect effect signfiicant for females?
summary(med.eff.network_size) #Average indirect effect (collapsing across males/females)

#----Physical Closeness as a Mediator, with Gender as a moderator----

  #a-path, predicting physical closeness from olfactory dysfunction (interacting with gender)
model.m.phys_closeness <- lm(phys_closeness ~ GENDER_num*olf_dys + AGE + DEGREE_CODED_num + ethgrp_d1 + ethgrp_d2 + ethgrp_d3 + HRTATK_num + HRTFAIL_num + STROKE_num + DIABETES_num + HYPERT_num + COPD_num + LIVERDAM_num + CANCER_num, data=NSHAP_fullData)
model.m.phys_closeness_R <- lm(phys_closeness ~ GENDER_numR*olf_dys + AGE + DEGREE_CODED_num + ethgrp_d1 + ethgrp_d2 + ethgrp_d3 + HRTATK_num + HRTFAIL_num + STROKE_num + DIABETES_num + HYPERT_num + COPD_num + LIVERDAM_num + CANCER_num, data=NSHAP_fullData)

  #b-path, predicting 5-year mortality from physical closeness
model.y.phys_closeness <- glm(DECEASED_num ~ phys_closeness + GENDER_num + olf_dys + AGE + DEGREE_CODED_num + ethgrp_d1 + ethgrp_d2 + ethgrp_d3 + HRTATK_num + HRTFAIL_num + STROKE_num + DIABETES_num + HYPERT_num + COPD_num + LIVERDAM_num + CANCER_num, family="binomial", data=NSHAP_fullData)

  #calculating indirect effect for males/females separately
med.eff.phys_closeness.male <- mediate(model.m.phys_closeness, model.y.phys_closeness, treat="olf_dys", mediator="phys_closeness", covariates = list(GENDER_num = 0), sims = 10000)
med.eff.phys_closeness.female <- mediate(model.m.phys_closeness, model.y.phys_closeness, treat="olf_dys", mediator="phys_closeness", covariates = list(GENDER_num = 1), sims = 10000)

  #calculating average indirect effect (across males/females)
med.eff.phys_closeness <- mediate(model.m.phys_closeness, model.y.phys_closeness, treat="olf_dys", mediator="phys_closeness", sims = 10000)

  #testing difference of indirect effect for males/females
test.modmed(med.eff.phys_closeness, covariates.1 = list(GENDER_num=0), covariates.2 = list(GENDER_num=1), sims=10000)

summary(model.m.phys_closeness) #a path coefficients (where olf_dys coeff. shows a path for males)
summary(model.m.phys_closeness_R) #a path coefficients (where olf_dys coeff. shows a path for females)
summary(model.y.phys_closeness) #b path coefficients
summary(med.eff.phys_closeness.male) #Is the indirect effect significant for males?
summary(med.eff.phys_closeness.female) #Is the indirect effect signfiicant for females?
summary(med.eff.phys_closeness) #Average indirect effect (collapsing across males/females)


#----Emotional Closeness as a Mediator, with Gender as a moderator----
  #a-path, predicting emotional closeness from olfactory dysfunction (interacting with gender)
model.m.emo_closeness <- lm(emo_closeness ~ GENDER_num*olf_dys + AGE + DEGREE_CODED_num + ethgrp_d1 + ethgrp_d2 + ethgrp_d3 + HRTATK_num + HRTFAIL_num + STROKE_num + DIABETES_num + HYPERT_num + COPD_num + LIVERDAM_num + CANCER_num, data=NSHAP_fullData)
model.m.emo_closeness_R <- lm(emo_closeness ~ GENDER_numR*olf_dys + AGE + DEGREE_CODED_num + ethgrp_d1 + ethgrp_d2 + ethgrp_d3 + HRTATK_num + HRTFAIL_num + STROKE_num + DIABETES_num + HYPERT_num + COPD_num + LIVERDAM_num + CANCER_num, data=NSHAP_fullData)

  #b-path, predicting 5-year mortality from emotional closeness
model.y.emo_closeness <- glm(DECEASED_num ~ emo_closeness + GENDER_num + olf_dys + AGE + DEGREE_CODED_num + ethgrp_d1 + ethgrp_d2 + ethgrp_d3 + HRTATK_num + HRTFAIL_num + STROKE_num + DIABETES_num + HYPERT_num + COPD_num + LIVERDAM_num + CANCER_num, family="binomial", data=NSHAP_fullData)

  #calculating indirect effect for males/females separately
med.eff.emo_closeness.male <- mediate(model.m.emo_closeness, model.y.emo_closeness, treat="olf_dys", mediator="emo_closeness", covariates = list(GENDER_num = 0), sims = 10000)
med.eff.emo_closeness.female <- mediate(model.m.emo_closeness, model.y.emo_closeness, treat="olf_dys", mediator="emo_closeness", covariates = list(GENDER_num = 1), sims = 10000)

  #calculating average indirect effect (across males/females)
med.eff.emo_closeness <- mediate(model.m.emo_closeness, model.y.emo_closeness, treat="olf_dys", mediator="emo_closeness", sims = 10000)

  #testing difference of indirect effect for males/females
test.modmed(med.eff.emo_closeness, covariates.1 = list(GENDER_num=0), covariates.2 = list(GENDER_num=1), sims=10000)

summary(model.m.emo_closeness) #a path coefficients (where olf_dys coeff. shows a path for males)
summary(model.m.emo_closeness_R) #a path coefficients (where olf_dys coeff. shows a path for females)
summary(model.y.emo_closeness) #b path coefficients
summary(med.eff.emo_closeness.male) #Is the indirect effect significant for males?
summary(med.eff.emo_closeness.female) #Is the indirect effect signfiicant for females?
summary(med.eff.emo_closeness) #Average indirect effect (collapsing across males/females)


#----Emotional Closeness as a Mediator (no moderator)----
  #a-path, predicting emotional closeness from olfactory dysfunction
model.m.emo_closeness_noMod <- lm(emo_closeness ~ olf_dys + GENDER_num + AGE + DEGREE_CODED_num + ethgrp_d1 + ethgrp_d2 + ethgrp_d3 + HRTATK_num + HRTFAIL_num + STROKE_num + DIABETES_num + HYPERT_num + COPD_num + LIVERDAM_num + CANCER_num, data=NSHAP_fullData)

  #b-path, predicting 5-year mortality from emotional closeness
model.y.emo_closeness_noMod <- glm(DECEASED_num ~ emo_closeness + GENDER_num + olf_dys + AGE + DEGREE_CODED_num + ethgrp_d1 + ethgrp_d2 + ethgrp_d3 + HRTATK_num + HRTFAIL_num + STROKE_num + DIABETES_num + HYPERT_num + COPD_num + LIVERDAM_num + CANCER_num, family="binomial", data=NSHAP_fullData)

  #calculating average indirect effect (across males/females)
med.eff.emo_closeness_noMod <- mediate(model.m.emo_closeness_noMod, model.y.emo_closeness_noMod, treat="olf_dys", mediator="emo_closeness", sims = 10000)

summary(model.m.emo_closeness_noMod) #a path coefficients
summary(model.y.emo_closeness_noMod) #b path coefficients
summary(med.eff.emo_closeness_noMod) #Average indirect effect (collapsing across males/females)

#----Physical Closeness as a Mediator, with Gender as a moderator, controlling for network size----
  #a-path, predicting physical closeness from olfactory dysfunction (interacting with gender), controlling for network size
model.m.phys_closeness2 <- lm(phys_closeness ~ GENDER_num*olf_dys + network_size + AGE + DEGREE_CODED_num + ethgrp_d1 + ethgrp_d2 + ethgrp_d3 + HRTATK_num + HRTFAIL_num + STROKE_num + DIABETES_num + HYPERT_num + COPD_num + LIVERDAM_num + CANCER_num, data=NSHAP_fullData)
model.m.phys_closeness2_R <- lm(phys_closeness ~ GENDER_numR*olf_dys + network_size + AGE + DEGREE_CODED_num + ethgrp_d1 + ethgrp_d2 + ethgrp_d3 + HRTATK_num + HRTFAIL_num + STROKE_num + DIABETES_num + HYPERT_num + COPD_num + LIVERDAM_num + CANCER_num, data=NSHAP_fullData)

  #b-path, predicting 5-year mortality from physical closeness, controlling for network size
model.y.phys_closeness2 <- glm(DECEASED_num ~ phys_closeness + network_size + GENDER_num + olf_dys + AGE + DEGREE_CODED_num + ethgrp_d1 + ethgrp_d2 + ethgrp_d3 + HRTATK_num + HRTFAIL_num + STROKE_num + DIABETES_num + HYPERT_num + COPD_num + LIVERDAM_num + CANCER_num, family="binomial", data=NSHAP_fullData)

  #calculating indirect effect for males/females separately
med.eff.phys_closeness2.male <- mediate(model.m.phys_closeness2, model.y.phys_closeness2, treat="olf_dys", mediator="phys_closeness", covariates = list(GENDER_num = 0), sims = 10000)
med.eff.phys_closeness2.female <- mediate(model.m.phys_closeness2, model.y.phys_closeness2, treat="olf_dys", mediator="phys_closeness", covariates = list(GENDER_num = 1), sims = 10000)

  #calculating average indirect effect (across males/females)
med.eff.phys_closeness2 <- mediate(model.m.phys_closeness2, model.y.phys_closeness2, treat="olf_dys", mediator="phys_closeness", sims = 10000)

  #testing difference of indirect effect for males/females
test.modmed(med.eff.phys_closeness2, covariates.1 = list(GENDER_num=0), covariates.2 = list(GENDER_num=1), sims=10000)

summary(model.m.phys_closeness2) #a path coefficients (where olf_dys coeff. shows a path for males)
summary(model.m.phys_closeness2_R) #a path coefficients (where olf_dys coeff. shows a path for females)
summary(model.y.phys_closeness2) #b path coefficients
summary(med.eff.phys_closeness2.male) #Is the indirect effect significant for males?
summary(med.eff.phys_closeness2.female) #Is the indirect effect signfiicant for females?
summary(med.eff.phys_closeness2) #Average indirect effect (collapsing across males/females)


#----Social Network Size as a Mediator, with Gender as a moderator, controlling for physical closeness----

  #a-path, predicting network size from olfactory dysfunction (interacting with gender), controlling for phys. closeness
model.m.network_size2 <- lm(network_size ~ GENDER_num*olf_dys + phys_closeness + AGE + DEGREE_CODED_num + ethgrp_d1 + ethgrp_d2 + ethgrp_d3 + HRTATK_num + HRTFAIL_num + STROKE_num + DIABETES_num + HYPERT_num + COPD_num + LIVERDAM_num + CANCER_num, data=NSHAP_fullData)
model.m.network_size2_R <- lm(network_size ~ GENDER_numR*olf_dys + phys_closeness + AGE + DEGREE_CODED_num + ethgrp_d1 + ethgrp_d2 + ethgrp_d3 + HRTATK_num + HRTFAIL_num + STROKE_num + DIABETES_num + HYPERT_num + COPD_num + LIVERDAM_num + CANCER_num, data=NSHAP_fullData)

  #b-path, predicting 5-year mortality from network size, controlling for phys. closeness
model.y.network_size2 <- glm(DECEASED_num ~ network_size + phys_closeness + GENDER_num + olf_dys + AGE + DEGREE_CODED_num + ethgrp_d1 + ethgrp_d2 + ethgrp_d3 + HRTATK_num + HRTFAIL_num + STROKE_num + DIABETES_num + HYPERT_num + COPD_num + LIVERDAM_num + CANCER_num, family="binomial", data=NSHAP_fullData)

  #calculating indirect effect for males/females separately
med.eff.network_size2.male <- mediate(model.m.network_size2, model.y.network_size2, treat="olf_dys", mediator="network_size", covariates = list(GENDER_num = 0), sims = 10000)
med.eff.network_size2.female <- mediate(model.m.network_size2, model.y.network_size2, treat="olf_dys", mediator="network_size", covariates = list(GENDER_num = 1), sims = 10000)

  #calculating average indirect effect (across males/females)
med.eff.network_size2 <- mediate(model.m.network_size2, model.y.network_size2, treat="olf_dys", mediator="network_size", sims = 10000)

#testing difference of indirect effect for males/females
test.modmed(med.eff.network_size2, covariates.1 = list(GENDER_num=0), covariates.2 = list(GENDER_num=1), sims=10000)

summary(model.m.network_size2) #a path coefficients (where olf_dys coeff. shows a path for males)
summary(model.m.network_size2_R) #a path coefficients (where olf_dys coeff. shows a path for females)
summary(model.y.network_size2) #b path coefficients
summary(med.eff.network_size2.male) #Is the indirect effect significant for males?
summary(med.eff.network_size2.female) #Is the indirect effect signfiicant for females?
summary(med.eff.network_size2) #Average indirect effect (collapsing across males/females)
