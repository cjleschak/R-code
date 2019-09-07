## ----Required packages----

#install.packages('haven')
#install.packages('lme4')
#install.packages('lmerTest')
#install.packages("emmeans")


## ----Import & prepare data file.----

#Load rating data file
library(haven)
subRating_data <- read_sav("Documents/UCLA/Research/Projects & Datasets/08 SAS/Sick Faces/corrected_longForm_trialBytrialRatings.sav")
subInflamAvg_data <- read_sav("Documents/UCLA/Research/Projects & Datasets/08 SAS/face_longForm_avgRatings_Inflamm.sav")
subInflamAvg_data <- read_sav("Documents/UCLA/Research/Projects & Datasets/08 SAS/Sick Faces/NewFolder/face_wideForm_avgRatings_Inflamm.sav")

detach(package:haven)

#Factor face condition & run
subRating_data$faceCond <- factor(subRating_data$faceCond)
subRating_data$runID <- factor(subRating_data$runID)
subRating_data$subID <- factor(subRating_data$subID)

subInflamAvg_data$faceCond <- factor(subInflamAvg_data$Cond)

install.packages("ggplot2")
library(ggplot2)
install.packages("scater")
library(scater)

plot1=ggplot(subInflamAvg_data, aes(x = lnIL6.T5, y = avgRating))+
  geom_point(aes(color = factor(Cond)))+ geom_smooth(aes(color=factor(Cond)),method = "lm", fill = NA)

plot2=ggplot(subInflamAvg_data, aes(x = lnIL6.T6, y = avgRating))+
  geom_point(aes(color = factor(Cond)))+ geom_smooth(aes(color=factor(Cond)),method = "lm", fill = NA)

multiplot(plot1, plot2, cols=2)

cor(subInflamAvg_data$avgRating,subInflamAvg_data$lnIL6.T5)
plot(subInflamAvg_data$avgRating,subInflamAvg_data$lnIL6.T5)
scatterplot(avgRating ~ lnIL6.T5 | Cond, data=subInflamAvg_data)

## ----Main analyses.----
library(lme4)
library(lmerTest)
library(emmeans)
## ----Hypothesis 1.----

#Fixed effects for faceType & run, random intercept for subject.
rating.FaceByRun <- lmer(rating ~ faceCond*runID + (1|subID), REML=FALSE, data=subRating_data)
rating.FaceAndRun <- lmer(rating ~ faceCond + runID + (1|subID), REML=FALSE, data=subRating_data)
rating.Face <- lmer(rating ~ faceCond + (1|subID), REML=FALSE, data=subRating_data)
rating.Run <- lmer(rating ~ runID + (1|subID), REML=FALSE, data=subRating_data)
summary(rating.FaceByRun)
summary(rating.FaceAndRun)
summary(rating.Face)
summary(rating.Run)

e<-allEffects(rating.FaceByRun)
plot(e)

#Interaction effect significance
anova(rating.FaceAndRun,rating.FaceByRun)
emmeans(rating.FaceAndRun, ~ faceCond|runID)

#Main effect of run significance
anova(rating.FaceAndRun,rating.Face)

#Main effect of face significance
anova(rating.FaceAndRun,rating.Run)
emmeans(rating.FaceAndRun, ~ faceCond)
install.packages("varhandle")
library(varhandle)
## ----Hypothesis 2a (IL-6).----
install.packages("effects")
library(effects)
subInflamAvg_data$time=factor(subInflamAvg_data$time)
subInflamAvg_data<-subInflamAvg_data


summary(rating.IL6)
allEffects
x<-aov(avgRating ~ time*IL6, data=subInflamAvg_data)
x_t0=allEffects(x)
plot(x_t0)

summary(rating.IL6)

newdata$time=unfactor(newdata$time)
rating.IL6 <- aov(avgRating ~ IL6*time +Error(factor(faceID)), data=newdata)
summary(rating.IL6)
eIL6<-allEffects(rating.IL6,xlevels=list(time=seq(0,3,1)))
plot(eIL6)#,main = "Liking as a Function of Sickness Response",xlab="IL-6 (T2-T0)",ylab="Liking Rating",ylim=c(3.5, 4.1))


T0_data <- subInflamAvg_data[ which(subInflamAvg_data$time==0), ]
T1_data <- subInflamAvg_data[ which(subInflamAvg_data$time==1), ]
T2_data <- subInflamAvg_data[ which(subInflamAvg_data$time==2), ]
T3_data <- subInflamAvg_data[ which(subInflamAvg_data$time==3), ]
T4_data <- subInflamAvg_data[ which(subInflamAvg_data$time==4), ]
T5_data <- subInflamAvg_data[ which(subInflamAvg_data$time==5), ]
T6_data <- subInflamAvg_data[ which(subInflamAvg_data$time==6), ]
rating.IL6_t0 <- aov(avgRating ~ IL6, data=T0_data)
rating.IL6_t1 <- aov(avgRating ~ IL6, data=T1_data)
rating.IL6_t2 <- aov(avgRating ~ IL6, data=T2_data)
rating.IL6_t3 <- aov(avgRating ~ IL6, data=T3_data)
rating.IL6_t4 <- aov(avgRating ~ IL6, data=T4_data)
rating.IL6_t5 <- aov(avgRating ~ IL6, data=T5_data)
rating.IL6_t6 <- aov(avgRating ~ IL6, data=T6_data)
summary(rating.IL6_t0)
summary(rating.IL6_t1)
summary(rating.IL6_t2)
summary(rating.IL6_t3)
summary(rating.IL6_t4)
summary(rating.IL6_t5)
summary(rating.IL6_t6)

t0_main<-allEffects(rating.IL6_t0)
t1_main<-allEffects(rating.IL6_t1)
t2_main<-allEffects(rating.IL6_t2)
t3_main<-allEffects(rating.IL6_t3)
t4_main<-allEffects(rating.IL6_t4)
t5_main<-allEffects(rating.IL6_t5)
t6_main<-allEffects(rating.IL6_t6)

plot(t0_main)
plot(t1_main)
plot(t2_main)
plot(t3_main)
plot(t4_main)
plot(t5_main)
plot(t6_main)

eIL6<-allEffects(rating.IL6)
plot(eIL6,main = "Liking as a Function of Sickness-Induced IL-6",xlab="IL-6 at T3",ylab="Liking Rating")

newdata <- subInflamAvg_data[ which(subInflamAvg_data$faceID!=93), ]
       #,main = "Liking as a Function of Sickness Response",xlab="IL-6 (T2-T0)",ylab="Liking Rating",ylim=c(3.5, 4.1))


subInflamAvg_data$time=factor(subInflamAvg_data$time)
emmeans(rating.IL6, ~ time*IL6)

install.packages("phia")
library(phia)

testInteractions(rating.IL6, fixed="time", across="IL6")


rating.IL6 <- lmer(avgRating ~ IL6 +(1|time), REML=FALSE,data=subInflamAvg_data)


rating.IL6 <- lmer(avgRating ~ IL6*time + (1|faceID), data=subInflamAvg_data)

rating.IL6 <- lmer(rating ~ lnIL6.T2_T0*faceCond +(1|subID) (), REML=FALSE, data=newdata)
rating.IL6 <- aov(rating ~ lnIL6.T2_T0*faceCond ,data=subRating_data)
rating.TNF <- lmer(rating ~ lnTNFa.T2_T0 +(1|subID) , REML=FALSE, data=subRating_data)
eIL6<-allEffects(rating.IL6)
plot(eIL6,main = "Liking as a Function of Sickness Response",xlab="IL-6 (T2-T0)",ylab="Liking Rating",ylim=c(3.5, 4.1))
eTNF<-allEffects(rating.TNF)
plot(eTNF,main = "Liking as a Function of Sickness Response",xlab="TNF-a (T2-T0)",ylab="Liking Rating",ylim=c(3.5, 4.1))

rating.IL6AndRun <- lmer(rating ~ lnIL6.T2_T0 + runID + (1|subID), REML=FALSE, data=subRating_data)


rating.IL6 <- lmer(rating ~ lnIL6.T2_T0 + (1|subID), REML=FALSE, data=subRating_data)
rating.Run <- lmer(rating ~ runID + (1|subID), REML=FALSE, data=subRating_data)
rating.IL6ByFace <- lmer(rating ~ lnIL6.T2_T0*faceCond + (1|subID)+  (1|faceID), REML=FALSE, data=subRating_data)
summary(rating.IL6ByFace)
#Interaction effect significance
anova(rating.IL6AndRun,rating.IL6ByRun)
emmeans(rating.IL6ByFace, ~ faceCond)


#Main effect of run significance
anova(rating.IL6AndRun,rating.IL6)

#Main effect of face significance
anova(rating.IL6AndRun,rating.Run)
emmeans(rating.IL6AndRun, ~ Run)

newdata <- subRating_data[ which(subRating_data$faceCond==0), ]
rating.IL6_sick <- lmer(rating ~ lnIL6.T2_T0 + (1|subID), REML=FALSE, data=newdata)
e<-allEffects(rating.IL6_sick)
plot(e)

summary(rating.IL6)
e<-allEffects(rating.IL6)
e<-allEffects(rating.IL6ByFace)
e<-allEffects(rating.Face)
plot(e)
e<-allEffects(rating.TNF)
plot(e)

e <- allEffects(rating.IL6ByFace)

## ----Hypothesis 2b (TNFa).----

rating.TNFByRun <- lmer(rating ~ lnTNFa.T2_T0*runID + (1|subID), REML=FALSE, data=subRating_data)
rating.TNFAndRun <- lmer(rating ~ lnTNFa.T2_T0 + runID + (1|subID), REML=FALSE, data=subRating_data)
rating.TNF <- lmer(rating ~ lnTNFa.T2_T0 + (1|subID), REML=FALSE, data=subRating_data)
rating.Run <- lmer(rating ~ runID + (1|subID), REML=FALSE, data=subRating_data)
rating.TNFByFace <- lmer(rating ~ lnTNFa.T2_T0*faceCond + (1|subID), REML=FALSE, data=subRating_data)

#Interaction effect significance
anova(rating.TNFAndRun,rating.TNFByRun)

#Main effect of run significance
anova(rating.TNFAndRun,rating.TNF)

#Main effect of face significance
anova(rating.TNFAndRun,rating.Run)
emmeans(rating.TNFAndRun, ~ Run)

newdata <- subRating_data[ which(subRating_data$faceCond==1), ]



ratings.FaceAndRunNoface <- lmer(rating ~ faceCond*runID + (1|subID), REML=FALSE, data=subRating_data)
ratings.FaceNoface <- lmer(rating ~ faceCond + (1|subID), REML=FALSE, data=subRating_data)
ratings.FaceWface <- lmer(rating ~ faceCond + (1|subID)+(1|faceID), REML=FALSE, data=subRating_data)
summary(ratings.FaceAndRunNoface)
anova(ratings.FaceAndRunNoface,ratings.FaceAndRunWface)
anova(ratings.FaceAndRunNoface,ratings.FaceNoface)
anova(ratings.FaceWface,ratings.FaceNoface)
summary(ratings.FaceNoface)
#Get estimated marginal means for ratings as a function of face type & run.
emmeans(ratings.FaceAndRunWface, ~ faceCond|runID)

#Model comparison to get main effect Fs.
ratings.INTX <- lmer(rating ~ f +(1|subID) + (1|faceID), REML=FALSE, data=subRating_data)
ratings.INTX <- lmer(rating ~ faceType*run +face+ face_lnIL6. (1|subID) + (1|faceID), REML=FALSE, data=subRating_data)
ratings.Run <- lmer(rating ~ run + (1|subID) + (1|faceID), REML=FALSE, data=subRating_data)
ratings.Face <- lmer(rating ~ faceType + (1|subID) + (1|faceID), REML=FALSE, data=subRating_data)
summary(ratings.INTX)
anova(ratings.FaceAndRun,ratings.Face)
anova(ratings.FaceAndRun,ratings.Run)

ratings.INTX <- lmer(rating ~ *run +(1|subID) + (1|faceID), REML=FALSE, data=subRating_data)
##

model1.runOnly <- lmer(ratings ~ run + (1|subID),REML=FALSE,data=subRating_data)
model1.faceOnly <- lmer(ratings ~ faceType + (1|subID),REML=FALSE,data=subRating_data)
model1.face_run <- lmer(ratings ~ faceType + run + (1|subID),REML=FALSE,data=subRating_data)
summary(model1.runOnly)
summary(model1.faceOnly)
summary(model1.face_run)
anova(model1.runOnly,model1.face_run)
anova(model1.faceOnly,model1.face_run)
anova(model1.face_run,model1.intx)


emmeans(ratings.INTX, ~ faceType)




testModel1 <- lmer(rating ~ lnIL6.T2_T0 + (1|subID)+(1|faceID),REML=FALSE,data=newdata)
testModel1 <- lmer(rating ~ lnIL6.T2_T0 + (1|subID),REML=FALSE,data=newdata)
testModel1 <- lmer(rating ~ faceCond + (1|subID),REML=FALSE,data=newdata)
summary(testModel1)



