### script 0-5: Analyzing General Social Survey (GSS) using R Language ####
### Objective : fiting stepwise regression models for any dependent varible selected in the script 0-1A  ###

############################################################
# Set working directory, where gss data, scripts, outputs will be stored.
setwd("C:/Users/gagranis/Documents/R/gss/tutorialGSS")

# load shortGSS.rda dataset created in 2.DummyVar_SurveypackGSS.R script.
load("C:/Users/gagranis/Documents/R/gss/tutorialGSS/shortGSS.rda")

############################################################
# Stepwise regression model for the depvar question#
############################################################

# The lowest model is the null model
shortGSS<-(na.omit(shortGSS))
nulldepvarmodel<-glm(depvar_g~ 1, family=binomial(link = "logit"), data=shortGSS)
# The largest model

fulldepvarmodel1<-glm(depvar_g~factor(marital_g)+factor(age_g)+factor(degree_g)+factor(sex_g)+factor(race_g)+factor(region_g)+factor(partyid_g)+factor(polviews_g)+factor(attend_g)+factor(realinc_g)+factor(pres_g),family=binomial(link = "logit"), data = shortGSS)

#Stepwise regression

# 'trace=FALSE' argument prints the final model only.

depvarStep<-step(nulldepvarmodel,scope=list(lower=nulldepvarmodel,upper=fulldepvarmodel1),direction ="both",trace=FALSE)
summary(depvarStep)
anova(depvarStep,test="Chisq")
depvarStep$anova

