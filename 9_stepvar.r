### script 0-9: Analyzing General Social Survey (GSS) using R Language ####
### Objective : Stepwise regressing aftre breaking at each level  ###

############################################################

# Set working directory, where gss data, scripts, outputs will be stored.
setwd("C:/Users/Vijaya/R/gss")
library(reshape2)
library(texreg)
 
# load shortGSS.rda dataset created in 2.DummyVar_SurveypackGSS.R script.
load("C:/Users/Vijaya/R/gss/shortGSS.rda")

############################################################
#for depvar question
############################################################
# Select the interest variables.
dvlist <- c('depvar_g','marital','age_g1',  'degree',  'sex',  'race',  'region',	'partyid',	'polviews',	'attend','realinc_g1','pres_g1')

#  Remove na values and convert all numeric and integer varibles in factors.
shortGSS1<- na.omit(shortGSS[c(dvlist)])
shortGSS1[dvlist] <- lapply(shortGSS1[dvlist], as.factor) 

# For converting levels as column names(library(reshape2))
dv <- melt(as.matrix(shortGSS1[,-1]))
dv1<-dcast(dv, Var1 ~ value, fun.aggregate = length, value.var="value")

colnames(dv1) <- c("Var1",  "month2_3x", "age0","age1","age2","age3","age4","age5", "bachelor","black","BushI","BushII" ,"Carter", "Clinton" ,"conservative",  "divorced","e_nor_central",   "e_sou_central", "every_week", "extremely_liberal", "extrmly_conservative", "female", "Ford", "graduate","high_school",  "ind_near_dem",  "ind_near_rep",  "independent", "junior_college","liberal","lt_high_school","lt_once_a_year","male",   "married", "middle_atlantic",  "moderate","more_thn_once_wk",  "mountain","never",   "never_married","new_england",   "not_str_democrat",  "not_str_republican","nrly_every_week","Obama",   "once_a_month", "once_a_year",   "other",   "other_party",   "pacific", "realinc1","realinc2","realinc3","realinc4","realinc5","Regan", "separated",    "sevrl_times_ayr",   "slghtly_conservative" ,"slightly_liberal",   "south_atlantic","strong_democrat",  "strong_republican" ,"w_nor_central",   "w_sou_central",   "white",   "widowed")

View(dv1)

dfdep<-as.data.frame(shortGSS1$depvar_g)
colnames(dfdep) <- "depvar_g"
dv2<-na.omit(cbind(dfdep,dv1))

nulldepvarmodel<-glm(depvar_g~ 1, family=binomial(link = "logit"), data=dv2)
# The largest model

fulldepvarmodel<-glm(depvar_g~  factor(month2_3x)+ factor(age0)+factor(age1)+factor(age2)+factor(age3)+factor(age4)+factor(age5)+factor(bachelor)+factor(black)+factor(Ford)+ factor(Carter)+factor(Regan)+factor(BushI)+factor(Clinton)+factor(BushII)+factor(Obama)+factor(conservative)+factor(divorced) + factor(e_nor_central)+factor(e_sou_central) +factor(every_week)+factor(extremely_liberal) + factor(extrmly_conservative)+ factor(female) +factor(graduate)  +factor(high_school)+factor(ind_near_dem) +factor(ind_near_rep)+factor(independent) +factor(junior_college)  +  factor(liberal)+factor(lt_high_school) + factor(lt_once_a_year) +   factor(male)+ factor(married)   +  factor(middle_atlantic) +  factor(moderate) +   factor(more_thn_once_wk) + factor(mountain)  +factor(never)+factor(never_married)+ factor(new_england)+ factor(not_str_democrat)  +   factor(not_str_republican)  + factor(nrly_every_week)   +   factor(once_a_month) +   factor(once_a_year)+ factor(other) +  factor(other_party)  +factor(pacific) +factor(realinc1)+factor(realinc2)+factor(realinc3)+factor(realinc4)+factor(realinc5)  +factor(separated) +   factor(sevrl_times_ayr)  +   factor(slghtly_conservative) +factor(slightly_liberal)    + factor(south_atlantic)   +    factor(strong_democrat)+    factor(strong_republican)   + factor(w_nor_central) +factor(w_sou_central) +  factor(white)     +  factor(widowed) ,family=binomial(link = "logit"), data = dv2)

depvarStep<-step(nulldepvarmodel,scope=list(lower=nulldepvarmodel,upper=fulldepvarmodel),direction ="both",trace=FALSE)

depvarsum<-summary(depvarStep)
depvarch<-anova(depvarStep,test="Chisq")
depvaran<-depvarStep$anova
#screenreg(depvarStep)
stargazer(depvarStep, type="text",out="model.txt")
save(depvarStep, file='depvarStep.rda')
