### Script 0-4 : Analyzing General Social Survey (GSS) using R Language ####
### Objective : fiting svyglm for any dependent varible selected in the script 0-1A ###

############################################################
# Set working directory, where gss data, scripts, outputs will be stored.
setwd("C:/Users/gagranis/Documents/R/gss/tutorialGSS")

# Load shortGSS.rda dataset.
load("C:/Users/gagranis/Documents/R/gss/tutorialGSS/shortGSS.rda")

# Download packages.
library("survey")
library("plyr")
library("scales")
library("reshape2")
library("ggplot2")

## Calculate the compwt and samplerc variables to match SDA specifications
############################################################
shortGSS <- transform( 
  shortGSS, 
  compwt =  oversamp  *  formwt * wtssall, 
  samplerc = 
    ifelse(sample %in% 3:4 , 3 , 
           ifelse( sample %in% 6:7 , 6 , 
                   sample ) )
)

# Specify a complex survey design. It combines a data frame and all the survey design information needed to analyse it, which is used for survey modelling and summary functions. Type ?svydesign for detail.
gss.designshort <- 
  svydesign( 
    ~sampcode, 
    strata = ~samplerc, 
    data = subset(shortGSS, !is.na(sampcode)), 
    weights = ~compwt, 
    nest = TRUE
  )

# Svyglm fits a generalised linear model from a complex survey design. Type ?svyglm for details.
depvarmodel<-svyglm(depvar_g~ factor(marital_g)+factor(age_g)+factor(degree_g)+factor(sex_g)+factor(race_g)+factor(region_g)+factor(partyid_g)+factor(polviews_g)+factor(attend_g)+factor(realinc_g)+factor(attend_g)+factor(pres_g), design = gss.designshort, family=quasibinomial(link = "logit"))

# Create a data frame from summary statistics output of depvarmodel.
depvarcoef<-as.data.frame(summary(depvarmodel)$coef)

# Created a list of factors to replace the dependent varible from the svyglm output.

laboc<-c('(Intercept)',  'Marital Status(Widowed)',  'Marital Status(Divorced)',  'Marital Status(Separated)',  'Marital Status(Never Married)',	'Age Group(<35-44yrs.)',	'Age Group(<45-54yrs.)',	'Age Group(<55-64yrs.)',	'Age Group(<65-74yrs.)',	'Age Group(<75yrs.)',	'Education Group(High School)',	'Education Group(College)',	'Sex(Female)',	'Race(Black)',	'Race(Other)',	'Census Region(Middle Atlantic)',	'Census Region(E.N. Central)',	'Census Region(W.N. Centra)',	'Census Region(South Atlantic)',	'Census Region(E. S. Centra)',	'Census Region(W. S. Central)',	'Census Region(Mountain)',	'Census Region(Pacific)',	'Political Party(Republican)',	'Political Party(Others)',	'Political Views(Conservative)',	'Political Views(Moderate)',	'Church Attendance(Sometimes)',	'Church Attendance(Rarely)',	'Income Group(40%)',	'Income Group(60%)',	'Income Group(80%)',	'Income Group(100%)', 'President(Carter)','President(Regan)','President(BushI)','President(Clinton)','President(BushII)','President(Obama)')

depvarcoef1<-cbind(laboc,depvarcoef)
og<-cbind(depvarcoef1[,'se'] <-depvarcoef1[,'Std. Error'])
# Fix to make  income group to plot in a sequence.
depvarcoef1$laboc <- factor(depvarcoef1$laboc, levels=unique(depvarcoef1$laboc))

# Coefficent plot for depvar model output.
depvarcoefPlot<- ggplot(depvarcoef1, aes(x=Estimate, y=laboc))+ geom_point(size=3,colour="blue")+ ggtitle(ttitle)+ geom_errorbarh(aes(xmin=Estimate-se, xmax=Estimate+se), width=.1,colour="blue")+theme(plot.title = element_text(lineheight=1.0, face="bold"))+xlab("Coefficients")+ylab(" ")+geom_vline(xintercept = 0,colour="blue")

# Odds ratios and 95% CI.
odds_depvar<-exp(cbind(OR = coef(depvarmodel), confint(depvarmodel)))

# Logit model average marginal effects. 
############################################################
mean_mareff_depvar <- mean(dlogis(predict(depvarmodel, type = "link"))) 
mareff_depvar<-mean_mareff_depvar * coef(depvarmodel)

mareff_depvar<-as.data.frame(mareff_depvar)
mareff_depvar1<-cbind(laboc,mareff_depvar)
mareff_depvar1$laboc <- factor(mareff_depvar1$laboc, levels=unique(mareff_depvar1$laboc))

# Average marginal effect plot 
mareff_depvarplot<- ggplot(mareff_depvar1, aes(x=mareff_depvar, y=laboc))+ geom_point(size=3,colour="blue")+ ggtitle(ttitle)+theme(plot.title = element_text(lineheight=1.0, face="bold"))+xlab("Coefficients")+ylab(" ")+geom_vline(xintercept = 0,colour="blue")
mareff_depvarplot
