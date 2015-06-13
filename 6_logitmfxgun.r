### Script 0-6a :  Analyzing General Social Survey (GSS) using R Language ####
### Objective : logit model and marginal effect using "mfx" package for the any dependent varible selected in the script 0-1A  ###

############################################################
# Set working directory, where gss data, scripts, outputs will be stored.
setwd("C:/Users/gagranis/Documents/R/gss/tutorialGSS")

# load shortGSS.rda dataset created in 2.DummyVar_SurveypackGSS.R script.
load("C:/Users/gagranis/Documents/R/gss/tutorialGSS/shortGSS.rda")
# Required Packages.
library("ggplot2")
library("scales")
library("gridExtra")
library("mfx")
library("stargazer")
library("dplyr")
library("ggvis")
############################################################
# depvar question#
############################################################

depvarlogitmfx<-logitmfx(depvar_g~ factor(marital_g)+factor(age_g)+factor(degree_g)+factor(sex_g)+factor(race_g)+factor(region_g)+factor(partyid_g)+factor(polviews_g)+factor(attend_g)+factor(realinc_g)+factor(attend_g)+factor(pres_g), data = shortGSS)

summary(depvarlogitmfx)
# The depvarlogitmfx consits of four values (mfxest,fit,dcvar, and call). Model fit is in fit values. generated a depvarmfxfit dtaframe.
depvarmfxfit<-as.data.frame(summary(depvarlogitmfx$fit)$coef)

labmfxfit<-c('Intercept', 'Marital Status(Widowed)',  'Marital Status(Divorced)',  'Marital Status(Separated)',  'Marital Status(Never Married)',  'Age Group(<35-44yrs.)',  'Age Group(<45-54yrs.)',  'Age Group(<55-64yrs.)',  'Age Group(<65-74yrs.)',	'Age Group(<75yrs.)',	'Education Group(High School)',	'Education Group(College)',	'Sex(Female)',	'Race(Black)',	'Race(Other)',	'Census Region(Middle Atlantic)',	'Census Region(E.N. Central)',	'Census Region(W.N. Centra)',	'Census Region(South Atlantic)',	'Census Region(E. S. Centra)',	'Census Region(W. S. Central)',	'Census Region(Mountain)',	'Census Region(Pacific)',	'Political Party(Republican)',	'Political Party(Others)',	'Political Views(Conservative)',	'Political Views(Moderate)',	'Church Attendance(Sometimes)',	'Church Attendance(Rarely)',	'Income Group(40%)',	'Income Group(60%)',	'Income Group(80%)',	'Income Group(100%)', 'President(Carter)','President(Regan)','President(BushI)','President(Clinton)','President(BushII)','President(Obama)')

depvarmfxfit1<-cbind(labmfxfit,depvarmfxfit)
dvf<-cbind(depvarmfxfit1[,'se'] <-depvarmfxfit1[,'Std. Error'])
# fix to make  income group to plot in a sequence.
depvarmfxfit1$labmfxfit <- factor(depvarmfxfit1$labmfxfit, levels=unique(depvarmfxfit1$labmfxfit))

depvarfitPlotmfx<- ggplot(depvarmfxfit1, aes(x=Estimate, y=labmfxfit))+ geom_point(size=2,colour="blue")+ ggtitle(ttitle)+ geom_errorbarh(aes(xmin=Estimate-se, xmax=Estimate+se), width=.5,colour="blue")+theme(plot.title = element_text(lineheight=1.0, face="bold"))+xlab("Coefficients")+ylab(" ")+geom_vline(xintercept = 0,colour="blue")+aes(color=labmfxw)

depvarfitPlotmfx
############################################################
#marginal effect for the depvar qiestion#
############################################################

# The depvarlogitmfx consits of four values (mfxest,fit,dcvar, and call). The average marignal effect is in mfxest values.
#extracted mfxestvalues and coerce as a data frame.
depvarlogitmfxdf<-as.data.frame(depvarlogitmfx$mfxest)

# Created a list of factors to replace the dependent varible from the svyglm output.
labmfx<-c('Widowed','Divorced', 'Separated','Never Married',  '<35-44yrs.',  '<45-54yrs.',	'<55-64yrs.',	'<65-74yrs.',	'<75yrs.',	'High School',	'College',	'Female',	'Black',	'Other',	'Middle Atlantic',	'E.N. Central',	'W.N. Centra',	'South Atlantic',	'E. S. Centra',	'W. S. Central',	'Mountain',	'Pacific',	'Republican',	'Others',	'Conservative',	'Moderate',	'Sometimes',	'Rarely',	'40%',	'60%',	'80%',	'100%', 'Carter','Regan','Bush-I','Clinton','Bush-II','Obama')

labmfxw<-c('Marital Status',  'Marital Status',  'Marital Status',  'Marital Status',  'Age Group',  'Age Group',  'Age Group',	'Age Group',	'Age Group',	'Education Group',	'Education Group',	'Sex',	'Race',	'Race',	'Census Region',	'Census Region',	'Census Region',	'Census Region',	'Census Region',	'Census Region',	'Census Region',	'Census Region',	'Political Party',	'Political Party',	'Political Views',	'Political Views',	'Church Attendance',	'Church Attendance',	'Income Group',	'Income Group',	'Income Group',	'Income Group','President','President','President','President','President','President')

depvarlogitmfxdf<-cbind(labmfx,labmfxw,depvarlogitmfxdf)
dvm<-cbind(depvarlogitmfxdf[,'se'] <-depvarlogitmfxdf[,'Std. Err.'])
dvm1<-cbind(depvarlogitmfxdf[,'Estimate'] <-depvarlogitmfxdf[,'dF/dx'])

# fix to make  income group to plot in a sequence.
depvarlogitmfxdf$labmfx <- factor(depvarlogitmfxdf$labmfx, levels=unique(depvarlogitmfxdf$labmfx))

#Save file.
############################################################
fname<- paste(paste("mfx",tvar,sep="_"),"rda",sep=".")
save(depvarlogitmfxdf,file=fname)

# Average marginal effect plot for depvar model output. 
############################################################
age<-filter(depvarlogitmfxdf,labmfxw == "Age Group")
a<-ggplot(age, aes(y=Estimate, x=labmfx))
a<-a+geom_point(size=2)
a<-a+ggtitle(age$labmfxw)
a<-a+ geom_errorbar(aes(ymin=Estimate-se, ymax=Estimate+se), width=.5)
a<-a+theme(plot.title = element_text(lineheight=1.0, face="bold"))+xlab(" ")
a<-a+ylab(" ")+geom_hline(xintercept = 0,colour="blue")
a<-a+aes(color=labmfxw)+coord_flip()+theme(legend.position="none")+ylim(-.30, .30)  
  
mar<-filter(depvarlogitmfxdf,labmfxw == "Marital Status")
m<-ggplot(mar, aes(y=Estimate, x=labmfx))
m<-m+geom_point(size=2)
m<-m+ggtitle(mar$labmfxw)
m<-m+ geom_errorbar(aes(ymin=Estimate-se, ymax=Estimate+se), width=.5)
m<-m+theme(plot.title = element_text(lineheight=1.0, face="bold"))+xlab(" ")
m<-m+ylab(" ")+geom_hline(xintercept = 0,colour="blue")
m<-m+aes(color=labmfxw)+coord_flip()+theme(legend.position="none")+ylim(-.30, .30) 

sxc<-filter(depvarlogitmfxdf,labmfxw == "Sex")
sx<-ggplot(sxc, aes(y=Estimate, x=labmfx))
sx<-sx+geom_point(size=2)
sx<-sx+ggtitle(sxc$labmfxw)
sx<-sx+ geom_errorbar(aes(ymin=Estimate-se, ymax=Estimate+se), width=.5)
sx<-sx+theme(plot.title = element_text(lineheight=1.0, face="bold"))+xlab(" ")
sx<-sx+ylab(" ")+geom_hline(xintercept = 0,colour="blue")
sx<-sx+aes(color=labmfxw)+coord_flip()+theme(legend.position="none")+ylim(-.30, .30) 

incg<-filter(depvarlogitmfxdf,labmfxw == "Income Group")
i<-ggplot(incg, aes(y=Estimate, x=labmfx))
i<-i+geom_point(size=2)
i<-i+ggtitle(incg$labmfxw)
i<-i+ geom_errorbar(aes(ymin=Estimate-se, ymax=Estimate+se), width=.5)
i<-i+theme(plot.title = element_text(lineheight=1.0, face="bold"))+xlab(" ")
i<-i+ylab(" ")+geom_hline(xintercept = 0,colour="blue")
i<-i+aes(color=labmfxw)+coord_flip()+theme(legend.position="none")+ylim(-.30, .30)

edc<-filter(depvarlogitmfxdf,labmfxw == "Education Group")
ed<-ggplot(edc, aes(y=Estimate, x=labmfx))
ed<-ed+geom_point(size=2)
ed<-ed+ggtitle(edc$labmfxw)
ed<-ed+ geom_errorbar(aes(ymin=Estimate-se, ymax=Estimate+se), width=.5)
ed<-ed+theme(plot.title = element_text(lineheight=1.0, face="bold"))+xlab(" ")
ed<-ed+ylab(" ")+geom_hline(xintercept = 0,colour="blue")
ed<-ed+aes(color=labmfxw)+coord_flip()+theme(legend.position="none")+ylim(-.35, .35) 

rcc<-filter(depvarlogitmfxdf,labmfxw == "Race")
rc<-ggplot(rcc, aes(y=Estimate, x=labmfx))
rc<-rc+geom_point(size=2)
rc<-rc+ggtitle(rcc$labmfxw)
rc<-rc+ geom_errorbar(aes(ymin=Estimate-se, ymax=Estimate+se), width=.5)
rc<-rc+theme(plot.title = element_text(lineheight=1.0, face="bold"))+xlab(" ")
rc<-rc+ylab(" ")+geom_hline(xintercept = 0,colour="blue")
rc<-rc+aes(color=labmfxw)+coord_flip()+theme(legend.position="none")+ylim(-.30, .30)  

ppary<-filter(depvarlogitmfxdf,labmfxw == "Political Party")
pp<-ggplot(ppary, aes(y=Estimate, x=labmfx))
pp<-pp+geom_point(size=2)
pp<-pp+ggtitle(ppary$labmfxw)
pp<-pp+ geom_errorbar(aes(ymin=Estimate-se, ymax=Estimate+se), width=.5)
pp<-pp+theme(plot.title = element_text(lineheight=1.0, face="bold"))+xlab(" ")
pp<-pp+ylab(" ")+geom_hline(xintercept = 0,colour="blue")
pp<-pp+aes(color=labmfxw)+coord_flip()+theme(legend.position="none")+ylim(-.30, .30)  

pviews<-filter(depvarlogitmfxdf,labmfxw == "Political Views")
pv<-ggplot(pviews, aes(y=Estimate, x=labmfx))
pv<-pv+geom_point(size=2)
pv<-pv+ggtitle(pviews$labmfxw)
pv<-pv+ geom_errorbar(aes(ymin=Estimate-se, ymax=Estimate+se), width=.5)
pv<-pv+theme(plot.title = element_text(lineheight=1.0, face="bold"))+xlab(" ")
pv<-pv+ylab(" ")+geom_hline(xintercept = 0,colour="blue")
pv<-pv+aes(color=labmfxw)+coord_flip()+theme(legend.position="none")+ylim(-.30, .30)

presd<-filter(depvarlogitmfxdf,labmfxw == "President")
pd<-ggplot(presd, aes(y=Estimate, x=labmfx))
pd<-pd+geom_point(size=2)
pd<-pd+ggtitle(presd$labmfxw)
pd<-pd+ geom_errorbar(aes(ymin=Estimate-se, ymax=Estimate+se), width=.5)
pd<-pd+theme(plot.title = element_text(lineheight=1.0, face="bold"))+xlab(" ")
pd<-pd+ylab(" ")+geom_hline(xintercept = 0,colour="blue")
pd<-pd+aes(color=labmfxw)+coord_flip()+theme(legend.position="none")+ylim(-.30, .30)  

cac<-filter(depvarlogitmfxdf,labmfxw == "Church Attendance")
ca<-ggplot(cac, aes(y=Estimate, x=labmfx))
ca<-ca+geom_point(size=2)
ca<-ca+ggtitle(cac$labmfxw)
ca<-ca+ geom_errorbar(aes(ymin=Estimate-se, ymax=Estimate+se), width=.5)
ca<-ca+theme(plot.title = element_text(lineheight=1.0, face="bold"))+xlab(" ")
ca<-ca+ylab(" ")+geom_hline(xintercept = 0,colour="blue")
ca<-ca+aes(color=labmfxw)+coord_flip()+theme(legend.position="none")+ylim(-.30, .30) 

crc<-filter(depvarlogitmfxdf,labmfxw == "Census Region")
cr<-ggplot(crc, aes(y=Estimate, x=labmfx))
cr<-cr+geom_point(size=2)
cr<-cr+ggtitle(crc$labmfxw)
cr<-cr+ geom_errorbar(aes(ymin=Estimate-se, ymax=Estimate+se), width=.5)
cr<-cr+theme(plot.title = element_text(lineheight=1.0, face="bold"))+xlab(" ")
cr<-cr+ylab(" ")+geom_hline(xintercept = 0,colour="blue")
cr<-cr+aes(color=labmfxw)+coord_flip()+theme(legend.position="none")+ylim(-.45, .45) 

grid.arrange(a,m,sx,i,ed,rc,pp,pv,pd,ca,cr, ncol=2,main=textGrob(ttitle,gp=gpar(fontsize=20,font=3)))
