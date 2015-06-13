### script0-3:  Analyzing General Social Survey (GSS) using R Language ####
### Objective : finding ratios for any dependent varible selected in the script 0-1A  ###

############################################################
# Set working directory, where gss data, scripts, outputs will be stored.
setwd("C:/Users/gagranis/Documents/R/gss/tutorialGSS")

# Load shortGSS.rda dataset created in 2.DummyVar_SurveypackGSS.R script.
load("C:/Users/gagranis/Documents/R/gss/tutorialGSS/shortGSS.rda")

# Download packages.
library("survey")
library("plyr")
library("scales")
library("reshape2")
library("ggplot2")
library("gridExtra")

############################################################
## Calculate the compwt and samplerc variables to match SDA specifications
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

# Create a list of varibles for the gun question.
var <- c('marital_g',  'age_g',  'degree_g',	'sex_g',	'race_g',	'region_g',	'partyid_g',	'polviews_g',	'attend_g',	'realinc_g')

# Gun ownership question.
############################################################
df=data.frame() 
for(i in 1: length(var)){  
  filename<- paste(paste(tvar,var[i],sep="_"),"csv",sep=".")
  dfr=data.frame()
  dfr<-svyby(
    ~depvar_g,
    as.formula(paste0( "~", var[i])),
    design = gss.designshort,
    svymean,
    na.rm = TRUE
  )
  write.csv(dfr, file = filename)
  
}

#plots
############################################################

rfile<-read.csv(paste0(getwd(),"/",paste(paste(tvar,"marital_g",sep="_"),"csv",sep=".")))

g1<-ggplot(rfile, aes(y= depvar_g, x=marital_g))+ geom_point(size=3)+ geom_errorbar(aes(ymin=depvar_g-se, ymax=depvar_g+se), width=.1)+theme(plot.title = element_text(lineheight=1.0, face="bold"))+xlab("Marital Status")+ylab(tylab)+theme(legend.position="none")

rfile<-read.csv(paste0(getwd(),"/",paste(paste(tvar,"degree_g",sep="_"),"csv",sep=".")))

g2<-ggplot(rfile, aes(y= depvar_g, x=degree_g))+ geom_point(size=3)+  geom_errorbar(aes(ymin=depvar_g-se, ymax=depvar_g+se), width=.1)+theme(plot.title = element_text(lineheight=1.0, face="bold"))+xlab("degree")+ylab(tylab)+theme(legend.position="none")

rfile<-read.csv(paste0(getwd(),"/",paste(paste(tvar,"age_g",sep="_"),"csv",sep=".")))

g3<-ggplot(rfile, aes(y= depvar_g, x=age_g))+ geom_point(size=3)+ geom_errorbar(aes(ymin=depvar_g-se, ymax=depvar_g+se), width=.1)+theme(plot.title = element_text(lineheight=1.0, face="bold"))+xlab("age")+ylab(tylab)+theme(legend.position="none")

rfile<-read.csv(paste0(getwd(),"/",paste(paste(tvar,"sex_g",sep="_"),"csv",sep=".")))

g4<-ggplot(rfile, aes(y= depvar_g, x=sex_g))+ geom_point(size=3)+  geom_errorbar(aes(ymin=depvar_g-se, ymax=depvar_g+se), width=.1)+theme(plot.title = element_text(lineheight=1.0, face="bold"))+xlab("sex")+ylab(tylab)+theme(legend.position="none")
rfile<-read.csv(paste0(getwd(),"/",paste(paste(tvar,"race_g",sep="_"),"csv",sep=".")))

g5<-ggplot(rfile, aes(y= depvar_g, x=race_g))+ geom_point(size=3)+ geom_errorbar(aes(ymin=depvar_g-se, ymax=depvar_g+se), width=.1)+theme(plot.title = element_text(lineheight=1.0, face="bold"))+xlab("race")+ylab(tylab)+theme(legend.position="none")

rfile<-read.csv(paste0(getwd(),"/",paste(paste(tvar,"region_g",sep="_"),"csv",sep=".")))

g6<-ggplot(rfile, aes(y= depvar_g, x=region_g))+ geom_point(size=3)+  geom_errorbar(aes(ymin=depvar_g-se, ymax=depvar_g+se), width=.1)+theme(plot.title = element_text(lineheight=1.0, face="bold"))+xlab("region")+ylab(tylab)+theme(legend.position="none")

rfile<-read.csv(paste0(getwd(),"/",paste(paste(tvar,"partyid_g",sep="_"),"csv",sep=".")))

g7<-ggplot(rfile, aes(y= depvar_g, x=partyid_g))+ geom_point(size=3)+ geom_errorbar(aes(ymin=depvar_g-se, ymax=depvar_g+se), width=.1)+theme(plot.title = element_text(lineheight=1.0, face="bold"))+xlab("partyid")+ylab(tylab)+theme(legend.position="none")

rfile<-read.csv(paste0(getwd(),"/",paste(paste(tvar,"polviews_g",sep="_"),"csv",sep=".")))

g8<-ggplot(rfile, aes(y= depvar_g, x=polviews_g))+ geom_point(size=3)+  geom_errorbar(aes(ymin=depvar_g-se, ymax=depvar_g+se), width=.1)+theme(plot.title = element_text(lineheight=1.0, face="bold"))+xlab("polviews")+ylab(tylab)+theme(legend.position="none")

rfile<-read.csv(paste0(getwd(),"/",paste(paste(tvar,"attend_g",sep="_"),"csv",sep=".")))
g9<-ggplot(rfile, aes(y= depvar_g, x=attend_g))+ geom_point(size=3)+  geom_errorbar(aes(ymin=depvar_g-se, ymax=depvar_g+se), width=.1)+theme(plot.title = element_text(lineheight=1.0, face="bold"))+xlab("attend")+ylab(tylab)+theme(legend.position="none")

rfile<-read.csv(paste0(getwd(),"/",paste(paste(tvar,"realinc_g",sep="_"),"csv",sep=".")))

g10<-ggplot(rfile, aes(y= depvar_g, x=realinc_g))+ geom_point(size=3)+ geom_errorbar(aes(ymin=depvar_g-se, ymax=depvar_g+se), width=.1)+theme(plot.title = element_text(lineheight=1.0, face="bold"))+xlab("realinc")+ylab(tylab)+theme(legend.position="none")


grid.arrange(g1,g2,g3,g4,g5,g6,g8,g9,g10, ncol=3,main=textGrob(ttitle,gp=gpar(fontsize=20,font=3)))

