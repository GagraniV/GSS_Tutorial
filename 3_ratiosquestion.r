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

rfile$marital <- with(rfile,ifelse(marital_g ==  0, "Married", 
                                 ifelse(marital_g == 1, "Widowed",
                                        ifelse(marital_g ==  2, "Divorced",
                                               ifelse(marital_g == 3, "Separated", "Never Married")))))

g1<-ggplot(rfile, aes(y= depvar_g, x=marital))+ geom_point(size=3)+ geom_errorbar(aes(ymin=depvar_g-se, ymax=depvar_g+se), width=.1)+theme(plot.title = element_text(lineheight=1.0, face="bold"))+xlab("Marital Status")+ylab(tylab)+theme(legend.position="none")

rfile<-read.csv(paste0(getwd(),"/",paste(paste(tvar,"degree_g",sep="_"),"csv",sep=".")))

rfile$degree <- with(rfile, ifelse(degree_g == 0, "LT High School", 
                                 ifelse(degree_g == 1, "High School","College")))
                                                                                      

g2<-ggplot(rfile, aes(y= depvar_g, x=degree))+ geom_point(size=3)+  geom_errorbar(aes(ymin=depvar_g-se, ymax=depvar_g+se), width=.1)+theme(plot.title = element_text(lineheight=1.0, face="bold"))+xlab("Education Level")+ylab(tylab)+theme(legend.position="none")

rfile<-read.csv(paste0(getwd(),"/",paste(paste(tvar,"age_g",sep="_"),"csv",sep=".")))

rfile$age <- with(rfile, ifelse(age_g == 0, "Age(<35)", 
                                 ifelse(age_g == 1, "Age(35-45)",
                                        ifelse(age_g == 2, "Age(45-55)",
                                               ifelse(age_g == 3, "Age(55-65)",
                                                      ifelse(age_g == 4,"Age(65-75)", "Age(>75)"))))))

g3<-ggplot(rfile, aes(y= depvar_g, x=age))+ geom_point(size=3)+ geom_errorbar(aes(ymin=depvar_g-se, ymax=depvar_g+se), width=.1)+theme(plot.title = element_text(lineheight=1.0, face="bold"))+xlab("Age Group")+ylab(tylab)+theme(legend.position="none")

rfile<-read.csv(paste0(getwd(),"/",paste(paste(tvar,"sex_g",sep="_"),"csv",sep=".")))

rfile$sex <- with(rfile, ifelse(sex_g == 0,"Male","Female"))

g4<-ggplot(rfile, aes(y= depvar_g, x=sex))+ geom_point(size=3)+  geom_errorbar(aes(ymin=depvar_g-se, ymax=depvar_g+se), width=.1)+theme(plot.title = element_text(lineheight=1.0, face="bold"))+xlab("Gender")+ylab(tylab)+theme(legend.position="none")
rfile<-read.csv(paste0(getwd(),"/",paste(paste(tvar,"race_g",sep="_"),"csv",sep=".")))

rfile$race <- with(rfile, ifelse(race_g == 0,"White", 
                                       ifelse(race_g == 1, "Black", "Others")))  

g5<-ggplot(rfile, aes(y= depvar_g, x=race))+ geom_point(size=3)+ geom_errorbar(aes(ymin=depvar_g-se, ymax=depvar_g+se), width=.1)+theme(plot.title = element_text(lineheight=1.0, face="bold"))+xlab("Race")+ylab(tylab)+theme(legend.position="none")

rfile<-read.csv(paste0(getwd(),"/",paste(paste(tvar,"region_g",sep="_"),"csv",sep=".")))

rfile$region<- with(rfile, ifelse(region_g == 0, "New England", 
                                 ifelse(region_g == 1, "Mid.Atlantic",
                                        ifelse(region_g == 2,"E.N.Central",
                                               ifelse(region_g == 3,"W.N.Central",
                                                      ifelse(region_g == 4, "S.Atlantic",
                                                             ifelse(region_g == 5, "E.S.Central",
                                                                    ifelse(region_g == 6, "W.S.Central",
                                                                           ifelse(region_g ==  7, "Mountain","Pacific")))))))))
                                                                                
g6<-ggplot(rfile, aes(y= depvar_g, x=region))+ geom_point(size=3)+  geom_errorbar(aes(ymin=depvar_g-se, ymax=depvar_g+se), width=.1)+theme(plot.title = element_text(lineheight=1.0, face="bold"))+xlab("Region")+ylab(tylab)+theme(legend.position="none")

rfile<-read.csv(paste0(getwd(),"/",paste(paste(tvar,"partyid_g",sep="_"),"csv",sep=".")))

rfile$partyid <- with(rfile,ifelse(partyid_g == 0, "Democrat", 
                                  ifelse(partyid_g == 1, "Republican","Independent")))
                                              
g7<-ggplot(rfile, aes(y= depvar_g, x=partyid))+ geom_point(size=3)+ geom_errorbar(aes(ymin=depvar_g-se, ymax=depvar_g+se), width=.1)+theme(plot.title = element_text(lineheight=1.0, face="bold"))+xlab("Political Party Affliation")+ylab(tylab)+theme(legend.position="none")

rfile<-read.csv(paste0(getwd(),"/",paste(paste(tvar,"polviews_g",sep="_"),"csv",sep=".")))

rfile$polviews <- with(rfile, ifelse(polviews_g == 0, "Liberal", 
                                    ifelse(polviews_g ==1, "Conservative", "Moderate")))
                                                     
g8<-ggplot(rfile, aes(y= depvar_g, x=polviews))+ geom_point(size=3)+  geom_errorbar(aes(ymin=depvar_g-se, ymax=depvar_g+se), width=.1)+theme(plot.title = element_text(lineheight=1.0, face="bold"))+xlab("Poltical Views")+ylab(tylab)+theme(legend.position="none")

rfile<-read.csv(paste0(getwd(),"/",paste(paste(tvar,"attend_g",sep="_"),"csv",sep=".")))

rfile$attend <- with(rfile, ifelse(attend_g == 2, "Yearly", 
                                  ifelse(attend_g == 1, "Monthly","Weekly")))                                                    
                                                    
g9<-ggplot(rfile, aes(y= depvar_g, x=attend))+ geom_point(size=3)+  geom_errorbar(aes(ymin=depvar_g-se, ymax=depvar_g+se), width=.1)+theme(plot.title = element_text(lineheight=1.0, face="bold"))+xlab("Church Attends")+ylab(tylab)+theme(legend.position="none")

rfile<-read.csv(paste0(getwd(),"/",paste(paste(tvar,"realinc_g",sep="_"),"csv",sep=".")))

rfile$realinc <- with(rfile, ifelse(realinc_g == 1, "Quant(0.2)",
                                    ifelse(realinc_g == 2, "Quant(0.4)",
                                          ifelse(realinc_g == 3, "Quant(0.6)",
                                                ifelse(realinc_g == 4, "Quant(0.8)","Quant(1.0)")))))
                      
g10<-ggplot(rfile, aes(y= depvar_g, x=realinc))+ geom_point(size=3)+ geom_errorbar(aes(ymin=depvar_g-se, ymax=depvar_g+se), width=.1)+theme(plot.title = element_text(lineheight=1.0, face="bold"))+xlab("Income Groups")+ylab(tylab)+theme(legend.position="none")

grid.arrange(g1,g2,g3,g4,g5,g6,g8,g9,g10, ncol=3,main=textGrob(ttitle,gp=gpar(fontsize=20,font=3)))

