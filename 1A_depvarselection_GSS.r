### script0_1A : Analyzing General Social Survey (GSS) using R Language ####
### Objective : Creating dummies for dependent varibles to analyze GSS data ###

############################################################
# Set working directory, where GSS data, scripts, outputs will be stored.

setwd("C:/Users/gagranis/Documents/R/gss/tutorialGSS")
# Load 'GSS.rda' saved in '0_downloadGSSdata.R' script.
load("GSS.rda")

# Creating dummies for demographic varibles.
############################################################
#Dependent Variable: 
GSSdvar<-GSS
table(GSSdvar$cappun)
depvar.sel<-function (dvar){
if(dvar=="owngun")
{
depvar<-GSSdvar[ ,dvar]
dv<-cbind(GSSdvar$depvar <-depvar)
dv1<-cbind(GSSdvar$depvar_g <-GSSdvar$depvar)
GSSdvar$depvar_g <- with(GSSdvar, ifelse(depvar_g == "yes", 1, 
                           ifelse(depvar_g == "no", 0,2)))
GSSdvar<- subset(GSSdvar, depvar_g ==0  | depvar_g ==1)
print(table(GSSdvar$depvar_g))
save(GSSdvar,file="GSSdvar.rda")
ttitle<<-"Gun Ownership in the US(Source:General Society Survey)"
tylab<<-"%With Gun at Home"
tvar<<- "owngun"  
} else if (dvar=="gunlaw")
{
depvar<-GSSdvar[ ,dvar]
dv<-cbind(GSSdvar$depvar <-depvar)
dv1<-cbind(GSSdvar$depvar_g <-GSSdvar$depvar)
GSSdvar$depvar_g <- with(GSSdvar, ifelse(depvar_g == "favor", 1, 
                            ifelse(depvar_g == "oppose", 0,2)))
GSSdvar<- subset(GSSdvar, depvar_g ==0  | depvar_g ==1)
print(table(GSSdvar$depvar_g))
save(GSSdvar,file="GSSdvar.rda")
ttitle<<-"Favor Police Permit to Buy Gun(Source:General Society Survey)"
tylab<<-"%Favoring police Permit"
tvar<<- "gunlaw"
}else if (dvar=="happy")
{
depvar<-GSSdvar[ ,dvar]
dv<-cbind(GSSdvar$depvar <-depvar)
dv1<-cbind(GSSdvar$depvar_g <-GSSdvar$depvar)
GSSdvar$depvar_g <- with(GSSdvar, ifelse(depvar_g == "very happy", 1, 0))
GSSdvar<- subset(GSSdvar, depvar_g ==0  | depvar_g ==1)
print(table(GSSdvar$depvar_g))  
save(GSSdvar,file="GSSdvar.rda")
ttitle<<-"General Happiness in the US (Source:General Society Survey)"
tylab<<-"%General Happiness"
tvar<<-"happy"
}else if (dvar=="grass")
{
depvar<-GSSdvar[ ,dvar]
dv<-cbind(GSSdvar$depvar <-depvar)
dv1<-cbind(GSSdvar$depvar_g <-GSSdvar$depvar)
GSSdvar$depvar_g <- with(GSSdvar, ifelse(depvar_g == "legal", 1, 0))
GSSdvar<- subset(GSSdvar, depvar_g ==0  | depvar_g ==1)
print(table(GSSdvar$depvar_g))  
save(GSSdvar,file="GSSdvar.rda")
ttitle<<-"Marijuana be Made Legal in the US (Source:General Society Survey)"
tylab<<-"%Favoring Marijuana legalization"
tvar<<-"grass"
}else if (dvar=="trust")
{
depvar<-GSSdvar[ ,dvar]
dv<-cbind(GSSdvar$depvar <-depvar)
dv1<-cbind(GSSdvar$depvar_g <-GSSdvar$depvar)
GSSdvar$depvar_g <- with(GSSdvar, ifelse(depvar_g == "can trust", 1, 
                                     ifelse(depvar_g == "cannot trust", 0,2)))
GSSdvar<- subset(GSSdvar, depvar_g ==0  | depvar_g ==1)
print(table(GSSdvar$depvar_g))  
save(GSSdvar,file="GSSdvar.rda")
ttitle<<-"Can People be Trusted in the US (Source:General Society Survey)"
tylab<<-"%Trusting People"
tvar<<-"trust"
}else if (dvar=="fair")
{
depvar<-GSSdvar[ ,dvar]
dv<-cbind(GSSdvar$depvar <-depvar)
dv1<-cbind(GSSdvar$depvar_g <-GSSdvar$depvar)
GSSdvar$depvar_g <- with(GSSdvar, ifelse(depvar_g == "fair", 1, 
                                     ifelse(depvar_g == "take advantage", 0,2)))
GSSdvar<- subset(GSSdvar, depvar_g ==0  | depvar_g ==1)
print(table(GSSdvar$depvar_g))  
save(GSSdvar,file="GSSdvar.rda")
ttitle<<-"People are Fair or Try to Take Advantage in the US (Source:General Society Survey)"
tylab<<-"%Treating People Fairly"
tvar<<-"fair"
}else if (dvar=="satfin")
{
depvar<-GSSdvar[ ,dvar]
dv<-cbind(GSSdvar$depvar <-depvar)
dv1<-cbind(GSSdvar$depvar_g <-GSSdvar$depvar)
GSSdvar$depvar_g <- with(GSSdvar, ifelse(depvar_g == "not at all sat", 0, 1))
GSSdvar<- subset(GSSdvar, depvar_g ==0  | depvar_g ==1)
print(table(GSSdvar$depvar_g))  
save(GSSdvar,file="GSSdvar.rda")
ttitle<<-"Satisfaction with Financial Situation in the US (Source:General Society Survey)"
tylab<<-"%Financial Satisfaction"
tvar<<-"satfin"
}else if (dvar=="cappun")
{
depvar<-GSSdvar[ ,dvar]
dv<-cbind(GSSdvar$depvar <-depvar)
dv1<-cbind(GSSdvar$depvar_g <-GSSdvar$depvar)
GSSdvar$depvar_g <- with(GSSdvar, ifelse(depvar_g == "favor",1, 0))
GSSdvar<- subset(GSSdvar, depvar_g ==0  | depvar_g ==1)
print(table(GSSdvar$depvar_g))  
save(GSSdvar,file="GSSdvar.rda")
ttitle<<-"Favor or Oppose Death Penalty for Murder in the US (Source:General Society Survey)"
tylab<<-"%Favoring Death Penalty"
tvar<<-"cappun"
}
else
{
print("no varible defiend")
}
}

############################################################
#Select varible of choice. 
############################################################

depvar.sel("owngun")
#depvar.sel("gunlaw")
#depvar.sel("happy")
#depvar.sel("grass")
#depvar.sel("trust")
#depvar.sel("fair")
#depvar.sel("satfin")
#depvar.sel("cappun")
#depvar.sel("xxx")





