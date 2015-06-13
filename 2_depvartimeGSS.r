### Script 0-2: Analyzing General Social Survey (GSS) using R Language ####
### Objective : Timeseries analysis for any dependent varible selected in the script 0-1A  ###

#######################################################################################
# Set working directory, where gss data, scripts, outputs will be stored.
setwd("C:/Users/gagranis/Documents/R/gss/tutorialGSS")

# Load shortGSS.rda dataset.
load("shortGSS.rda")

#Check missing values for years.Subset dataset for the years that particular question being not asked.
#Apply factor() to the year after subsetting.
tshortGSS<- subset(shortGSS, !is.na(shortGSS$polviews_g))
tshortGSS$year<-as.character(tshortGSS$year)
tshortGSS$year<-as.factor(tshortGSS$year)
year<-levels(tshortGSS$year)

# Download packages.
library("survey")
library("plyr")
library("scales")
library("reshape2")
library("ggplot2")
library("grid")

## Calculate the compwt and samplerc variables to match SDA specifications
tshortGSS <- transform( 
  tshortGSS, 
  compwt =  oversamp  *  formwt * wtssall, 
  samplerc = 
    ifelse(sample %in% 3:4 , 3 , 
           ifelse( sample %in% 6:7 , 6 , 
                   sample ) )
)

# Specify a complex survey design. It combines a data frame and all the survey design information needed to analyse it, which is used for survey modelling and summary functions. Type ?svydesign for detail.
############################################################
gss.designshort <- 
  svydesign( 
    ~sampcode, 
    strata = ~samplerc, 
    data = subset(tshortGSS, !is.na(sampcode)), 
    weights = ~compwt, 
    nest = TRUE
  
  )



## Function to calculate ratio of favor/oppose police permit to own a gun for each survey year.
############################################################
df = data.frame()

for (i in year) {
  
  dfr=data.frame()
  dfr<-tryCatch({
    svyby(
      ~factor(depvar_g),
      ~polviews_g+year,
      design = subset(gss.designshort, year==i),
      svymean,
      na.rm = TRUE
    )
  },error=function(e){
    (paste("error on year:", i))
    
  }
  ) 
  {
    df<-rbind(df,dfr)
  }  
}

depvartime<-df
# Change varible name "factor(depvar_g)0" and "se.factor(depvar_g)0" into 'depvar' and 'se'.
depvartime$depvar<-depvartime[,"factor(depvar_g)1"]
depvartime$se<-depvartime[,"se.factor(depvar_g)1"]

# Change varible year into date class.
depvartime$year <- as.Date(depvartime$year, format = "%Y")

# create a seperate column year_g, with year extracted from the date.
depvartime$year_g<-as.POSIXlt(depvartime$year)$year+1900 
# Change varible year into date class.
depvartime$year_g <- as.Date(as.factor(depvartime$year_g), format = "%Y")

#Change 'polviews_g' varible into factor class.
depvartime$polviews_g<-as.factor(depvartime$polviews_g)

#Save file.
############################################################
fname<- paste(paste("time",tvar,sep="_"),"rda",sep=".")
save(depvartime,file=fname)

#Seting x and y ranges.
yrng <- range(depvartime$depvar, na.rm=TRUE)
xrng <- range(depvartime$year_g, na.rm=TRUE)

#plot
############################################################
#creating a dataframe depvartime1 for geom rect().
pres1<-c("Ford","Carter","Regan","Bush-I","Clinton","Bush-II","Obama")
start1<-c(1974,1977,1981,1989,1993,2001,2008)
end1<-c(1977,1981,1989,1993,2001,2008,2012)
party1<-c("R","D","R","R","D","R","D")
depvartime1<-data.frame(pres1,start1,end1,party1)
depvartime1$start1 <- as.Date(as.factor(depvartime1$start1), format = "%Y")
depvartime1$end1 <- as.Date(as.factor(depvartime1$end1), format = "%Y")

dv<-ggplot(depvartime, aes(x=year_g, y=depvar))+ geom_point(size=4,aes(group=depvartime$polviews_g, colour=depvartime$polviews_g))+geom_line(size=2,aes(group=depvartime$polviews_g, colour=depvartime$polviews_g))+ ggtitle(ttitle)+theme(plot.title = element_text(lineheight=1.0, face="bold"))+ ylab(tylab) + scale_y_continuous(limits=c(yrng[1]-.1,yrng[2]+.1),labels = percent_format())+xlab("Year")+scale_x_date(limits = c(xrng[1], xrng[2]),breaks = date_breaks("5 years"),labels = date_format("%Y"))

dv<-dv+geom_text(aes(label = percent(depvartime$depvar)),hjust=-0.2, vjust=-.5,size = 3)+scale_colour_manual(values=c("blue", "red","darkgreen"),breaks=c("0", "1", "2"),labels=c("Liberal","Conservative", "Moderate"))+ theme_minimal()

dv<-dv+ theme(legend.position="right",legend.direction="vertical")+theme(legend.title=element_blank())+guides(color=guide_legend(override.aes=list(fill=NA)))

dv<-dv+geom_rect(data=depvartime1, aes(NULL, NULL, xmin=start1, xmax=end1,fill=party1),ymin=yrng[1]-.1, ymax=yrng[2]+.1)+scale_fill_manual(values = alpha(c("blue", "red"), 0.2),labels= c("Democratic", "Republican"))

dv<-dv+geom_text(data=depvartime1, aes(x=start1, y=(yrng[2]+.08), label=pres1,size=4, hjust= 1.1, vjust= 2, angle=90))
  
dv<-dv+geom_vline(data=depvartime1, aes(xintercept=as.numeric(depvartime1$start1)),color="grey50", linetype=5, size=0.5)+scale_size(guide = "none")
dv
