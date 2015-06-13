### script0-0: Analyzing General Social Survey (GSS) using R Language ####
### Objective : Downloading and subsetting varibles of interest ###

############################################################
#Load required packages.
library("survey")
library("downloader")
library("foreign")
# Set working directory, where gss data, scripts, outputs will be stored.
setwd("C:/Users/gagranis/Documents/R/gss/tutorialGSS")
############################################################
# Dowload data and save as .rda file. It takes time, so run the below source_url once, and then use load command for future use.
############################################################
#uncomment the following line if you wish to download data. Alternatevley, use "GSS.rda" data provided in working directory.
#source_url( "https://raw.github.com/ajdamico/usgsd/master/General%20Social%20Survey/1972-2012%20cumulative%20cross-sectional%20-%20analysis%20examples.R" , prompt = FALSE , echo = TRUE)

#Load GSS in R workspace.
load("GSS.2012.CS.rda")
#Finding class for the dataframe.
class(GSS.2012.CS.df)
#Finding list of all variblse in GSS dataset.
var_gss<- names(GSS.2012.CS.df)
View(var_gss)

### Subset gss data to keep interest varibles ###
############################################################
# 1: create a list of varibles to keep.
vars.to.keep <- c("year", "id","oversamp", "formwt", "wtssall", "sampcode","sample" ,"wrkstat","marital","agewed","sibs","childs","age","agekdbrn","educ","paeduc","maeduc","speduc","degree","padeg","madeg","spdeg","sex","race","res16","reg16","mobile16","income","rincome","region","xnorcsiz","srcbelt","size","partyid","polviews","natspac","natenvir","natheal","natcity","natcrime","natdrug","nateduc","natrace","natarms","nataid","natfare","natroad","natsoc","natmass","natpark","natchld","natsci","eqwlth","tax","spkath","colath","libath","spkrac","colrac","librac","spkcom","colcom","libcom","spkmil","colmil","libmil","spkhomo","colhomo","libhomo","cappun","gunlaw","courts","wirtap","grass","relig","denom","fund","attend","reliten","postlife","pray","neargod","sprel","relig16","denom16","fund16","spden","spfund","sprel16","spden16","spfund16","prayer","bible","world1","world4","racmar","racdin","racpush","racseg","racopen","raclive","racclos","racdis","racinteg","rachome","racfew","rachaf","racmost","busing","racpres","racchurh","affrmact","wrkwayup","closeblk","closewht","happy","hapmar","health","life","helpful","fair","trust","satcity","sathobby","satfam","satfrnd","sathealt","confinan","conbus","conclerg","coneduc","confed","conlabor","conpress","conmedic","contv","conjudge","consci","conlegis","conarmy","obey","popular","thnkself","workhard","helpoth","socrel","socommun","socfrend","socbar","socpars","socsibs","aged","weekswrk","partfull","drink","drunk","smoke","anomia5","anomia6","anomia7","joblose","jobfind","satjob","richwork","jobinc","jobsec","jobhour","jobpromo","jobmeans","class","satfin","finalter","finrela","wksub","wksubs","wksup","wksups","unemp","govaid","union","getahead","parsol","kidssol","fehome","fework","fepres","fepol","abdefect","abnomore","abhlth","abpoor","abrape","absingle","abany","chldidel","chldmore","pillok","sexeduc","divlaw","premarsx","teensex","xmarsex","homosex","porninf","pornmorl","pornrape","pornout","pornlaw","xmovie","spanking","letdie1","suicide1","suicide2","suicide3","suicide4","hit","gun","hitok","hitmarch","hitdrunk","hitchild","hitbeatr","hitrobbr","polhitok","polabuse","polmurdr","polescap","polattak","fear","burglr","robbry","owngun","pistol","shotgun","rifle","rowngun","ticket","arrest","hunt","news","tvhours","phone","coop","comprend","form","fechld","fehelp","fepresch","fefam","racdif1","racdif2","racdif3","racdif4","divorce5","unemp5","hosdis5","death5","death16","padeath","madeath","chlddth","sibdeath","spdeath","trauma1","trauma5","helppoor","helpnot","helpsick","helpblk","memfrat","memserv","memvet","mempolit","memunion","memsport","memyouth","memschl","memhobby","memgreek","memnat","memfarm","memlit","memprof","memchurh","memother","memnum","god","reborn","savesoul","wlthwhts","wlthblks","workwhts","workblks","intlwhts","intlblks","liveblks","marblk","discaff","relpersn","sprtprsn","othlang","goodlife","goveqinc","meovrwrk","localnum","relexper","relactiv","partners","matesex","sexsex","sexfreq","numwomen","nummen","partnrs5","sexsex5","evpaidsx","evstray","condom","relatsex","evidu","evcrack","povline","incdef","realinc","realrinc","ethnic","wtss","wtssnr","wtssall","vstrat","vpsu")

#2: define a new data frame(GSS by selecting varibles listed in vars.to.keep)
GSS <- GSS.2012.CS.df[ , vars.to.keep]

#3: save GSS data file in working directory.
save(GSS,file="GSS.rda")

# garbage collection: clear up RAM
gc()

############################################################
### Useful Links ####
# Analyze survey data for free with the r language : http://asdfree.com
# Rcode file: https://github.com/ajdamico/usgsd/tree/master/General%20Social%20Survey
# Official General Social Survey Website: http://www3.norc.org/GSS+Website/
