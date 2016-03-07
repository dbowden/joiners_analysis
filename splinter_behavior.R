## Analysis of Fragmentation and Civilian Targeting ##
library(dplyr)
library(tidyr)
library(lubridate)
library(countrycode)

#### Import and Clean UCDP dyadic data ####
dyad <- read.csv("ucdp_dyadic.csv")

#limit to civil wars
dyad <- subset(dyad,TypeOfConflict>2)

#limit to needed vars
dyad <- subset(dyad,select=c(DyadId,ConflictId,Year,GWNoA,GWNoLoc,SideB,SideBID,Incompatibility,IntensityLevel,StartDate,StartDate2))

#fix dates
dyad$StartDate <- ymd(dyad$StartDate)
dyad$StartDate2 <- ymd(dyad$StartDate2)

#### Merge in UCDP Actor Data 
ucdp <- read.csv("ucdp_actor.csv")

#remove states
ucdp <- subset(ucdp,actorid>999)

#code splinter sources
ucdp$split.source <- ifelse(ucdp$name.data %in% ucdp$name.prev, 1, 0)

#code alliances
ucdp$is.alliance <- ifelse(ucdp$name.data %in% ucdp$name.alliance, 1, 0)

#code coalition
ucdp$coalition <- ifelse(ucdp$name.data %in% ucdp$group.name, 1, 0)

#limit to needed variables
ucdp <- subset(ucdp,select=c(dyadid,splinter,split.source,alliance,is.alliance,split.temp,join.group,coalition))

#split into separate obs for each dyadid
ucdp$dyadid <- as.character(ucdp$dyadid)

ucdp <- ucdp %>% 
  mutate(dyadid=strsplit(dyadid,", ")) %>% 
  unnest(dyadid)

colnames(ucdp)[8] <- "DyadId"

dyad$DyadId <- as.character(dyad$DyadId)

dyad <- merge(dyad,ucdp,all.x=T,all.y=F)
rm(ucdp)

## Code Group Variables

#get group start
dyad <- dyad %>% 
  group_by(SideBID) %>% 
  mutate(group.start=year(min(StartDate)))

#get group entries
dyad$entry <- ifelse(dyad$Year==dyad$group.start, 1, 0)

#get totally new groups
dyad$new.entry <- ifelse(dyad$entry==1 & dyad$splinter==0 & dyad$is.alliance==0, 1, 0)

#### Merge in Fatality Measures 

##Battle Deaths
deaths <- read.csv("ucdp_deaths.csv")

#subset
deaths <- subset(deaths,select=c(DyadId,Year,BdBest,GWNoBattle))

#merge
dyad <- merge(dyad,deaths,all.x=T,all.y=F)
rm(deaths)

##OSV
osv <- read.csv("ucdp_osv.csv")

#subset
osv <- subset(osv,select=c(actorid,year,bestfatalityestimate))

#merge in gov fatalties
colnames(osv) <- c("GWNoA","Year","gov.civ.fatal")
osv$GWNoA <- as.factor(osv$GWNoA)
dyad <- merge(dyad,osv,all.x=T,all.y=F)

#merge in reb fatalties
colnames(osv) <- c("SideBID","Year","reb.civ.fatal")
dyad <- merge(dyad,osv,all.x=T,all.y=F)

#merge in total by country-year
osv <- read.csv("ucdp_osv.csv")

osv <- osv %>% 
  group_by(gwnolocation,year) %>% 
  summarize(tot.civ.fata=sum(bestfatalityestimate))

colnames(osv) <- c("GWNoLoc","Year","tot.civ.fatal")

dyad <- merge(dyad,osv,all.x=T,all.y=F)
rm(osv)

#### Analysis ####

dyad <- subset(dyad,Year>=1989)

summary(lm(log(reb.civ.fatal) ~ splinter + log(BdBest), data=dyad))

summary(glm(split.source ~ log(reb.civ.fatal) + log(BdBest) + splinter, data=dyad, family=binomial("logit")))