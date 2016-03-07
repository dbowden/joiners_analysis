library(dplyr)
library(tidyr)
library(lubridate)
library(countrycode)

#### Import and Clean UCDP dyadic data ####
dyad <- read.csv("ucdp_dyadic.csv")

#limit to civil wars
dyad <- subset(dyad,TypeOfConflict>2)

#limit to needed vars
dyad <- subset(dyad,select=c(DyadId,ConflictId,Year,SideA,GWNoA,GWNoLoc,SideB,SideBID,Incompatibility,IntensityLevel,StartDate,StartDate2))

#fix dates
dyad$StartDate <- ymd(dyad$StartDate)
dyad$StartDate2 <- ymd(dyad$StartDate2)

#### Create splinter dates ####
ucdp <- read.csv("ucdp_actor.csv")

#remove states
ucdp <- subset(ucdp,actorid>999)

#remove actors that don't appear in dyadic conflicts
ucdp <- subset(ucdp,dyadid!="")

#find start dates for splinter groups
#colnames(ucdp)[8] <- "DyadId"

actors <- dyad %>% 
  group_by(SideBID) %>% 
  summarize(group.start=min(Year))

ucdp$SideBID <- as.factor(ucdp$actorid)

ucdp <- merge(ucdp,actors,all.x=T,all.y=F)
rm(actors)

#get actorid
split <- subset(ucdp,splinter==1)

#get rid of groups that don't appear in dyadic data
split <- subset(split,!is.na(group.start)==T)

split <- subset(split,select=c(name.prev,group.start))

colnames(split) <- c("name.data","Year")

split$split.start <- 1

ids <- subset(ucdp,select=c(dyadid,name.data))

split <- merge(split,ids,all.x=T,all.y=F)
rm(ids,ucdp)

colnames(split)[4] <- "DyadId"

split <- subset(split,select=c(DyadId,Year,split.start))

dyad <- merge(dyad,split,all.x=T,all.y=F)

#there's one exact duplicate
dyad <- subset(dyad,duplicated(dyad)==F)
rm(split)

#### Merge in External Support Data ####
support <- read.csv("ucdp_external.csv")

#limit to needed vars
support <- subset(support,select=c(ywp_year,actorID,bwd_id,external_exists,external_alleged,external_name,external_type__X,external_type__L,external_type__Y,external_type__W,external_type__M,external_type__T,external_type__.,external_type__I,external_type__O,external_type__U))

colnames(support) <- c("Year","SideBID","DyadId","external_exists","external_alleged","supporter_name","troops","operations","territory","weapons","materiel","training","funding","intelligence","other","unknown")

support$SideBID <- as.factor(support$SideBID)

support <- subset(support,duplicated(support)==F)

#remove a duplicate
support <- subset(support,duplicated(support[,1:3],fromLast = T)==F)

#standardize NA codes
support$external_exists[support$external_exists < 0] <- NA
support$external_alleged[support$external_alleged < 0] <- NA

#get start of support
support <- support %>% 
  group_by(SideBID) %>% 
  mutate(sup.start=min(Year))

support$sup.start <- ifelse(support$sup.start==1975, NA, support$sup.start)

support$sup.start <- ifelse(support$sup.start==support$Year, 1, 0)

dyad <- merge(dyad,support,all.x=T,all.y=F)
rm(support)

#### Merge in Fatality Measures ####

##Battle Deaths
deaths <- read.csv("ucdp_deaths.csv")

#subset
deaths <- subset(deaths,select=c(DyadId,Year,BdBest))

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

#### Merge w/ gleditsch et al data ####
gled <- read.csv("final_actor_data_yearly.csv")

dyad <- subset(dyad,select=-c(GWNoLoc,GWNoA,SideB,ConflictId,SideA))

colnames(dyad)[1] <- "year"
colnames(dyad)[3] <- "dyadid"

split <- merge(dyad,gled,all.x=T,all.y=F)

split <- subset(split,duplicated(split[,c(1:2,8:10)])==F)

split$split.start[is.na(split$split.start)] <- 0

write.csv(split,"splinter_panel.csv")
rm(dyad,gled)
