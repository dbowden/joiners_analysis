#### Create Data for Joiner Analysis ####
library(dplyr)
library(stringr)
library(multiwayvcov)
library(lmtest)

#### Load and Clean Data ####

#load dyadic conflict data
dyad <- read.csv("ucdp_dyadic.csv")

#remove inter and extrastate conflicts
dyad <- subset(dyad,TypeOfConflict>2)

#load actor data
actor <- read.csv("ucdp_actor.csv")

#remove states
actor <- subset(actor, actorid>=1000)

#remove unneeded columns
actor <- subset(actor,select=-c(name.data,name.orig,name.orig.full,name.orig.fulleng,namechange,newname,newname.full.mt,newname.fulleng,conflictid,dyadid,primary.party,osvcoalitionid,nscoalitionid,region))

#### create conflict episodes - 2 year break constitutes a new episode ####

conflict.years <- dyad %>% 
  group_by(ConflictId,Year) %>% 
  summarize()

conflict.years <- conflict.years %>% 
  group_by(ConflictId) %>% 
  mutate(lag.year=lag(Year),start=min(Year))

#code a new episode if there are 2 calendar years without fighting, and for first year
conflict.years$new.ep <- ifelse(conflict.years$Year==conflict.years$start | conflict.years$Year - conflict.years$lag.year >=3, 1, 0)

#create episode numbers

conflict.years <- conflict.years %>% 
  group_by(ConflictId) %>% 
  mutate(epnum=cumsum(new.ep))
  
#create conflict episode identifier
conflict.years$ConflictEpisode <- as.factor(paste(sapply(strsplit(as.character(conflict.years$ConflictId), "-"), "[", 2),conflict.years$epnum, sep="-"))

#remove unneeded variables and merge back into full data
conflict.years <- subset(conflict.years,select=-c(lag.year,start))

dyad <- merge(dyad,conflict.years,all=T)
rm(conflict.years)

#### merge in actor dyad and code some variables ####

##merge
colnames(actor)[1] <- "SideBID"

dyad <- merge(dyad,actor,all.x=T,all.y=F)
rm(actor)

##code variables from actor data

#was the group an alliance?
dyad$is.alliance <- ifelse(dyad$SideBID %in% dyad$actorid.alliance, 1, 0)

#was the group the source of a splinter org?
dyad$splinter.source <- ifelse(dyad$SideB %in% dyad$name.prev, 1, 0)

#was the group active in multiple countries
dyad$transnational <- ifelse(str_count(dyad$location, ",")>0,1,0)

#### code dyad variables ####

#how many dyads appear in a single conflict-year?
dyad <- dyad %>% 
  group_by(ConflictEpisode,Year) %>% 
  mutate(dyads.year=n_distinct(DyadId))

#how many dyads appear over the course of an episode? What is the peak number of simultaneous dyads in an episode?
dyad <- dyad %>% 
  group_by(ConflictEpisode) %>% 
  mutate(dyads.episode=n_distinct(DyadId),dyads.max.simul=max(dyads.year))

#what is the first year in which a dyad is active?
dyad <- dyad %>% 
  group_by(DyadId) %>% 
  mutate(dyad.start=min(Year),dyad.end=max(Year))

#get first year of the conflict
dyad <- dyad %>% 
  group_by(ConflictId) %>% 
  mutate(conflict.start=min(Year))

dyad$join <- ifelse(dyad$Year==dyad$dyad.start & dyad$Year>dyad$conflict.start, 1, 0)

#how many groups were active in previous episode?
dyad <- dyad %>% 
  group_by(ConflictEpisode) %>% 
  mutate(epi.start=min(Year))

dyad$prior.episode <- ifelse(dyad$dyad.start < dyad$epi.start, 1, 0)

#how many groups joined during a group's tenure in the conflict? First get joiners per conflict year, then sum for all years that a gruop is active excluding the first year (i.e. excluding itself)
dyad <- dyad %>% 
  group_by(ConflictId,Year) %>% 
  mutate(conflict.year.tot.joiners=sum(join))

dyad$conflict.year.tot.joiners <- ifelse(dyad$join==1, dyad$conflict.year.tot.joiners-1, dyad$conflict.year.tot.joiners)

dyad <- dyad %>% 
  group_by(ConflictId,DyadId) %>% 
  mutate(group.tenure.entries=sum(conflict.year.tot.joiners))

#### Descriptive analysis ####

## Conflict / Episode starts and entries

with(subset(dyad,join==1),table(new.ep)) #313/497 joins occur at the start of a new episode

with(subset(dyad,join==1),table(Year==conflict.start)) #209/497 joined at the first year of conflict. This means 104 joined at the beginning of a later episode. 184 joined in the middle of an episode.

## Splinters and alliances

with(subset(dyad,join==1 & Year!=conflict.start),table(splinter)) #60/287 grups that joined after the first year of conflict are splinters

with(subset(dyad,join==1 & new.ep==0),table(splinter)) #49/184 groups how joined in the middle of an episode are splinters

with(subset(dyad,join==1 & Year!=conflict.start),table(is.alliance)) #21/287 grups that joined after the first year of conflict are alliances

with(subset(dyad,join==1 & new.ep==0),table(is.alliance)) #18/184 groups how joined in the middle of an episode are splinters

with(subset(dyad,join==1 & Year!=conflict.start),table(splinter | is.alliance)) #80/287 grups that joined after the first year of conflict are splinters or alliances

with(subset(dyad,join==1 & new.ep==0),table(splinter | is.alliance)) #118/184 groups how joined in the middle of an episode are splinters or alliances

## Joiners during a group's tenure

with(subset(dyad,Year==conflict.start),table(group.tenure.entries>0)) #only 16/209 groups present on day 1 of conflict experiencing joining

with(subset(dyad,Year==dyad.start),table(group.tenure.entries>0)) #159/497 groups experiencing joining

with(subset(dyad,Year==dyad.start & group.tenure.entries>0),table(splinter.source)) #Of these, 29 are the source of a splinter

with(subset(dyad,Year==dyad.start & group.tenure.entries>0),table(alliance)) #And 32 joined an alliance. That leaves 101 that are not the product of reconfiguration (and some of those probably have multiple joiners).

## How many multi-dyad years driven by splintering?

with(subset(dyad,dyads.year>1),table(dyads.year-(splinter + splinter.source)>0)) #914/956 multi-dyad years have at least one group not involved in splintering

with(subset(dyad,is.alliance==1),table(dyads.year>1))

#### Regressions ####

#some options for unit of analysis. first try groups that were present at start of an episode.
init <- subset(dyad,Year==dyad.start & new.ep==1)
init$duration <- init$dyad.end - init$dyad.start

m1 <- glm((group.tenure.entries>0) ~ duration + dyads.year, data=init, family=binomial("logit"))
vcov1 <- cluster.vcov(m1, init$SideA)
coeftest(m1, vcov=vcov1)


m2 <- glm((group.tenure.entries>0) ~ duration + dyads.year + !is.na(osid), data=subset(init,Year>1988), family=binomial("logit"))
vcov2 <- cluster.vcov(m2, subset(init,Year>1988)["SideA"])
coeftest(m2, vcov=vcov2)
