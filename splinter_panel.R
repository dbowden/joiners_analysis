library(pglm)

split <- read.csv("splinter_panel.csv")

#create conflict age
split$conflict.year <- split$year - split$styr

#create starts for specific types
split$troops.start <- ifelse(split$sup.start==1 & split$troops==1, 1, 0)
split$mil.start <- ifelse(split$sup.start==1 & (split$operations==1 | split$territory==1 | split$weapons==1 | split$materiel==1 | split$training==1 | split$intelligence==1), 1, 0)
split$fund.start <- ifelse(split$sup.start==1 & split$funding==1, 1, 0)

split <- split %>% 
  group_by(SideBID) %>% 
  mutate(group.start=min(year))

panel <- subset(split, year>styr)

#### Panel Analysis ####

panel <- pdata.frame(split,c("dyadid","year"))

summary(pglm(split.start ~ sup.start + splinter + (gold.sites>0) + conflict.year, data=panel, model="pooling", effect="twoways", family=binomial("logit")))

summary(pglm(split.start ~ mil.start + lag(mil.start) + conflict.year, data=panel, model="pooling", effect="twoways", family=binomial("logit")))

summary(pglm(sup.start ~  lag(split.start) + (gold.sites>0) + conflict.year, data=panel, model="pooling", effect="twoways", family=binomial("logit")))

#### fatalities
panel$frag <- panel$entries - panel$non.prev.entries

summary(pglm(frag ~ lag(sup.start) + lag(log(BdBest)) + lag(log(reb.civ.fatal)) + epr_discriminated + log(pop) + log(area) + (drug.sites>0) + (gold.sites>0), data=panel, model="pooling", effect="twoways", family=binomial("logit")))

summary(pglm(entries ~  epr_discriminated + log(pop) + log(area) + (drug.sites>0) + (gold.sites>0), data=panel, model="pooling", effect="twoways", family=binomial("logit")))

summary(pglm(non.prev.entries ~ lag(log(BdBest)) + lag(log(reb.civ.fatal)) + epr_discriminated + log(pop) + log(area) + (drug.sites>0) + (gold.sites>0), data=panel, model="pooling", effect="twoways", family=binomial("logit")))

summary(plm(log(reb.civ.fatal) ~ splinter, data=panel, model="within", effect="twoways"))

summary(pglm(dyads ~ lag(dyads) + (sup.start) + (gold.sites>0), data=panel, model="pooling", effect="twoways", family="poisson"))
