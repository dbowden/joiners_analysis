library(multiwayvcov)
library(lmtest)

gled <- read.csv("gled_controls.csv")

#### Initiators as UofA ####

init <- subset(gled, year==conflict.start)

init$joiners <- ifelse(init$group.tenure.entries>0, 1, 0)

init$simul <- ifelse(duplicated(init$ucdpid)==T | duplicated(init$ucdpid, fromLast=T)==T, 1, 0)

init$polwing <- ifelse(init$rebpolwing=="explicit link" | init$rebpolwing=="acknowledged link", 1, 0)
init$strong <- ifelse(init$rebstrength=="much stronger" | init$rebstrength=="stronger" | init$rebstrength=="parity", 1, 0)
init$duration <- init$eyr - init$styr

#models

summary(glm(joiners ~ polwing + strong + (rebpresosts=="extensive" | rebpresosts=="some") + prevactive + (conflicttype=="secessionist conflict"), data=init, family=binomial("logit")))

summary(glm(joiners ~ polwing + strong + (rebpresosts=="extensive" | rebpresosts=="some") + (conflicttype=="secessionist conflict") + log(area), data=init, family=binomial("logit")))

summary(glm(group.tenure.entries ~ polwing + strong, data=init, family="poisson"))

summary(glm(group.tenure.entries ~ polwing + strong + (rebpresosts=="extensive" | rebpresosts=="some") + prevactive + (conflicttype=="secessionist conflict"), data=init, family="poisson"))

#clustered standard errors on country
m1 <- glm(group.tenure.entries ~ polwing + strong + (rebpresosts=="extensive" | rebpresosts=="some") + (conflicttype=="secessionist conflict"), data=init, family="poisson")

country_c_vcov <- cluster.vcov(m1, init$side_a)
coeftest(m1)
coeftest(m1, vcov=country_c_vcov) #secession not sig, polwing marginal

m2 <- glm(group.tenure.entries ~ polwing + strong + (rebpresosts=="extensive" | rebpresosts=="some"), data=init, family="poisson")

country_c_vcov2 <- cluster.vcov(m2, init$side_a)
coeftest(m2, vcov=country_c_vcov2) #secession not sig, polwing marginal

m3 <- glm(group.tenure.entries ~ polwing + strong + (rebpresosts=="extensive" | rebpresosts=="some")  + (rebel.support=="explicit"), data=init, family="poisson")

country_c_vcov3 <- cluster.vcov(m3, init$side_a)
coeftest(m3, vcov=country_c_vcov3) #secession not sig, polwing marginal

summary(glm(group.tenure.entries ~ polwing + strong + (rebpresosts=="extensive" | rebpresosts=="some") + terrcont + simul + log(gdppc), data=init, family="poisson"))

#### Joiners as UofA ####

gled <- gled %>%
  group_by(side_b) %>%
  mutate(group.start=min(styr))

first <- subset(gled, year==group.start)

first$joiner <- ifelse(first$year > first$conflict.start, 1, 0)

first <- subset(first, selcect=-joiners)

first$polwing <- ifelse(first$rebpolwing=="explicit link" | first$rebpolwing=="acknowledged link", 1, 0)
first$strong <- ifelse(first$rebstrength=="much stronger" | first$rebstrength=="stronger" | first$rebstrength=="parity", 1, 0)

l1 <- glm(joiner ~ prevactive + terrcont + centcontrol + rebel.support, data=first, family=binomial("logit"))
country_c_vcov4 <- cluster.vcov(l1, first$side_a)
coeftest(l1, vcov=country_c_vcov4)

