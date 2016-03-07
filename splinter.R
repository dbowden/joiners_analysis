gled <- read.csv("final_actor_data_group.csv")

gled <- subset(gled,duplicated(dyadid)==F)

summary(glm(split.source ~ (rebel.support=="explicit") + (gov.support=="explicit") + splinter + strong, data=gled, family=binomial("logit")))

summary(glm(split.source ~ (rebel.support=="explicit") + (styr>1988), data=gled, family=binomial("logit")))

## Use THIS ##
summary(glm(split.source ~ (rtypesup=="military") + (styr>1988), data=gled, family=binomial("logit")))

#fix missing
gled$rmil.sup <- ifelse(gled$rtypesup=="military", 1, 0)
gled$rmil.sup[is.na(gled$rmil.sup)] <- 0

summary(glm(split.source ~ rmil.sup + (styr>1988) + (diamond.sites>0) + (gold.sites>0) + (drug.sites>0) + (gem.sites>0) + (oil.sites>0), data=gled, family=binomial("logit")))




summary(glm(split.source ~ (rebel.support=="explicit") + splinter + centcontrol, data=gled, family=binomial("logit")))

summary(glm(alliance ~ (rebel.support=="no") + (gov.support=="explicit") + splinter + centcontrol, data=gled, family=binomial("logit")))

summary(glm((rebel.support=="explicit") ~ splinter + (gov.support=="explicit") + (styr>1988), data=gled, family=binomial("logit")))
