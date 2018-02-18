##
## Fit GEE models
##
library(geepack)
library(MuMIn)
library(arm)

source("data functions/standardize-gee.r")
source("data functions/confint-gee.r")
source("data functions/vcov.averaging.geeglm.r")

source("init.r")

hh.all$Ego.VillageID = factor(hh.all$Ego.VillageID)

hh.all$r.bin = cut(hh.all$r, c(0, 0.0039, 0.0078, 0.015, 0.031, 0.063, 0.125, 0.25, 0.5, 1), right=F)
hh.all$r.bin = forcats::fct_drop(hh.all$r.bin)  # `cut()` adds an empty factor "[0.5,1)" -- drop it

##
## kin
## (control + partners and kids + r x distance + r x kids)
##
gee.r = geeglm(HelpObserved ~ Distance + RelativeWealthRank + Ego.Size + Alter.Size + Ego.VillageID + 
                 r.bin,
               id=Ego.HH, family="binomial", data=hh.all, corstr="exchangeable", scale.fix=T)

##
## kin
## (control + partners and kids + r x distance + r x kids)
##
gee.kin = geeglm(HelpObserved ~ Distance + RelativeWealthRank + Ego.Size + Alter.Size + Ego.VillageID +
                   r.bin + r.bin:Distance + AnyWivesInAlter + AnyHusbandsInAlter + ChildrenInAlter + r.bin:ChildrenInAlter,
                 id=Ego.HH, family="binomial", data=hh.all, corstr="exchangeable", scale.fix=T)

summary(gee.r)


m.terms = terms(gee.r)

new.data = expand.grid(r.bin = levels(hh.all$r.bin),
                       # set these predictors to their means
                       Distance = mean(hh.all$Distance),
                       RelativeWealthRank = mean(hh.all$RelativeWealthRank),
                       Ego.Size = mean(hh.all$Ego.Size),
                       Alter.Size = mean(hh.all$Alter.Size),
                       Ego.VillageID = unique(hh.all$Ego.VillageID),
                       # set response to zero for now
                       HelpObserved=0)

new.data = predict_gee(gee.r, m.terms, new.data)

new.data %>% 
  filter(Ego.VillageID==1) %>% 
  ggplot(aes(x=r.bin, y=HelpObserved)) +
  geom_point() +
  geom_pointrange(aes(ymin=tlo, ymax=thi), alpha=0.2) +
  xlab("Coefficient of relatedness") +
  ylab("Probability of farm help") +
  
  #scale_x_log10(breaks=c(0.0001, 0.0039, 0.0078, 0.015, 0.031, 0.063, 0.125, 0.25, 0.5)) +  # plot relatedness on log scale with breaks at familiar points
  #scale_x_continuous(breaks=c(0.0001, 0.063, 0.125, 0.25, 0.5)) +
  
  plot_style
