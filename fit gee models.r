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

##
## intercept-only model
##
gee.null = geeglm(HelpObserved ~ 1, id=Ego.HH, family="binomial", data=hh.all, corstr="exchangeable", scale.fix=T)

##
###
##
gee.village = geeglm(HelpObserved ~ Ego.VillageID, id=Ego.HH, family="binomial", data=hh.all, corstr="exchangeable", scale.fix=T)

##
## control model (distance + size + wealth)
##
gee.control = geeglm(HelpObserved ~ Distance + RelativeWealthRank + Ego.Size + Alter.Size + Ego.VillageID,
                     id=Ego.HH, family="binomial", data=hh.all, corstr="exchangeable", scale.fix=T)

##
## reciprocated farm help
##
gee.ra = geeglm(HelpObserved ~ Distance + RelativeWealthRank + Ego.Size + Alter.Size + Ego.VillageID +
                  HelpReceived,
                id=Ego.HH, family="binomial", data=hh.all, corstr="exchangeable", scale.fix=T)

##
## need
##
gee.need_afb = geeglm(HelpObserved ~ Distance + RelativeWealthRank + Ego.Size + Alter.Size + Ego.VillageID +
                        Ego.Need.AFB + Alter.Need.AFB,
                      id=Ego.HH, family="binomial", data=hh.all, corstr="exchangeable", scale.fix=T)

##
## gifts
##
gee.gift = geeglm(HelpObserved ~ Distance + RelativeWealthRank + Ego.Size + Alter.Size + Ego.VillageID +
                    GiftGiven.ind,
                  id=Ego.HH, family="binomial", data=hh.all, corstr="exchangeable", scale.fix=T)

##
## kin
## (control + partners and kids + r x distance + r x kids)
##
gee.kin = geeglm(HelpObserved ~ Distance + RelativeWealthRank + Ego.Size + Alter.Size + Ego.VillageID +
                   r + r:Distance + AnyWivesInAlter + AnyHusbandsInAlter + ChildrenInAlter + r:ChildrenInAlter,
                 id=Ego.HH, family="binomial", data=hh.all, corstr="exchangeable", scale.fix=T)

##
## kin + need
##
gee.kin_need = geeglm(HelpObserved ~ Distance + RelativeWealthRank + Ego.Size + Alter.Size + Ego.VillageID +
                        r + r:Distance + AnyWivesInAlter + AnyHusbandsInAlter + ChildrenInAlter + r:ChildrenInAlter +
                        Ego.Need.AFB + Alter.Need.AFB,
                      id=Ego.HH, family="binomial", data=hh.all, corstr="exchangeable", scale.fix=T)

##
## kin + reciprocity
##
gee.kin_ra = geeglm(HelpObserved ~ Distance + RelativeWealthRank + Ego.Size + Alter.Size + Ego.VillageID +
                      r + r:Distance + AnyWivesInAlter + AnyHusbandsInAlter + ChildrenInAlter + r:ChildrenInAlter +
                      HelpReceived,
                    id=Ego.HH, family="binomial", data=hh.all, corstr="exchangeable", scale.fix=T)

##
## kin + gifts
##
gee.kin_gift = geeglm(HelpObserved ~ Distance + RelativeWealthRank + Ego.Size + Alter.Size + Ego.VillageID +
                        r + r:Distance + AnyWivesInAlter + AnyHusbandsInAlter + ChildrenInAlter + r:ChildrenInAlter +  # kin
                        GiftGiven.ind,  # gift
                      id=Ego.HH, family="binomial", data=hh.all, corstr="exchangeable", scale.fix=T)

##
## kin + reciprocity + need
##
gee.kin_ra_need = geeglm(HelpObserved ~ Distance + RelativeWealthRank + Ego.Size + Alter.Size + Ego.VillageID +
                           r + r:Distance + AnyWivesInAlter + AnyHusbandsInAlter + ChildrenInAlter + r:ChildrenInAlter +
                           HelpReceived +
                           Ego.Need.AFB + Alter.Need.AFB,
                         id=Ego.HH, family="binomial", data=hh.all, corstr="exchangeable", scale.fix=T)

##
## kin + need + gifts
##
gee.kin_need_gift = geeglm(HelpObserved ~ Distance + RelativeWealthRank + Ego.Size + Alter.Size + Ego.VillageID +
                             r + r:Distance + AnyWivesInAlter + AnyHusbandsInAlter + ChildrenInAlter + r:ChildrenInAlter +  # kin
                             Ego.Need.AFB + Alter.Need.AFB +  # need
                             GiftGiven.ind,  # gift
                           id=Ego.HH, family="binomial", data=hh.all, corstr="exchangeable", scale.fix=T)

##
## kin + need + gifts + recip
##
gee.kin_ra_need_gift = geeglm(HelpObserved ~ Distance + RelativeWealthRank + Ego.Size + Alter.Size + Ego.VillageID +
                                r + r:Distance + AnyWivesInAlter + AnyHusbandsInAlter + ChildrenInAlter + r:ChildrenInAlter +
                                HelpReceived +
                                Ego.Need.AFB + Alter.Need.AFB +
                                GiftGiven.ind,
                              id=Ego.HH, family="binomial", data=hh.all, corstr="exchangeable", scale.fix=T)

##
## kin x need + gifts + recip
##
gee.kin_ra_need_gift_int = geeglm(HelpObserved ~ Distance + RelativeWealthRank + Ego.Size + Alter.Size + Ego.VillageID +
                                    r + r:Distance + AnyWivesInAlter + AnyHusbandsInAlter + ChildrenInAlter + r:ChildrenInAlter +
                                    HelpReceived +
                                    (Ego.Need.AFB + Alter.Need.AFB) * r +
                                    GiftGiven.ind,
                                  id=Ego.HH, family="binomial", data=hh.all, corstr="exchangeable", scale.fix=T)

##
## kin x need + kin x recip + gifts
##
gee.kin_ra_need_gift_int_2 = geeglm(HelpObserved ~ Distance + RelativeWealthRank + Ego.Size + Alter.Size + Ego.VillageID +
                                      r + r:Distance + AnyWivesInAlter + AnyHusbandsInAlter + ChildrenInAlter + r:ChildrenInAlter +
                                      HelpReceived + r:HelpReceived +
                                      (Ego.Need.AFB + Alter.Need.AFB) * r +
                                      GiftGiven.ind,
                                    id=Ego.HH, family="binomial", data=hh.all, corstr="exchangeable", scale.fix=T)

##
## model comparison and averaging
##
best.models = model.sel(gee.null, gee.village, gee.control, 
                        gee.kin, gee.ra, gee.need_afb, gee.gift,
                        gee.kin_need, gee.kin_ra, gee.kin_gift, 
                        gee.kin_ra_need, gee.kin_need_gift,
                        gee.kin_ra_need_gift, gee.kin_ra_need_gift_int, gee.kin_ra_need_gift_int_2,
                        rank=QIC)
best.models

# average the best (delta QIC < 2) GEE models
# get the top deltaQIC < 2 models and standardize them
top.models   = mget(row.names(subset(best.models, delta<2)))
top.models.z = lapply(top.models, standardize.gee, standardize.y=F, binary.inputs="full")

gee.best   = top.models[[1]]
gee.best.z = top.models.z[[1]]

# average the std and unstd models if there was more than one top models
if (length(top.models) > 1)
{
  gee.best   = model.avg(top.models,   rank=QIC, revised.var=T, fit=T)
  gee.best.z = model.avg(top.models.z, rank=QIC, revised.var=T)
}

save.image(file=file.path(models.dir, "gees - villages.RData"))
