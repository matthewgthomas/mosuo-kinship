##
## Analyse the best-fitting GEE models (averaged)
##
library(ggplot2)
library(gridExtra)
library(aod)
library(data.table)
library(dplyr)
library(xlsx)
library(geepack)

source("init.r")

# either load GEE models or fit them from scratch
gee.path = file.path(models.dir, "gees.RData")
if (file.exists(gee.path)) {
  load(gee.path)
} else {
  source("fit gee models.r")
}

############################################################################
## Save best models table
##
best.models.df = as.data.frame(best.models)
best.models.df$Model = row.names(best.models.df)
row.names(best.models.df) = NULL

best.models.df$Model = gsub("^gee.null$", "Intercept-only", best.models.df$Model)
best.models.df$Model = gsub("^gee.control$", "Control model (Distance + HH size + Relative wealth)", best.models.df$Model)
best.models.df$Model = gsub("^gee.kin$", "Relatedness", best.models.df$Model)
best.models.df$Model = gsub("^gee.ra$", "Reciprocated help", best.models.df$Model)
best.models.df$Model = gsub("^gee.gift$", "Gifts", best.models.df$Model)
best.models.df$Model = gsub("^gee.need_afb$", "Relative need", best.models.df$Model)
best.models.df$Model = gsub("^gee.kin_ra$", "Relatedness + Reciprocity", best.models.df$Model)
best.models.df$Model = gsub("^gee.kin_gift$", "Relatedness + Gifts", best.models.df$Model)
best.models.df$Model = gsub("^gee.kin_need$", "Relatedness + Relative need", best.models.df$Model)
best.models.df$Model = gsub("^gee.kin_ra_need$", "Relatedness + Reciprocity + Relative need", best.models.df$Model)
best.models.df$Model = gsub("^gee.kin_need_gift$", "Relatedness + Relative need + Gifts", best.models.df$Model)
best.models.df$Model = gsub("^gee.kin_ra_need_gift$", "Relatedness + Reciprocity + Relative need + Gifts", best.models.df$Model)
best.models.df$Model = gsub("^gee.kin_ra_need_gift_int$", "Relatedness × Relative need + Reciprocity + Gifts", best.models.df$Model)
best.models.df$Model = gsub("^gee.kin_ra_need_gift_int_2$", "Relatedness × Relative need + Relatedness × Reciprocity + Gifts", best.models.df$Model)

best.models.df$qLik = round(best.models.df$qLik, 3)
best.models.df$delta = round(best.models.df$delta, 3)
best.models.df$weight = round(best.models.df$weight, 3)

write.csv(subset(best.models.df, select=c(Model, qLik, delta, weight)), 
          file.path(results.dir, "best gee models.csv"), row.names=F)


############################################################################
## Plot odds ratios from best-fitting GEE model(s)
##
# put ORs and CIs in a data frame
gee.or = cbind( data.frame(Estimate = exp(coef(gee.best.z, full=T))), exp(confint(gee.best.z, full=T, digits=3)) )
names(gee.or) = c("Estimate", "lwr", "upr")

setDT(gee.or, keep.rownames = TRUE)[]  # convert row names to first column

# put ORs into a more sensible order (list is reverse because of coordinate flip in ggplot)
rn_order = rev(c("(Intercept)", "z.r", "z.r:z.HelpReceived", "z.Distance:z.r", "z.r:z.ChildrenInAlter", "z.r:z.Ego.Need.AFB", "z.r:z.Alter.Need.AFB",
                 "z.HelpReceived", "z.GiftGiven.ind", "z.Ego.Need.AFB", "z.Alter.Need.AFB",
                 "z.ChildrenInAlter", "z.AnyWivesInAlter", "z.AnyHusbandsInAlter",
                 "z.Distance", "z.RelativeWealthRank", "z.Ego.Size", "z.Alter.Size", 
                 "Ego.VillageID2", "Ego.VillageID5", "Ego.VillageID6", "Ego.VillageID8"))
gee.or$rn = factor(gee.or$rn, levels=rn_order)

levels(gee.or$rn)[ levels(gee.or$rn)=="z.Distance" ] = "Distance (km)"
levels(gee.or$rn)[ levels(gee.or$rn)=="z.RelativeWealthRank" ] = "Relative wealth rank"
levels(gee.or$rn)[ levels(gee.or$rn)=="z.Ego.Size" ] = "Size of helper household (ego)"
levels(gee.or$rn)[ levels(gee.or$rn)=="z.Alter.Size" ] = "Size of landowner household (alter)"
levels(gee.or$rn)[ levels(gee.or$rn)=="z.r" ] = "Relatedness (r)"
levels(gee.or$rn)[ levels(gee.or$rn)=="z.AnyWivesInAlter" ] = "Any wives in alter?"
levels(gee.or$rn)[ levels(gee.or$rn)=="z.AnyHusbandsInAlter" ] = "Any husbands in alter?"
levels(gee.or$rn)[ levels(gee.or$rn)=="z.ChildrenInAlter" ] = "Any children in alter?"
levels(gee.or$rn)[ levels(gee.or$rn)=="z.HelpReceived" ] = "Help reciprocated?"
levels(gee.or$rn)[ levels(gee.or$rn)=="z.Ego.Need.AFB" ] = "Helper household's need"
levels(gee.or$rn)[ levels(gee.or$rn)=="z.Alter.Need.AFB" ] = "Landowner household's need"
levels(gee.or$rn)[ levels(gee.or$rn)=="z.GiftGiven.ind" ] = "Gift given from ego to alter?"
levels(gee.or$rn)[ levels(gee.or$rn)=="z.Distance:z.r" ] = "r × Distance"
levels(gee.or$rn)[ levels(gee.or$rn)=="z.r:z.ChildrenInAlter" ] = "r × Any children in alter?"
levels(gee.or$rn)[ levels(gee.or$rn)=="z.r:z.Ego.Need.AFB" ] = "r × Helper household's need?"
levels(gee.or$rn)[ levels(gee.or$rn)=="z.r:z.Alter.Need.AFB" ] = "r × Landowner household's need?"
levels(gee.or$rn)[ levels(gee.or$rn)=="z.r:z.HelpReceived" ] = "r × Help reciprocated?"
levels(gee.or$rn)[ levels(gee.or$rn)=="Ego.VillageID2" ] = "Village B"
levels(gee.or$rn)[ levels(gee.or$rn)=="Ego.VillageID5" ] = "Village C"
levels(gee.or$rn)[ levels(gee.or$rn)=="Ego.VillageID6" ] = "Village D"
levels(gee.or$rn)[ levels(gee.or$rn)=="Ego.VillageID8" ] = "Village E"

gee.or$text = paste0("(OR = ", round(gee.or$Estimate, 3), "; 95% CI [", round(gee.or$lwr, 3), ", ", round(gee.or$upr, 3), "])")

##
## save odds ratio and candidate models tables
##
write.xlsx(gee.or, file=file.path(results.dir, "GEE tables.xlsx"), sheetName="Odds ratios", row.names=F)
write.xlsx(subset(best.models.df, select=c(Model, qLik, delta, weight)), file=file.path(results.dir, "GEE tables.xlsx"), append=T, sheetName="Best models", row.names=F)

##
## plot odds ratios
##
ggplot(subset(gee.or, rn != "(Intercept)"),
       aes(x=rn, y=Estimate)) +
  geom_hline(yintercept=1, colour = "grey80", linetype=2) +
  geom_point(size=2) +
  geom_errorbar(aes(ymax=upr, ymin=lwr), width=.2, size=.3) +
  
  xlab("") +
  ylab("Odds ratio") +  # because of flipped plot
  
  scale_y_log10(breaks=c(0.01, 0.1, 1, 10), limits=c(0.1, 10)) +
  coord_flip() + 
  #facet_grid(.~Year) +
  
  theme_bw() +
  #eliminates baground, gridlines, and chart border
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    #,panel.border = element_blank()
    ,panel.background = element_blank()
  ) + 
  theme(axis.line = element_line(color = 'black')) +
  # hide legend
  theme(legend.position="none")

ggsave(filename=file.path(plots.dir, "fig 3 - odds ratios.png"), width=175, height=100, units="mm")
ggsave(filename=file.path(plots.dir, "fig 3 - odds ratios.pdf"), width=175, height=100, units="mm")

##
## what's the intercept?
##
intercept = gee.or[1,]
paste0("OR = ", round(intercept$Estimate, 3), " [", round(intercept$lwr, 3), ", ", round(intercept$upr, 3), "]")


############################################################################
## Unstandardised odds ratios (reported in Results text)
##
gee.logodds = data.frame(Estimate = coef(gee.best, full=T), confint(gee.best, full=T, digits=3))

# we want to know the increased odds of helping when r=0.5 compared to r=0, rather than the default interpretation, which is when r=1 compared to r=0
# therefore, half the log-odds for relatedness
gee.logodds[row.names(gee.logodds) == "r",] = gee.logodds[row.names(gee.logodds) == "r",] / 2

# put ORs and CIs in a data frame
gee.or = exp(gee.logodds)
gee.or = gee.logodds
names(gee.or) = c("Estimate", "lwr", "upr")

setDT(gee.or, keep.rownames = TRUE)[]  # convert row names to first column

# put ORs into a more sensible order (list is reverse because of coordinate flip in ggplot)
rn_order = rev(c("(Intercept)", "r", "r:HelpReceived", "Distance:r", "r:ChildrenInAlter", "r:Ego.Need.AFB", "r:Alter.Need.AFB",
                 "HelpReceived", "GiftGiven.ind", "Ego.Need.AFB", "Alter.Need.AFB",
                 "ChildrenInAlter", "AnyWivesInAlter", "AnyHusbandsInAlter",
                 "Distance", "RelativeWealthRank", "Ego.Size", "Alter.Size", 
                 "Ego.VillageID2", "Ego.VillageID5", "Ego.VillageID6", "Ego.VillageID8"))
gee.or$rn = factor(gee.or$rn, levels=rn_order)

levels(gee.or$rn)[ levels(gee.or$rn)=="Distance" ] = "Distance (km)"
levels(gee.or$rn)[ levels(gee.or$rn)=="RelativeWealthRank" ] = "Relative wealth rank"
levels(gee.or$rn)[ levels(gee.or$rn)=="Ego.Size" ] = "Size of helper household (ego)"
levels(gee.or$rn)[ levels(gee.or$rn)=="Alter.Size" ] = "Size of landowner household (alter)"
levels(gee.or$rn)[ levels(gee.or$rn)=="r" ] = "Relatedness (r)"
levels(gee.or$rn)[ levels(gee.or$rn)=="AnyWivesInAlter" ] = "Any wives in alter?"
levels(gee.or$rn)[ levels(gee.or$rn)=="AnyHusbandsInAlter" ] = "Any husbands in alter?"
levels(gee.or$rn)[ levels(gee.or$rn)=="ChildrenInAlter" ] = "Any children in alter?"
levels(gee.or$rn)[ levels(gee.or$rn)=="HelpReceived" ] = "Help reciprocated?"
levels(gee.or$rn)[ levels(gee.or$rn)=="Ego.Need.AFB" ] = "Helper household's need"
levels(gee.or$rn)[ levels(gee.or$rn)=="Alter.Need.AFB" ] = "Landowner household's need"
levels(gee.or$rn)[ levels(gee.or$rn)=="GiftGiven.ind" ] = "Gift given from ego to alter?"
levels(gee.or$rn)[ levels(gee.or$rn)=="Distance:r" ] = "r × Distance"
levels(gee.or$rn)[ levels(gee.or$rn)=="r:ChildrenInAlter" ] = "r × Any children in alter?"
levels(gee.or$rn)[ levels(gee.or$rn)=="r:Ego.Need.AFB" ] = "r × Helper household's need?"
levels(gee.or$rn)[ levels(gee.or$rn)=="r:Alter.Need.AFB" ] = "r × Landowner household's need?"
levels(gee.or$rn)[ levels(gee.or$rn)=="r:HelpReceived" ] = "r × Help reciprocated?"
levels(gee.or$rn)[ levels(gee.or$rn)=="Ego.VillageID2" ] = "Village B"
levels(gee.or$rn)[ levels(gee.or$rn)=="Ego.VillageID5" ] = "Village C"
levels(gee.or$rn)[ levels(gee.or$rn)=="Ego.VillageID6" ] = "Village D"
levels(gee.or$rn)[ levels(gee.or$rn)=="Ego.VillageID8" ] = "Village E"

gee.or$text = paste0("(log odds = ", round(gee.or$Estimate, 3), "; 95% CI [", round(gee.or$lwr, 3), ", ", round(gee.or$upr, 3), "])")

##
## save odds ratio and candidate models tables
##
write.xlsx(gee.or, file=file.path(results.dir, "GEE tables.xlsx"), sheetName="Odds ratios", row.names=F)
write.xlsx(subset(best.models.df, select=c(Model, qLik, delta, weight)), file=file.path(results.dir, "GEE tables.xlsx"), append=T, sheetName="Best models", row.names=F)


############################################################################
## Function to calculate 95% CIs for GEE models
## adapted from: https://danieljhocking.wordpress.com/2012/07/25/plotting-95-confidence-bands-in-r/
##
m.terms = terms(gee.kin_ra_need_gift_int_2)

predict_gee = function(m, m.terms, new.data)
{
  mm.geeEX = model.matrix(m.terms, new.data)  # use the full model to create the design matrix, since can't run terms() on an averaged model
  new.data$HelpObserved = mm.geeEX %*% coef(m)
  
  tvar1.gee = diag(mm.geeEX %*% tcrossprod(aod::vcov(m), mm.geeEX))
  
  new.data = data.frame(
    new.data
    , tlo = new.data$HelpObserved - 2*sqrt(tvar1.gee)
    , thi = new.data$HelpObserved + 2*sqrt(tvar1.gee)
  )
  
  # keep log odds
  # new.data$HelpObserved_logodds = new.data$HelpObserved
  # new.data$tlo_logodds = new.data$tlo
  # new.data$thi_logodds = new.data$thi
  
  # transform log odds to probabilities
  new.data$HelpObserved = as.numeric(plogis(new.data$HelpObserved))
  new.data$tlo          = plogis(new.data$tlo)
  new.data$thi          = plogis(new.data$thi)
  return(new.data)
}

##
## to make the plots look pretty
##
plot_style = theme_bw() +
  #eliminates baground, gridlines, and chart border
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.background = element_blank()
    ,axis.text=element_text(size=12)
    ,axis.title=element_text(size=12)
    ,axis.text.x = element_text(angle = 45, hjust = 1)  # rotate x-axis labels
  ) +
  theme(legend.position="bottom", legend.title=element_blank())  # legends at the bottom, with no titles


############################################################################
## Plot model predictions
##
new.data = expand.grid(r=seq(from=0, to=0.5, length.out = 50),
                       ChildrenInAlter = 0:1,
                       # set these predictors to their means
                       Distance = mean(hh.all$Distance),
                       RelativeWealthRank = mean(hh.all$RelativeWealthRank),
                       Ego.Size = mean(hh.all$Ego.Size),
                       Alter.Size = mean(hh.all$Alter.Size),
                       AnyWivesInAlter = mean(hh.all$AnyWivesInAlter),
                       AnyHusbandsInAlter = mean(hh.all$AnyHusbandsInAlter),
                       ChildrenInAlter = mean(hh.all$ChildrenInAlter),
                       Ego.Need.AFB = mean(hh.all$Ego.Need.AFB),
                       Alter.Need.AFB = mean(hh.all$Alter.Need.AFB),
                       GiftGiven.ind = mean(hh.all$GiftGiven.ind),
                       HelpReceived = mean(hh.all$HelpReceived),
                       Ego.VillageID = unique(hh.all$Ego.VillageID),
                       # set response to zero for now
                       HelpObserved=0)

new.data = predict_gee(gee.best, m.terms, new.data)

new.data$ChildrenInAlter = factor(new.data$ChildrenInAlter)
levels(new.data$ChildrenInAlter) = c("No children alter", "Has children in alter")

plt.r_children = new.data %>% 
  filter(Ego.VillageID==1) %>% 
  ggplot(aes(x=r, y=HelpObserved)) +
  geom_line(aes(colour=factor(ChildrenInAlter))) +
  geom_ribbon(aes(ymin=tlo, ymax=thi, fill=factor(ChildrenInAlter)), alpha=0.2) +
  xlab("Coefficient of relatedness") +
  ylab("Probability of farm help") +
  
  #scale_x_log10(breaks=c(0.0001, 0.0039, 0.0078, 0.015, 0.031, 0.063, 0.125, 0.25, 0.5)) +  # plot relatedness on log scale with breaks at familiar points
  scale_x_continuous(breaks=c(0.0001, 0.063, 0.125, 0.25, 0.5)) +
  
  plot_style
#ggsave(filename="predicted farm help - gee - by relatedness and children in alter.png", width=300, height=120, units="mm")

##
## relatedness and help reciprocated
##
new.data = expand.grid(r = c(seq(from=0, to=0.5, length.out = 50), 0.25),
                       HelpReceived = 0:1,
                       # set these predictors to their means
                       Distance = mean(hh.all$Distance),
                       RelativeWealthRank = mean(hh.all$RelativeWealthRank),
                       Ego.Size = mean(hh.all$Ego.Size),
                       Alter.Size = mean(hh.all$Alter.Size),
                       AnyWivesInAlter = mean(hh.all$AnyWivesInAlter),
                       AnyHusbandsInAlter = mean(hh.all$AnyHusbandsInAlter),
                       ChildrenInAlter = mean(hh.all$ChildrenInAlter),
                       Ego.Need.AFB = mean(hh.all$Ego.Need.AFB),
                       Alter.Need.AFB = mean(hh.all$Alter.Need.AFB),
                       GiftGiven.ind = mean(hh.all$GiftGiven.ind),
                       ChildrenInAlter = mean(hh.all$ChildrenInAlter),
                       Ego.VillageID = unique(hh.all$Ego.VillageID),
                       # set response to zero for now
                       HelpObserved=0)

new.data = predict_gee(gee.best, m.terms, new.data)

new.data$HelpReceived = factor(new.data$HelpReceived)
levels(new.data$HelpReceived) = c("No help reciprocated", "Help reciprocated")

new.data %>% 
  filter(r == 0.25 & HelpReceived=="No help reciprocated")

plt.r_recip = new.data %>% 
  filter(Ego.VillageID==1) %>% 
  ggplot(aes(x=r, y=HelpObserved)) +
  geom_line(aes(colour=factor(HelpReceived))) +
  geom_ribbon(aes(ymin=tlo, ymax=thi, fill=factor(HelpReceived)), alpha=0.2) +
  xlab("Coefficient of relatedness") +
  ylab("Probability of farm help") +
  
  #scale_x_log10(breaks=c(0.0001, 0.0039, 0.0078, 0.015, 0.031, 0.063, 0.125, 0.25, 0.5)) +  # plot relatedness on log scale with breaks at familiar points
  scale_x_continuous(breaks=c(0.0001, 0.063, 0.125, 0.25, 0.5)) +
  
  plot_style

##
## relatedness and gifts
##
new.data = expand.grid(r=seq(from=0, to=0.5, length.out = 50),
                       GiftGiven.ind = 0:1,
                       # set these predictors to their means
                       Distance = mean(hh.all$Distance),
                       RelativeWealthRank = mean(hh.all$RelativeWealthRank),
                       Ego.Size = mean(hh.all$Ego.Size),
                       Alter.Size = mean(hh.all$Alter.Size),
                       AnyWivesInAlter = mean(hh.all$AnyWivesInAlter),
                       AnyHusbandsInAlter = mean(hh.all$AnyHusbandsInAlter),
                       ChildrenInAlter = mean(hh.all$ChildrenInAlter),
                       Ego.Need.AFB = mean(hh.all$Ego.Need.AFB),
                       Alter.Need.AFB = mean(hh.all$Alter.Need.AFB),
                       ChildrenInAlter = mean(hh.all$ChildrenInAlter),
                       HelpReceived = mean(hh.all$HelpReceived),
                       Ego.VillageID = unique(hh.all$Ego.VillageID),
                       # set response to zero for now
                       HelpObserved=0)

new.data = predict_gee(gee.best, m.terms, new.data)

new.data$GiftGiven.ind = factor(new.data$GiftGiven.ind)
levels(new.data$GiftGiven.ind) = c("No gifts", "Ego gave gifts")

plt.r_gift = new.data %>% 
  filter(Ego.VillageID==1) %>% 
  ggplot(aes(x=r, y=HelpObserved)) +
  geom_line(aes(colour=factor(GiftGiven.ind))) +
  geom_ribbon(aes(ymin=tlo, ymax=thi, fill=factor(GiftGiven.ind)), alpha=0.2) +
  xlab("Coefficient of relatedness") +
  ylab("Probability of farm help") +
  
  #scale_x_log10(breaks=c(0.0001, 0.0039, 0.0078, 0.015, 0.031, 0.063, 0.125, 0.25, 0.5)) +  # plot relatedness on log scale with breaks at familiar points
  scale_x_continuous(breaks=c(0.0001, 0.063, 0.125, 0.25, 0.5)) +
  
  plot_style

##
## relatedness and landowner's (alter's) need (for need values: 0, 1, 2)
##
new.data = expand.grid(r=seq(from=0, to=0.5, length.out = 50),
                       Alter.Need.AFB = 0:2,
                       # set these predictors to their means
                       Distance = mean(hh.all$Distance),
                       RelativeWealthRank = mean(hh.all$RelativeWealthRank),
                       Ego.Size = mean(hh.all$Ego.Size),
                       Alter.Size = mean(hh.all$Alter.Size),
                       AnyWivesInAlter = mean(hh.all$AnyWivesInAlter),
                       AnyHusbandsInAlter = mean(hh.all$AnyHusbandsInAlter),
                       ChildrenInAlter = mean(hh.all$ChildrenInAlter),
                       Ego.Need.AFB = mean(hh.all$Ego.Need.AFB),
                       ChildrenInAlter = mean(hh.all$ChildrenInAlter),
                       HelpReceived = mean(hh.all$HelpReceived),
                       GiftGiven.ind = mean(hh.all$GiftGiven.ind),
                       Ego.VillageID = unique(hh.all$Ego.VillageID),
                       # set response to zero for now
                       HelpObserved=0)

new.data = predict_gee(gee.best, m.terms, new.data)

new.data$Alter.Need.AFB = factor(new.data$Alter.Need.AFB)
levels(new.data$Alter.Need.AFB) = c("No need", "Medium need", "High need")

plt.r_need = new.data %>% 
  filter(Ego.VillageID==1) %>% 
  ggplot(aes(x=r, y=HelpObserved)) +
  geom_line(aes(colour=factor(Alter.Need.AFB))) +
  geom_ribbon(aes(ymin=tlo, ymax=thi, fill=factor(Alter.Need.AFB)), alpha=0.2) +
  xlab("Coefficient of relatedness") +
  ylab("Probability of farm help") +
  
  #scale_x_log10(breaks=c(0.0001, 0.0039, 0.0078, 0.015, 0.031, 0.063, 0.125, 0.25, 0.5)) +  # plot relatedness on log scale with breaks at familiar points
  scale_x_continuous(breaks=c(0.0001, 0.063, 0.125, 0.25, 0.5)) +
  
  plot_style

##
## relatedness and distance
##
new.data = expand.grid(r=seq(from=0, to=0.5, length.out = 50),
                       Distance = c(0, mean(hh.all$Distance), max(hh.all$Distance)),
                       # set these predictors to their means
                       RelativeWealthRank = mean(hh.all$RelativeWealthRank),
                       Ego.Size = mean(hh.all$Ego.Size),
                       Alter.Size = mean(hh.all$Alter.Size),
                       AnyWivesInAlter = mean(hh.all$AnyWivesInAlter),
                       AnyHusbandsInAlter = mean(hh.all$AnyHusbandsInAlter),
                       ChildrenInAlter = mean(hh.all$ChildrenInAlter),
                       Ego.Need.AFB = mean(hh.all$Ego.Need.AFB),
                       Alter.Need.AFB = mean(hh.all$Alter.Need.AFB),
                       ChildrenInAlter = mean(hh.all$ChildrenInAlter),
                       HelpReceived = mean(hh.all$HelpReceived),
                       GiftGiven.ind = mean(hh.all$GiftGiven.ind),
                       Ego.VillageID = unique(hh.all$Ego.VillageID),
                       # set response to zero for now
                       HelpObserved=0)

new.data = predict_gee(gee.best, m.terms, new.data)

new.data$Distance = factor(new.data$Distance)
levels(new.data$Distance) = c("Close neighbours", "Mean distance", "Furthest")

plt.r_dist = new.data %>% 
  filter(Ego.VillageID==1) %>% 
  ggplot(aes(x=r, y=HelpObserved)) +
  geom_line(aes(colour=factor(Distance))) +
  geom_ribbon(aes(ymin=tlo, ymax=thi, fill=factor(Distance)), alpha=0.2) +
  xlab("Coefficient of relatedness") +
  ylab("Probability of farm help") +

  #scale_x_log10(breaks=c(0.0001, 0.0039, 0.0078, 0.015, 0.031, 0.063, 0.125, 0.25, 0.5)) +  # plot relatedness on log scale with breaks at familiar points
  scale_x_continuous(breaks=c(0.0001, 0.063, 0.125, 0.25, 0.5)) +
  
  plot_style

##
## Save the plots
##
# first give them titles
plt.r_children = plt.r_children + ggtitle("(a)")
plt.r_recip    = plt.r_recip    + ggtitle("(b)")
plt.r_gift     = plt.r_gift     + ggtitle("(c)")
plt.r_need     = plt.r_need     + ggtitle("(d)")
plt.r_dist     = plt.r_dist     + ggtitle("(e)")

png(filename = file.path(plots.dir, "fig 4 - predicted  farm labour.png"), height=20, width=25, units = "cm", res=300)
grid.arrange(plt.r_children, plt.r_recip, plt.r_gift, plt.r_need, plt.r_dist,
             ncol=3, nrow=2)
dev.off()

pdf(file = file.path(plots.dir, "fig 4 - predicted  farm labour.pdf"), height=7.9, width=9.8)
grid.arrange(plt.r_children, plt.r_recip, plt.r_gift, plt.r_need, plt.r_dist,
             ncol=3, nrow=2)
dev.off()

## distance stats
round( mean(hh.all$Distance), 3 )
round( max(hh.all$Distance), 3 )
