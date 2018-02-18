##
## Witch reproductive success - load data
##
library(data.table)
library(ggplot2)
library(wesanderson)
library(plyr)

source("data functions/load-data-functions.r")
source("data functions/load-mosuo-data.r")


####################################################################################
## Load/manipulate data
##
hh.all = as.data.table(load_hh_dyads())

# load household covars and set up as a data table
hh.covars = as.data.table(load_hh_covars())
setnames(hh.covars, "HHID2012", "HH")
hh.covars = hh.covars[HH %in% unique(hh.all$Ego.HH)]

ethnic.groups = c("Mosuo", "Han")

mosuo.people = subset(mosuo.people, HHID2012 %in% unique(hh.all$Ego.HH) & EthnicGroup %in% ethnic.groups)
mosuo.adults = subset(mosuo.adults, HHID2012 %in% unique(hh.all$Ego.HH) & EthnicGroup %in% ethnic.groups)
mosuo.children = subset(mosuo.children, HHID2012 %in% unique(hh.all$Ego.HH) & EthnicGroup %in% ethnic.groups)

# calculate age categories
mosuo.adults$Age.cat = cut(mosuo.adults$Age, seq(0, 100, by=5), include.lowest=T, right=F)  # create age categories

##
## Flag witch houses
##
witch_houses = subset(hh.covars, select=c(HH, zhubo1, WealthRank))
setnames(witch_houses, "HH", "HHID2012")
witch_houses[, HHID2012 := as.character(HHID2012)]

# merge
setkey(witch_houses, HHID2012)
setkey(mosuo.adults, HHID2012)
mosuo.adults = witch_houses[mosuo.adults]
setkey(mosuo.children, HHID2012)
mosuo.children = witch_houses[mosuo.children]
setkey(mosuo.people, HHID2012)
mosuo.people = witch_houses[mosuo.people]

# calculate AFB
mosuo.adults$AFB = mosuo.adults$FirstBirth - mosuo.adults$BirthYear
mosuo.people$AFB = mosuo.people$FirstBirth - mosuo.people$BirthYear

mosuo.heads = subset(mosuo.adults, ID %in% hh.covars$headID)  # list of Mosuo heads of households

# load control variables
control.vars = as.data.table(read.csv(file.path(data.dir, "education n4470.csv")))
setnames(control.vars, "sortid", "ID")
setkey(control.vars, ID)
setkey(mosuo.people, ID)
mosuo.people = control.vars[mosuo.people]

# sort out missing kids
hh.all = hh.all[, Ego.NumChildren     := ifelse(is.na(Ego.NumChildren),     0, Ego.NumChildren)]
hh.all = hh.all[, Alter.NumChildren   := ifelse(is.na(Alter.NumChildren),   0, Alter.NumChildren)]


####################################################################################
## Effects of (reproductive age) sisters on witches and non-witches
##
##
## For each person, calculate their number of older and younger same-sex siblings
##
# narrow down relatedness matrix to only r >= 0.5 (may be slightly closer than 0.5 due to inbreeding)
sibs = subset(mosuo.r.df, r >= 0.5, select=c(Ego, Alter, r))

# add parent IDs and ages for each ego and alter
sibs.covars.names = c("HHID2012", "Sex", "Age", "MotherID", "fatherID")
sibs.covars = rbind( subset(mosuo.adults, select=c("ID", sibs.covars.names)),
                     subset(mosuo.children, select=c("ID", sibs.covars.names)) )
setkey(sibs.covars, ID)
## ... ego
setnames(sibs.covars, "ID", "Ego")
setkey(sibs, Ego)
sibs = sibs.covars[sibs]
setnames(sibs, sibs.covars.names, paste("Ego", sibs.covars.names, sep="."))
## ... alter
setnames(sibs.covars, "Ego", "Alter")
setkey(sibs, Alter)
sibs = sibs.covars[sibs]
setnames(sibs, sibs.covars.names, paste("Alter", sibs.covars.names, sep="."))

##
## Calculate number of same-sex sibs
##
# keep entries who share at least one parent and same household
# if alter is female (i.e. they're sisters), limit to 50 years or younger
sibs_same = subset(sibs, Alter.Age >= 15 & #ifelse(Alter.Sex=="f", Alter.Age <= 50, Alter.Age <= 200) & 
                     Ego.Sex==Alter.Sex & (Ego.MotherID==Alter.MotherID | Ego.fatherID==Alter.fatherID) &
                     Ego.HHID2012==Alter.HHID2012)

# for each ego, count how many older and younger alters (same-sex)
setkey(sibs_same, Ego)
sibs.older   = subset(sibs_same, Alter.Age > Ego.Age, select=c(Ego))
sibs.younger = subset(sibs_same, Alter.Age < Ego.Age, select=c(Ego))
## summarise
sibs.older.n   = as.data.table(table(sibs.older))
sibs.younger.n = as.data.table(table(sibs.younger))
## rename columns and convert IDs to int
setnames(sibs.older.n,   c("ID", "NumOlderSibs.SameSex"))
setnames(sibs.younger.n, c("ID", "NumYoungerSibs.SameSex"))
sibs.older.n[,   ID := as.integer(ID)]
sibs.younger.n[, ID := as.integer(ID)]

# merge sib numbers into people table
setkey(sibs.older.n, ID); setkey(sibs.younger.n, ID)
setkey(mosuo.people, ID)
mosuo.people = sibs.older.n[mosuo.people]
mosuo.people = sibs.younger.n[mosuo.people]

# clean up NAs
mosuo.people[, NumOlderSibs.SameSex := ifelse(is.na(NumOlderSibs.SameSex), 0, NumOlderSibs.SameSex)]
mosuo.people[, NumYoungerSibs.SameSex := ifelse(is.na(NumYoungerSibs.SameSex), 0, NumYoungerSibs.SameSex)]

mosuo.people[, NumSibs.SameSex := NumYoungerSibs.SameSex + NumOlderSibs.SameSex]

##
## Calculate number of opposite-sex sibs
##
# keep entries who share at least one parent
# if female sibling, limit her age to 50 or younger
sibs_diff = subset(sibs, Alter.Age >= 15 & #ifelse(Alter.Sex=="f", Alter.Age <= 50, Alter.Age <= 200) & 
                     Ego.Sex!=Alter.Sex & (Ego.MotherID==Alter.MotherID | Ego.fatherID==Alter.fatherID) &
                     Ego.HHID2012==Alter.HHID2012)

# for each ego, count how many older and younger alters (same-sex)
setkey(sibs_diff, Ego)
sibs.older   = subset(sibs_diff, Alter.Age > Ego.Age, select=c(Ego))
sibs.younger = subset(sibs_diff, Alter.Age < Ego.Age, select=c(Ego))
## summarise
sibs.older.n   = as.data.table(table(sibs.older))
sibs.younger.n = as.data.table(table(sibs.younger))
## rename columns and convert IDs to int
setnames(sibs.older.n,   c("ID", "NumOlderSibs.DiffSex"))
setnames(sibs.younger.n, c("ID", "NumYoungerSibs.DiffSex"))
sibs.older.n[,   ID := as.integer(ID)]
sibs.younger.n[, ID := as.integer(ID)]

# merge sib numbers into people table
setkey(sibs.older.n, ID); setkey(sibs.younger.n, ID)
setkey(mosuo.people, ID)
mosuo.people = sibs.older.n[mosuo.people]
mosuo.people = sibs.younger.n[mosuo.people]

# clean up NAs
mosuo.people[, NumOlderSibs.DiffSex := ifelse(is.na(NumOlderSibs.DiffSex), 0, NumOlderSibs.DiffSex)]
mosuo.people[, NumYoungerSibs.DiffSex := ifelse(is.na(NumYoungerSibs.DiffSex), 0, NumYoungerSibs.DiffSex)]

mosuo.people[, NumSibs.DiffSex := NumYoungerSibs.DiffSex + NumOlderSibs.DiffSex]


####################################################################################
## Add farm help instances for each person
##
# sum help instances for each alter house
helps = hh.all[, .(TotalHelp = sum(HelpInstances)), by=Alter.HH]
setnames(helps, "Alter.HH", "HHID2012")
helps[, HHID2012 := as.character(HHID2012)]
setkey(helps, HHID2012)
setkey(mosuo.people, HHID2012)
mosuo.people = helps[mosuo.people]

# clean up
rm(control.vars, helps, hh.covars, sibs, sibs_diff, sibs_same, sibs.covars, sibs.older, sibs.older.n, sibs.younger, sibs.younger.n, sibs.covars.names, witch_houses )


####################################################################################
## Save data for analyses
##
setkey(hh.all, Ego.HH, Alter.HH)  # make sure data are in correct order for GEE

# set up binary variables for whether/not gifts were given
hh.all = hh.all[, GiftGiven.ind := ifelse(TotalGifts.ind > 0, 1, 0)]
hh.all = hh.all[, ChildrenInAlter := ifelse(TotalChildrenInAlter > 0, 1, 0)]

hh.all = subset(hh.all, select=c(DyadID, Ego.HH, Alter.HH, HelpObserved, HelpInstances, Distance, RelativeWealthRank, Ego.Size, Alter.Size,
                                 r, AnyWivesInAlter, AnyHusbandsInAlter,
                                 Ego.NumAdults, Ego.NumChildren, Alter.NumAdults, Alter.NumChildren,
                                 ChildrenInAlter, GiftGiven.ind, TotalGifts.ind,
                                 Ego.VillageID))

hh.all = na.omit(hh.all)  # get rid of dyads with missing data (otherwise GEE won't run)

# restrict analyses to working on other households' farms
hh.all = subset(hh.all, Ego.HH != Alter.HH)

farm.2012.fall = load_farm_help_2012()  # for finding the age of the youngest person observed working on a farm

# find ages of youngest person observed helping on a farm and youngest age at first birth
youngest.help = min(farm.2012.fall$age, na.rm=T)
#youngest.birth = min(mosuo.people$AFB, na.rm=T)  # this is 15y, which has already been calculated in the Num.Children column of hh.all, so ignore here

##
## calculate no. dependents in each household based on age of youngest farm helper
##
hhs = subset(mosuo.people, i.Age < youngest.help)$HHID2012           # list of all houses for children
hhs = hhs[nchar(hhs)==4]                # get rid of invalid entries
hh_sizes = as.data.table( table(hhs) )  # count number of children in each house
hh_sizes[, hhs := as.integer(hhs)]      # convert to integer
setkey(hh_sizes, hhs)
## prepare for merge - ego
setnames(hh_sizes, c("Ego.HH", "Ego.NumDependents"))
setkey(hh.all, Ego.HH)
hh.all = hh_sizes[hh.all]
## ... and alter
setnames(hh_sizes, c("Alter.HH", "Alter.NumDependents"))
setkey(hh.all, Alter.HH)
hh.all = hh_sizes[hh.all]

rm(hhs, hh_sizes, farm.2012.fall)

# set NAs to zero
hh.all = hh.all[, Ego.NumDependents   := ifelse(is.na(Ego.NumDependents),   0, Ego.NumDependents)]
hh.all = hh.all[, Alter.NumDependents := ifelse(is.na(Alter.NumDependents), 0, Alter.NumDependents)]

# calculate need: no. dependents / no. producers
hh.all = hh.all[, Ego.Need.AFB    := Ego.NumChildren     / Ego.NumAdults]
hh.all = hh.all[, Alter.Need.AFB  := Alter.NumChildren   / Alter.NumAdults]

# calcaulte differences in related need for each dyad: if alter's need > ego's need, the number will be larger
# hh.all$NeedDiff.AFB  = hh.all$Alter.Need.AFB  - hh.all$Ego.Need.AFB

##
## save
##
hh.anon = hh.all %>% 
  select(DyadID, Ego.HH, Alter.HH, HelpObserved, Distance, RelativeWealthRank, 
         r, AnyWivesInAlter, AnyHusbandsInAlter, ChildrenInAlter, GiftGiven.ind,
         TotalGifts.ind, HelpInstances,
         Ego.Need.AFB, Alter.Need.AFB)

hh.all = hh.all %>% 
  select(DyadID, Ego.HH, Alter.HH, Ego.VillageID, HelpObserved, Distance, RelativeWealthRank, Ego.Size, Alter.Size, 
         r, AnyWivesInAlter, AnyHusbandsInAlter, ChildrenInAlter, GiftGiven.ind, TotalGifts.ind, HelpInstances,
         Ego.NumChildren, Ego.NumAdults, Alter.NumChildren, Alter.NumAdults)

library(readr)
write_csv(hh.all, "data/mosuo dyads.csv")
write_csv(hh.anon, "data/mosuo dyads - anon.csv")
