library(data.table)
library(tidyverse)

##
## set up output folders
##
# where stuff is
data.dir = "data"
plots.dir = "plots"
models.dir = "models"
results.dir = "results"

# create folders if needed (except data folder: if that's missing, you can't do much more here)
if (!dir.exists(plots.dir))    # plots
  dir.create(plots.dir)
if (!dir.exists(models.dir))   # models
  dir.create(models.dir)
if (!dir.exists(results.dir))  # results
  dir.create(results.dir)


# load data
hh.all = read_csv(file.path(data.dir, "mosuo dyads.csv")) %>% setDT()


#################################################################
## Calculate relative need
## - producer:consumer ratio
## ... where consumers are the number of people younger than (age at first birth / age of youngest observed farm helper)
##
# calculate need: no. dependents / no. producers
hh.all = hh.all[, Ego.Need.AFB    := Ego.NumChildren     / Ego.NumAdults]
hh.all = hh.all[, Alter.Need.AFB  := Alter.NumChildren   / Alter.NumAdults]


#################################################################
## Reciprocated farm help
##
# figure out whether alter reciprocated ego's farm help
help.received = subset(hh.all, HelpObserved==1, select=c(Ego.HH, Alter.HH, HelpObserved))  # keep only instances where alters received help from egos
setnames(help.received, c("Alter.HH", "Ego.HH", "HelpReceived"))                           # alters become egos (because we want to know who received help from whom)
setkey(help.received, Ego.HH, Alter.HH)
setkey(hh.all, Ego.HH, Alter.HH)
hh.all = help.received[hh.all]                  # merge help received into main table
hh.all[is.na(HelpReceived), HelpReceived := 0]  # remove NAs
rm(help.received)
