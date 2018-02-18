##
## Load processed Mosuo data
## -------------------------
##
## See Python code in 'mosuo data preparation.html' (in same directory as this file) to see how these files were created
##
## Running this file will create the following data.tables:
## - mosuo.people   : all people who were alive at the time of the census and living in the study villages
## - mosuo.adults   : all adults (aged >= 18) in mosuo.people
## - mosuo.children : all children (aged < 18) in mosuo.people
## - mosuo.gifts    : edge list of gift amounts from the individual-to-individual game
## - mosuo.r.df     : edge list of relatedness coefficients between individuals
## - mosuo.spouses  : edge list of who is spouse of whom
## - mosuo.covars   : subset of 'mosuo.people' containing variables used to create the individual-individual dyads table
##
#install.packages("data.table")
library(data.table)
require(Matrix)

if (!exists("data.dir")) {
  data.dir = "C:/Users/matthew.thomas/Google Drive/Anth/Mosuo/final/matt"  # "C:/Users/mgt/Dropbox/Anth/Projects/China/data"
}

people.file = "mosuo.csv"
gifts.file = "mosuo-edge-gifts.csv"
spouses.file = "mosuo-edge-spouses.csv"
r.file = "mosuo-adj-r.mtx"
ids.file = "mosuo-ids.csv"

covars = c("HHID2012", "familyid", "EthnicGroup", "Sex", "BirthYear", "ChildNum", "RSD2012", "Small.Village2012", "VillageID")

adult.age = 15


#################################
## Load people and clean the data
## Get subset of people who are adults, alive and in study villages
##
mosuo.people = data.table(read.csv( file.path(data.dir, people.file), stringsAsFactors=F ))
setkey(mosuo.people, ID)

mosuo.people$Age = 2012 - mosuo.people$BirthYear
mosuo.people[, HHID2012 := as.character(HHID2012)]

# convert small village into village ID (by taking first digit)
mosuo.people[, VillageID := substr(Small.Village2012, 1, 1)]
# convert back to integer
mosuo.people$VillageID = as.integer(mosuo.people$VillageID)

# convert residence into ID (first digit only)
mosuo.people[, Residence := substr(RSD2012, 1, 1)]
# convert back to integer
mosuo.people$Residence = as.factor(mosuo.people$Residence)

mosuo.people$TouristIncome = as.factor(mosuo.people$TouristIncome)

# villages 3 and 7 not included in census; 9 and 10 outside of study area
villages.to.exclude = c(3, 7, 9, 10)
#ethnic.groups = c("Mosuo", "Han")

# keep only adult-age, alive people who live in study area
mosuo.people = subset(mosuo.people, Alive2012==1 & !Small.Village2012 %in% villages.to.exclude)
# also remove people with missing value in their village
mosuo.people = mosuo.people[ !is.na(mosuo.people$Small.Village2012), ]

# split into kids and adults
mosuo.children = subset(mosuo.people, Age<adult.age)
mosuo.adults   = subset(mosuo.people, Age>=adult.age)

# covariates to merge into wide/long dataframes
mosuo.covars = subset(mosuo.people,  select=c("ID", covars))  # person covariates
setkey(mosuo.covars, ID)

rm(villages.to.exclude)

participants = unique(mosuo.people$ID)  # keep adults and children


#################################
## Load the other files and filter out any non-participants
##
mosuo.gifts = data.table(read.csv( file.path(data.dir, gifts.file) ))
mosuo.gifts = subset(mosuo.gifts, select=c("Ego", "Alter", "Amount"))
setkeyv(mosuo.gifts, c("Ego", "Alter"))
mosuo.gifts = unique(mosuo.gifts)  # remove duplicates
mosuo.gifts = subset(mosuo.gifts, Ego != Alter & Ego %in% participants & Alter %in% participants)  # remove gifts given to self (probably a coding error)

mosuo.spouses = data.table(read.csv( file.path(data.dir, spouses.file) ))
setnames(mosuo.spouses, c("Ego", "Alter"))
mosuo.spouses$IsSpouse = 1
setkeyv(mosuo.spouses, c("Ego", "Alter"))
mosuo.spouses = subset(mosuo.spouses, Ego %in% participants & Alter %in% participants)

mosuo.ids = data.table(read.csv( file.path(data.dir, ids.file), header=F ))
setnames(mosuo.ids, c("PersonID", "idx"))
setkey(mosuo.ids, PersonID)

# put known relatives in a dataframe
mosuo.r = readMM(file.path(data.dir, r.file))
mosuo.r.df = as.data.table(summary(mosuo.r))
setnames(mosuo.r.df, c("Ego_idx", "Alter_idx", "r"))

# copy original person IDs into the relatedness table
# ... for ego
setnames(mosuo.ids, c("Alter", "Alter_idx"))
setkey(mosuo.ids, Alter_idx)
setkey(mosuo.r.df, Alter_idx)
mosuo.r.df = mosuo.ids[mosuo.r.df]
# ... for alter
setnames(mosuo.ids, c("Ego", "Ego_idx"))
setkey(mosuo.ids, Ego_idx)
setkey(mosuo.r.df, Ego_idx)
mosuo.r.df = mosuo.ids[mosuo.r.df]

# set keys
setkeyv(mosuo.r.df, c("Ego", "Alter"))
setnames(mosuo.ids, c("PersonID", "idx"))  # reset names for mosuo.ids

# remove unused variables from relatedness data frame
mosuo.r.df[, Ego_idx := NULL]; mosuo.r.df[, Alter_idx := NULL];  

mosuo.r.df = subset(mosuo.r.df, Ego %in% participants & Alter %in% participants)
mosuo.ids = subset(mosuo.ids, PersonID %in% participants)

rm(people.file, gifts.file, spouses.file, r.file, ids.file, mosuo.r, mosuo.ids)
