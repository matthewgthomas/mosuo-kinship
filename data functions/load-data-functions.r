##
## Functions to load various Mosuo data files
## ------------------------------------------
##
## Before running, change the directories in the 'google.drive' variable
##
## Main useful functions:
## - load_wide_data() : returns a data.table of all individual-individual dyads, including data from the individual-to-individual game
## - load_wide_participant_data() : same as above but Egos are only people who participated in the individual-to-individual gift game
## - load_hh_dyads() : returns a data.frame of all household-household dyads, including aggregated counts of gifts (from both games) and farm work observations
## - load_hh_dyads_bivar() : same as above but contains only one row per dyad (and more columns, e.g. for gifts from ego to alter as well as alter to ego)
## - load_hh_covars() : returns a data.frame of household covariates (from 'mosuoHousholdinfo2014.txt')
## - load_ind_hh_gifts_raw() : returns a data.frame of individual-to-household gifts (from 'MosuoHouseholdGift_24Apr2015.csv')
## - load_farm_help_2011() : returns a data.frame of farm work observations, with variables re-coded and cleaned
## - load_farm_help_2012() : same as above but for 2012's observations
## 
## Supporting functions (you don't need to worry about these)
## - load_from_sqlite(file_type, dbname) : called by load_wide_data() and load_wide_participant_data() to import from 'mosuo.db'
##

# directories to use
google.drive = "C:/Users/mgt/Google Drive/Anth/Mosuo/final"
data.dir = file.path(google.drive, "matt")  #"C:/Users/mgt/Dropbox/Anth/Projects/China/data"
raw.data.dir = file.path(google.drive, "social_mosuo_all")


##################################################
## Main database-loader function
##
load_from_sqlite = function(file_type, dbname="mosuo.db") {
  require(RSQLite)
  require(data.table)
  
  # Set up database    
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv, dbname=file.path(data.dir, dbname))
  
  # check mosuo table exists
  #dbListTables(con)
  # read data table specified by 'file_type' parameter
  mosuo = as.data.table(dbReadTable(con, file_type))
  # don't need the database anymore
  dbDisconnect(con)
  
  return(mosuo)
}


##################################################
## Load from SQLite -- full database
##
load_wide_data = function() {
  return(load_from_sqlite("mosuo_wide"))
}

load_wide_participant_data = function() {
  return(load_from_sqlite("mosuo_wide_participants"))
}


##################################################
## Load gift and farm work data aggregated into households
##
load_hh_dyads = function() {
  hh.all = read.csv(file.path(data.dir, "all household dyads.csv"))
  return(hh.all)
}

load_hh_dyads_bivar = function() {
  hh.all = read.csv(file.path(data.dir, "all household dyads - bivariate.csv"))
  return(hh.all)
}


##################################################
## Load household covariates
##
load_hh_covars = function() {
  hh.covars = read.csv(file.path(raw.data.dir, "mosuoHousholdinfo2014.txt"), sep="\t")
  return(hh.covars)
}


##################################################
## Load individual-to-household gifts
##
# raw data
load_ind_hh_gifts_raw = function() {
  hh.gifts = read.csv(file.path(raw.data.dir, "MosuoHouseholdGift_24Apr2015.csv"), stringsAsFactors=F)
  return(hh.gifts)
}


##################################################
## Load farm help data
##
## Columns:
## - ObsYear : year you observed the work 
## - LandNo : ID of farm (coded in order observed)
## - IndivCode : ID of person (coded in order observed; not related to census IDs)
## - ID2007 : person's census ID (missing value if they weren't part of the 2007 census)
## - relationship : ... to the farm household (see body of function for codes)
## - relationtotal : a rearrangement of the relationship column (see body of function for codes)
##
load_farm_help_2011 = function() {
  farm.2011 = read.csv(file.path(raw.data.dir, "MosuoHelpingFarm2011_2012spring.csv"), stringsAsFactors=F)
  
  # re-code columns
  library(car)
  farm.2011$relationship = recode(farm.2011$relationship, "'0'='owner'; '1'='maternal kin'; '2'='paternal kin'; '3'='spouse';
                                  '4'='children'; '5'='others in spouse house'; '6'='other kin of spouse'; '7'='spouse of kin';
                                  '8'='in same village'; '9'='other relationship'; '10'='friends'; '11'='parent or sibling'")
  farm.2011$relationship = as.factor(farm.2011$relationship)
  
  farm.2011$relationtotal = recode(farm.2011$relationtotal, "'0'='owner'; '1'='maternal kin'; '2'='paternal kin'; '3'='spouse';
                                   '4'='other people in husband house'; '5'='other people in wife house or children';
                                   '6'='kin of husband'; '7'='kin of wife'; '8'='spouse of maternal kin';
                                   '9'='spouse of paternal kin'; '10'='neighbours'; '11'='other relation'")
  farm.2011$relationtotal = as.factor(farm.2011$relationtotal)
  
  # missing values in ID2007 should be NA
  farm.2011$ID2007 = recode(farm.2011$ID2007, "''=NA")
  farm.2011$ID2007 = as.integer(farm.2011$ID2007)
  
  return(farm.2011)
}

##
## Columns:
## - ID : person ID, coded in order of observation (not linked to census IDs)
## - date2012fall : date of the observation
## - group : farm ID, coded in order of observation
## - ID2007 : person's ID number in the census
## - ethnic : see below for codes
## - sex : see below for codes
## - age : helper's age
## - helper : relationship between helper and farm (see below for codes)
## - ifkin : =1 if any kind of relationship through blood or marriage (what about if =2?)
## - Owner : =1 if they own the land
##
load_farm_help_2012 = function() {
  farm.2012 = read.csv(file.path(raw.data.dir, "MosuoHelpingFarm2012fall.csv"), stringsAsFactors=F)
  
  # remove the coding text column at the end of the data frame
  farm.2012[, 11] = NULL
  
  # re-code columns
  library(car)
  ## ethnic group
  farm.2012$ethnic = car::recode(farm.2012$ethnic, "'1'='Mosuo'; '2'='Han'; '3'='Yi'; '4'='Tibet'; '5'='Pumi'; '6'='Zhuang'; '7'='Naxi'")
  farm.2012$ethnic = as.factor(farm.2012$ethnic)
  ## sex
  farm.2012$sex = car::recode(farm.2012$sex, "'0'='female'; '1'='male'")
  farm.2012$sex = as.factor(farm.2012$sex)
  ## relationship
  farm.2012$helper = car::recode(farm.2012$helper, "'0'='owner'; '1'='maternal kin'; '2'='paternal kin'; '3'='spouse'; '4'='neighbour';
                            '5'='other'; '6'='parents and children'; '7'='siblings'; '8'='spouse of close kin'; '9'='affine'; 'gs'='grandchildren';
                            else=NA")
  farm.2012$helper = as.factor(farm.2012$helper)
  
  # 'na' or missing values should be NA
  farm.2012$ID2007 = car::recode(farm.2012$ID2007, "'na'=NA")
  farm.2012$ID2007 = as.integer(farm.2012$ID2007)
  farm.2012$age = car::recode(farm.2012$age, "''=NA")
  farm.2012$age = as.integer(farm.2012$age)
  farm.2012$ifkin = car::recode(farm.2012$ifkin, "c('', 'na', '999' ,'2')=NA")
  farm.2012$ifkin = as.integer(farm.2012$ifkin)
  
  return(farm.2012)
}
