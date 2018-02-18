##
## Descriptive stats
##
source("init.r")
#library(ggplot2)
library(tidyverse)
library(wesanderson)

##
## Sample size
##
length(unique(hh.all$Ego.HH))  # no. households
nrow(hh.all)  # no. dyads

# how many households were in a reciprocal relationship?
hh.all %>% 
  group_by(Ego.HH) %>% 
  summarise(recip = sum(HelpReceived)) %>% 
  ungroup() %>% 
  mutate(recip = ifelse(recip > 0, 1, 0)) %>% 
  select(-Ego.HH) %>% 
  table(.)

##
## Village-level descriptives
##
# src: https://stackoverflow.com/a/8189441
Mode <- function(x) {
  ux <- unique(x[x>0])
  ux[which.max(tabulate(match(x, ux)))]
}

village.hh = hh.all %>% 
  select(Ego.HH, Ego.VillageID, Ego.Size) %>% 
  distinct() %>% 
  group_by(Ego.VillageID) %>% 
  dplyr::summarise(N = n(),
                   size.mean = mean(Ego.Size),
                   size.sd   = sd(Ego.Size))

village.r = hh.all %>% 
  group_by(Ego.VillageID) %>% 
  dplyr::summarise(dist.mean = mean(Distance),
                   dist.sd   = sd(Distance),
                   gift.mode = Mode(TotalGifts.ind),
                   help.mode = Mode(HelpInstances))

# save descriptives table
village.hh %>% 
  left_join(village.r, by="Ego.VillageID") %>% 
  write_csv(file.path(results.dir, "Village descriptives.csv"))

# median gifts and help, for Table 2 caption
hh.all %>% 
  filter(TotalGifts.ind > 0) %>% 
  group_by(Ego.VillageID) %>% 
  summarise(mean = mean(TotalGifts.ind),
            median = median(TotalGifts.ind))

hh.all %>% 
  filter(HelpInstances > 0) %>% 
  group_by(Ego.VillageID) %>% 
  summarise(mean = mean(HelpInstances),
            median = median(HelpInstances))

##
## Relatedness
##
# keep only first instance of each dyad
hh.all.r = hh.all %>% 
  dplyr::select(DyadID, Ego.HH, Alter.HH, r, Ego.VillageID) %>% 
  group_by(DyadID) %>% 
  top_n(1, Alter.HH) %>% 
  arrange(Ego.HH, Alter.HH) %>% 
  setDT()

# put relatedness into bins
hh.all.r$r.bin = cut(hh.all.r$r, c(0, 0.0039, 0.0078, 0.015, 0.031, 0.063, 0.125, 0.25, 0.5, 1), right=F)
# hh.all.r$r.bin2 = cut(hh.all.r$r, c(0, 0.0039, 0.125, 0.25, 0.5, 1), right=F)

table(hh.all.r$r.bin) %>% 
  as.data.frame() %>% 
  write_csv(file.path(results.dir, "household relatedness.csv"))

ggplot(hh.all.r, aes(x=r.bin, group=as.factor(Ego.VillageID))) + 
  geom_bar(position="dodge", alpha=0.8, aes(fill=as.factor(Ego.VillageID))) +
  
  coord_cartesian(ylim=c(1, 63000)) +
  scale_y_log10(limits=c(1, 63000)) +
  
  scale_fill_manual(values = wes_palette("Zissou")) +
  xlab("Relatedness") +
  ylab("Number of households") +
  
  #facet_grid(. ~ Type) + 
  
  theme_bw() +
  #eliminates baground, gridlines, and chart border
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
  ) + theme(axis.line = element_line(color = 'black')) +
  # hide legend
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # show relatedness bins at 45 degree angle

ggsave(filename="fig 1 - relatedness.pdf", width=200, height=100, units="mm")
ggsave(filename="fig 1 - relatedness.png", width=200, height=100, units="mm")


##
## Distributions of relative need
##
needs = unique(subset(hh.all, select=c(Ego.HH, Ego.Need.AFB, Ego.VillageID)))

nrow( subset(needs, Ego.Need.AFB > 1) )  # how many households where kids outnumber adults?

ggplot(needs, aes(Ego.Need.AFB)) + 
  geom_histogram(binwidth=0.1) +
  
  # scale_y_continuous(limits=c(0, max(..count..)+10), expand=c(0,0)) +
  # scale_x_continuous(expand=c(0,0)) +
  
  xlab("Relative need of household") +
  ylab("Count") +
  
  theme_bw() +
  #eliminates baground, gridlines, and chart border
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.background = element_blank()
    ,axis.text=element_text(size=12)
    ,axis.title=element_text(size=12)
  ) 

ggsave(filename="fig 2 - relative need.pdf", width=100, height=100, units="mm")
ggsave(filename="fig 2 - relative need.png", width=100, height=100, units="mm")

##
## Are closer kin more likely to reciprocate farm help?
##
# ggplot(hh.all, aes(x=factor(HelpReceived), y=r)) +
#   geom_boxplot()
# 
# r_recip_test = broom::tidy( t.test(r ~ HelpReceived, data=hh.all) )
# 
# paste0("t(", round(r_recip_test$parameter,3), ")=", round(r_recip_test$estimate,3), 
#        " [", round(r_recip_test$conf.low,3), ", ", round(r_recip_test$conf.high,3), 
#        "]; P=", round(r_recip_test$p.value,3))

##
## plot correlations between predictors
##
# get upper triangle of the correlation matrix
# (source: http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization)
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

# calculate, summarise and melt correlation matrix
# cormat = d %>% 
#   select(-NumGifts, -ends_with(".z")) %>% 
#   cor(.) %>% 
#   round(., 2) %>% 
#   get_upper_tri(.) %>% 
#   reshape2::melt(.)

d = hh.all %>% 
  select(HelpObserved, Distance, RelativeWealthRank, Ego.Size, Alter.Size, 
  r, AnyWivesInAlter, AnyHusbandsInAlter, ChildrenInAlter, 
  HelpReceived, Ego.Need.AFB, Alter.Need.AFB, GiftGiven.ind)

cor_vars = c("Help observed?", "Distance", "Relative wealth", "Ego size", "Alter size",
             "Relatedness", "Any wives in alter?", "Any husbands in alter?", "Any children in alter?",
             "Help received?", "Ego relative need", "Alter relative need", "Gift given?")

# calculate correlations and p-values
cormats = d %>% 
  as.matrix(.) %>% 
  Hmisc::rcorr(.)

# format p-values
cormat.p = cormats$P %>% 
  round(., 2) %>% 
  get_upper_tri(.) %>% 
  reshape2::melt(.)

# format correlations and merge with p-values
cormat = cormats$r %>% 
  round(., 2) %>% 
  get_upper_tri(.) %>% 
  reshape2::melt(.) %>% 
  rename(r = value) %>% 
  left_join(cormat.p, by=c("Var1", "Var2")) %>% 
  rename(p = value)

# rename variables
cormat$Var1 = factor(cormat$Var1)
levels(cormat$Var1) = cor_vars
cormat$Var2 = factor(cormat$Var2)
levels(cormat$Var2) = cor_vars

# point out where p < 0.01 (rather than show it as p = 0)
cormat$p = as.character(cormat$p)
cormat$p = ifelse(cormat$p == "0", "< 0.01", cormat$p)

ggplot(data = cormat, aes(Var2, Var1, fill = p)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = r), size=3) +
  
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", na.value="grey75",
                       # midpoint = 0, limit = c(-1,1), space = "Lab", 
                       # name="Pearson\nCorrelation") +
                       midpoint = 0.05, limit = c(0, 0.15), space = "Lab", 
                       name="P-value") +
                       
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed() +
  xlab("") +
  ylab("")

ggsave(file.path(plots.dir, "predictors correlations.png"), height=200, width=200, units="mm")
ggsave(file.path(plots.dir, "predictors correlations.pdf"), height=200, width=200, units="mm")
