library(readr)
dat <- read_csv("C:/Users/mgj06/OneDrive/Documents/grad/decomp2023NAfix.csv")

dat$site <- as.factor(dat$Site)
dat$trt <- as.factor(dat$Trt)
dat$trap <- as.factor(dat$Trap)

library(lme4)
# install.packages("blmeco")
library(blmeco)
# install.packages("emmeans")
library(emmeans)
# install.packages("car")
library(car)
# install.packages("vegan")
library(vegan)
library(tidyr)
library(tidyverse)
# install.packages("devtools")
library(devtools)
# install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis)

##########################################################################
# Differences in community composition

dis.matrix <- vegdist(dat[,6:23], method = "bray", na.rm=TRUE)
dis.matrix


# run the nonmetric multidimensional scaling model-all

nmds.decom <- metaMDS(dis.matrix, trymax = 500, autotransform = TRUE, k = 2)
nmds.decom # stress is quality of fit
  #stress= 0.0831434
stressplot(nmds.decom)
plot(nmds.decom) # basic plot with no trt distinctions

# plot the NMDS model
ordiplot(nmds.decom, disp = "sites", type = "n", xlim = c(-1, 1), ylim = c(-1, 1))
points(nmds.decom, dis = "sites", select = which(dat$trt=="1"), pch = 17, cex = 2, col = "#73D055FF")
points(nmds.decom, dis = "sites", select = which(dat$trt=="2"), pch = 18, cex = 2, col = "#481567FF")
points(nmds.decom, dis = "sites", select = which(dat$trt=="3"), pch = 15, cex = 2, col = "#2D708EFF")
levels(dat$trt)
ordiellipse(nmds.decom, groups = dat$trt, draw = "lines", col = c("#73D055FF", "#481567FF", "#2D708EFF"), 
            lwd = 3, kind = "sd", conf = 0.90, label = FALSE)

legend("bottomright", legend = c("1", "2", "3"),
       pch = c(17, 18, 15), cex = 1.5, bty = "n", col = c("#73D055FF", "#481567FF", "#2D708EFF"))

## Test for differences in composition among trts

# PERMANOVA tests whether the group centroid of communities differs among groups
# in multivariate space (e.g. different community composition)
adonis2(dis.matrix ~ dat$trt, permutations = 999)
  #not significant

######################################################################
#USE THIS CODE NMDS by time period
#THIS IS WHERE I NEED HELP

periods <- read_csv("C:/Users/mgj06/OneDrive/Documents/grad/decomp2023final.csv")
periods$site <- as.factor(periods$Site)
periods$trt <- as.factor(periods$Trt)
periods$trap <- as.factor(periods$Trap)
periods$datecollected <- as.factor(periods$DateCollected)

#add column for month
table(periods$DateCollected)


periods$month <- month(mdy(periods$DateCollected))
table(periods$month)
class(periods$month)
periods$month <- as.factor(periods$month)

periods$summer <- ifelse(periods$month=="8" | periods$month=="9", "late", "early")
periods$summer <- as.factor(periods$summer) 

periods$trt_summer <- interaction(periods$trt, periods$summer)
table(periods$trt_summer)
#matrix
dis.matrix <- vegdist(periods[,6:23], method = "bray", na.rm=TRUE)
dis.matrix


# run the nonmetric multidimensional scaling model-all

nmds.decom <- metaMDS(dis.matrix, trymax = 500, autotransform = TRUE, k = 2)
nmds.decom # stress is quality of fit
#stress= 0.0831434
stressplot(nmds.decom)
plot(nmds.decom) # basic plot with no trt distinctions

#plot
ordiplot(nmds.decom, disp = "sites", type = "n", xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5))
points(nmds.decom, dis = "sites", select = which(periods$trt=="1" & periods$summer=="early"), pch = 0, cex = 2, col = "#08f590")
points(nmds.decom, dis = "sites", select = which(periods$trt=="1" & periods$summer=="late"), pch = 15, cex = 2, col = "#08f590")
points(nmds.decom, dis = "sites", select = which(periods$trt=="2" & periods$summer=="early"), pch = 1, cex = 2, col = "#f5d608")
points(nmds.decom, dis = "sites", select = which(periods$trt=="2" & periods$summer=="late"), pch = 16, cex = 2, col = "#f5d608")
points(nmds.decom, dis = "sites", select = which(periods$trt=="3" & periods$summer=="early"), pch = 2, cex = 2, col = "#0898f5")
points(nmds.decom, dis = "sites", select = which(periods$trt=="3" & periods$summer=="late"), pch = 17, cex = 2, col = "#0898f5")
levels(periods$trt)
levels(periods$summer)
ordiellipse(nmds.decom, groups = periods$trt_summer, draw = "lines", col = c("#08f590", "#f5d608", "#0898f5"), 
            lwd = 3, kind = "sd", conf = 0.90, label = FALSE)

legend("bottomright", legend = c("1", "2", "3"),
       pch = c(17, 18, 15), cex = 1.5, bty = "n", col = c("#08f590", "#f5d608", "#0898f5"))

adonis2(dis.matrix ~ periods$trt * periods$summer, permutations = 999)
pairwise.adonis(periods$trt , periods$summer)
#help with this pairwise not working
######################################################################


