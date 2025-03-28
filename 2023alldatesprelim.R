library(readr)
decomp2023 <- read_csv("C:/Users/mgj06/OneDrive/Documents/grad/decomp2023final.csv")

str(decomp2023)
decomp2023$site <- as.factor(decomp2023$Site)
decomp2023$trt <- as.factor(decomp2023$Trt)
decomp2023$trap <- as.factor(decomp2023$Trap)
str(decomp2023)
decomp2023$DateCollected <- as.factor(decomp2023$DateCollected)


# install.packages("lme4")
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
#install.packages("svglite")
library(svglite)

# Follow this formula
  # In the formula after Trt and before + (1|site/trap), add "* DateCollected"
  # glmer(TotalDecomp ~ trt * DateCollected + (1|site/trap), family = poisson, data= decomp2023)
  # Then for emmeans, use the term "Trt:DateCollected"



################################################################



# Total number of decomposers
hist(decomp2023$TotalDecomp)
boxplot(decomp2023$TotalDecomp ~ decomp2023$trt)

total.decompstat <- glmer(TotalDecomp ~ trt + 
                            (1|site/trap), family = poisson, data= decomp2023)
summary(total.decompstat)
plot(total.decompstat)
qqnorm(resid(total.decompstat))
qqline(resid(total.decompstat))
Anova(total.decompstat)
emmeans(total.decompstat, pairwise ~ trt)
  

# Collembola
hist(decomp2023$O.Collembola)
boxplot(decomp2023$O.Collembola ~ decomp2023$trt)

total.coll <- glmer(O.Collembola ~ trt + (1|site/trap), family = poisson, data = decomp2023)
summary(total.coll)
plot(total.coll)
qqnorm(resid(total.coll))
qqline(resid(total.coll))
Anova(total.coll)
emmeans(total.coll, pairwise ~ trt)
  #no significant differences in total collembola (order) abundance

# Isopoda
hist(decomp2023$O.Isopoda)
boxplot(decomp2023$O.Isopoda ~ decomp2023$trt)

total.iso <- glmer(O.Isopoda ~ trt + (1|site/trap), family= poisson, data = decomp2023)
summary(total.iso)
plot(total.iso)
qqnorm(resid(total.iso))
qqline(resid(total.iso))
Anova(total.iso)
emmeans(total.iso, pairwise ~ trt)
  #significant difference between Isopod abundance in trts 2 and 3 p=0.0425

# Julida
hist(decomp2023$O.Julida)
boxplot(decomp2023$O.Julida ~ decomp2023$trt)

total.jul <- glmer(O.Julida ~ trt + (1|site/trap), family= poisson, data = decomp2023)
summary(total.jul)
plot(total.jul)
qqnorm(resid(total.jul))
qqline(resid(total.jul))
Anova(total.jul)
emmeans(total.jul, pairwise ~ trt)
  #plots all bad fit, no sig

# Polydesmida
hist(decomp2023$O.Polydesmida)
boxplot(decomp2023$O.Polydesmida ~ decomp2023$trt)

total.polydes <- glmer(O.Polydesmida ~ trt + (1|site/trap), family= poisson, data = decomp2023)
summary(total.polydes)
plot(total.polydes)
qqnorm(resid(total.polydes))
qqline(resid(total.polydes))
Anova(total.polydes)
emmeans(total.polydes, pairwise ~ trt)
  #no significant difference in O. Polydesmida abundance between trts

#Staphylinidae 
hist(decomp2023$Staphylinidae)
boxplot(decomp2023$Staphylinidae ~ decomp2023$trt)

total.staph <- glmer(Staphylinidae ~ trt + (1|site/trap), family= poisson, data = decomp2023)
summary(total.staph)
plot(total.staph)
qqnorm(resid(total.staph))
qqline(resid(total.staph))
Anova(total.staph)
emmeans(total.staph, pairwise ~ trt)
  #no significant difference in Staphylinidae abundance between trts 

# Group richness
  # Recheck columns
decomp2023$rich <- specnumber(decomp2023[,6:30])

str(decomp2023)

hist(decomp2023$rich)
boxplot(decomp2023$rich ~ decomp2023$trt)

total.rich <- glmer(rich ~ trt * DateCollected + (1|site/trap), family = poisson, data = decomp2023)
summary(total.rich)
plot(total.rich)
qqnorm(resid(total.rich))
qqline(resid(total.rich))
  #weird Q-Q, what analysis should I do here instead?
Anova(total.rich)
emmeans(total.rich, pairwise ~ Trt:DateCollected)
  #no significant differences in richness between trts


# changing trt names for plot
levels(decomp2023$trt)[levels(decomp2023$trt)=="1"] <- "control"
levels(decomp2023$trt)[levels(decomp2023$trt)=="2"] <- "thinning"
levels(decomp2023$trt)[levels(decomp2023$trt)=="3"] <- "plus invasive removal"
str(decomp2023)

boxplot(rich ~ trt, data = decomp2023, col = c("gray60", "aquamarine3"),
        ylab = "Richness", xlab = "Treatment", cex.main = 2,
        cex.lab = 2.2, cex.axis = 2, ylim = c(-0,12), outline = FALSE)
stripchart(rich ~ trt, data = decomp2023, col = c("gray28", "aquamarine4"), vertical = TRUE,
           pch = 19, cex = 2, add = TRUE, method = "jitter", jitter = 0.2)

######################################################################################################

#same analyses, but now include time as a factor
#new dataset-final (changes number of rows i think)
# Total number of decomposers
df <- read_csv("C:/Users/mgj06/OneDrive/Documents/grad/decomp2023final.csv")

# changing time to factor
table(df$DateCollected)
df$DateCollected <- as.factor(df$DateCollected)

#total abundance
total.decompstat <- glmer(TotalDecomp ~ Trt * DateCollected + 
                            (1|Site/Trap), family = poisson, data= df)

Anova(total.decompstat)
emmeans(total.decompstat, pairwise ~ Trt:DateCollected)
# All sig different except  (Trt2.00826446280992 6/14/2023) - (Trt2.00826446280992 8/28/2023)

# new dataset with only July and August
juag <- df[df$DateCollected == "7/12/2023" | df$DateCollected == "7/31/2023" | df$DateCollected == "8/14/2023" | df$DateCollected == "8/28/2023", ]
table(juag$DateCollected)
boxplot(juag$TotalDecomp ~ juag$Trt * juag$DateCollected)

# Collembola
hist(decomp2023$O.Collembola)
boxplot(decomp2023$O.Collembola ~ decomp2023$trt)

total.coll <- glmer(O.Collembola ~ trt * DateCollected + (1|site/trap), family = poisson, data = decomp2023)
summary(total.coll)
plot(total.coll)
qqnorm(resid(total.coll))
qqline(resid(total.coll))
Anova(total.coll)
emmeans(total.coll, pairwise ~ trt:DateCollected)
#trt*date significant

# Isopoda
hist(decomp2023$O.Isopoda)
boxplot(decomp2023$O.Isopoda ~ decomp2023$trt)

total.iso <- glmer(O.Isopoda ~ trt * DateCollected + (1|site/trap), family= poisson, data = decomp2023)
summary(total.iso)
plot(total.iso)
qqnorm(resid(total.iso))
qqline(resid(total.iso))
Anova(total.iso)
emmeans(total.iso, pairwise ~ trt:DateCollected)
#trt*date significant

# Julida
hist(decomp2023$O.Julida)
boxplot(decomp2023$O.Julida ~ decomp2023$trt)

total.jul <- glmer(O.Julida ~ trt * DateCollected+ (1|site/trap), family= poisson, data = decomp2023)
summary(total.jul)
plot(total.jul)
qqnorm(resid(total.jul))
qqline(resid(total.jul))
Anova(total.jul)
emmeans(total.jul, pairwise ~ trt:DateCollected)
#trt*date significant
#plots still weird and bad here

# Polydesmida
hist(decomp2023$O.Polydesmida)
boxplot(decomp2023$O.Polydesmida ~ decomp2023$trt)

total.polydes <- glmer(O.Polydesmida ~ trt * DateCollected+ (1|site/trap), family= poisson, data = decomp2023)
summary(total.polydes)
plot(total.polydes)
qqnorm(resid(total.polydes))
qqline(resid(total.polydes))
Anova(total.polydes)
emmeans(total.polydes, pairwise ~ trt:DateCollected)
#trt*date significant
#plot weird

#Staphylinidae 
hist(decomp2023$Staphylinidae)
boxplot(decomp2023$Staphylinidae ~ decomp2023$trt)

total.staph <- glmer(Staphylinidae ~ trt*DateCollected + (1|site/trap), family= poisson, data = decomp2023)
summary(total.staph)
plot(total.staph)
qqnorm(resid(total.staph))
qqline(resid(total.staph))
Anova(total.staph)
emmeans(total.staph, pairwise ~ trt:DateCollected)
#trt*date significant


# Group richness
# Recheck columns
decomp2023$rich <- specnumber(decomp2023[,6:23])

str(decomp2023)

hist(decomp2023$rich)
boxplot(decomp2023$rich ~ decomp2023$trt)

total.rich <- glmer(rich ~ trt * DateCollected + (1|site/trap), family = poisson, data = decomp2023)
summary(total.rich)
plot(total.rich)
qqnorm(resid(total.rich))
qqline(resid(total.rich))
#weird Q-Q, what analysis should I do here instead?
Anova(total.rich)
emmeans(total.rich, pairwise ~ trt:DateCollected)
#still not significant


##################################################################################

#same analyses but by time period- early, middle, late summer?