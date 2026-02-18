#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
climate_data <- load(url("https://github.com/ASDS-TCD/StatsII_2026/blob/main/datasets/climateSupport.RData?raw=true"))

logit_add <- glm(
  choice ~ countries + sanctions,
  data   = climateSupport,
  family = binomial(link = "logit")
)

summary(logit_add)      # this replaces summary(all) from part 1


######################
# Problem 2a
######################
# build two hypothetical cases and look at linear predictor difference
lp_160_5  <- predict(logit_add, newdata = data.frame(
  countries = "160 of 192",
  sanctions = "5%"
), type = "link")

lp_160_15 <- predict(logit_add, newdata = data.frame(
  countries = "160 of 192",
  sanctions = "15%"
), type = "link")

log_OR_160_5_to_15 <- lp_160_15 - lp_160_5
OR_160_5_to_15     <- exp(log_OR_160_5_to_15)
OR_160_5_to_15


######################
# Problem 2b
######################
lp_20_5  <- predict(logit_add, newdata = data.frame(
  countries = "20 of 192",
  sanctions = "5%"
), type = "link")

lp_20_15 <- predict(logit_add, newdata = data.frame(
  countries = "20 of 192",
  sanctions = "15%"
), type = "link")

log_OR_20_5_to_15 <- lp_20_15 - lp_20_5
OR_20_5_to_15     <- exp(log_OR_20_5_to_15)
OR_20_5_to_15



######################
# Problem 2c
######################

# Make sure factors have the same levels as in the model
newdat_80_none <- data.frame(
  countries = factor("80 of 192", levels = levels(climateSupport$countries)),
  sanctions = factor("None",      levels = levels(climateSupport$sanctions))
)

p_80_none <- predict(logit_add, newdata = newdat_80_none, type = "response")
p_80_none



##################
# Problem 3
##################

# Additive model
logit_add <- glm(
  choice ~ countries + sanctions,
  data   = climateSupport,
  family = binomial(link = "logit")
)

# Model with interaction
logit_int <- glm(
  choice ~ countries * sanctions,
  data   = climateSupport,
  family = binomial(link = "logit")
)

# Likelihood ratio test
anova(logit_add, logit_int, test = "Chisq")



