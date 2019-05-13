## ----setup, echo = FALSE, message=FALSE, warning=FALSE, results='hide'----
knitr::opts_chunk$set(
  cache=FALSE,
  comment="#>",
  collapse=TRUE, 
  echo=TRUE, 
  fig.width = 7
)
library(knitr); library(merTools)
amelia_eval <- "Amelia" %in% rownames(installed.packages())
amelia_uneval <- !amelia_eval

## ------------------------------------------------------------------------
data(hsb)

# Create a function to randomly assign NA values

add_NA <- function(x, prob){
  z <- rbinom(length(x), 1, prob = prob)
  x[z==1] <- NA
  return(x)
}

hsb$minority <- add_NA(hsb$minority, prob = 0.05)
table(is.na(hsb$minority))

hsb$female <- add_NA(hsb$female, prob = 0.05)
table(is.na(hsb$female))

hsb$ses <- add_NA(hsb$ses, prob = 0.05)
table(is.na(hsb$ses))

hsb$size <- add_NA(hsb$size, prob = 0.05)
table(is.na(hsb$size))


## ----impute, message=FALSE, eval = amelia_eval---------------------------
# Load imputation library
library(Amelia)
# Declare the variables to include in the imputation data
varIndex <- names(hsb)
# Declare ID variables to be excluded from imputation
IDS <- c("schid", "meanses")
# Imputate
impute.out <- amelia(hsb[, varIndex], idvars = IDS, 
                         noms = c("minority", "female"), 
                         m = 5)
summary(impute.out)

## ----boot, message=FALSE, eval = amelia_uneval---------------------------
#  # Amelia is not available so let's just boostrap resample our data
#  impute.out <- vector(mode = "list", 5)
#  
#  for (i in 1:5) {
#    impute.out[[i]] <- hsb[sample(nrow(hsb), nrow(hsb), replace = TRUE), ]
#  }
#  
#  # Declare the variables to include in the imputation data
#  summary(impute.out)

## ------------------------------------------------------------------------
fmla <- "mathach ~ minority + female + ses + meanses + (1 + ses|schid)"
mod <- lmer(fmla, data = hsb)
if(amelia_eval) {
  modList <- lmerModList(fmla, data = impute.out$imputations)
} else {
  # Use bootstrapped data instead
  modList <- lmerModList(fmla, data = impute.out)
}


## ------------------------------------------------------------------------
fixef(mod) # model with dropped missing
fixef(modList)

## ------------------------------------------------------------------------
VarCorr(mod) # model with dropped missing
VarCorr(modList) # aggregate of imputed models

## ------------------------------------------------------------------------
lapply(modList, fixef)

## ------------------------------------------------------------------------
fixef(modList[[1]])
fixef(modList[[2]])

## ------------------------------------------------------------------------
print(modList)

## ------------------------------------------------------------------------
summary(modList)

## ------------------------------------------------------------------------
fastdisp(modList)

## ------------------------------------------------------------------------
modelRandEffStats(modList)
modelFixedEff(modList)
VarCorr(modList)

## ------------------------------------------------------------------------
modelInfo(mod)

## ------------------------------------------------------------------------
lapply(modList, modelInfo)

## ------------------------------------------------------------------------
summary(modList)

## ------------------------------------------------------------------------
modelFixedEff(modList)

## ------------------------------------------------------------------------
ranef(modList)

