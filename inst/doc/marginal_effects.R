## ----setup, echo = FALSE, message=FALSE, warning=FALSE, results='hide', cache=FALSE----
library(ggplot2); library(knitr); library(merTools)
knitr::opts_chunk$set(
  cache=FALSE,
  comment="#>",
  collapse=TRUE,
  echo = TRUE
)

## ------------------------------------------------------------------------
fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
mfx <- REmargins(merMod = fm1, newdata = sleepstudy[1:10,])
head(mfx)

## ------------------------------------------------------------------------
ggplot(mfx) + aes(x = breaks, y = fit_Subject, group = case) +
  geom_line() +
  facet_wrap(~term)


