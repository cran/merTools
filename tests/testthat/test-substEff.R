# Test substantive effects
library(lme4)
set.seed(157)

# Test all user parameters for REimpact----
#context("Test all user parameters for REimpact")

test_that("REimpact parameters are respected", {
  skip_on_cran()
  g1 <- lmer(y ~ lectage + studage + (1|d) + (1|s), data=InstEval)
  #Warning is about %dopar% call in predictInterval
  zed <- suppressWarnings(REimpact(g1, newdata = InstEval[9:12, ], groupFctr = "d", n.sims = 50,
                  include.resid.var = TRUE))
  expect_identical(names(zed), c("case", "bin", "AvgFit", "AvgFitSE", "nobs"))
  zed2 <- REimpact(g1, newdata = InstEval[9:12, ], groupFctr = "s", n.sims = 50,
                  include.resid.var = TRUE)
  expect_equal(nrow(zed), 3 * nrow(InstEval[9:12, ]))
  expect_false(all(zed$AvgFit == zed2$AvgFit))
  expect_false(all(zed$AvgFitSE == zed2$AvgFitSE))
  expect_identical(names(zed2), c("case", "bin", "AvgFit", "AvgFitSE", "nobs"))
  zed <- REimpact(g1, newdata = InstEval[9:12, ], groupFctr = "d", breaks = 5,
                  n.sims = 50, include.resid.var = TRUE)
  expect_equal(nrow(zed), 5 * nrow(InstEval[9:12, ]))
})

test_that("REimpact respects passed values for predictInterval", {
  skip_on_cran()
  skip_on_travis()
  d <- expand.grid(fac1=LETTERS[1:5], grp=factor(1:30),
                   obs=1:100)
  suppressMessages({
    d$y <- simulate(~fac1+(1|grp),family = gaussian,
                    newdata=d,
                    newparams=list(beta=c(2,1,3,4,7), theta=c(.25),
                                   sigma = c(.23)))[[1]]
  })

  subD <- d[sample(row.names(d), 1000),]

  g1 <- lmer(y ~ fac1 + (1|grp), data=subD)
  zed <- REimpact(g1, newdata = subD[23:25, ], groupFctr = "grp", breaks = 5,
                  include.resid.var = FALSE, n.sims = 100, level = 0.8)
  zed2 <- REimpact(g1, newdata = subD[23:25, ], groupFctr = "grp", breaks = 5,
                   n.sims = 500, include.resid.var = TRUE, level = 0.99)
  # expect_true(all(zed2$AvgFitSE > zed$AvgFitSE))
  expect_true(!all(zed2$AvgFit > zed$AvgFit))
  expect_identical(names(zed), c("case", "bin", "AvgFit", "AvgFitSE", "nobs"))
  expect_identical(names(zed2), c("case", "bin", "AvgFit", "AvgFitSE", "nobs"))

})

# Test for slopes, intercepts, and combinations----
#context("Test for slopes, intercepts, and combinations")

test_that("Multiple terms can be accessed", {
  skip_on_cran()
  data(grouseticks)
  grouseticks$HEIGHT <- scale(grouseticks$HEIGHT)
  grouseticks <- merge(grouseticks, grouseticks_agg[, 1:3], by = "BROOD")
  grouseticks$TICKS_BIN <- ifelse(grouseticks$TICKS >=1, 1, 0)
  # GLMER 3 level + slope
  form <- TICKS_BIN ~ YEAR + HEIGHT + (1 + HEIGHT|BROOD) + (1|LOCATION) + (1|INDEX)
  suppressMessages({
    glmer3LevSlope  <- glmer(form, family="binomial",data=grouseticks,
                             control = glmerControl(optimizer="bobyqa",
                                                    optCtrl=list(maxfun = 1e5)))
  })

  # This is the same issue of zero mean zero variance in the predict interval call
  zed1 <- suppressWarnings(REimpact(glmer3LevSlope, newdata = grouseticks[5, ], groupFctr = "BROOD",
                  term = "HEIGHT", n.sims = 500,
                  include.resid.var = FALSE, breaks = 4, type = "probability"))
  # This is the same issue of zero mean zero variance in the predict interval call
  zed2 <- suppressWarnings(REimpact(glmer3LevSlope, newdata = grouseticks[5, ], groupFctr = "BROOD",
                   term = "Intercept",
                  n.sims = 500,
                  include.resid.var = FALSE, breaks = 4, type = "probability"))
  # This is the same issue of zero mean zero variance in the predict interval call
  zed4 <- suppressWarnings(REimpact(glmer3LevSlope, newdata = grouseticks[5, ], groupFctr = "LOCATION",
                   n.sims = 500,
                   include.resid.var = FALSE, breaks = 4))

  expect_true(all(zed4$AvgFit < zed2$AvgFit))
  expect_true(all(zed4$AvgFit < zed1$AvgFit))
  expect_false(identical(zed1, zed2))
  expect_false(identical(zed1, zed2))
  # No longer an error after revision 0.2.3
  # expect_error(zed3 <- suppressWarnings(REimpact(glmer3LevSlope, newdata = grouseticks[5, ], groupFctr = "BROOD",
  #                  n.sims = 500,
  #                  include.resid.var = FALSE, breaks = 4)), "Must specify which")
  # Don't think we need this ... it throws an subsetting error
  expect_error(zed5 <- suppressWarnings(REimpact(glmer3LevSlope, newdata = grouseticks[5, ], groupFctr = "LOCATION",
                   term = "HEIGHT",
                   n.sims = 500,
                   include.resid.var = FALSE, breaks = 4)), "undefined columns selected")

})

# Custom breaks----
#context("Custom breaks")

test_that("Custom breakpoints can be set", {
  skip_on_cran()
  g1 <- lmer(y ~ lectage + studage + (1|d) + (1|s), data=InstEval)
  zed <- REimpact(g1, newdata = InstEval[9, ], breaks = c(0, 10, 50, 90, 100),
                  groupFctr = "d", n.sims = 50,
                  include.resid.var = TRUE)
  zed2 <- REimpact(g1, newdata = InstEval[9, ], breaks = c(1, 20, 40, 60, 80, 100),
                   groupFctr = "d", n.sims = 50,
                   include.resid.var = TRUE)
  zed3 <- REimpact(g1, newdata = InstEval[9, ], breaks = 5,
                   groupFctr = "d", n.sims = 50,
                   include.resid.var = TRUE)
  expect_false(nrow(zed) == nrow(zed2))
  expect_gt(sd(zed$nobs), sd(zed2$nobs))
  expect_gt(mean(zed$nobs), mean(zed2$nobs))
  expect_equal(zed3$nobs, zed2$nobs, tolerance = .05)
})
