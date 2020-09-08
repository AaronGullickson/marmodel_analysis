#' ---
#' title: "run_samplesize_tests.R"
#' author: ""
#' ---


#This program will draw samples of different sizes to see how this affects the 
#results of models

#This program is crunching data on a lot of very large datasets - it takes a 
#long time!


Sys.time()
set.seed(2)

# Load libraries and data -------------------------------------------------

library(here)
source(here("check_packages.R"))
source(here("useful_functions.R"))
load(here("output","real_unions.RData"))
load(here("output","potential_partners.RData"))
load(here("output","potential_swingers.RData"))


# Generate samples --------------------------------------------------------

#create samples with different sample sizes to see how much it affects things
#based on time trial

#I now want to create twenty samples of each sample size to get more reliable
#estimates. To do that I need to split things into two separate draws of 10 each
#for most sizes and four separate draws of 5 each for he 50 and 100 cases, 
#and then recombine after model estimation

markets.size3.1 <- mclapply(1:10, 
                          function(i) {createVariables(generateCouples(3,actual,men,women,
                                                                       "met2013",weight="perwt", 
                                                                       verbose=FALSE))})
markets.size3.2 <- mclapply(1:10, 
                            function(i) {createVariables(generateCouples(3,actual,men,women,
                                                                         "met2013",weight="perwt", 
                                                                         verbose=FALSE))})

markets.size5.1 <- mclapply(1:10, 
                          function(i) {createVariables(generateCouples(5,actual,men,women,
                                                                       "met2013",weight="perwt", 
                                                                       verbose=FALSE))})
markets.size5.2 <- mclapply(1:10, 
                            function(i) {createVariables(generateCouples(5,actual,men,women,
                                                                         "met2013",weight="perwt", 
                                                                         verbose=FALSE))})

markets.size10.1 <- mclapply(1:10, 
                          function(i) {createVariables(generateCouples(10,actual,men,women,
                                                                       "met2013",weight="perwt", 
                                                                       verbose=FALSE))})
markets.size10.2 <- mclapply(1:10, 
                             function(i) {createVariables(generateCouples(10,actual,men,women,
                                                                          "met2013",weight="perwt", 
                                                                          verbose=FALSE))})

markets.size20.1 <- mclapply(1:10, 
                             function(i) {createVariables(generateCouples(20,actual,men,women,
                                                                          "met2013",weight="perwt", 
                                                                          verbose=FALSE))})
markets.size20.2 <- mclapply(1:10, 
                             function(i) {createVariables(generateCouples(20,actual,men,women,
                                                                          "met2013",weight="perwt", 
                                                                          verbose=FALSE))})
markets.size50.1 <- mclapply(1:5, 
                             function(i) {createVariables(generateCouples(50,actual,men,women,
                                                                          "met2013",weight="perwt", 
                                                                          verbose=FALSE))})
markets.size50.2 <- mclapply(1:5, 
                             function(i) {createVariables(generateCouples(50,actual,men,women,
                                                                          "met2013",weight="perwt", 
                                                                          verbose=FALSE))})
markets.size50.3 <- mclapply(1:5, 
                             function(i) {createVariables(generateCouples(50,actual,men,women,
                                                                          "met2013",weight="perwt", 
                                                                          verbose=FALSE))})
markets.size50.4 <- mclapply(1:5, 
                             function(i) {createVariables(generateCouples(50,actual,men,women,
                                                                          "met2013",weight="perwt", 
                                                                          verbose=FALSE))})

# Run models --------------------------------------------------------------

formula_base <- formula(choice~I(ageh-agew)+I((ageh-agew)^2)+rexog.gs+
                          edcross_hs+edcross_sc+edcross_c+edhyper+edhypo+
                          strata(group))

#size 3
models.size3.1 <- mclapply(markets.size3.1, function(market) {
  clogit(formula_base, data=market)
})
models.size3.2 <- mclapply(markets.size3.2, function(market) {
  clogit(formula_base, data=market)
})
coef.size3 <- cbind(sapply(models.size3.1, coef), 
                     sapply(models.size3.2, coef))
se.size3 <- cbind(sapply(models.size3.1, function(model) {summary(model)$coef[,3]}),
                   sapply(models.size3.2, function(model) {summary(model)$coef[,3]}))
rm(markets.size3.1)
rm(markets.size3.2)
rm(models.size3.1)
rm(models.size3.2)

#size 5
models.size5.1 <- mclapply(markets.size5.1, function(market) {
  clogit(formula_base, data=market)
})
models.size5.2 <- mclapply(markets.size5.2, function(market) {
  clogit(formula_base, data=market)
})
coef.size5 <- cbind(sapply(models.size5.1, coef), 
                    sapply(models.size5.2, coef))
se.size5 <- cbind(sapply(models.size5.1, function(model) {summary(model)$coef[,3]}),
                  sapply(models.size5.2, function(model) {summary(model)$coef[,3]}))
rm(markets.size5.1)
rm(markets.size5.2)
rm(models.size5.1)
rm(models.size5.2)

#size 10
models.size10.1 <- mclapply(markets.size10.1, function(market) {
  clogit(formula_base, data=market)
})
models.size10.2 <- mclapply(markets.size10.2, function(market) {
  clogit(formula_base, data=market)
})
coef.size10 <- cbind(sapply(models.size10.1, coef), 
                    sapply(models.size10.2, coef))
se.size10 <- cbind(sapply(models.size10.1, function(model) {summary(model)$coef[,3]}),
                  sapply(models.size10.2, function(model) {summary(model)$coef[,3]}))
rm(markets.size10.1)
rm(markets.size10.2)
rm(models.size10.1)
rm(models.size10.2)

#size 20
models.size20.1 <- mclapply(markets.size20.1, function(market) {
  clogit(formula_base, data=market)
})
models.size20.2 <- mclapply(markets.size20.2, function(market) {
  clogit(formula_base, data=market)
})
coef.size20 <- cbind(sapply(models.size20.1, coef), 
                    sapply(models.size20.2, coef))
se.size20 <- cbind(sapply(models.size20.1, function(model) {summary(model)$coef[,3]}),
                  sapply(models.size20.2, function(model) {summary(model)$coef[,3]}))
rm(markets.size20.1)
rm(markets.size20.2)
rm(models.size20.1)
rm(models.size20.2)

#size 50
models.size50.1 <- mclapply(markets.size50.1, function(market) {
  clogit(formula_base, data=market)
})
models.size50.2 <- mclapply(markets.size50.2, function(market) {
  clogit(formula_base, data=market)
})
models.size50.3 <- mclapply(markets.size50.3, function(market) {
  clogit(formula_base, data=market)
})
models.size50.4 <- mclapply(markets.size50.4, function(market) {
  clogit(formula_base, data=market)
})
coef.size50 <- cbind(sapply(models.size50.1, coef), 
                     sapply(models.size50.2, coef),
                     sapply(models.size50.3, coef),
                     sapply(models.size50.4, coef))
se.size50 <- cbind(sapply(models.size50.1, function(model) {summary(model)$coef[,3]}),
                   sapply(models.size50.2, function(model) {summary(model)$coef[,3]}),
                   sapply(models.size50.3, function(model) {summary(model)$coef[,3]}),
                   sapply(models.size50.4, function(model) {summary(model)$coef[,3]}))
rm(markets.size50.1)
rm(markets.size50.2)
rm(markets.size50.3)
rm(markets.size50.4)
rm(models.size50.1)
rm(models.size50.2)
rm(models.size50.3)
rm(models.size50.4)


# Output results ----------------------------------------------------------

save(coef.size3, coef.size5, coef.size10, coef.size20, coef.size50,
     se.size3, se.size5, se.size10, se.size20, se.size50, 
     file=here("analysis","output","parameters_samplesize.RData"))

Sys.time()