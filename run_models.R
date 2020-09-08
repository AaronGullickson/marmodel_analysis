#' ---
#' title: "run_models.R"
#' author: ""
#' ---



#these models are a bear to run, so I will run them in a script and then output
#the pooled results for graphical analysis.
Sys.time()

#set the seed to be able to reproduce results
set.seed(2)

# Load libraries, data, and functions -------------------------------------
library(here)
source(here("check_packages.R"))
source(here("useful_functions.R"))
load(here("output","real_unions.RData"))
load(here("output","potential_partners.RData"))
load(here("output","potential_swingers.RData"))

#number of replications and number of fake couples samples
nreps <- 5
nfakes <- 50

#instead of replicate, I will now use a lapply approach to generateCouples following advice here:
#https://stackoverflow.com/questions/19281010/simplest-way-to-do-parallel-replicate
#this allows me to enable parallel processing for the generation of markets

# Nested models -----------------------------------------------------------

cat("\nrunning nested models for metro areas")

#create markets for metro areas
markets <- mclapply(1:nreps, 
                    function(i) {createVariables(generateCouples(nfakes,actual,men,women,
                                                                 "met2013",weight="perwt", 
                                                                 verbose=FALSE))})
#create nested models of racial exogamy

formula_base <- formula(choice~I(ageh-agew)+I((ageh-agew)^2)+rexog.gs+strata(group))
formula_educ <- update(formula_base, .~.+edcross_hs+edcross_sc+edcross_c+edhyper+edhypo)
formula_immigration <- update(formula_educ, .~.+nexog+bendog+lendog)

model_base <- poolChoiceModel(formula_base, markets)
model_educ <- poolChoiceModel(formula_educ, markets)
model_immigration <- poolChoiceModel(formula_immigration, markets)

#garbage clean up to keep memory available
rm(markets)

# Alternate market models -------------------------------------------------

cat("\nrunning alternate market models")

#run fullest model on state and country markets

#an alternative education model for comparison to loglinear models without age controls
formula_educ2 <- formula(choice~rexog.gs+edcross_hs+edcross_sc+edcross_c+edhyper+edhypo+strata(group))

#states
markets_state <- mclapply(1:nreps, 
                          function(i) {createVariables(generateCouples(nfakes,actual,men,women,
                                                                       "statefip",weight="perwt", 
                                                                       verbose=FALSE))})
model_state <- poolChoiceModel(formula_immigration, markets_state)
model_state_educ <- poolChoiceModel(formula_educ2, markets_state)
rm(markets_state)

#country
markets_country <- mclapply(1:nreps, 
                            function(i) {createVariables(generateCouples(nfakes,actual,men,women,
                                                                         "country",weight="perwt", 
                                                                         verbose=FALSE))})
model_country <- poolChoiceModel(formula_immigration, markets_country)
model_country_educ <- poolChoiceModel(formula_educ2, markets_country)
rm(markets_country)

# Swinger models ----------------------------------------------------------

cat("\nrunning swinger models")

markets_swinger <- mclapply(1:nreps, 
                          function(i) {createVariables(generateCouples(nfakes,actual,
                                                                       men.swinger,women.swinger,
                                                                       "met2013",weight="perwt", 
                                                                       verbose=FALSE))})

#run fullest model on swinger markets
model_swinger <- poolChoiceModel(formula_immigration, markets_swinger)
rm(markets_swinger)

# Log-linear comparison ---------------------------------------------------

#swinger model for whole USA
markets_swinger_usa <- mclapply(1:nreps, 
                            function(i) {createVariables(generateCouples(nfakes,actual,
                                                                         men.swinger,women.swinger,
                                                                         "country",weight="perwt", 
                                                                         verbose=FALSE))})
model_swinger_country <- poolChoiceModel(formula_educ2, markets_swinger_usa)
rm(markets_swinger_usa)

#swinger model for whole USA without weights
markets_swinger_usa_noweight <- mclapply(1:nreps, 
                                function(i) {createVariables(generateCouples(nfakes,actual,
                                                                             men.swinger,women.swinger,
                                                                             "country",
                                                                             verbose=FALSE))})
model_swinger_country_noweight <- poolChoiceModel(formula_educ2, markets_swinger_usa_noweight)
rm(markets_swinger_usa_noweight)

# Save the result ---------------------------------------------------------
cat("\nsaving output")

save(model_base, model_educ, model_immigration, model_state, model_country, model_swinger, 
     model_swinger_country, model_swinger_country_noweight, model_country_educ, model_state_educ,
     file=here("analysis","output","models.RData"))

cat("done")

Sys.time()
