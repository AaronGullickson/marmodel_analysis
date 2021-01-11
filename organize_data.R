#' ---
#' title: "organize_data.R"
#' author: ""
#' ---

# This script will read in raw data from the input directory, clean it up to produce 
# the analytical dataset, and then write the analytical data to the output directory. 

#load libraries
library(here)

#source in any useful functions
source(here("check_packages.R"))
source(here("useful_functions.R"))

# Read in raw data --------------------------------------------------------

# I have split the IPUMS download into multiple files to keep them under 100MB.
# They will be loaded in separately and then combined.

start_idx <- c(1, 7,28,38,40,47,51,61,63,64,67,68,69,75,79,83,91,93,101)
end_idx   <- c(4,14,37,39,44,50,60,62,63,66,67,68,72,75,79,85,92,94,103)
var_names <- c("year","serial","hhwt","statefip","met2013","pernum","perwt",
               "sploc","sex","age","marst","marrinyr","yrmarr","race","hispan",
               "bpl","yrsusa1","lang","educd")

census <- rbind(
  # read_fwf(here("analysis","input", "usa_00078.dat.gz"), 
  #          col_positions = fwf_positions(start = start_idx,
  #                                        end   = end_idx,
  #                                        col_names = var_names),
  #          col_types = cols(.default = "i"), #ensure that all variables are read in as integers
  #          progress = FALSE),
  # read_fwf(here("analysis","input", "usa_00079.dat.gz"), 
  #          col_positions = fwf_positions(start = start_idx,
  #                                        end   = end_idx,
  #                                        col_names = var_names),
  #          col_types = cols(.default = "i"), #ensure that all variables are read in as integers
  #          progress = FALSE),
  read_fwf(here("input", "usa_00080.dat.gz"), 
           col_positions = fwf_positions(start = start_idx,
                                         end   = end_idx,
                                         col_names = var_names),
           col_types = cols(.default = "i"), 
           progress = FALSE),
  read_fwf(here("input", "usa_00081.dat.gz"), 
           col_positions = fwf_positions(start = start_idx,
                                         end   = end_idx,
                                         col_names = var_names),
           col_types = cols(.default = "i"), 
           progress = FALSE),
  read_fwf(here("input", "usa_00082.dat.gz"), 
           col_positions = fwf_positions(start = start_idx,
                                         end   = end_idx,
                                         col_names = var_names),
           col_types = cols(.default = "i"),
           progress = FALSE))

#turn back into data.frame cause fake union can't handle tibbles ATM
census <- as.data.frame(census)

# Sample restrictions -----------------------------------------------------

# I use a vareity of sample restrictions here, but the analytically important
# ones are ones that restrict the sample to the pool of people who married in
# the last year or who were single for all of the previous year and present in a
# single marriage market.

# some sample selection is done on the IPUMS selection routine prior to data
# download. I restrict to people living in the US, age 18 and over who were not
# divorced or widowed in the prior year with a valid place of birth and language
# who did not migrate out of state in the previous year

#drp immigrants who have been here for one year or less and keep only those who 
#are single or were married in the last yera
#we also drop all people not living in an identifiable metro area so that 
#the state and country analysis is using the same set of people.
census <- subset(census, yrsusa1!=1 & #immigrants less than a year
                   ((marst>=4 & sploc==0) | #single
                      (marst==1 & marrinyr==2 & sploc!=0)) & #married in last yr
                   census$met2013!=0) 


# Code variables ----------------------------------------------------------

# create a unique id for each person (we can ignore datanum)
census$id <- census$year*10^11+census$serial*10^4+census$pernum
sum(duplicated(census$id))

#create a similar id for spouse
census$sp_id <- ifelse(census$sploc==0, NA, 
                       census$year*10^11+census$serial*10^4+census$sploc)
sum(duplicated(na.omit(census$sp_id)))

#recode state birth places to a single US one
census$bpl <- ifelse(census$bpl<100,1,census$bpl)
with(subset(census, bpl<100), table(bpl))

#code in missing values of met2013
census$met2013 <- ifelse(census$met2013==0, NA, census$met2013)
summary(census$met2013)

#code education into a simple four category system of LHS, HS, SC, C
#nothing should be missing here because only those under 3 are NA
temp <- factor(ifelse(census$educd<2, NA, 
                      ifelse(census$educd<62, "LHS",
                             ifelse(census$educd<70, "HS",
                                    ifelse(census$educd<101, "SC", "C")))),
               levels=c("LHS","HS","SC","C"), ordered=TRUE)
table(census$educd, temp, exclude=NULL)
census$educ <- temp

#code race - we are going to keep this simple and just use single categories
#with hispanic as a separate category. Multiracials and NH others will be dropped
temp <- factor(ifelse(census$hispan!=0, "H",
                      ifelse(census$race==1, "W",
                             ifelse(census$race==2, "B",
                                    ifelse(census$race==3, "I",
                                           ifelse(census$race<7,"A",NA))))),
               levels=c("W","B","I","A","H"))
table(census$race, temp, exclude=NULL)
census$race <- temp

# Sex of respondent
# - Male (sex 1)
# - Female (sex 2)
temp <- ifelse(census$sex==1, "Male",
               ifelse(census$sex==2, "Female", NA))
table(temp, census$sex, exclude=NULL)
census$sex <- factor(temp)

#create a country variable
census$country <- factor("USA")

#treat met2013 and state as factor variables
census$statefip <- factor(as.character(census$statefip))
census$met2013 <- factor(as.character(census$met2013))

#attach metro names to id variable
#read metro names directly from the codebook
met_names <- read_fwf(here("input","usa_00078_cbk.txt"),
                      skip=962, n_max=294,
                      col_positions = fwf_positions(start = c(1,6),
                                                    end   = c(5,100),
                                                    col_names = c("met2013",
                                                                  "met_name")))
census <- merge(census, met_names, all.x=TRUE, all.y=FALSE)

#now just keep the variables we need, also drop if race or education missings
#we also drop if met2013 is missing so that our state and country datasets
#use the same set of people
census <- subset(census, !is.na(race) & !is.na(educ),
                 select = c("id","sp_id","year","hhwt","perwt",
                            "statefip","met2013","met_name","country",
                            "sex","age","race","educ","bpl","lang"))
summary(census)

# Separate datasets -------------------------------------------------------

# separate men and women as potential partners. rename vars to end in h and w

#this is not as robust as it could be - variables must be entered in the 
#same order as in data.frame to get everything right
var_to_change <- c("id","age","race","educ","bpl","lang")

men <- subset(census, sex=="Male",
              select=c("statefip","met2013","met_name","country","perwt",
                       var_to_change))
colnames(men)[which(colnames(men) %in% var_to_change)] <- paste(var_to_change, 
                                                                "h", sep="")

women <- subset(census, sex=="Female",
                select=c("statefip","met2013","met_name","country","perwt",
                         var_to_change))
colnames(women)[which(colnames(women) %in% var_to_change)] <- paste(var_to_change, 
                                                                    "w", sep="")

#now create married men and women in order to link actual couples
var_to_change <- c("perwt",var_to_change)
married_men <- subset(census, sex=="Male" & !is.na(sp_id),
                      select=c("statefip","met2013","met_name","country","hhwt", 
                               var_to_change))
colnames(married_men)[which(colnames(married_men) %in% var_to_change)] <- paste(var_to_change, 
                                                                                "h", sep="")

married_women <- subset(census, sex=="Female" & !is.na(sp_id),
                        select=c("sp_id",var_to_change))
colnames(married_women)[which(colnames(married_women) %in% var_to_change)] <- paste(var_to_change, 
                                                                                    "w", sep="")

actual <- merge(married_men, married_women, by.x="idh", by.y="sp_id",
                all.x=FALSE, all.y=FALSE)

#create "swinger" version of alternative partners from only the actual marriage
#partners
men.swinger <- actual[,c("perwth",colnames(actual)[colnames(actual) %in% colnames(men)])]
colnames(men.swinger)[[1]] <- "perwt"
women.swinger <- actual[,c("perwtw",colnames(actual)[colnames(actual) %in% colnames(women)])]
colnames(women.swinger)[[1]] <- "perwt"

#re-order actual marriage variables and remove perwt
actual <- subset(actual,
                 select=c("idh","idw","statefip","met2013","met_name","country","hhwt",
                          "ageh","raceh","educh","bplh","langh",
                          "agew","racew","educw","bplw","langw"))

#drop unused levels of metro2013
actual$met2013 <- droplevels(actual$met2013)

#garbage cleanup
rm(married_men, married_women)

save(actual, file=here("output","real_unions.RData"))
save(men, women, file=here("output","potential_partners.RData"))
save(men.swinger, women.swinger, file=here("output",
                                           "potential_swingers.RData"))


## Create dissimilarity indices -----------------------------------------------

#dissimilarity indices based on race and education for all alternate partners
#in the data, using metro areas as the "neighborhood"

colnames(men) <- colnames(women) <- c("statefip","met2013","met_name","country","perwt",
                                      "id","age","race","educ","bpl","lang")
partners <- rbind(men, women)


## Dissimilarities by race ##
tab <- table(partners$met2013, partners$race)
props <- prop.table(tab, 2)

d_race <- matrix(NA, 5, 5)
colnames(d_race) <- rownames(d_race) <- colnames(props)
for(i in colnames(props)) {
  for(j in colnames(props)) {
    d_race[i,j] <- 0.5*sum(abs(props[,i]-props[,j]))
  }
}
d_race <- as.data.frame.table(d_race)
colnames(d_race) <- c("race1","race2","D")
d_race <- subset(d_race, D>0 & !duplicated(D))

## Dissimilaries by education ##
tab <- table(partners$met2013, partners$educ)

# a little more complicated because it is all above vs lower
props <- prop.table(cbind(tab[,1], apply(tab[,2:4], 1, sum)), 2)
d_lhs <- 0.5*sum(abs(props[,1]-props[,2]))
props <- prop.table(cbind(apply(tab[,1:2],1,sum), 
                          apply(tab[,2:4], 1, sum)), 2)
d_hs <- 0.5*sum(abs(props[,1]-props[,2]))
props <- prop.table(cbind(apply(tab[,1:3],1,sum), tab[,4]), 2)
d_sc <- 0.5*sum(abs(props[,1]-props[,2]))
d_crossing <- c(d_lhs, d_hs, d_sc)
d_crossing <- data.frame(crossing=c("LHS/HS","HS/SC","SC/C"), 
                         D=c(d_lhs, d_hs, d_sc))

save(d_race, d_crossing, 
     file=here("output","dissimilarity_indices.RData"))