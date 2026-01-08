
# Loading libraries
library(ggplot2)
library(dplyr)
library(tidyr)

#Function
source("functions/Energy_requirements.R")
source("functions/Energy_adjustments.R")

#Data 
roster <- haven::read_dta(here::here("data", "2022", "individuals_microdata.dta"))
str(roster)

# Assumptions
school.days <- 195 # Based on GCNF (2019). 

## Roster: Calculating Energy requirements for HHs members ----

grep("feed|school|meal", labelled::var_label(roster), ignore.case = TRUE, value = TRUE)

roster <- roster %>% 
  rename(sex = "b04", age_y = "b05_years", relation_head = "b03", 
         age_m = "b05_months", dob_year = "b06_yrs", 
         school_attend = "c03",  school_attend_last = "c05", school_grade = "c06", exp_feed = "c09o") 

# Id variables
sum(duplicated(roster$hhid))
sum(duplicated(roster$clid))
sum(duplicated(roster$interview__id)) # same as cluster
sum(duplicated(paste0(roster$clid,"_",  roster$hhid)))
sum(duplicated(paste0(roster$clid,"_",  roster$hhid, "_", roster$hhid_id)))

# Household unique ID
roster$clhhid <- paste0(roster$clid,"_",  roster$hhid)
# HH member unique ID
roster$uniqueid <- paste0(roster$clid,"_",  roster$hhid, "_", roster$hhid_id)


# Checking the data
sum(is.na(roster$sex))
sum(is.na(roster$age_y))
sum(is.na(roster$age_m))
sum(is.na(roster$age_m) & roster$age_y<6) # Up to 6 years
sum(roster$age_y[roster$age_y>200])
roster$age_y[roster$age_y==999]
hist(roster$age_y)
hist(roster$age_m)
unique(roster$sex)
unique(roster$age_y)

## 1) Energy requirements ----
# Calculating the Energy requirements for each HH member
roster_test <-  Enerc_requirement(roster, pal = 1.6, weight.m = 65, weight.f = 55,
                                  lac = TRUE, preg = TRUE, prev.preg = 0.05)

## 2) Energy adjustment for HH food allocation ----- 
roster_test2 <-  Enerc_adjustment(roster_test, 
                                  excl.bf = TRUE, excl.age = 3, 
                                  comple.bf = TRUE, 
                                  feeding = TRUE, 
                                  school = FALSE,  #meal_kcal = 629,
                                  at_home = FALSE)

## 3) Calculating AFE ----- 

# Selecting the reference (women)
years <- 25
S <- 2
(Energy_afe <- unique(roster_test$enerc_kcal[roster_test$age == years & roster_test$sex == S & is.na(roster_test$lac_women) & roster_test$preg_0.05 == 0]))

### Calculating AFE
roster_afe <- roster_test2 %>% 
  mutate(
    afe = enerc_kcal/Energy_afe) %>% 
  group_by(clhhid) %>% 
  summarise(clhhid, county, resid, weight_hh, 
            afe =sum(afe) ) %>% 
  distinct()

sum(is.na(roster_afe$afe))
