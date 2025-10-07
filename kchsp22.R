# Loading libraries
library(ggplot2)
library(dplyr)
library(tidyr)

#Function
source("functions/Energy_requirements.R")

# Loading data ----

data <- haven::read_dta(here::here("data", "2022", "food_items_microdata.dta"))
names(data)
head(data)
str(data)

roster <- haven::read_dta(here::here("data", "2022", "individuals_microdata.dta"))
str(roster)



## Food consumption at HHs (7days) ----
df <- data %>% 
  rename(
    purchased = "f5_qtyb", 
  #  purchased_unit = "t04_su", 
 #   pop = "t05",  # point of purchase
    cons_purch = "f6_qty",
  #  cons_purch_unit = "t06_su",
    cons_stock = "f7_qty",
 #   cons_stock_unit = "t07_su",
    cons_own = "f8_qty",
 #   cons_own_unit = "t08_su",
    cons_gift = "f9_qty",
  #  cons_gidt_unit = "t09_su",
    cons_total = "qcons"
#    cons_unit = "t10_su"
  )

# Amend food item = to coicop `NA` to . Based on COICOP legislation (see page 69)
# https://unstats.un.org/unsd/classifications/unsdclassifications/COICOP_2018_pre_copy_edit_publication.pdf
df$coicop[df$fooditem_code == 726] <- 7
# df$coicop_desc[df$fooditem_code == 726]

# Extracting the food name from the stata label
df$fooditem_name <- as.character(forcats::as_factor(df[["fooditem_code"]]))
df$coicop_desc <- as.character(forcats::as_factor(df[["coicop"]]))
df$residency <- as.character(forcats::as_factor(df[["resid"]]))
df$county_name <- as.character(forcats::as_factor(df[["county"]]))
# Creating a unique id for indiv. hh in cluster
df$clhhid <-  paste0(df$clid,"_",  df$hhid)

food_list <- df %>% filter(!is.na(cons_total)) %>%  group_by(across(starts_with("fooditem")), coicop, coicop_desc) %>% 
  count() %>% arrange(desc(n))

df %>% filter(!is.na(cons_total) & is.na(coicop)) %>% select(starts_with("fooditem"))
 
df %>% filter(!is.na(cons_total)) %>% 
  ggplot(aes(cons_total/hhsize/7)) + 
  geom_histogram() +
  facet_wrap(~coicop_desc, scales = "free")


df %>% filter(!is.na(cons_total) & coicop == 1) %>% 
  ggplot(aes(cons_total/hhsize*1000)) + 
  geom_histogram() +
  facet_wrap(~fooditem_name)
  
df %>% filter(!is.na(cons_total) 
              & coicop == 1
              ) %>% 
  ggplot(aes(reorder(fooditem_name, cons_total/hhsize), cons_total/hhsize*1000/7)) + 
  geom_boxplot() # +
#  facet_wrap(~coicop_desc, scales = "free")


# Checking bread units
hist(df$cons_total[df$fooditem_code == 145]*1000/7/df$hhsize)
summary(df$cons_total[df$fooditem_code == 149]*1000/7/df$hhsize)

## Converting L to Kg
# Items in L (see docu)
litres <- c(401, 405, 406, 402, 404, 403, 407, 409, 411, 507, 905, 907, 1202, 
            1204, 1205, 1601, 1602, 1603, 1701, 1702, 1703, 1504, 1505)

### Loading density -----
density <- read.csv(here::here("inter-outputs",
                  "food-items-denisty_v.1.0.0.csv"), fileEncoding="latin1") %>% 
  rename(density_Kg_L = "Density.in.g.ml..including.mass.and.bulk.density.") #g/mL = Kg/L
names(density)

# Checking if info for all foods needed
density %>% filter(item_code %in% litres) %>% filter(BiblioID == "")

# Adding density to re-calculate total_cons to g/day
df <- df %>% filter(!is.na(cons_total)) %>% 
  left_join(., density %>% filter(item_code %in% litres), 
                 by =c( "fooditem_code" ="item_code", 
                        "fooditem_name" = "food_name")) %>% 
  mutate(total_cons_g_day = ifelse(is.na(density_Kg_L), 
                    cons_total*1000/7,cons_total*density_Kg_L*1000/7))

### Loading NCT (See docu)
nct <- readxl::read_excel(here::here("data", "Kenya_NCT_JRC.xlsx")) %>% 
  janitor::clean_names()
names(nct)

# Calculating the nutrient app. consumption per hh
nutrient_summary <- df %>% left_join(., nct, by =c("fooditem_code" = "code")) %>% 
   #filter(is.na(edible_portion)) 
  mutate(cons_edible = total_cons_g_day*as.numeric(edible_portion)) %>% 
  mutate(across(names(nct)[c(4,12:23)], ~as.numeric(.)*cons_edible/100,
                .names = "cons_{.col}")) %>% 
  group_by(clid, hhid, clhhid, county, county_name, resid, residency, adq_scale, hhsize) %>% 
  summarise(across(starts_with("cons_"), ~sum(.))) %>% select(-c( "cons_purch",                                                   
                                                                  "cons_stock" ,                                                  
                                                                  "cons_own",                                                     
                                                                  "cons_gift",                                                    
                                                                  "cons_total" ))

## Roster: Calculating Energy requirements for HHs members ----

str(roster)
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

roster$clhhid <- paste0(roster$clid,"_",  roster$hhid)
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

roster_test <-  Enerc_requirement(roster, pal = 1.6, weight.m = 65, weight.f = 55,
                                  lac = FALSE, preg = TRUE, prev.preg = 0.05)

# Checking lactating
# Mothers who are HH head or spouse
roster_test2 %>% filter(is.na(lac_women) & age <45 & b03 %in% c("1", "2")) %>% 
  select(hhid_id, clhhid, age, b03, lac_women) 

# Checking pregnancy
roster_test3 %>% select(starts_with("preg"), age_y, sex, enerc_kcal) %>% 
  filter(age_y == 30) %>% distinct()
roster_test3 %>% filter(preg_0.05 == 1) %>% count()

id_check <- roster_test1 %>% filter(!is.na(lac_women) & age >45 & b03 %in% c("1", "2")) %>% 
  distinct(clhhid) %>% pull()

roster_test1 %>% filter(clhhid %in% id_check)

roster_test1 %>% names()

## 2) Energy adjustment for HH food allocation ----- 
roster_test2 <-  Enerc_adjustment(roster_test, 
                                  excl.bf = TRUE, excl.age = 3, 
                                  comple.bf = TRUE, 
                                  school = TRUE, feeding = TRUE, meal_kcal = 629,
                                  at_home = FALSE)

# Testing excl. bf
roster_test2 %>% filter(months <= 11) %>% 
  select(months, age_y, enerc_kcal) %>% unique()

# Testing complementary bf
roster_test2 %>% filter(months >= 12 & months <= 24) %>% 
  select(months, age_y, enerc_kcal, complebf_0.6) %>% unique() %>%  View()

# Testing school feeding
roster_test2 %>% filter(age_y >= 6 & age_y <= 13) %>% 
  select(months, age_y, enerc_kcal, enerc_kcal_school, school_attend, school_feeding) %>% unique() %>%  View()

# Testing feeding
roster_test2 %>% filter(age_y >= 6 & age_y <= 13) %>% 
  select(months, age_y, enerc_kcal, school_attend, school_feeding) %>% unique() %>%  View()

# Testing feeding & school
roster_test2 %>% filter(age_y >= 6 & age_y <= 13) %>% 
  select(months, age_y, enerc_kcal, enerc_kcal_school, school_attend, school_feeding, c05,
       enerc_kcal_feeding, exp_feed, expendi_feeding) %>% unique() %>%  View()

sum(is.na(roster_test$enerc_kcal))

# Selecting the reference (women)
years <- 25
S <- 2
(Energy_afe <- unique(roster_test$enerc_kcal[roster_test$age == years & roster_test$sex == S & is.na(roster_test$lac_women) & roster_test$preg_0.05 == 0]))

### Calculating impact on AFE ----
roster_afe <- roster_test2 %>% 
  mutate(
    afe = enerc_kcal/Energy_afe,
    afe_school = enerc_kcal_school/Energy_afe, 
    afe_feed = enerc_kcal_feeding/Energy_afe) %>% 
  group_by(clhhid) %>% 
  summarise(clhhid, county, resid, weight_hh, 
            afe =sum(afe),
            afe_school = sum(afe_school),
            afe_feed = sum(afe_feed),
            ) %>% 
  distinct()


nutrient_summary %>% left_join(., roster_afe %>% pivot_longer(cols =starts_with("afe"), 
                                                              names_to = "hh_alloc")) %>% 
#  filter(!is.na(sace)) %>% #select(adq_scale, sace, hhsize) %>% 
  ggplot(aes(cons_energy_kcal/value, fill = hh_alloc)) + 
  geom_boxplot() +
  coord_flip()

nutrient_afe <- nutrient_summary %>% 
  left_join(., roster_afe %>% pivot_longer(cols =starts_with("afe"), 
   names_to = "hh_alloc")) %>% 
  mutate(
   # across(starts_with("cons_"), ~./as.numeric(sace), .names = "sace_{.col}"),
    across(starts_with("cons_"), ~./as.numeric(value), .names = "hh_alloc_{.col}"))

### Survey design ----
library(survey) # survey design
library(srvyr) # survey design
# PSU = ea_id
#ihs4_summary$region <- as.factor(ihs4_summary$region)
survey_design <- nutrient_afe %>% ungroup() %>% 
  as_survey_design(strata = c(county, resid), weights = weight_hh)

survey_design %>% 
ggplot(aes(hh_alloc_cons_energy_kcal, fill = hh_alloc)) + 
  geom_boxplot() +
  coord_flip()

survey_design %>% 
  ggplot(aes(hh_alloc_cons_vitamin_b12_mcg, fill = hh_alloc)) + 
  geom_boxplot() +
  coord_flip()


### Calculating for School age children -----

hh_sac <- roster_test$clhhid[roster_test$age_y>=6 & roster_test$age_y<= 12]

roster_test2 %>% filter(clhhid %in% hh_sac) %>% 
  ggplot(aes(enerc_kcal, age_y, colour = as.character(sex))) + geom_point()

## Selecting the reference
years <- 10
S <- 2
(Energy_sac <- unique(roster_test$enerc_kcal[roster_test$age == years & roster_test$sex == S & !is.na(roster_test$enerc_kcal)]))

# Calculating SACE
roster_sac <- roster_test2 %>% filter(clhhid %in% hh_sac) %>% 
  mutate(
#    afe = enerc_kcal/Energy_afe,
    sace = enerc_kcal/Energy_sac,
         sace_school = enerc_kcal_school/Energy_sac) %>% 
  group_by(clhhid) %>% 
  summarise(clhhid, county, resid, weight_hh, 
 #           afe =sum(afe),
            sace = sum(sace), 
            sace_school = sum(sace_school)) %>% 
  distinct()

sum(is.na(roster_sac$sace))

df %>% left_join(., roster_sac) %>% 
  filter(!is.na(sace)) %>% select(adq_scale, sace, hhsize) %>% distinct() %>% 
  ggplot(aes(adq_scale, sace)) + geom_point()


nutrient_summary %>% left_join(., roster_sac) %>% 
  filter(!is.na(sace)) %>% #select(adq_scale, sace, hhsize) %>% 
  ggplot(aes(cons_energy_kcal/adq_scale)) + 
  geom_histogram() +
  facet_wrap(~county_name)


nutrient_sace <- nutrient_summary %>% left_join(., roster_sac) %>% 
  filter(!is.na(sace)) %>% 
  mutate(
    across(starts_with("cons_"), ~./as.numeric(sace), .names = "sace_{.col}"),
    across(starts_with("cons_"), ~./as.numeric(adq_scale), .names = "ame_{.col}"))


# Survey design ----
library(survey) # survey design
library(srvyr) # survey design
# PSU = ea_id
#ihs4_summary$region <- as.factor(ihs4_summary$region)
survey_design <- nutrient_sace %>% ungroup() %>% 
  as_survey_design(strata = c(county, resid), weights = weight_hh)


