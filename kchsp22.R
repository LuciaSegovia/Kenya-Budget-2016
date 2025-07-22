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
df$coicop_desc[df$fooditem_code == 726]

# Extracting the food name from the stata label
df$fooditem_name <- as.character(forcats::as_factor(df[["fooditem_code"]]))
df$coicop_desc <- as.character(forcats::as_factor(df[["coicop"]]))
df$residency <- as.character(forcats::as_factor(df[["resid"]]))
df$county_name <- as.character(forcats::as_factor(df[["county"]]))

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

nutrient_summary <- df %>% left_join(., nct, by =c("fooditem_code" = "code")) %>% 
   #filter(is.na(edible_portion)) 
  mutate(cons_edible = total_cons_g_day*as.numeric(edible_portion)) %>% 
  mutate(across(names(nct)[c(4,12:23)], ~as.numeric(.)*cons_edible/100,
                .names = "cons_{.col}")) %>% 
  group_by(clhhid, county,county_name, resid, residency, adq_scale, hhsize) %>% 
  summarise(across(starts_with("cons_"), ~sum(.))) %>% select(-c( "cons_purch",                                                   
                                                                  "cons_stock" ,                                                  
                                                                  "cons_own",                                                     
                                                                  "cons_gift",                                                    
                                                                  "cons_total" ))

## Roster: Calculating Energy requirements for HHs members ----

str(roster)
roster <- roster %>% 
  rename(sex = "b04", age_y = "b05_years", 
         age_m = "b05_months", dob_year = "b06_yrs") 

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


# Calculating the Energy requirements foe each HH member
roster_test <-  Enerc_requirement(roster, pal = 1.6, weight.m = 65, weight.f = 55,
                                  lac = FALSE, preg = FALSE, at_home = FALSE)
sum(is.na(roster_test$enerc_kcal))

roster_test %>% # filter(age_y>6 & age_y<12) %>% 
  ggplot(aes(enerc_kcal, weight, colour = as.character(sex))) + geom_point()

roster_test %>% # filter(age_y>6 & age_y<12) %>% 
  ggplot(aes(enerc_kcal, colour = as.character(sex))) + geom_histogram()


roster_test %>% filter(enerc_kcal <1000) %>% 
  ggplot(aes(as.character(age_y), enerc_kcal)) + geom_boxplot()

roster_test %>% filter(age_y ==0 &age_m<=6) %>% 
  ggplot(aes(enerc_kcal)) + geom_histogram()


roster_test %>% filter(age_y ==0 &age_m<=6) %>% 
  count(b03)

roster_test %>% filter(age_y ==0 &age_m<=6) %>% 
  filter(b03==3) %>% select(hhid, hhid_id)

roster_test$hhid[roster_test$age_y == 0 & roster_test$age_m <= 6]

roster_test$hhid_id[roster_test$hhid %in% roster_test$hhid[roster_test$age_y == 0 & roster_test$age_m <= 6]]
names(roster_test)

lac_hhid <-  roster_test$clhhid[roster_test$age_y == 0 & roster_test$age_m <= 6 & !roster_test$b03 %in% c(3, 4, 7)]
lac_b03 <-  roster_test$b03[roster_test$age_y == 0 & roster_test$age_m <= 6 & !roster_test$b03 %in% c(3, 4, 7)]

roster_test <- roster_test %>% 
  mutate(lac_women = ifelse(hhid_id %in% c(1, 2) & sex == 2, case_when(
            clhhid %in% roster_test$clhhid[roster_test$age_y == 0 & roster_test$age_m == 1 & roster_test$b03 ==3] ~ 2569/4.18,
            clhhid %in% roster_test$clhhid[roster_test$age_y == 0 & roster_test$age_m == 2 & roster_test$b03 ==3] ~ 2686/4.18,
            clhhid %in% roster_test$clhhid[roster_test$age_y == 0 & roster_test$age_m == 3 & roster_test$b03 ==3] ~ 2760/4.18,
            clhhid %in% roster_test$clhhid[roster_test$age_y == 0 & roster_test$age_m == 4 & roster_test$b03 ==3] ~ 2867/4.18,
            clhhid %in% roster_test$clhhid[roster_test$age_y == 0 & roster_test$age_m == 5 & roster_test$b03 ==3] ~ 2925/4.18,
            clhhid %in% roster_test$clhhid[roster_test$age_y == 0 & roster_test$age_m == 6 & roster_test$b03 ==3] ~ 3138/4.18), lac_women)) %>% 
  mutate(lac_women = ifelse(hhid_id %in% c(3) & sex == 2, case_when(
    clhhid %in% roster_test$clhhid[roster_test$age_y == 0 & roster_test$age_m == 1 & roster_test$b03 ==4] ~ 2569/4.18,
    clhhid %in% roster_test$clhhid[roster_test$age_y == 0 & roster_test$age_m == 2 & roster_test$b03 ==4] ~ 2686/4.18,
    clhhid %in% roster_test$clhhid[roster_test$age_y == 0 & roster_test$age_m == 3 & roster_test$b03 ==4] ~ 2760/4.18,
    clhhid %in% roster_test$clhhid[roster_test$age_y == 0 & roster_test$age_m == 4 & roster_test$b03 ==4] ~ 2867/4.18,
    clhhid %in% roster_test$clhhid[roster_test$age_y == 0 & roster_test$age_m == 5 & roster_test$b03 ==4] ~ 2925/4.18,
    clhhid %in% roster_test$clhhid[roster_test$age_y == 0 & roster_test$age_m == 6 & roster_test$b03 ==4] ~ 3138/4.18), lac_women)) %>%
  mutate(lac_women = ifelse(hhid_id %in% c(5) & sex == 2, case_when(
    clhhid %in% roster_test$clhhid[roster_test$age_y == 0 & roster_test$age_m == 1 & roster_test$b03 ==7] ~ 2569/4.18,
    clhhid %in% roster_test$clhhid[roster_test$age_y == 0 & roster_test$age_m == 2 & roster_test$b03 ==7] ~ 2686/4.18,
    clhhid %in% roster_test$clhhid[roster_test$age_y == 0 & roster_test$age_m == 3 & roster_test$b03 ==7] ~ 2760/4.18,
    clhhid %in% roster_test$clhhid[roster_test$age_y == 0 & roster_test$age_m == 4 & roster_test$b03 ==7] ~ 2867/4.18,
    clhhid %in% roster_test$clhhid[roster_test$age_y == 0 & roster_test$age_m == 5 & roster_test$b03 ==7] ~ 2925/4.18,
    clhhid %in% roster_test$clhhid[roster_test$age_y == 0 & roster_test$age_m == 6 & roster_test$b03 ==7] ~ 3138/4.18), lac_women)) #%>% 
 # filter(!is.na(lac_women)) %>% 
#  select(hhid_id, b03, sex, lac_women ) %>% summary()

hh_lac <-  roster_test$clhhid[roster_test$age_y == 0 & roster_test$age_m <= 6]
hh_lac_wom <-  roster_test$clhhid[!is.na(roster_test$lac_women)]

roster_test %>% filter(!clhhid %in% hh_lac_wom & clhhid %in% hh_lac)  %>% distinct(clhhid)
  
roster_test %>% filter(!hhid_id %in% c(1,2,3,5) & sex == 2 & 
                         clhhid %in% lac_hhid & age_y>15 & age_y<49) %>%
  arrange(desc(clhhid)) %>% View()

hh_sac <- roster_test$clhhid[roster_test$age_y>=6 & roster_test$age_y<= 12]


roster_test %>% filter(clhhid %in% hh_sac) %>% 
  ggplot(aes(enerc_kcal, age_y, colour = as.character(sex))) + geom_point()

## Selecting the reference
years <- 10
S <- 2
(Energy_sac <- unique(roster_test$enerc_kcal[roster_test$age == years & roster_test$sex == S & !is.na(roster_test$enerc_kcal)]))

# Calculating SACE
roster_sac <- roster_test %>% filter(clhhid %in% hh_sac) %>% 
  mutate(sace = enerc_kcal/Energy_sac) %>% 
  group_by(clhhid) %>% 
  summarise(clhhid, county, resid, weight_hh,  sace = sum(sace)) %>% distinct()

sum(is.na(roster_sac$sace))

df$clhhid <-  paste0(df$clid,"_",  df$hhid)

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

