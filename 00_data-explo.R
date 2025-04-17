

library(ggplot2)
library(dplyr)
library(tidyr)

# Data
# Food consumption at HHs (7days)
df <- haven::read_dta(here::here("data", "food.dta")) %>% 
  rename(
         purchased = "t04_qy", 
         purchased_unit = "t04_su", 
         pop = "t05",  # point of purchase
         cons_purch = "t06_qy",
         cons_purch_unit = "t06_su",
         cons_stock = "t07_qy",
         cons_stock_unit = "t07_su",
         cons_own = "t08_qy",
         cons_own_unit = "t08_su",
         cons_gift = "t09_qy",
         cons_gidt_unit = "t09_su",
         cons_total = "t10_qy",
         cons_unit = "t10_su"
         )
# Best to explore the meaning of variables in stata dataset.
str(df) 
# Roster info
hh <- haven::read_dta(here::here("data", "HH_Members_Information.dta")) %>% 
  rename(
         lineNum = "b01", 
         sex = "b04", 
         age_y = "b05_yy", 
         age_m = "b05_mm", 
         school = "c03", 
         school_lastyear = "c07", # attended school during the last school/academic year
         school_km = "c04", 
         school_feed = "c05",
         school_level = "c06_l", 
         school_grade = "c06_g", 
         drop_reason1 = "c09_r1",
         drop_reason2 = "c09_r2",
         school_feed_shill = "c13_o",
         sickness1 = "e03_1", # 26 pregnancy related
         sickness2 = "e03_2", # 26 pregnancy related
         weight_kg = "f21", 
         height_cm = "f22"
         ) %>% # generating a unique hhid
mutate(bmi = weight_kg/((height_cm/100)^2), 
  hhid_unique = paste(clid,hhid, sep = "_")) %>% 
  relocate( hhid_unique, .before = "clid")
  
# Expenditure aggregate HHs
haven::read_dta(here::here("data", "Consumption_aggregate.dta"))
names(haven::read_dta(here::here("data", "food.dta")))
# Agriculture outputs (12 months) (Section L of Q1B)
names(haven::read_dta(here::here("data", "Agriculture_output_L1_L20.dta")))
df3 <- haven::read_dta(here::here("data", "Agriculture_output_L1_L20.dta")) %>% 
  filter(k01 == 1)

# Checking food items and NSU
sort(unique(haven::as_factor(df$item_code)))
unique(haven::as_factor(df$purchased_unit))
unique(haven::as_factor(df3$l02_cr))



haven::read_dta(here::here("data", "Consumption_aggregate.dta")) %>% 
  mutate(hhsize_cat = as.factor(ifelse(hhsize>=10, "10+", hhsize)), 
         edu = padqeduc/hhsize) %>% 
  ggplot(aes(reorder(edu, hhsize_cat), edu)) + geom_violin() +
  coord_flip()
