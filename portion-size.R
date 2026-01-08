# Loading libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(openxlsx)

# Loading data ----
#Consumption data
data <- haven::read_dta(here::here("data", "2022", "food_items_microdata.dta"))
#names(data)
#head(data)
#str(data)

### Loading NCT (See docu)
nct <- readxl::read_excel(here::here("data", "Kenya_NCT_JRC.xlsx")) %>% 
  janitor::clean_names()
names(nct)

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

#No.of unique HHs
length(unique(df$clhhid))
#No. of unique food items
length(unique(df$fooditem_code))
# Checking consumers and non-consumers
table(df$clhhid, df$fooditem_code)

#Checking consumption - missing values
sum(is.na(df$cons_total))
# df %>% filter(is.na(cons_total)) %>% View()

# Adding consumption from the other columns if missing
df <- df %>% 
  mutate(clean_cons = ifelse(is.na(cons_total), 
            sum(c(cons_purch, cons_stock, cons_own, cons_gift), na.rm = TRUE),
            cons_total)) 

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
df <- df  %>% # filter(!is.na(cons_total)) %>% 
  left_join(., density %>% filter(item_code %in% litres), 
            by =c( "fooditem_code" ="item_code", 
                   "fooditem_name" = "food_name")) %>% 
  mutate(total_cons_g_day = ifelse(is.na(density_Kg_L), 
                                   clean_cons*1000/7, clean_cons*density_Kg_L*1000/7))

# Because non-consumer are not included in this clean dataset, we need to add them

# Generating food list
food_list <- df %>% filter(!is.na(cons_total)) %>%  group_by(across(starts_with("fooditem")), coicop, coicop_desc) %>% 
  count() %>% arrange(desc(n))

df %>% count(fooditem_code)

# Adding consumers and non-consumers
df <- df %>% right_join(., merge(food_list, unique(df$clhhid)), 
                  by = c("clhhid" = "y",
                  "fooditem_code", "fooditem_name", "coicop", "coicop_desc")) 

# Checking changes
df %>% 
  count(fooditem_code)

sum(!is.na(df$total_cons_g_day))


## Outliers -----

outliers <- df %>% left_join(., nct, by =c("fooditem_code" = "code")) %>% 
  #filter(is.na(edible_portion)) 
  mutate(cons_edible = total_cons_g_day*as.numeric(edible_portion))  %>% 
  group_by(fooditem_code) %>% 
  #  summarise(across(cons_edible, ~mean(.)))
  summarise(across(cons_edible, ~quantile(., 0.75, na.rm = TRUE), .names = "outliers")) 


## Portions  -----

portion <- df %>% 
   select(-c( "cons_purch","cons_stock" ,"cons_own","cons_gift","cons_total", 
              "n.x", "Food.name.and.description", "density_Kg_L",
              "Specific.gravity", "BiblioID", "f5_amnt","purchased","uv", "X", "n.y")) %>% 
  left_join(., nct %>% select(code, edible_portion), by =c("fooditem_code" = "code")) %>% 
  #filter(is.na(edible_portion)) 
  mutate(cons_edible = total_cons_g_day*as.numeric(edible_portion)) 

## Getting AFE
## First need to run the AFE script (afe.R) - Check the assumptions
source("afe.R")
portion <- portion %>% 
left_join(., roster_afe, by = join_by(county, resid, clhhid)) %>% 
  mutate(cons_afe_g_d = cons_edible/afe)

portion %>% filter(!is.na(cons_edible), is.na(afe)) 

hist(portion$cons_afe_g_d)

portion <- portion %>% filter(!is.na(cons_afe_g_d))

# Saving dataset 
write.csv(portion, here::here("inter-outputs", 
         paste0(Sys.Date(), "_portion-consumer-non-consumers_afe-g-d.csv")))


 
 wb <- createWorkbook()
 
 portion %>% 
   group_by(county_name) %>%
   group_walk(~{
     addWorksheet(wb, sheetName = as.character(.y$county_name))
     writeData(wb, sheet = as.character(.y$county_name), .x)
   })
 
 saveWorkbook(wb, here::here("inter-outputs", 
      paste0(Sys.Date(), "_county_portion-consumer-non-consumers_afe-g-d.xlsx")), overwrite = TRUE)
 
          