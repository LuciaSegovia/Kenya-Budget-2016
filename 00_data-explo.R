


#install.package("labelled") # Manunpulate stata files
# https://mcroche.github.io/Labeling_in_R/chp-2.html

# Loading libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Chekcing data files
str(haven::read_dta(here::here("data", "2021", "KCHS-2021-individuals_microdata.dta")))
str(haven::read_dta(here::here("data",  "HH_Information.dta")))
str(haven::read_dta(here::here("data", "2021", "food_items_microdata.dta")))
str(haven::read_dta(here::here("data", "2021", "consagg_microdata.dta")))
str(haven::read_dta(here::here("data", "2021", "nonfood_items_microdata.dta")))

# Household data
hh.df <- haven::read_dta(here::here("data", "2021", "households_microdata.dta"))
hh.df2 <- haven::read_dta(here::here("data",  "HH_Information.dta"))

# N.of HH 
nrow(hh.df)
head(hh.df)
length(unique(hh.df$hhid))

apply(hh.df2, 2, labelled::var_label)

str(hh.df2[,c(163:183)])

names(hh.df2)

grep("coor|long|lat|loc|GPS", labelled::var_label(hh.df2), ignore.case = TRUE, value = TRUE)



# Data
# Roster info
hh <- haven::read_dta(here::here("data", "HH_Members_Information.dta")) %>% 
  rename(
         lineNum = "b01", 
         sex = "b04", 
         age_y = "b05_yy", 
         age_m = "b05_mm", 
         school = "c03", # attended to school
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
  hhid_unique = paste(clid, hhid, sep = "_"), 
  id_unique = paste(clid, hhid,lineNum, sep = "_")) %>% 
  relocate( c(hhid_unique,id_unique), .before = "clid")

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


# Loading GIS data ----
library(dplyr)
library(sf)
library(tmap)

crop <- st_read(here::here("data", "gis", "ke_crops_foodshare.shp"))
market <- st_read(here::here("data", "gis", "ke_market_centers.shp"))
head(market)

plot(crop[, "FOOD_SHARE"])
plot(market[, "TOWN_TYPE"])

# Reading csv (lon(x)/lat(y))
crop <- read.csv(here::here("data", "spam2017V2r1_SSA_Y_TA.csv")) %>% 
  dplyr::filter(iso3 == "KEN") %>% st_as_sf(.,  coords = c("x", "y"))

crop_names <- read.csv(here::here("data", "crop_names.csv")) 

plot(crop[, "fs_name"])
names(crop)

tmap_mode("view")

crop_map <- names(crop)[8:49]

map_crop <- list()

# for(i in 1:length(crop_map)){
  
# No palm oil cultivation in KEN
crop_var <- "maiz_a"
crop_name <- crop_names$crop_name[crop_names$crop_short %in% gsub("_a", "", crop_var)]


#(map_crop[[i]] <-
    
(test  <-   tm_shape(World %>% dplyr::filter(iso_a3 == "KEN"))+
  tm_borders(col = "grey8", lwd  =1.3) +
tm_shape(crop %>% filter(!!sym(crop_var) >0))+
  tm_symbols( fill = crop_var, col = crop_var,  size = 0.3, 
              fill.scale = tm_scale_continuous(values = palette1), 
              col.scale = tm_scale_continuous(values =  palette1), 
              fill.legend = tm_legend(crop_name),
              col.legend = tm_legend(show = FALSE)) +
  tm_title(paste0("Yield of ", crop_name, " (kg/ha) in Kenya (data from 2017)") ) +
  tm_basemap("OpenStreetMap"))

tmap_save(test, filename = paste0("ke-", crop_var, ".html"), selfcontained = FALSE)

palette1 <- c("#edf8fb",  "#bfd3e6",  "#9ebcda",  "#8c96c6",  "#8c6bb1",  "#88419d",  "#6e016b")

palette2 <- c("#edf8fb",  "#ccece6",  "#99d8c9",  "#66c2a4",  "#41ae76",  "#238b45",  "#005824")

palette3 <- c("#f0f9e8",  "#ccebc5",  "#a8ddb5",  "#7bccc4",  "#4eb3d3",  "#2b8cbe",  "#08589e")

palette4 <- paletteer::paletteer_c("ggthemes::Red", 30)
palette5 <- paletteer::paletteer_c("ggthemes::Red-Gold", 30)
palette6 <- paletteer::paletteer_c("grDevices::PurpOr", 30, direction = -1)
palette7<- paletteer::paletteer_c("grDevices::SunsetDark", 15, direction = -1)
