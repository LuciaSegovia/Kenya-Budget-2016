## This function fill the BMI of the older childrens (>10years up to 19years old) in the survey using
# the WHO standard data, it can be modified to use a different percentile

# The data needed in the dataset is the sex (boys or girls) and the age
# The function transform it into months.

# Loading packages
library(dplyr)

# This set of functions obtain the BMI of older children -----

# Loading data needs for the function 
who_bmi_boys <- readxl::read_excel(here::here("functions", "data",
                                                 "WHO_bmi_months_boys_10-19.xlsx")) %>% 
  rename(bmi = "P50", months = "Month") # Here we can change the percentile

# Loading data needs for the function 
who_bmi_girls <- readxl::read_excel(here::here("functions", "data",
                                                  "WHO_bmi_months_girls_10-19.xlsx")) %>% 
  rename(bmi = "P50", months = "Month") # Here we can change the percentile

who_bmi <- function(A, who_bmi, bmi = "bmi") {
  
  # Ensure 'bmi' column exists
  if (!"bmi" %in% colnames(A)) {
    A$bmi <- NA  # Create a new 'bmi' column if missing
  }
  
  #Converting age in year to months
  A$months <- ifelse(is.na(A$age_m), A$age_y*12, A$age_y*12+A$age_m)
  
  # Identify rows where bmi is missing
  missing_rows <- is.na(A[[bmi]])
  
  # Match missing months to who_bmi_boys and retrieve corresponding bmis
  matched_indices <- match(A$months[missing_rows], who_bmi$months)
  
  # Filter out NA matches before replacing values
  valid_matches <- !is.na(matched_indices)
  
  # Replace missing values with corresponding bmis
  A[[bmi]][missing_rows][valid_matches] <- who_bmi$bmi[matched_indices[valid_matches]]
  
  return(A)
}


# Test dataset:
# 
# rand_df <- data.frame(
#   id = 1:6, 
#   age_y = c(10, 12, 8, 15, 11, 18), 
#   age_m = c(6, NA, 3, 10, 1, 7), 
#   sex = c("1", "2", "1", "2", "1", "2"),  # "M" = Male, "F" = Female
#   bmi = c(NA, NA, NA, NA, NA, NA)  # Missing bmis to be filled
# )
# 
#rand_df <- as.data.frame(ihs4.roster[sample(nrow(ihs4.roster), size=100), ])

# test <- who_bmi(rand_df, who_bmi_boys)
# test <- who_bmi(rand_df, who_bmi_girls)
# 
# test %>% filter(age_y>10) %>% dplyr::select(bmi, months) %>% View()
# 

bmi_children <- function(A, male_values = c("male", "1"), 
                            female_values = c("female", "2")){
  
  # Fill the bmis for girls 
  A_girls <-  A %>% filter(sex %in% female_values) %>% 
    who_bmi(., who_bmi_girls)
  
  # Fill the bmis for boys
  A_boys <- A %>% filter(sex %in% male_values) %>% 
    who_bmi(., who_bmi_boys)
  
  # Handle cases where sex is missing or doesn't match expected values
  A_other <- A %>% filter(!(sex %in% c(female_values, male_values)))
  
  
  A <- bind_rows(A_girls, A_boys, A_other)
  
  return(A)
  
}

# fill_A <- bmi_children(rand_df)
