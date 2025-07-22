
## This function fill the weight of the children in the survey using
# the WHO standard data, it can be modified to use a different percentile

# The data needed in the dataset is the sex (boys or girls) and the age
# The function transform it into months.

# Loading packages
library(dplyr)

# Loading data needs for the function 
who_weight_boys <- readxl::read_excel(here::here("functions", "data",
                                                 "WHO_weight_months_boys_0-10.xlsx")) %>% 
  rename(weight = "P50", months = "Month") # Here we can change the percentile

# Loading data needs for the function 
who_weight_girls <- readxl::read_excel(here::here("functions","data",
                                                 "WHO_weight_months_girls_0-10.xlsx")) %>% 
  rename(weight = "P50", months = "Month") # Here we can change the percentile

## NOTE: We may want to add the data into the function so you don't need to
## add it in the function.
who_weights <- function(A, who_weight, weight = "weight") {
  
  # Ensure 'weight' column exists
  if (!"weight" %in% colnames(A)) {
    A$weight <- NA  # Create a new 'weight' column if missing
  }
  
  #Converting age in year to months
  A$months <- ifelse(is.na(A$age_m), A$age_y*12, A$age_y*12+A$age_m)
  
  # Identify rows where weight is missing
  missing_rows <- is.na(A[[weight]])
  
  # Match missing months to who_weight_boys and retrieve corresponding weights
  matched_indices <- match(A$months[missing_rows], who_weight$months)
  
  # Filter out NA matches before replacing values
  valid_matches <- !is.na(matched_indices)
  
  # Replace missing values with corresponding weights
  A[[weight]][missing_rows][valid_matches] <- who_weight$weight[matched_indices[valid_matches]]
  
  return(A)
}


# Test dataset:
# 
# rand_df <- data.frame(
#   id = 1:6, 
#   age_y = c(0, 1, 2, 3, 4, 5), 
#   age_m = c(6, NA, 3, 10, 1, 7), 
#   sex = c("1", "2", "1", "2", "1", "2"),  # "M" = Male, "F" = Female
#   weight = c(NA, 12, 15, NA, 20, NA)  # Missing weights to be filled
# )

#rand_df <- as.data.frame(ihs4.roster[sample(nrow(ihs4.roster), size=100), ])

# test <- who_weights(rand_df, who_weight_boys)
# test <- who_weights(rand_df, who_weight_girls)
# 
# test %>% filter(age_y<10) %>% dplyr::select(weight, months) %>% View()


weight_children <- function(A, male_values = c("male", "1"), 
                        female_values = c("female", "2")){

  # Fill the weights for girls 
   A_girls <-  A %>% filter(sex %in% female_values) %>% 
       who_weights(., who_weight_girls)
    
  # Fill the weights for boys
    A_boys <- A %>% filter(sex %in% male_values) %>% 
      who_weights(., who_weight_boys)
    
  # Handle cases where sex is missing or doesn't match expected values
    A_other <- A %>% filter(!(sex %in% c(female_values, male_values)))
    
    
A <- bind_rows(A_girls, A_boys, A_other)

  return(A)
  
}

fill_A <- weight_children(rand_df)

# fill_A %>% filter(age_y<10) %>% 
#   dplyr::select(weight, months, sex, case_id) %>% View()


source("functions/who_height.R")
source("functions/who_bmi_10-19.R")

## Weight from BMI older children and adolescents (10-19yo)

weight_older_children <- function(df, male_values = c("male", "1"), 
                            female_values = c("female", "2")){
  
  # Ensure 'weight' column exists
  if (!"weight" %in% colnames(df)) {
    df$weight <- NA  # Create a new 'weight' column if missing
  }
  
  # Ensure 'height' column exists
  df <- height_children(df)  # Create a new 'height' column if missing
  
  # Ensure 'height' column exists
  df <- bmi_children(df)  # Create a new 'bmi' column if missing
  
   # Calculating the weights from heights and BMI
  df <- df %>% mutate(
    weight = case_when(
      is.na(weight) ~ bmi*(height/100)^2,
      TRUE ~ weight))
  
  return(df)
  
}

# Testing dataset
# 
# rand_df <- data.frame(
#   id = 1:6, 
#   age_y = c(10, 12, 8, 15, 11, 18), 
#   age_m = c(6, NA, 3, 10, 1, 7), 
#   sex = c("1", "2", "1", "2", "1", "2"),  # "M" = Male, "F" = Female
#   height = c(NA, NA, NA, NA, NA, NA)  # Missing heights to be filled
# )
# 
# test <- weight_older_children(rand_df)
# 