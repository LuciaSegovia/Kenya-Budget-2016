

#' Calculate Energy Requirements Based on Age, Sex, and Activity
#'
#' This function estimates energy requirements (in kcal) for individuals in a dataset
#' based on age, sex, physical activity level (PAL), and weight. It uses WHO growth reference
#' data for children and user-supplied adult weights. An optional adjustment is included for lactation.
#'
#' @param df A data frame containing individual-level data. Must include columns: `age_y` (years), `age_m` (months, optional), `sex`, and optionally `weight`.
#' @param male_values A character or numeric vector specifying the values used to identify males in the `sex` column. Default is `c("male", "1")`.
#' @param female_values A character or numeric vector specifying the values used to identify females in the `sex` column. Default is `c("female", "2")`.
#' @param weight.m Numeric value for average male adult weight (used if no weight is provided in `df`).
#' @param weight.f Numeric value for average female adult weight (used if no weight is provided in `df`).
#' @param pal Physical Activity Level (PAL) coefficient. Used in energy requirement calculations.
#' @param lac Logical. If `TRUE` (default), energy requirements are increased for lactating individuals (bewteen 0 to 6 months).
#' @param preg Logical. If `TRUE` (default), energy requirements are increased for pregnant individuals.
#' @param at_home Logical. If `FALSE` (default), energy requirements are calculated for all HHs members, if `TRUE` members that reported not eating at home during the past 7days are excluded.
#'
#' @return A modified version of the input data frame, with:
#' - `months`: computed age in months.
#' - `weight_final`: final weight used in energy calculations (from children’s growth reference or adult weight inputs).
#' - `enerc_kcal`: estimated daily energy requirement in kilocalories.
#'
#' @details
#' - The function uses the `weight_children()` function to calculate weight for children ≤10 years based on WHO growth standards.
#' - If weight is missing for adults (>10 years), it uses `weight.m` or `weight.f` based on sex.
#' - If `lac = TRUE`, it applies additional kcal needs for lactating children up to 6 months.
#' - If `preg = TRUE`, it applies additional kcal needs for pregnant women.
#' - If `at_home = TRUE`, exclude HHs member not eating at home. 
#' - Warnings are issued if any weights remain missing after processing.
#'
#' @examples
#' df <- data.frame(age_y = c(2, 25, 7), age_m = c(6, NA, NA),
#'                  sex = c("male", "female", "2"), weight = c(NA, 60, NA))
#' Enerc_requirement(df, weight.m = 70, weight.f = 60, pal = 1.6)
#'
#' @export

## Generating a function for calculating Energy requirement for populations
library(dplyr)
# Using Kcal and values from WHO, FAO, UNU, 2004.
# https://www.fao.org/4/y5686e/y5686e00.htm#Contents
# Function adapted from Gareth Osman: 
# https://github.com/LuciaSegovia/VHE_Metric/blob/main/Script/vuln.calc.R

source("functions/who_weight.R")

Enerc_requirement <- function(df, male_values = c("male", "1"), 
                              female_values = c("female", "2"), 
                              age, weight.m,weight.f, pal, 
                              lac = TRUE, preg = TRUE, at_home = FALSE){

  # Filling 'weight' if missing (<10 yo)
    df <- weight_children(df)  # Create a new 'weight' column if missing
    
  # Filling 'weight' if missing (10-19 yo)
    df <- weight_older_children(df)

  # Ensure 'age' column exists
    if (!"age" %in% colnames(df)) {
      df$age <- df$age_y  # Create a new 'age' column if missing
    }
    
 # Create final weight column
    df <- df %>%
      mutate(
        weight_final = case_when(
          # Children: always use weight from weight_children()
          age <= 10 ~ weight,
          
          # Adults: only apply weight.m if weight is missing
          age > 15 & sex %in% male_values & is.na(weight) ~ weight.m,
          
          # Adults: only apply weight.f if weight is missing
          age > 15 & sex %in% female_values & is.na(weight) ~ weight.f,
          
          # Adults with existing weight: use as is
          age > 10 ~ weight,
          
          # Fallback
          TRUE ~ NA
        )
      )
    
    # Log how many weights were substituted
    adult_filled_count <- df %>%
      filter(age > 10, is.na(weight), !is.na(weight_final)) %>%
      nrow()
    
    message(glue::glue("{adult_filled_count} adult weight(s) were filled using weight.m or weight.f."))
    
    # Warn if any weights are still missing
    missing_weights_count <- sum(is.na(df$weight_final))
    
    if (missing_weights_count > 0) {
      warning(glue::glue("{missing_weights_count} individual(s) still have missing weights after substitution."))
    }
  
 # Calculate energy requirements
    
    df <-   df %>%
      mutate(
        enerc_kcal = case_when(
          sex %in% male_values & age < 3 ~ (59.512 * weight_final - 30.4) * pal,
          sex %in% male_values & age >= 3 & age < 10 ~ (22.706 * weight_final + 504.3) * pal,
          sex %in% male_values & age >= 10 & age < 18 ~ (17.686 * weight_final + 658.2) * pal,
          sex %in% male_values & age >= 18 & age < 30 ~ (15.057 * weight_final + 692.2) * pal,
          sex %in% male_values & age >= 30 & age < 60 ~ (11.472 * weight_final + 873.1) * pal,
          sex %in% male_values & age >= 60 ~ (11.711 * weight_final + 587.7) * pal,
          
          sex %in% female_values & age < 3 ~ (58.317 * weight_final - 31.1) * pal,
          sex %in% female_values & age >= 3 & age < 10 ~ (20.315 * weight_final + 485.9) * pal,
          sex %in% female_values & age >= 10 & age < 18 ~ (13.384 * weight_final + 692.6) * pal,
          sex %in% female_values & age >= 18 & age < 30 ~ (14.818 * weight_final + 486.6) * pal,
          sex %in% female_values & age >= 30 & age < 60 ~ (8.126 * weight_final + 845.6) * pal,
          sex %in% female_values & age >= 60 ~ (9.082 * weight_final + 658.5) * pal,
          
          TRUE ~ NA
        )
      )
    

# Adding increase HH Energy req. based on lactation

if(lac){
  
  df <- df %>% mutate(
    enerc_kcal = case_when(
      months <=1 ~ enerc_kcal + 2569/4.18,
      months ==2 ~ enerc_kcal + 2686/4.18,
      months ==3 ~ enerc_kcal + 2760/4.18,
      months ==4 ~ enerc_kcal + 2867/4.18,
      months ==5 ~ enerc_kcal + 2925/4.18,
      months ==6 ~ enerc_kcal + 3138/4.18, 
      TRUE ~ enerc_kcal))
  
}  
    
 # Adding increase HH Energy req. based on pregnancy
    
    if(preg){
      
      df <- df %>% mutate(
        enerc_kcal = case_when(
         pregnancy == 1 ~ enerc_kcal + 77100/(9*30),
                    TRUE ~ enerc_kcal))
      
    } 

  # Excl. hh member that did not eat at home past 7 days
    
    if(at_home){
      
      df <- df %>% mutate(
        enerc_kcal = case_when(
          eat_home == "0" ~ NA,
          TRUE ~ enerc_kcal))
      
    } 
    
return(df)

}

# Testing the function

# rand_df <- data.frame(
#   id = 1:10, 
#   age_y = c(0, 1, 2, 3, 4, 5, 40, 80, 36, 12), 
#   age_m = c(6, NA, 3, 10, 1, 7, NA, NA, NA, NA), 
#   sex = c("1", "2", "1", "2", "1", "2", "2", "1", "2", "1"),  # "M" = Male, "F" = Female
#   weight = c(NA, 12, 15, NA, 20, NA, NA, NA, 48, NA)  # Missing weights to be filled
# ) #%>% 
#   #rename(age = "age_y")
# 
# test2 <- Enerc_requirement(rand_df, weight.m = 60, weight.f = 58, pal = 1.6, lac = TRUE)
