

#' Calculate Energy Requirements Based on Age, Sex, Weight and Physical Activity Levels (PAL)
#'
#'
#' Version 1.0.0 
#' Update date: 2025.09.11
#' 
#' This function estimates energy requirements (in kcal) for individuals in a dataset
#' based on age, sex, physical activity level (PAL), and weight. It uses WHO growth reference
#' data for children and user-supplied adult weights. An optional adjustment is included for pregnancy & lactating mothers.
#'
#' @param df A data frame containing individual-level data. Must include columns: `age_y` (years), `age_m` (months, optional), `sex`, and optionally `weight`.
#' @param male_values A character or numeric vector specifying the values used to identify males in the `sex` column. Default is `c("male", "1")`.
#' @param female_values A character or numeric vector specifying the values used to identify females in the `sex` column. Default is `c("female", "2")`.
#' @param weight.m Numeric value for average male adult weight (used if no weight is provided in `df`).
#' @param weight.f Numeric value for average female adult weight (used if no weight is provided in `df`).
#' @param pal Physical Activity Level (PAL) coefficient. Used in energy requirement calculations.
#' @param lac Logical. If `TRUE` (default), energy requirements are increased for lactating women.
#' @param preg Logical. If `TRUE` (default), energy requirements are increased for pregnant individuals.
#' @param prev.preg Numeric variable from 0 to 1. If `preg = TRUE` (default). It is the prevalence of pregnancy in a given context. 
#'
#' @return A modified version of the input data frame, with:
#' - `months`: computed age in months.
#' - `weight_final`: final weight used in energy calculations (from children’s growth reference or adult weight inputs).
#' - `enerc_kcal`: estimated daily energy requirement in kilocalories (kcal).
#' - `lac_women`: estimated extra daily energy requirement in kilocalories (kcal) due to lactation.
#'
#' @details
#' - The function uses the `weight_children()` function to calculate weight for children ≤10 years based on WHO growth standards.
#' - If weight is missing for adults (>10 years), it uses `weight.m` or `weight.f` based on sex.
#' - If `lac = TRUE`, it applies additional kcal needs for lactating women based on children below 24 months were based on US Guidelines 2020-2025.
#' - If `preg = TRUE`, it applies additional kcal needs for pregnant women.
#' - Warnings are issued if any weights remain missing after processing, or if prev.preg is not provided when needed.
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
                              age, weight.m, weight.f, pal, 
                              lac = TRUE, preg = TRUE, prev.preg = NA){

   # Function checks!
  
  # Filling 'weight' if missing (<10 yo)
    df <- weight_children(df)  # Create a new 'weight' column if missing
    
  # Filling 'weight' if missing (10-19 yo)
    df <- weight_older_children(df)

  # Ensure 'age' column exists
    if (!"age" %in% colnames(df)) {
      df$age <- df$age_y  # Create a new 'age' column if missing
    }
  
  # Ensuring unique 'clhhid' (household id) is generated
    if(!"clhhid" %in% colnames(df)){
      
      df$clhhid <- paste0(df$clid,"_",  df$hhid)
    }
    
    # Ensure if preg = TRUE, 'pregnancy' column exists  
    if(preg){
      
      if (!"pregnancy" %in% colnames(df) & is.na(prev.preg)) {
        
        stop(glue::glue("`preg = TRUE` but variable pregnancy is missing. 
                        Please, check variable names or provide `prev.preg`"))
      }
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
    


    
# Adding increase Women Energy req. based on lactation

if(lac){
  
  if ("lactating" %in% colnames(df)) {
    
    df <- df %>% 
      mutate(lac_women = ifelse(lactating %in% c("1", "Y", "yes", "Yes"), case_when(
        clhhid %in% df$clhhid[df$months <= 6] ~ 330,
        clhhid %in% df$clhhid[df$months < 6 & df$months < 24] ~ 400), NA))
    
  } else{
    
    if("relation_head" %in% colnames(df)){
  
  df <- df %>% 
    # Mother and son/daughter (household head or spouse)
    mutate(lac_women = ifelse(hhid_id %in% c(1, 2) & sex == 2, case_when(
      clhhid %in% df$clhhid[df$months <= 6 & df$relation_head ==3] ~ 330,
      clhhid %in% df$clhhid[df$months < 6 & df$months < 24 & df$relation_head ==3] ~ 400), NA)) %>% 
    # Mother and son/daughter (daughter and grandchild of household head)
    mutate(lac_women = ifelse(hhid_id %in% c(3) & sex == 2, case_when(
      clhhid %in% df$clhhid[df$months <= 6 & df$relation_head ==4] ~ 330,
      clhhid %in% df$clhhid[df$months < 6 & df$months < 24 & df$relation_head ==4] ~ 400), lac_women)) %>% 
    # Mother and son/daughter (sister and nephew/niece of household head)
    mutate(lac_women = ifelse(hhid_id %in% c(5) & sex == 2, case_when(
      clhhid %in% df$clhhid[df$months <= 6 & df$relation_head ==7] ~ 330,
      clhhid %in% df$clhhid[df$months < 6 & df$months < 24 & df$relation_head ==7] ~ 400), lac_women)) %>% 
    # In case incorrect labeling of the relationship to the head.
    mutate(lac_women = ifelse(!is.na(lac_women) & (age_y <14 | age_y >45), NA, lac_women))
  
  # Log how many lactating women were added
  
  lactating_women_prop <- df %>%
    filter(!is.na(lac_women)) %>%
    nrow()/length(df$clhhid[df$sex == 2 & df$age_y>14 | df$age_y <45])*100 
  
  message(glue::glue("The percentage of lactating women were {round(lactating_women_prop, 2)} by relation_head"))
  
    }  else {
  
      df <- df %>% 
        mutate(lac_women = case_when(
          clhhid %in% df$clhhid[df$months <= 6] ~ 330,
          clhhid %in% df$clhhid[df$months < 6 & df$months < 24] ~ 400), 
          lac_women = ifelse(!is.na(lac_women) & (age_y <14 | age_y >45 | sex == 1), NA, lac_women))
      
      # Log how many lactating women were added
      
      lactating_women_prop <- df %>%
        filter(!is.na(lac_women)) %>%
        nrow()/length(df$clhhid[df$sex == 2 & df$age_y>14 | df$age_y <45])*100 %>% round(., 2)
      
      message(glue::glue("The percentage of lactating women were {round(lactating_women_prop, 2)} by infants <24months"))
      
    }
  
      
  }
  
  # Summing the energy needs from lactation
  df <- df %>% 
    mutate(enerc_kcal = enerc_kcal + lac_women)
    
}
    
 # Adding increase HH Energy req. based on pregnancy
    
    if(preg){
    
      if ("pregnancy" %in% colnames(df)) {
        
      df <- df %>% mutate(
        enerc_kcal = case_when(
         pregnancy == 1 ~ enerc_kcal + 77100/(9*30),
                    TRUE ~ enerc_kcal))
      
      } else {

      # No info on the database about women being pregnat hence, we need to use the country prevalence
      
      # 1) Count N0 of Women in reproductive age (n)
      n <- which(df$sex %in% c("female", "2") & df$age_y >14 &  df$age_y <45) 
      
      # 2) Generate a dummy variable with the pregnancy Y/N based on prevalence
      set.seed(120)
      preg_var <- paste0("preg_", prev.preg)
      df[n, preg_var] <- rbinom(length(n), 1, prev.preg)
      
      # 3) Adding the extra energy for pregnancy based on 
          df <- df %>% mutate(
            enerc_kcal = case_when(
             !!sym(preg_var) == 1 ~ enerc_kcal + 77100/(9*30),
              TRUE ~ enerc_kcal))
          
      # 4) Log how many pregnant women were added
          
          pregnant_women_count <- df %>%
            filter(!!sym(preg_var) == 1) %>%
            nrow()
          
          message(glue::glue("{pregnant_women_count} pregnant women were added using {prev.preg} prevalence"))
          
      } 
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
