
#' Adjust Energy Requirements based on lactation and other factors
#'
#' This function estimates energy intakes/ needs (in kcal) for individuals in a datasets
#' based on age, sex, breasfeeding and others. It uses WHO growth reference
#' An optional adjustment is included for lactation.
#'
#' @param df A data frame containing individual-level data. Must include columns: `age_y` (years), `age_m` (months, optional), `sex`, and optionally `weight`.
#' @param male_values A character or numeric vector specifying the values used to identify males in the `sex` column. Default is `c("male", "1")`.
#' @param female_values A character or numeric vector specifying the values used to identify females in the `sex` column. Default is `c("female", "2")`.
#' @param weight.m Numeric value for average male adult weight (used if no weight is provided in `df`).
#' @param weight.f Numeric value for average female adult weight (used if no weight is provided in `df`).
#' @param pal Physical Activity Level (PAL) coefficient. Used in energy requirement calculations.
#' @param excl.lact Logical. If `TRUE` (default), energy requirements are removed (between 0 to 6 months, context specific).
#' @param lac Logical. If `TRUE` (default), energy requirements are increased for lactating individuals (between 0 to 6 months).
#' @param preg Logical. If `TRUE` (default), energy requirements are increased for pregnant individuals.
#' @param at_home Logical. If `FALSE` (default), energy requirements are calculated for all HHs members, if `TRUE` members that reported not eating at home during the past 7days are excluded.
#'
#' @return A modified version of the input data frame, with:
#' - `months`: computed age in months.
#' - `weight_final`: final weight used in energy calculations (from children’s growth reference or adult weight inputs).
#' - `enerc_kcal`: estimated daily energy requirement in kilocalories.
#'
#' @details
#' - First need the Energy requirements to be calculated. See `Energy_requirements()` function
#' - If `excl.bf = TRUE`, decreases the energy needs for lactating children up to 11months.
#' - If `comple.bf = TRUE`, decreases the energy needs for lactating children from 12-24months.
#' - If `school = TRUE`, decreases the energy needs for school age children based on school attendance.
#' - If `feeding = TRUE`, decreases the energy needs for school age children based on school grade and info on feeding programme. 
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


Enerc_adjustment <- function(df, 
                             excl.bf = TRUE, excl.age = 6, 
                             comple.bf = TRUE, prev.complebf = 0.60,
                             school = TRUE, #grade = NA,
                             feeding = TRUE, meal_kcal = 629,
                             at_home = FALSE){

  # Ensure exclusive lactation age is correct
  
  if(excl.age>6){
    stop(glue::glue("excl.age is set to {excl.age}, but it can't be higher than 6 months."))
  }
  
  
  # Ensure if feeding = TRUE, 'school feeding' column exists  
   if(feeding){
    
    if(!any(c("exp_feed", "school_feed") %in% colnames(df))) {
      
      stop(glue::glue("`feeding = TRUE` but school feeding variable is missing. 
                        Please, check variable names (exp_feed or school_feed)"))
      
    }
 #    if(school){stop(glue::glue("`feeding = TRUE` & `school = TRUE`,
  #                              you need to select one option")) }
     }
  
# 1) Excluding breast milk contribution towards energy intakes in infants

# Exclusive breastfeeding/lactating - Removing their contribution to HHs energy (not eating food)

if(excl.bf){
  
  df <- df %>% mutate(
    enerc_kcal = case_when(
      months > 8 & months <= 11 ~ enerc_kcal - 379, 
      months > 5 & months <= 8 ~ enerc_kcal - 413, 
      months > excl.age & months <= 5 ~ enerc_kcal - 474, 
      months <= excl.age ~ 0, #Assuming only exclusive bf of certain age, for the other is complementary
      TRUE ~ enerc_kcal), 
    enerc_kcal = ifelse(months<12 & enerc_kcal <0, NA, enerc_kcal))
  
  
}      

  # Continued breastfeeding - Decreasing HH Energy contribution based prevalence
  
  if(comple.bf){
    
    if ("comple.bf" %in% colnames(df)) {
      
      df <- df %>% mutate(
        enerc_kcal = case_when(
          comple.lact %in% c("1", "Y", "yes") ~ enerc_kcal - 346 ,
          TRUE ~ enerc_kcal))
      
    } else {
      
      # No info on the database about infant being continued breastfed hence, we need to use the country prevalence
      
      # 1) Count N0 of infant from 6-24 months (n)
      n <- which(df$months >= 12 &  df$months <24) 
      
      # 2) Generate a dummy variable with the continued breasfeeding Y/N based on prevalence
      set.seed(120)
      comple_bf_var <- paste0("complebf_", prev.complebf)
      df[n, comple_bf_var] <- rbinom(length(n), 1, prev.complebf)
      
      # 3) Decreasing HH Energy contribution based prevalence
      df <- df %>% mutate(
        enerc_kcal = case_when(
          !!sym(comple_bf_var) == 1 ~ enerc_kcal - 346,
          TRUE ~ enerc_kcal))
      
      # 4) Log how many pregnant women were added
      
      comple_bf_count <- df %>%
        filter(!!sym(comple_bf_var) == 1) %>%
        nrow()
      
      message(glue::glue("The energy was reduced for {comple_bf_count} infant (12-24months) using {prev.complebf} prevalence"))
      
    } 
  }
  
 # School feeding
    
    if(feeding){
      
      if ("exp_feed" %in% colnames(df)) {
      
        df <- df %>% mutate(
          expendi_feeding = ifelse(age_y >=6 & age_y <= 13 & # SAC
                                     school_attend == 1 & # only if currently attending to school now
                                     exp_feed >0 & !is.na(exp_feed), 1 , 0), 
          enerc_kcal_feeding = ifelse(expendi_feeding == 1,  enerc_kcal - meal_kcal, enerc_kcal))
        
    }
    
      if ("school_feed" %in% colnames(df)) {
        
        df <- df %>% mutate(
          enerc_kcal_feeding = 
        ifelse(age_y >=6 & age_y <= 13 & school_feed %in% c("1", "Y", "yes") & !is.na(school_feed), 
               enerc_kcal - meal_kcal, enerc_kcal))
      
      }
    }
  
  #Assume all school children of certain age got feeding
  if(school){
    
    df <- df %>% mutate(
      school_feeding = ifelse(age_y >=6 & age_y <= 13 & school_attend == 1 & !is.na(school_attend), 1 , 0), 
      enerc_kcal_school = ifelse(school_feeding == 1,  enerc_kcal - meal_kcal, enerc_kcal))
  }    
  
# ) Excluding household members eating out

# Excl. hh member that did not eat at home past 7 days

if(at_home){
  
  df <- df %>% mutate(
    enerc_kcal = case_when(
      eat_home == "0" ~ NA,
      TRUE ~ enerc_kcal))
  
} 
  return(df)
  
}