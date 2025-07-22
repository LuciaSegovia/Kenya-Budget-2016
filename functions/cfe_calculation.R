




# This are the parameters to calculate the CFE or CME
years <- 9 # Age of reference
S <-"female" # Sex of bird of reference


cfe_calculation <- function(HH.df, ame, years, S){

  
  
(Energy <- ame$kcal[ame$age == years & ame$sex == S])

if(S == "female"){
  
# Generating AFE, AME and CFE for the household
df <- left_join(HH.df, ame) %>% 
  mutate(cfe = round(kcal/Energy, 2)) %>% #str()
  group_by(HHID) %>% 
  summarise(across(where(is.numeric), sum))

} else{
  
# Generating AFE, AME and CME for the household
df <- left_join(HH.df, ame) %>% 
    mutate(cme = round(kcal/Energy, 2)) %>% #str()
    group_by(HHID) %>% 
    summarise(across(where(is.numeric), sum))
  
  
}
  
}
