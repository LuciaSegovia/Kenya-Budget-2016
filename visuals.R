





# The three methods ------
#pal_base <- c("#EFAC00", "#28A87D")
pal_base <- c("#ffa600","#003f5c", "#35D0BA" )
n <- length(unique(nutrient_afe$county_name)) - 1


  nutrient_afe %>% ungroup() %>%  select(clid, hhid, clhhid, county, county_name, resid,adq_scale,weight_hh, 
                           residency, hh_alloc,  hh_alloc_cons_energy_kcal) %>% 
  tidyr::pivot_wider(., names_from = hh_alloc, values_from = hh_alloc_cons_energy_kcal) %>% 
  as_survey_design(strata = c(county, resid), weights = weight_hh) %>% 
    group_by(county_name) %>% 
    summarise(across(starts_with("afe"),
                     ~srvyr::survey_median(.x))) %>%  select(-ends_with("se")) %>% 
  rowwise() %>% 
  mutate(mymean = mean(c(afe, afe_feed, afe_school) )) %>% 
  arrange(mymean) %>% 
  mutate(variable = factor(county_name, county_name)) %>% 
    pivot_longer(cols = starts_with("afe"),
                 names_to = "method", 
                 values_to = "app_En") %>% 
    ggplot(aes(x = app_En, y = variable )) +
    stat_summary(
      geom = "linerange", fun.min = "min", fun.max = "max", 
      linewidth = 0.8, color = c(rep("black", n-1), rep("grey", 2)))+
    ## white point to overplot line endings
    geom_point(
      aes(x = app_En), size = 5,  stroke = 1, color = "white", fill = "white"
    ) +
    geom_point(
      aes(x = app_En, colour = method), size = 5, alpha =.6 ,stroke = 1) +
    ## app. estima labels
    # geom_text(
    #   aes(label = round(app_Se), 
    #       x = app_Se, vjust = -1, color = method),
    # #  fontface = c(rep("plain", n*2), rep("bold", 2)),
    #   family = "sans", size = 4.2) +
    scale_colour_manual(values = pal_base) +
    xlab("Apparent Energy intake (kcal/AFE/day)") +
    ylab("") +
    theme_bw()# +
 
  
  
 # Current vs potential three methods ------
  
nut_var <- "hh_alloc_cons_energy_kcal"
  
  colours <- c("Current" = "#003f5c", "Potential improvement" = "#ffa600")
  
  nutrient_afe %>% ungroup() %>%  select(clid, hhid, clhhid, county, county_name, resid,adq_scale,weight_hh, 
                                         residency, hh_alloc,  !!sym(nut_var)) %>% 
    tidyr::pivot_wider(., names_from = hh_alloc, values_from = nut_var) %>% 
    as_survey_design(strata = c(county, resid), weights = weight_hh) %>% 
    group_by(county_name) %>% 
    summarise(across(starts_with("afe"),
                     ~srvyr::survey_median(.x))) %>%  select(-ends_with("se")) %>% 
    rowwise() %>% 
    mutate(mymean = mean(c(afe, afe_feed, afe_school) )) %>% 
    arrange(mymean) %>% 
    mutate(variable = factor(county_name, county_name)) %>% 
    ggplot() +
    geom_segment(aes(x=variable, xend=variable, 
                     y=afe_feed, yend=afe_school), color="darkgrey") +
    geom_point(aes(x=variable, y=afe_feed, color= "Current"), alpha = 0.6, size=4) +
    geom_point( aes(x=variable, y=afe_school, color= "Potential improvement"),alpha = 0.6, size=4) +
  #  geom_hline(yintercept = 6.7) + # semi-refined
  xlab("") +
  theme_bw() +
  coord_flip(
    clip = "off") +
    labs(
      y = paste("Apparent", gsub("hh_alloc_cons_", "",  nut_var), "AFE/day"), 
      color = "Kenya apparent intakes") +
    scale_color_manual(values = colours)
    