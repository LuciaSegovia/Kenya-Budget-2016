





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
  
  ## SAC only ------
  
  
  nut_var <- "sac_cons_energy_kcal"
  nut_var <- "sac_cons_zinc_mg"
  
  colours <- c("Current" = "#003f5c", "Potential improvement" = "#ffa600")
  
  survey_design_dummy %>% 
    group_by(county_name) %>% 
    summarise(across(starts_with(nut_var),
                     ~srvyr::survey_median(.x))) %>%  select(-ends_with("se")) %>% 
    rowwise() %>% 
    mutate(mymean = mean(c(sac_cons_zinc_mg, sac_cons_zinc_mg_school))) %>% 
    arrange(mymean) %>% 
    mutate(variable = factor(county_name, county_name)) %>% 
    filter(!is.na(sac_cons_zinc_mg)) %>% 
    ggplot() +
    geom_segment(aes(x=variable, xend=variable, 
                     y=sac_cons_zinc_mg, yend=sac_cons_zinc_mg_school), color="darkgrey") +
    geom_point(aes(x=variable, y=sac_cons_zinc_mg, color= "Current"), alpha = 0.6, size=4) +
    geom_point( aes(x=variable, y=sac_cons_zinc_mg_school, color= "Potential improvement"),alpha = 0.6, size=4) +
    #  geom_hline(yintercept = 6.7) + # semi-refined
    xlab("") +
    theme_bw() +
    coord_flip(
      clip = "off") +
    labs(
      y = paste("Apparent Zn intakes mg/SAC/day"), 
   #   y = paste("Apparent Energy intakes kcal/SAC/day"), 
 #     y = paste("Apparent", gsub("hh_alloc_cons_", "",  nut_var), "AFE/day"), 
      color = "School Age Children apparent intakes") +
    scale_color_manual(values = colours)
  
  
  
  ## KENYA MAP-----
  
  library(dplyr)
  library(ggplot2)
  library(sf) # spatial data manipulation
  library(tmap) # spatial data viz
  
  # Data
  # GIS data
  ken_admin2 <- st_read(here::here("data", "gis", "KEN", "AgroMaps", "Africa", "shapefiles",
                                   "KEN", "admin2", "ken.shp"))
  ken_admin2 <- ken_admin2 %>%
    mutate(county_name = stringr::str_to_title(NAME2_), 
           county_name = case_when(
             county_name == "E. Marakwet" ~ "Elgeyo Marakwet", 
             county_name == "Homa_bay" ~ "Homa Bay", 
             county_name == "Muranga" ~ "Murang'a", 
             county_name == "Nairobi" ~ "Nairobi", 
             county_name == "Taita Taveta" ~ "Taita Taveta", 
             county_name == "Nithi" ~ "Tharaka Nithi", 
             county_name == "Trans-Nzoia" ~ "Trans Nzoia", 
             .default = county_name))
  
  
  # Defining the three different palettes
  palette1 <- c("#edf8fb",  "#bfd3e6",  "#9ebcda",  "#8c96c6",  "#8c6bb1",  "#88419d",  "#6e016b")
  palette2 <- c("#edf8fb",  "#ccece6",  "#99d8c9",  "#66c2a4",  "#41ae76",  "#238b45",  "#005824")
  palette3 <- c("#f0f9e8",  "#ccebc5",  "#a8ddb5",  "#7bccc4",  "#4eb3d3",  "#2b8cbe",  "#08589e")
 
  component_title <- "Zinc (mg/day)" 
  component_title <- "Energy (kcal/day)" 
  
  survey_design %>% group_by(county_name) %>% 
    # group_by(!!sym(var)) %>% 
    summarise(across(starts_with("sac"),
                     ~srvyr::survey_quantile(.x, c(0.25, 0.5, 0.75)))) %>% 
    select(-ends_with("se")) %>%
    left_join(., ken_admin2, by = c("county_name")) %>% 
    # filter(is.na(geometry))
    # class
    #  filter(!is.na(CNTNAME3) & Indicator %in% component & Element == "Average") %>% 
    st_as_sf() %>% 
    tm_shape() +
    tm_polygons(fill = "sac_cons_energy_kcal_q50",
                fill.scale = tm_scale_intervals(values = palette1),
                fill_alpha = 0.5,
                fill.legend =  tm_legend(title = component_title)
    ) +
    tm_credits("Based on Kenya Countinous Household Survey programme 2022.", position = tm_pos_out("center", "bottom"))
  
    