


###  - State of Canada's Birds
YYYY <- 2022

webmaps <- FALSE # set to true if needing to create all map images for ECCC website

library(bbsBayes2)
library(tidyverse)
setwd("C:/GitHub/HRE_testing")


source("functions/mapping.R")
source("functions/loess_func.R")
# custom functions to calculate reliability categories and determine website inclusion



output_dir <- "output"
sp_list <- readRDS("species_list.rds") %>%
  filter(model == TRUE)


# Compile all trends and indices ------------------------------------------------------

trends <- NULL
indices <- NULL
indices_smooth <- NULL

for(i in 1:nrow(sp_list)){


  aou <- as.integer(sp_list[i,"aou"])

  if(file.exists(paste0("Indices/list_",aou,"_indices.rds"))){
  inds_1 <- readRDS(paste0("Indices/list_",aou,"_indices.rds"))

  trends_1 <- readRDS(paste0("Trends/",aou,"_trends.rds"))

  trends <- bind_rows(trends,trends_1)

  inds_1s <- inds_1 %>%
    filter(indices_type == "smooth")
  inds_1f <- inds_1 %>%
    filter(indices_type == "full")

  indices_smooth <- bind_rows(indices_smooth,inds_1s)
  indices <- bind_rows(indices,inds_1f)
}


  print(round(i/nrow(sp_list),2))



}







# Compile and organize trends and indices for SOCB ------------------------

# reconcile with template and Catherine's email








trends_out <- trends %>%
  filter((for_web == TRUE | region %in% c("continent","United States of America"))) %>%
  mutate(prob_LD = prob_decrease_50_percent,
         prob_MD = prob_decrease_25_percent - prob_decrease_50_percent,
         prob_LC = (prob_decrease_0_percent-prob_decrease_25_percent)+(prob_increase_0_percent-prob_increase_33_percent) ,
         prob_MI = prob_increase_33_percent - prob_increase_100_percent,
         prob_LI = prob_increase_100_percent)

test_probs <- trends_out %>%
  mutate(prob_test = prob_LD+prob_MD+prob_LC+prob_MI+prob_LI)

if(any(round(test_probs$prob_test,2) != 1)){stop("probabilites of change categories don't sum properly")}


trends_out <- trends_out %>%
  mutate(years = paste(start_year,end_year,sep = "-"),
         results_code = "BBS",
         season = "breeding",
         version = YYYY,
         species_id = "",
         area_code = ifelse(Region_type == "prov_state",Region,Region_alt),
         area_code = gsub(area_code,pattern = "United States of America",
                          replacement = "USA"),
         model_type = "GAMYE") %>%
  rename(species_code = bbs_num,
         species_name = species,
         period = Trend_Time,
         year_start = Start_year,
         year_end = End_year,
         trnd = Trend,
         lower_ci = Trend_Q0.025,
         upper_ci = Trend_Q0.975,
         percent_change = Percent_Change,
         percent_change_low = Percent_Change_Q0.025,
         percent_change_high = Percent_Change_Q0.975,
         prob_decrease_0 = prob_decrease_0_percent,
         prob_decrease_25 = prob_decrease_25_percent,
         prob_decrease_30 = prob_decrease_30_percent,
         prob_decrease_50 = prob_decrease_50_percent,
         prob_increase_0 = prob_increase_0_percent,
         prob_increase_33 = prob_increase_33_percent,
         prob_increase_100 = prob_increase_100_percent,
         confidence = reliability,
         precision_num = Width_of_95_percent_Credible_Interval,
         precision_cat = precision,
         coverage_num = reliab.cov,
         coverage_cat = coverage,
         sample_size = Mean_Number_of_Routes,
         sample_total = Number_of_Routes,
         prob_LD = prob_LD,
         prob_MD = prob_MD,
         prob_LC = prob_LC,
         prob_MI = prob_MI,
         prob_LI = prob_LI)




trends_socb <- trends_out %>%
  relocate(results_code,
           season,
           version,
           model_type,
           area_code,
           species_code,
           species_id,
           species_name,
           period,
           years,
           year_start,
           year_end,
           trnd,
           lower_ci,
           upper_ci,
           percent_change,
           percent_change_low,
           percent_change_high,
           prob_decrease_0,
           prob_decrease_25,
           prob_decrease_30,
           prob_decrease_50,
           prob_increase_0,
           prob_increase_33,
           prob_increase_100,
           confidence,
           precision_num,
           precision_cat,
           coverage_num,
           coverage_cat,
           sample_size,
           sample_total,
           prob_LD,
           prob_MD,
           prob_LC,
           prob_MI,
           prob_LI)

readr::write_excel_csv(trends_socb,
                       paste0("website/BBS_",YYYY,"_trends_for_socb.csv"))



# SOCB indices ------------------------------------------------------------

indices_round <- readRDS("output/allindices.rds")
smooth_indices_round <- readRDS("output/allsmooth_indices.rds")
smooth_join <- smooth_indices_round %>%
  select(species,region,region_type,trend_time,
         year,index) %>%
  rename(smooth_index = index)

indices_socb <- indices_round %>%
  filter((For_web == TRUE | Region %in% c("Continental","US"))) %>%
  inner_join(.,smooth_join,
             by = c("species",
                    "Region",
                    "Region_type",
                    "Trend_Time",
                    "Year")) %>%
  group_by(species,Region,Region_type,Trend_Time) %>%
  mutate(LOESS_index = loess_func(Index,Year),
         area_code = ifelse(Region_type == "prov_state",Region,Region_alt),
         area_code = gsub(area_code,pattern = "United States of America",
                          replacement = "USA"),
         results_code = "BBS",
         season = "breeding",
         version = YYYY,
         model_type = "GAMYE") %>%
  ungroup() %>%
  rename(species_code = bbs_num,
         species_id = species,
         index = Index,
         year = Year,
         period = Trend_Time,
         upper_ci = Index_q_0.95,
         lower_ci = Index_q_0.05) %>%
  select(-c(Index_q_0.025,
            Index_q_0.975,
            Region_type,
            Region)) %>%
  relocate(results_code,
           season,
           version,
           model_type,
           area_code,
           year,
           period,
           species_code,
           species_id,
           index,
           upper_ci,
           lower_ci,
           LOESS_index,
           smooth_index)

readr::write_excel_csv(indices_socb,
                       file = paste0("website/BBS_",YYYY,"_annual_indices_for_socb.csv"))








