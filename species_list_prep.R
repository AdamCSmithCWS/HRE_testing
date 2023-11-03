### setup up HRE run for bbs

library(bbsBayes2)
library(tidyverse)

all <- load_bbs_data()
species_list <- all$species %>%
  filter(unid_combined == TRUE)

bird <- all$birds %>%
  select(aou,route_data_id,species_total) %>%
  left_join(.,species_list,
            by = "aou")

route <- all$routes

sp_sum <- route %>%
  select(country_num,state_num,route,route_name,bcr,
         year,state,country,route_data_id) %>%
  inner_join(.,bird,
            by = "route_data_id")


sp_list <- sp_sum %>%
  group_by(aou,english,french,species) %>%
  summarise(n_obs = n(),
            n_birds = sum(species_total),
            n_routes = length(unique(route_name)),
            n_years = length(unique(year))) %>%
  arrange(n_routes,n_obs,n_years) %>%
  mutate(model = ifelse((n_obs > 20 &
                        n_years > 20 &
                        n_routes > 2),
                        TRUE,FALSE))
sp_list[,"vm"] <- rep(1:10,length.out = nrow(sp_list))

saveRDS(sp_list, "species_list.rds")




