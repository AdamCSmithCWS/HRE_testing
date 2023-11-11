
## calculating the trends for BBS analyses

library(bbsBayes2)
library(tidyverse)
library(foreach)
library(doParallel)

setwd("C:/github/HRE_testing")

output_dir <- "output/"
index_dir <- "indices/"
trends_dir <- "trends/"
Y <- 2022
Y_start <- 1970
Y_ten <- Y - 10

machine = 1
n_cores = 5


sp_list <- readRDS("species_list.rds")


# load three-generation times ---------------------------------------------
three_gen <- read_csv("data/full_bbs_species_list_w3_generations.csv") %>%
    select(aou,GenLength) %>%
    mutate(aou = as.integer(aou))

sp_list <- sp_list %>%
  inner_join(.,three_gen,
             by = c("aou"))

completed_files <- list.files("output",pattern = "fit_")
completed_aou <- as.integer(str_extract_all(completed_files,
                                            "[[:digit:]]{1,}",
                                            simplify = TRUE))
sp_list <- sp_list %>%
  filter(aou %in% completed_aou)


cluster <- makeCluster(n_cores, type = "PSOCK")
registerDoParallel(cluster)


test <- foreach(i = 1:nrow(sp_list), #nrow(sp_list),
                .packages = c("bbsBayes2",
                              "tidyverse",
                              "cmdstanr"),
                .errorhandling = "pass") %dopar%
  {
    sp <- as.character(sp_list[i,"english"])
    aou <- as.integer(sp_list[i,"aou"])
    gen_3 <- max(10,as.integer(round(sp_list[i,"GenLength"]*3)))

    i_file <- paste0(index_dir,paste0("inds_smooth_",aou,".rds"))
    inds <- readRDS(i_file)

    fy <- max(1970,min(inds$indices$year))
    if(aou %in% c(4661,4660)){ #Alder and Willow Flycatcher
      fy <- 1978 #5 years after the split
    }
    if(aou %in% c(10,11,22860)){ # Clark's and Western Grebe and EUCD
      fy <- 1990 #5 years after the split and first year EUCD observed on > 3 BBS routes
    }
    if(aou == 6121){ # CAve Swallow
      fy = 1985
    }


    fy_short <- Y-10
    fy_3 <- Y-gen_3

    tt <- generate_trends(inds,
                          min_year = fy,
                          quantiles = c(0.025, 0.05, 0.10, 0.25, 0.75, 0.9, 0.95, 0.975),
                          prob_decrease = c(0,25,30,50),
                          prob_increase = c(0,33,100))
    saveRDS(tt,file = paste0(trends_dir,paste0("trends_long_",aou,".rds")))

    tt <- generate_trends(inds,
                          min_year = fy_short,
                          quantiles = c(0.025, 0.05, 0.10, 0.25, 0.75, 0.9, 0.95, 0.975),
                          prob_decrease = c(0,25,30,50),
                          prob_increase = c(0,33,100))
    saveRDS(tt,file = paste0(trends_dir,paste0("trends_short_",aou,".rds")))


    tt <- generate_trends(inds,
                          min_year = fy_3,
                          quantiles = c(0.025, 0.05, 0.10, 0.25, 0.75, 0.9, 0.95, 0.975),
                          prob_decrease = c(0,25,30,50),
                          prob_increase = c(0,33,100))
    saveRDS(tt,file = paste0(trends_dir,paste0("trends_3Gen_",aou,".rds")))



  }#end cluster loop



parallel::stopCluster(cluster)



