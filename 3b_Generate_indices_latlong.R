## generate indices for BBS results
library(bbsBayes2)
library(tidyverse)
library(foreach)
library(doParallel)


#setwd("C:/Users/SmithAC/Documents/GitHub/HRE_testing")
setwd("C:/GitHub/HRE_testing")

#output_dir <- "D:/output_BBS"
output_dir <- "output"
n_cores = 2

re_run <- FALSE

sp_list <- readRDS("species_list.rds") %>%
  filter(model == TRUE)

regs_to_estimate <- c("continent","stratum")



# build cluster -----------------------------------------------------------


# cluster <- makeCluster(n_cores, type = "PSOCK")
# registerDoParallel(cluster)
#
#
# test <- foreach(i = rev(1:nrow(sp_list)),
#                 .packages = c("bbsBayes2",
#                               "tidyverse",
#                               "cmdstanr"),
#                 .errorhandling = "pass") %dopar%
#   {

 for(i in rev(1:nrow(sp_list))){
    sp <- as.character(sp_list[i,"english"])
    aou <- as.integer(sp_list[i,"aou"])

    if(file.exists(paste0(output_dir,"/fit_latlong_",aou,".rds")) &
       (!file.exists(paste0("Indices/Ind_latlong_",aou,".rds")) | re_run)){

      # identifying first years for selected species ----------------------------
      fy <- NULL
      if(aou %in% c(4661,4660)){ #Alder and Willow Flycatcher
        fy <- 1978 #5 years after the split
      }
      if(aou %in% c(10,11,22860)){ # Clark's and Western Grebe and EUCD
        fy <- 1990 #5 years after the split and first year EUCD observed on > 3 BBS routes
      }
      if(aou == 6121){ # CAve Swallow
        fy = 1985
      }



      strat <- "latlong"


      fit <- readRDS(paste0(output_dir,"/fit_latlong_",aou,".rds"))

      ind <- generate_indices(fit,
                               alternate_n = "n",
                               hpdi = TRUE,
                               max_backcast = 15,
                               regions = regs_to_estimate)
      saveRDS(ind,paste0("Indices/Ind_latlong_",aou,".rds"))

      raw_data <- fit$raw_data

      saveRDS(raw_data,paste0("Raw_data/Raw_latlong_",aou,".rds"))

    }

print(round(i/nrow(sp_list),2))
  }


parallel::stopCluster(cluster)


