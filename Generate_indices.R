## generate indices for BBS results
library(bbsBayes2)
library(tidyverse)
library(foreach)
library(doParallel)


#setwd("C:/Users/SmithAC/Documents/GitHub/HRE_testing")
setwd("C:/GitHub/HRE_testing")

output_dir <- "output"
n_cores = 4


sp_list <- readRDS("species_list.rds") %>%
  filter(model == TRUE)

regs_to_estimate <- c("continent","country","prov_state","bcr","stratum","bcr_by_country")



# build cluster -----------------------------------------------------------


cluster <- makeCluster(n_cores, type = "PSOCK")
registerDoParallel(cluster)


test <- foreach(i = rev(1:nrow(sp_list)),
                .packages = c("bbsBayes2",
                              "tidyverse",
                              "cmdstanr"),
                .errorhandling = "pass") %dopar%
  {

    # for(i in 1:4){
    sp <- as.character(sp_list[i,"english"])
    aou <- as.integer(sp_list[i,"aou"])

    if(file.exists(paste0(output_dir,"/fit_",aou,".rds")) &
       !file.exists(paste0("Indices/Inds_",aou,".rds"))){

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



      strat <- "bbs_cws"


      fit <- readRDS(paste0(output_dir,"/fit_",aou,".rds"))

      inds <- generate_indices(fit,
                               alternate_n = "n_smooth",
                               hpdi = TRUE,
                               max_backcast = 15,
                               n_obs_backcast = 2,
                               regions = regs_to_estimate)
      saveRDS(inds,paste0("Indices/Inds_",aou,".rds"))

      ind <- generate_indices(fit,
                               alternate_n = "n",
                               hpdi = TRUE,
                               max_backcast = 15,
                               n_obs_backcast = 2,
                               regions = regs_to_estimate)
      saveRDS(ind,paste0("Indices/Ind_plot_",aou,".rds"))




    }


  }


