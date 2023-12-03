## convergence confirmation for BBS results
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
       !file.exists(paste0("Convergence/summ_",aou,".rds"))){

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

      summ <- get_summary(fit)
      saveRDS(summ,paste0("Convergence/summ_",aou,".rds"))


    }


  }




# Compile convergence values ----------------------------------------------

summ_comb <- NULL

for(i in 1:nrow(sp_list)){

  sp <- as.character(sp_list[i,"english"])
  aou <- as.integer(sp_list[i,"aou"])

  if(file.exists(paste0("Convergence/summ_",aou,".rds"))){


summ <- readRDS(paste0("Convergence/summ_",aou,".rds")) %>%
  mutate(species = sp,
         sp_n = aou)

summ_comb <- bind_rows(summ_comb,
                       summ)


  }

}

fail <- summ_comb %>%
  filter(rhat > 1.05) %>%
  mutate(variable_type = str_extract(variable,"^\\w+")) %>%
  group_by(species,sp_n,variable_type) %>%
  summarise(n_fail = n(),
            max_rhat = max(rhat))

