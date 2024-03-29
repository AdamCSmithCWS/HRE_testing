## convergence confirmation for BBS results
library(bbsBayes2)
library(tidyverse)
library(foreach)
library(doParallel)


#setwd("C:/Users/SmithAC/Documents/GitHub/HRE_testing")
setwd("C:/GitHub/HRE_testing")


# set output_dir to the directory where the saved modeling output rds files are stored
output_dir <- "D:/output_BBS"
# output_dir <- "output"
# output_dir <- "F:/HRE_testing/output"



n_cores = 2
re_run <- FALSE # set to TRUE if re-assessing convergence of models


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
       (!file.exists(paste0("Convergence/summ_",aou,".rds")) | re_run )){

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


parallel::stopCluster(cluster)


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

saveRDS(summ_comb,"Convergence/All_species_convergence_summary.rds")



fail <- summ_comb %>%
  filter(rhat > 1.05) %>%
  mutate(variable_type = str_extract(variable,"^\\w+")) %>%
  group_by(species,sp_n,variable_type) %>%
  summarise(n_fail = n(),
            max_rhat = max(rhat))


sp <- "Sandhill Crane"
sp_sel <- summ_comb %>%
  filter(species == sp) %>%
  mutate(variable_type = str_extract(variable,"^\\w+"))


sdste <- sp_sel %>%
  filter(grepl("sdste",variable_type))
ste <- sp_sel %>%
  filter(grepl("ste_raw",variable_type)) %>%
  mutate(scaled_mean = mean*as.numeric(sdste$mean),
         scaled_lci = q5*as.numeric(sdste$mean),
         scaled_uci = q95*as.numeric(sdste$mean))

