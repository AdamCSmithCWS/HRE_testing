## testing bbsBayes2 parallel in HRE env

shhh <- suppressPackageStartupMessages # so I don't get a bunch of start up messages in the output file, a tip I encountered while searching through StackOverflow...
shhh(library(bbsBayes2))
shhh(library(tidyverse))
library(foreach)
library(doParallel)

setwd("C:/github/HRE_testing")

machine = 1
n_cores = 3


sp_list <- readRDS("species_list.rds") %>%
  filter(vm == machine,
         model == TRUE)


cluster <- makeCluster(n_cores, type = "PSOCK")
registerDoParallel(cluster)



test <- foreach(i = 9:11, #nrow(sp_list),
        .packages = c("bbsBayes2",
                      "tidyverse",
                      "cmdstanr"),
        .errorhandling = "pass") %dopar%
  {

   # for(i in 1:4){
    sp <- as.character(sp_list[i,"english"])
    aou <- as.integer(sp_list[i,"aou"])


   strat <- "bbs_cws"

   s <- stratify(by = strat,
              species = sp,
              quiet = TRUE) %>%
  prepare_data(min_max_route_years = 2,
               quiet = TRUE)

   if(nrow(s$meta_strata) == 1){next}

   if(nrow(s$meta_strata) > 3){
  bbs_dat <- prepare_spatial(s,
                  strata_map = load_map(strat)) %>%
  prepare_model(.,
                model = "gamye",
                model_variant = "spatial")

   }else{
     bbs_dat <- prepare_model(s,
                     model = "gamye",
                     model_variant = "hier")
   }

fit <- run_model(model_data = bbs_dat,
                 refresh = 100,
                 iter_warmup = 300,
                 iter_sampling = 100,
                 output_dir = "output",
                 output_basename = paste0("fit_",aou))

  }

parallel::stopCluster(cluster)

