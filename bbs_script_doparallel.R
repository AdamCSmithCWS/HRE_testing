## testing bbsBayes2 parallel in HRE env

shhh <- suppressPackageStartupMessages # so I don't get a bunch of start up messages in the output file, a tip I encountered while searching through StackOverflow...
library(bbsBayes2)
library(tidyverse)
library(foreach)
library(doParallel)

#setwd("C:/github/HRE_testing")
setwd("C:/Users/SmithAC/Documents/GitHub/HRE_testing")

output_dir <- "F:/HRE_testing/output"
machine = 4
n_cores = 8


sp_list <- readRDS("species_list.rds") %>%
  filter(vm == machine,
         model == TRUE)

# completed_files <- list.files("output",pattern = "fit_")
# completed_aou <- as.integer(str_extract_all(completed_files,
#                              "[[:digit:]]{1,}",
#                              simplify = TRUE))
# sp_list <- sp_list %>%
#     filter(!aou %in% completed_aou)



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

    if(!file.exists(paste0(output_dir,"/fit_",aou,".rds"))){

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

   s <- stratify(by = strat,
              species = sp,
              quiet = TRUE) %>%
  prepare_data(min_max_route_years = 2,
               quiet = TRUE,
               min_year = fy)

   ## bbsBayes2 models do not currently work unless n_strata > 1
   if(nrow(s$meta_strata) == 1){stop(paste("Only 1 stratum for",sp,"skipping to next species"))}

   if(nrow(s$meta_strata) > 2){ #spatial models are irrelevant with < 3 strata
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
                 #iter_warmup = 300,
                 #iter_sampling = 100,
                 output_dir = output_dir,
                 output_basename = paste0("fit_",aou))

    }# end of if file.exists

  }

parallel::stopCluster(cluster)


