## testing bbsBayes2 parallel in HRE env

shhh <- suppressPackageStartupMessages # so I don't get a bunch of start up messages in the output file, a tip I encountered while searching through StackOverflow...
shhh(library(bbsBayes2))
shhh(library(tidyverse))
library(foreach)
library(doParallel)

setwd("C:/Users/SmithAC/Documents/GitHub/HRE_testing")

machine = c(1:3)#c(4,5,6,7,8,9,10)
n_cores = 10

re_run <- FALSE # set to TRUE if re-running poorly converged models

already <- readRDS("completed.rds") %>%
  filter(is.na(test))


miss <- FALSE
output_dir <- "F:/HRE_testing/output"

if(re_run){
sp_list <- readRDS("species_list.rds") %>%
  filter(model == TRUE)
}else{
  sp_list <- readRDS("species_list.rds") %>%
    filter(model == TRUE,
           aou %in% already$aou)
}
if(!is.null(machine)){
sp_list <- sp_list %>%
  filter(vm %in% machine)
}

if(miss){
  sp_list <- readRDS("species_missing.rds") %>%
    filter(vm %in% machine,
           model == TRUE)
}

#if(re_run){
#  sp_list <- sp_list %>%
#    filter(english %in% sp_rerun)
#}
# completed_files <- list.files("output",pattern = "fit_")
# completed_aou <- as.integer(str_extract_all(completed_files,
#                              "[[:digit:]]{1,}",
#                              simplify = TRUE))
# sp_list <- sp_list %>%
#     filter(!aou %in% completed_aou)

# sp_list <- sp_list %>%
#     filter(aou %in% c(3250,2730))


# build cluster -----------------------------------------------------------


cluster <- makeCluster(n_cores, type = "PSOCK")
registerDoParallel(cluster)


test <- foreach(i = c(171:141),
        .packages = c("bbsBayes2",
                      "tidyverse",
                      "cmdstanr"),
        .errorhandling = "pass") %dopar%
  {

   # for(i in 1:4){
    sp <- as.character(sp_list[i,"english"])
    aou <- as.integer(sp_list[i,"aou"])

if(!file.exists(paste0(output_dir,"/fit_latlong_",aou,"-1.csv"))){

    if(!file.exists(paste0(output_dir,"/fit_latlong_",aou,".rds")) |
       re_run){

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

   s <- stratify(by = strat,
              species = sp,
              quiet = TRUE) %>%
  prepare_data(min_max_route_years = 2,
 		min_n_routes = 1,
               quiet = TRUE,
               min_year = fy)

   ## bbsBayes2 models do not currently work unless n_strata > 1
   if(nrow(s$meta_strata) == 1){stop(paste("Only 1 stratum for",sp,"skipping to next species"))}

   if(nrow(s$meta_strata) > 2){ #spatial models are irrelevant with < 3 strata
  bbs_dat <- prepare_spatial(s,
                  strata_map = load_map(strat)) %>%
  prepare_model(.,
                model = "first_diff",
                model_variant = "spatial")

   }else{
     bbs_dat <- prepare_model(s,
                     model = "first_diff",
                     model_variant = "hier")
   }
if(re_run){
fit <- run_model(model_data = bbs_dat,
                 refresh = 400,
                 iter_warmup = 2000,
                 iter_sampling = 2000,
                 thin = 2,
                 output_dir = output_dir,
                 output_basename = paste0("fit_latlong_",aou))

}else{
  fit <- run_model(model_data = bbs_dat,
                   refresh = 400,
                   output_dir = output_dir,
                   output_basename = paste0("fit_latlong_",aou))

}
    }# end of if file.exists
}# end of if already running .csv
  }

parallel::stopCluster(cluster)




