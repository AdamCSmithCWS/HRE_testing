
## calculating the annual indices for BBS analyses

shhh(library(bbsBayes2))
shhh(library(tidyverse))
library(foreach)
library(doParallel)

setwd("C:/github/HRE_testing")

output_dir <- "output/"
index_dir <- "indices/"
machine = 1
n_cores = 5


sp_list <- readRDS("species_list.rds")

completed_files <- list.files("output",pattern = "fit_")
completed_aou <- as.integer(str_extract_all(completed_files,
                                            "[[:digit:]]{1,}",
                                            simplify = TRUE))
sp_list <- sp_list %>%
  filter(aou %in% completed_aou)

regions_to_calculate <- c("continent",
  "country",
  "prov_state",
  "bcr",
  "stratum")


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

    sp_file <- paste0("fit_",aou)

    fit <- readRDS(paste0(output_dir,sp_file,".rds"))

    inds <- generate_indices(fit,
                             regions = regions_to_calculate,
                             hpdi = TRUE)
    saveRDS(inds,file = paste0(index_dir,paste0("inds_",aou,".rds")))
    inds <- generate_indices(fit,
                             regions = regions_to_calculate,
                             alternate_n = "n_smooth",
                             hpdi = TRUE)
    saveRDS(inds,file = paste0(index_dir,paste0("inds_smooth_",aou,".rds")))

  }#end cluster loop



parallel::stopCluster(cluster)



