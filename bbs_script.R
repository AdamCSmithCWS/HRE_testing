## testing bbsBayes2 parallel in HRE env

shhh <- suppressPackageStartupMessages # so I don't get a bunch of start up messages in the output file, a tip I encountered while searching through StackOverflow...
shhh(library(bbsBayes2))
shhh(library(tidyverse))


## read in dataset
args <- commandArgs(trailingOnly = TRUE) # to get the input files from command line

machine <- args[1] #ids the vm
i <- args[2] # ids the species to select (1:nspecies?)

sp_list <- readRDS("species_list.rds") %>%
  filter(vm == machine)

sp <- as.character(sp_list[i,"english"])
aou <- sp_list[i,"aou"]

strat <- "bbs_cws"

bbs_dat <- stratify(by = strat,
              species = sp,
              quiet = TRUE) %>%
  prepare_data(min_max_route_years = 2,
               quiet = TRUE) %>%
  prepare_spatial(.,
                  strata_map = load_map(strat)) %>%
  prepare_model(.,
                model = "gamye",
                model_variant = "spatial")

fit <- run_model(model_data = bbs_dat,
                 refresh = 500,
                 output_dir = "output",
                 output_basename = paste0("fit_",aou))



