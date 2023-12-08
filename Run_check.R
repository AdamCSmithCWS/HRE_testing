## testing bbsBayes2 parallel in HRE env

library(bbsBayes2)
library(tidyverse)
library(foreach)
library(doParallel)

#setwd("C:/github/HRE_testing")
#setwd("C:/Users/SmithAC/Documents/GitHub/HRE_testing")

#output_dir <- "F:/HRE_testing/output"
output_dir <- "output"


  sp_list <- readRDS("species_list.rds") %>%
    filter(model == TRUE)

  sp_track <- sp_list


for(i in 50:nrow(sp_list)){

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

    s <- try(stratify(by = strat,
                  species = sp,
                  quiet = TRUE) %>%
      prepare_data(min_max_route_years = 2,
                   quiet = TRUE,
                   min_year = fy),silent = TRUE)


    if(class(s) == "try-error"){sp_track[i,"test"] <- as.character(paste(s[1]))
    next}
    ## bbsBayes2 models do not currently work unless n_strata > 1
    if(nrow(s$meta_strata) == 1){ sp_track[i,"test"] <- "One-stratum"}

    if(nrow(s$meta_strata) > 2){ #spatial models are irrelevant with < 3 strata
      sp_track[i,"test"] <- "Sufficient data but Missing"
    }


  }else{# end of if file.exists

    sp_track[i,"test"] <- "Complete"

  }








}


  saveRDS(sp_track,paste0("Convergence/sp_track",as.character(Sys.info()["nodename"]),".rds"))
