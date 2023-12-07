## generate indices for BBS results
library(bbsBayes2)
library(tidyverse)
library(foreach)
library(doParallel)
library(patchwork)

YYYY <- 2022
short_time <- 10

#setwd("C:/Users/SmithAC/Documents/GitHub/HRE_testing")
setwd("C:/GitHub/HRE_testing")


# custom functions to calculate reliability categories and determine website inclusion
source("functions/web_trends.R")
source("functions/reliability.R")



output_dir <- "output"
n_cores = 4


sp_list <- readRDS("species_list.rds") %>%
  filter(model == TRUE)

regs_to_estimate <- c("continent","country","prov_state","bcr","stratum","bcr_by_country")

# load previous coverage data -----------------------------------------------------------

lastyear = read_csv("data/All_2021_BBS_trends.csv")
covs = lastyear[,c("species","bbs_num","Region","Region_alt","Trend_Time","reliab.cov")] %>%
  mutate(Region = ifelse(Region == "Continental","continent",Region),
         Region = ifelse(Region_alt == "Canada","Canada",Region),
         Region = ifelse(Region == "US","United States of America",Region))

three_gens <- read_csv("data/full_bbs_species_list_w3_generations.csv")

three_gens <- three_gens %>%
  select(sp.bbs,GenLength)

sp_list <- sp_list %>%
  inner_join(.,three_gens,
             by = c("aou" = "sp.bbs"))



# CV_threshold <- function(m,ci,thresh = 100){
#   y <- ifelse(ci/m > thresh,TRUE,FALSE)
#   return(y)
# }
#



# reliability category definitions ----------------------------------------

prec_cuts = c(abs(2*((0.7^(1/20))-1)),
              abs(2*((0.5^(1/20))-1)))*100
names(prec_cuts) <- c("High","Medium")

cov_cuts = c(0.5,0.25)
names(cov_cuts) <- c("High","Medium")

pool_cuts = c(0.33,0.1)
names(pool_cuts) <- c("High","Medium")

backcast_cuts = c(0.90,0.75)
names(backcast_cuts) <- c("High","Medium")




# build cluster -----------------------------------------------------------


cluster <- makeCluster(n_cores, type = "PSOCK")
registerDoParallel(cluster)


test <- foreach(i = rev(c(1:99,131:nrow(sp_list))),
                .packages = c("bbsBayes2",
                              "tidyverse",
                              "cmdstanr",
                              "patchwork"),
                .errorhandling = "pass") %dopar%
  {

    # for(i in 1:4){
    sp <- as.character(sp_list[i,"english"])
    esp <- as.character(sp_list[i,"french"])
    aou <- as.integer(sp_list[i,"aou"])
    species_f_bil <- gsub(paste(esp,sp),pattern = "[[:space:]]|[[:punct:]]",
                          replacement = "_")

    cov_sp <- covs %>%
      filter(bbs_num == aou)

    if(file.exists(paste0("Indices/Inds_",aou,".rds"))){



      # identifying first years for selected species ----------------------------
      fy <- 1970
      if(aou %in% c(4661,4660)){ #Alder and Willow Flycatcher
        fy <- 1978 #5 years after the split
      }
      if(aou %in% c(10,11,22860)){ # Clark's and Western Grebe and EUCD
        fy <- 1990 #5 years after the split and first year EUCD observed on > 3 BBS routes
      }
      if(aou == 6121){ # CAve Swallow
        fy = 1985
      }

      ## set three generations
      ## unless < 10, then 10   or  unless > number of years available, then n-years
      gen3 <- min((YYYY-fy),max(10,round(as.numeric(sp_list[i,"GenLength"])*3)))


      inds <- readRDS(paste0("Indices/Inds_",aou,".rds"))


# Rolling trend calculations by three-generations -------------------------


      starts <- c(seq(fy,(YYYY-gen3),by = 1))

      roll_trends_out <- NULL

      trajs <- readRDS(paste0("Figures/temp_rds_storage/",aou,"_highlevel_simple_trajs.RDS"))


      pdf(paste0("trends/rolling_trend_maps/",species_f_bil,"_rolling_trend_map.pdf"),
          height = 11,
          width = 8.5)

      for(dd in starts){
        trends_10temp <- generate_trends(inds,
                                         min_year = dd,
                                         max_year = dd+gen3,
                                         prob_decrease = c(0,25,30,50),
                                         prob_increase = c(0,33,100))

        roll_trends_out <- bind_rows(roll_trends_out,trends_10temp$trends)

        ends <- inds$indices %>%
          filter(region == "continent",
                 year %in% c(dd,(dd+gen3)))

        map_tmp <- plot_map(trends_10temp,
                                title = FALSE)

        traj1 <- trajs[["continent"]]+
          geom_errorbar(data = ends,
                    aes(x = year, ymin = 0,
                        ymax = index),
                    width = 0,
                    linewidth = 1,
                     colour = "black")+
          # geom_line(data = ends,
          #               aes(x = year, y = index),
          #               linewidth = 1)+
          theme(axis.title = element_text(size = 6))

        displ <- traj1 + map_tmp + plot_layout(design = "
                                               1111
                                               2222
                                               2222
                                               2222
                                               2222")
        print(displ)
      }
      dev.off()

roll_trends_out <- roll_trends_out%>%
  mutate(species = sp,
         espece = esp,
         bbs_num = aou) %>%
  mutate(across(where(is.double),~signif(.,3)))

saveRDS(roll_trends_out, file = paste0("Trends/Rolling_trends/",aou,"_rolling_trends.rds"))


}


  }




parallel::stopCluster(cluster)



