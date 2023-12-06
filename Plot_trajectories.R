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

output_dir <- "output"
n_cores = 4


sp_list <- readRDS("species_list.rds") %>%
  filter(model == TRUE)

regs_to_estimate <- c("continent","country","prov_state","bcr","stratum","bcr_by_country")

# load previous coverage data -----------------------------------------------------------

lastyear = read.csv("data/All_2021_BBS_trends.csv",stringsAsFactors = F)
covs = lastyear[,c("species","bbs_num","Region","Region_alt","Trend_Time","reliab.cov")]




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
    esp <- as.character(sp_list[i,"french"])
    aou <- as.integer(sp_list[i,"aou"])
    species_f_bil <- gsub(paste(esp,sp),pattern = "[[:space:]]|[[:punct:]]",
                          replacement = "_")


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



      inds <- readRDS(paste0("Indices/Inds_",aou,".rds"))

      ind <- readRDS(paste0("Indices/Ind_plot_",aou,".rds"))




      # species diagnostic trajectory plots -------------------------------------

      summ <- readRDS(paste0("Convergence/summ_",aou,".rds"))
      raw_data <- readRDS(paste0("Raw_data/Raw_",aou,".rds"))

      # sdobs <- summ %>%
      #   filter(grepl("sdobs",variable,fixed = TRUE))
      #
      obs_effs <- summ %>%
        filter(grepl("obs_raw[",variable,fixed = TRUE)) %>%
        mutate(observer = row_number())%>%
        mutate(obs_effect = mean) %>% #*sdobs$mean) %>%
        select(observer,obs_effect)

      ste_effs <- summ %>%
        filter(grepl("ste_raw[",variable,fixed = TRUE)) %>%
        mutate(site = row_number())%>%
        mutate(ste_effect = mean) %>% #*sdobs$mean) %>%
        select(site,ste_effect)

      raw_data <- raw_data %>%
        inner_join(.,obs_effs,
                   by = "observer") %>%
        inner_join(ste_effs,
                   by = "site")

      trajs <- plot_indices(ind,
                            title = FALSE,
                            ci_width = 0.9,
                            add_observed_means = TRUE,
                            add_number_routes = TRUE,
                            min_year = fy,
                            title_size = 12,
                            axis_title_size = 10,
                            axis_text_size = 10)



      pdf(file = paste0("Figures/diagnostic_trajectories/",species_f_bil,"_diagnostic_trajectories.pdf"),width = 11,height = 8.5)

      for(j in names(trajs)){
        t1 <- trajs[[j]]
        #t2 <- trajshort[[j]]
        if(grepl("United_States_of_America",j)){
        rr <- str_replace_all(j,"_"," ")
        }else{
          rr <- str_replace_all(j,"_","-")
        }
        rr <- str_replace_all(rr," BCR","-BCR")
        rr <- str_replace_all(rr,"BCR ","BCR_")
        rr <- str_replace_all(rr,"BCR-","BCR_")

        labl <- paste(esp,"/",sp,"-",rr)

          n1 <- inds$indices %>%
            filter(region == rr,
                   year >= fy)

          strats <- str_split_1(unlist(n1[1,"strata_included"]),
                                pattern = " ; ")
          upy <- max(n1$index_q_0.95,na.rm = TRUE)/2

# Summaries of observer and site effects by year --------------------------
          raw_tmp <- raw_data %>%
            filter(strata_name %in% strats,
                   year >= fy) %>%
            group_by(year) %>%
            summarise(mean_obs_eff = mean(obs_effect),
                      q75_obs = mean_obs_eff + 2*(sd(obs_effect)/sqrt(n())),
                      q25_obs = mean_obs_eff - 2*(sd(obs_effect)/sqrt(n())),
                      mean_ste_eff = mean(ste_effect),
                      q75_ste = mean_ste_eff + 2*(sd(ste_effect)/sqrt(n())),
                      q25_ste = mean_ste_eff - 2*(sd(ste_effect)/sqrt(n()))) %>%
            rowwise() %>%
            mutate(mean_obs_eff = max(0,upy+(mean_obs_eff*upy)),
                   q75_obs = min(upy*2,upy+(q75_obs*upy)),
                   q25_obs = max(0,upy+(q25_obs*upy)),
                   mean_ste_eff = max(0,upy+(mean_ste_eff*upy)),
                   q75_ste = min(upy*2,upy+(q75_ste*upy)),
                   q25_ste = max(0,upy+(q25_ste*upy)))


          t1 <- t1 +
            geom_ribbon(data = n1, aes(x = year,y = index,ymin = index_q_0.05,ymax = index_q_0.95),
                        fill = grey(0.5),alpha = 0.2)+
            geom_line(data = n1, aes(x = year,y = index),
                      colour = grey(0.5))+
            geom_pointrange(data = raw_tmp,
                            aes(x = year-0.1,y = mean_obs_eff,
                                ymin = q25_obs,ymax = q75_obs),
                            size = 0, alpha = 0.1, colour = "blue")+
            geom_pointrange(data = raw_tmp,
                            aes(x = year+0.1,y = mean_ste_eff,
                                ymin = q25_ste,ymax = q75_ste),
                            size = 0, alpha = 0.1, colour = "red")+
            geom_hline(yintercept = upy,
                       alpha = 0.1)+
            annotate("text",
                     label = c("Route Effects","Observer Effects"),
                     x = c(fy+5,fy+5),
                     y = c(upy*0.9,upy*1.1),
                     colour = c("red","blue"),
                     alpha = 0.2,
                     size = 2)+
            labs(subtitle = labl)



        print(t1)


      }

      dev.off()  # close diagnostic trajectory plotting



    }


  }


