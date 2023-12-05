## generate indices for BBS results
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

regs_to_estimate <- c("continent","country","prov_state","bcr","stratum","bcr_by_country")

# load previous coverage data -----------------------------------------------------------

lastyear = read.csv("C:/Users/SmithAC/Documents/GitHub/BBS_Summaries/2017estimates/All BBS trends 2017 w reliab.csv",stringsAsFactors = F)
covs = lastyear[,c("sp","species","geo.area","trendtype","trendtime","startyear","reliab.cov")]

covs <- covs[which((covs$trendtime == "full")),]

oldregs = read.csv("C:/Users/SmithAC/Documents/GitHub/BBS_Summaries/old region names.csv",stringsAsFactors = F)

covs = merge(covs,oldregs,by = "geo.area")




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




      inds <- readRDS(paste0("Indices/Inds_",aou,".rds"))

      ind <- readRDS(paste0("Indices/Ind_plot_",aou,".rds"))




      # species diagnostic trajectory plots -------------------------------------


      trajs <- plot_indices(inds,
                            species = paste(sp,esp),
                            ci_width = 0.9,
                            add_observed_means = TRUE,
                            add_number_routes = TRUE,
                            add_adjusted_means = TRUE,
                            title_size = 12,
                            axis_title_size = 10,
                            axis_text_size = 10)

      trajshort <- plot_indices(inds,
                                species = paste(sp,esp),
                                add_observed_means = TRUE,
                                add_number_routes = TRUE,
                                min_year = ((YYYY-short_time)-5), # add five years to the short-term trajectories
                                ci_width = 0.9,
                                title_size = 12,
                                axis_title_size = 10,
                                axis_text_size = 10)



      pdf(file = paste0("Figures/diagnostic_trajectories/",species_f_bil,"_diagnostic_trajectories.pdf"),width = 11,height = 8.5)

      for(i in names(trajs)){
        t1 <- trajs[[i]]
        t2 <- trajshort[[i]]

        # st <- str_trunc(t1$labels$title, width = 8,
        #                 side = "left",
        #                 ellipsis = "")
        if(!is.na(alt_n)){
          n1 <- ind$data_summary %>%
            mutate(Reg_traj = gsub(Region_alt,pattern = "[[:punct:]]|[[:space:]]",replacement = "")) %>%
            filter(Reg_traj == gsub(i,pattern = "[[:punct:]]|[[:space:]]",replacement = "")) #,
          #Region_type == "stratum"
          t1 <- t1 +
            geom_ribbon(data = n1, aes(x = Year,y = Index,ymin = Index_q_0.05,ymax = Index_q_0.95),
                        fill = grey(0.5),alpha = 0.2)+
            geom_line(data = n1, aes(x = Year,y = Index),
                      colour = grey(0.5),)

          n2 <- ind$data_summary %>%
            mutate(Reg_traj = gsub(Region_alt,pattern = "[[:punct:]]|[[:space:]]",replacement = "")) %>%
            filter(Reg_traj == gsub(i,pattern = "[[:punct:]]|[[:space:]]",replacement = ""),
                   #Region_type == "stratum",
                   Year >= ((YYYY-short_time)-5))
          t2 <- t2 +
            geom_ribbon(data = n2, aes(x = Year,y = Index,ymin = Index_q_0.05,ymax = Index_q_0.95),
                        fill = grey(0.5),alpha = 0.2)+
            geom_line(data = n2, aes(x = Year,y = Index),
                      colour = grey(0.5))
        }
        print(t1 + t2)


      }

      dev.off()  # close diagnostic trajectory plotting



    }


  }


