## Study the correlation between the performances of participants in each language

library(xtable)
library(coda)
library(tidyverse)
library(magrittr)
library(dplyr)

studyname <- "ASSA_young_3types"
comparison <- c(s2="M",s1="part")

resultsA <- readRDS(paste0("../results/type2_flux_A1_",studyname,"_comp_",comparison["s2"],"_vs_",comparison["s1"],".rds"))
resultsB <- readRDS(paste0("../results/type2_flux_B1_",studyname,"_comp_",comparison["s2"],"_vs_",comparison["s1"],".rds"))

# Combine the 3 chains into one data frame
resultsA.df <- as.data.frame(do.call(rbind,resultsA))
resultsB.df <- as.data.frame(do.call(rbind,resultsB))

# Correlation between d'
dprimeA <- resultsA.df %>% select(starts_with("d1"))
dprimeB <- resultsB.df %>% select(starts_with("d1"))

corr.AB <- sapply(1:ncol(dprimeA), FUN = function(i){
  cor(dprimeA[,i],dprimeB[,i])
  })
