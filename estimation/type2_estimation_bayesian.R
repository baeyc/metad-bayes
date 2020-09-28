## Bayesian estimation of meta-d based on Flemming code

## Packages ----------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(reshape2)
library(rjags)
library(coda)
library(lattice)
library(broom)
library(ggpubr)
library(ggmcmc)
library(gridExtra)
library(RColorBrewer)

## Import data
studyname <- "ASSA_young_3types"
data <- read.csv(paste0("data/",studyname,".csv"))

# Create a unique ID for participant x flux (uncomment if both fluxes are analyzed together, comment otherwise)
#data$id <- unique(paste0(data$id,data$flux))

# Flux to be analyzed (uncomment if fluxes are analyzed separately, comment otherwise)
flux_analyze <- "B1"
data %<>% filter(flux %in% flux_analyze)

# Create data in the format required by Flemming's code
comparison <- c(s2="M",s1="part")
nR_S1 <- data.frame(t(data[data$type==comparison["s1"],c("n1","n2","n3","n4","n5","n6")]))
nR_S2 <- data.frame(t(data[data$type==comparison["s2"],c("n1","n2","n3","n4","n5","n6")]))
names(nR_S1) <- unique(data$id)
names(nR_S2) <- unique(data$id)

## Hierarchical meta_d group function ------------------------------------------------------

# List creation for model inputs
nR_S1 <- list(nR_S1)
nR_S2 <- list(nR_S2)

# Generate initialisation for JAGS (for reproducible results) (there should be as many inits as chains)
inits1 <- list(.RNG.name="base::Mersenne-Twister", .RNG.seed=10)
inits2 <- list(.RNG.name="base::Mersenne-Twister", .RNG.seed=20)
inits3 <- list(.RNG.name="base::Mersenne-Twister", .RNG.seed=30)

source("estimation/Function_metad_group.R")
output <- metad_group(nR_S1 = nR_S1, nR_S2 = nR_S2, inits = list(inits1,inits2,inits3), nbchains = 3)

# save results
suffix <- ifelse(exists("flux_analyze"),paste0("flux_",flux_analyze),"all_fluxes")
saveRDS(output,paste0("results/type2_",suffix,"_",studyname,"_comp_",comparison["s2"],"_vs_",comparison["s1"],".rds"))
