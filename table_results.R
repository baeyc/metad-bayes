## Analyze of results

library(xtable)
library(coda)
library(tidyverse)
library(magrittr)
library(bayestestR)
library(dplyr)

studyname <- "ASSA_young_3types"
flux_analyze <- "B1"
comparison <- c(s2="M",s1="part")

suffix <- ifelse(exists("flux_analyze"),paste0("flux_",flux_analyze),"all_fluxes")
results <- readRDS(paste0("../results/type2_",suffix,"_",studyname,"_comp_",comparison["s2"],"_vs_",comparison["s1"],".rds"))

# Summary (combine the different chains)
summ.res <- summary(results)
# Mean a posteriori
postmean <- data.frame(mean = summ.res$statistics[,"Mean"])
postmean %<>%
  rownames_to_column(var = "Parameter")

# Maximum a posteriori
results.df <- as.data.frame(do.call(rbind,results))
map <- map_estimate(results.df)

# Highest posterior density intervals
HDI <- data.frame(HPDinterval(results, prob = 0.95))
HDI %<>%
  rownames_to_column(var = "Parameter")

# All values in the same dataframe
Fit <- left_join(postmean,map) %>%
  cbind(lower = HDI$lower,
        upper = HDI$upper)


# Table with posterior mean and 95% HDI
tt <- Fit %>% filter(grepl("Mratio|d1|xi|mu|sigma",Parameter))
tt$CI <- paste0("[",signif(tt$lower,3)," ; ",signif(tt$upper,3),"]")
xt <- xtable(tt, digits=3,
             caption = c("Posterior mean, maximum a posteriori (MAP) and 95\\% highest density interval for the model's parameters"),
             label = "tab:posttype2")
print(xt, include.rownames=FALSE)
saveRDS(tt,paste0("../results/postmean_map_hdi_",suffix,"_",studyname,"_comp_",comparison["s2"],"_vs_",comparison["s1"],".rds"))


