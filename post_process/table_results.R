# Create table with posterior mean and 95% HDI

library(xtable)

studyname <- "ASSA_young_3types"
comparison <- c(s2="M",s1="part")

flux_analyze <- "A1"
suffix <- ifelse(exists("flux_analyze"),paste0("flux_",flux_analyze),"all_fluxes")
ttA <- readRDS(paste0("results/postmean_map_hdi_",suffix,"_",studyname,"_comp_",comparison["s2"],"_vs_",comparison["s1"],".rds"))

flux_analyze <- "B1"
suffix <- ifelse(exists("flux_analyze"),paste0("flux_",flux_analyze),"all_fluxes")
ttB <- readRDS(paste0("results/postmean_map_hdi_",suffix,"_",studyname,"_comp_",comparison["s2"],"_vs_",comparison["s1"],".rds"))


ttA %<>% filter(grepl("mu",Parameter) | grepl("sigma",Parameter)) #%>% mutate(flux="A")
ttB %<>% filter(grepl("mu",Parameter) | grepl("sigma",Parameter)) #%>% mutate(flux="B")
tt <- rbind(ttA,ttB)

xt <- xtable(tt[,c(1,2,3,6)], digits=3,
             caption = c("Posterior mean, maximum a posteriori (MAP) and 95\\% highest density interval for the model's parameters"),
             label = "tab:posttype2")
print(xt, include.rownames=FALSE)
