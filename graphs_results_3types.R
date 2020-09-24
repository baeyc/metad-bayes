# Plots results

library(reshape)
library(ggplot2)
library(ggmcmc)


# Import result files
studyname <- "ASSA_young_3types"

comparison <- c(s2="M",s1="Rule")
flux_analyze <- "A1"
suffix <- ifelse(exists("flux_analyze"),paste0("flux_",flux_analyze),"all_fluxes")
resultsA1_MRule <- readRDS(paste0("../results/postmean_map_hdi_",suffix,"_",studyname,"_comp_",comparison["s2"],"_vs_",comparison["s1"],".rds"))
mcmcA1_MRule <- readRDS(paste0("../results/type2_",suffix,"_",studyname,"_comp_",comparison["s2"],"_vs_",comparison["s1"],".rds"))

flux_analyze <- "B1"
suffix <- ifelse(exists("flux_analyze"),paste0("flux_",flux_analyze),"all_fluxes")
resultsB1_MRule <- readRDS(paste0("../results/postmean_map_hdi_",suffix,"_",studyname,"_comp_",comparison["s2"],"_vs_",comparison["s1"],".rds"))
mcmcB1_MRule <- readRDS(paste0("../results/type2_",suffix,"_",studyname,"_comp_",comparison["s2"],"_vs_",comparison["s1"],".rds"))


comparison <- c(s2="M",s1="part")
flux_analyze <- "A1"
suffix <- ifelse(exists("flux_analyze"),paste0("flux_",flux_analyze),"all_fluxes")
resultsA1_Mpart <- readRDS(paste0("../results/postmean_map_hdi_",suffix,"_",studyname,"_comp_",comparison["s2"],"_vs_",comparison["s1"],".rds"))
mcmcA1_Mpart <- readRDS(paste0("../results/type2_",suffix,"_",studyname,"_comp_",comparison["s2"],"_vs_",comparison["s1"],".rds"))

flux_analyze <- "B1"
suffix <- ifelse(exists("flux_analyze"),paste0("flux_",flux_analyze),"all_fluxes")
resultsB1_Mpart <- readRDS(paste0("../results/postmean_map_hdi_",suffix,"_",studyname,"_comp_",comparison["s2"],"_vs_",comparison["s1"],".rds"))
mcmcB1_Mpart <- readRDS(paste0("../results/type2_",suffix,"_",studyname,"_comp_",comparison["s2"],"_vs_",comparison["s1"],".rds"))


# HDI for individuals dprime
# Comparison of languages
dplotA_MRule <- resultsA1_MRule %>% filter(grepl("d1\\[",Parameter)) %>% select(mean,MAP_Estimate,lower,upper)
dplotA_MRule$id <- 1:nrow(dplotA_MRule)
dplotA_MRule$flux <- "A"
dplotA_MRule$comp <- "M vs Rule"

dplotB_MRule <- resultsB1_MRule %>% filter(grepl("d1\\[",Parameter)) %>% select(mean,MAP_Estimate,lower,upper)
dplotB_MRule$id <- (1:nrow(dplotB_MRule)+0.2)
dplotB_MRule$flux <- "B"
dplotB_MRule$comp <- "M vs Rule"

dplotA_Mpart <- resultsA1_Mpart %>% filter(grepl("d1\\[",Parameter)) %>% select(mean,MAP_Estimate,lower,upper)
dplotA_Mpart$id <- 1:nrow(dplotA_Mpart)
dplotA_Mpart$flux <- "A"
dplotA_Mpart$comp <- "M vs part"

dplotB_Mpart <- resultsB1_Mpart %>% filter(grepl("d1\\[",Parameter)) %>% select(mean,MAP_Estimate,lower,upper)
dplotB_Mpart$id <- (1:nrow(dplotB_Mpart)+0.2)
dplotB_Mpart$flux <- "B"
dplotB_Mpart$comp <- "M vs part"

dplotdprime <- rbind(dplotA_MRule,
                     dplotB_MRule,
                     dplotA_Mpart,
                     dplotB_Mpart)


# Plot posterior mean and 95% HDI and MAP and 95% HDI
p <- ggplot(data=dplotdprime,aes(x=mean,y=id,color=flux)) + scale_y_continuous(breaks=1:nrow(dplotA_Mpart)) + facet_wrap(~comp) +
  geom_pointrange(aes(xmin=lower,xmax=upper,y=id)) + geom_vline(xintercept = 0, col="red", alpha=0.5) +
  xlab("d' (95% HDI)") + ylab("Participant ID") + scale_color_grey(start=0.5,end=0.7)
ggsave(paste0("../figures/type2_",studyname,"_comp_dprime_postmean_flux_A_B.pdf"),p,width = 9, height = 7)

p <- ggplot(data=dplotdprime,aes(x=MAP_Estimate,y=id,color=flux)) + scale_y_continuous(breaks=1:nrow(dplotA_Mpart)) + facet_wrap(~comp) +
  geom_pointrange(aes(xmin=lower,xmax=upper,y=id)) + geom_vline(xintercept = 0, col="red", alpha=0.5) +
  xlab("d' (95% HDI)") + ylab("Participant ID") + scale_color_grey(start=0.5,end=0.7)
ggsave(paste0("../figures/type2_",studyname,"_comp_dprime_MAP_flux_A_B.pdf"),p,width = 9, height = 7)


# Idem for Mratio
dplotA_MRule <- resultsA1_MRule %>% filter(grepl("Mratio\\[",Parameter)) %>% select(mean,MAP_Estimate,lower,upper)
dplotA_MRule$id <- 1:nrow(dplotA_MRule)
dplotA_MRule$flux <- "A"
dplotA_MRule$comp <- "M vs Rule"

dplotB_MRule <- resultsB1_MRule %>% filter(grepl("Mratio\\[",Parameter)) %>% select(mean,MAP_Estimate,lower,upper)
dplotB_MRule$id <- (1:nrow(dplotB_MRule)+0.2)
dplotB_MRule$flux <- "B"
dplotB_MRule$comp <- "M vs Rule"

dplotA_Mpart <- resultsA1_Mpart %>% filter(grepl("Mratio\\[",Parameter)) %>% select(mean,MAP_Estimate,lower,upper)
dplotA_Mpart$id <- 1:nrow(dplotA_Mpart)
dplotA_Mpart$flux <- "A"
dplotA_Mpart$comp <- "M vs part"

dplotB_Mpart <- resultsB1_Mpart %>% filter(grepl("Mratio\\[",Parameter)) %>% select(mean,MAP_Estimate,lower,upper)
dplotB_Mpart$id <- (1:nrow(dplotB_Mpart)+0.2)
dplotB_Mpart$flux <- "B"
dplotB_Mpart$comp <- "M vs part"


dplotMratio <- rbind(dplotA_MRule,
                     dplotB_MRule,
                     dplotA_Mpart,
                     dplotB_Mpart)


p2 <- ggplot(data=dplotMratio,aes(x=mean,y=id,color=flux)) + scale_y_continuous(breaks=1:nrow(dplotA_Mpart)) + facet_wrap(~comp) +
  geom_pointrange(aes(xmin=lower,xmax=upper,y=id)) + geom_vline(xintercept = 1, col="red", alpha=0.5) +
  xlab("ratio meta-d'/d' (95% HDI)") + ylab("Participant ID") + scale_color_grey(start=0.5,end=0.7)
ggsave(paste0("../figures/type2_",studyname,"_comp_Mratio_postmean_flux_A_B.pdf"),p2,width = 9, height = 7)

p2 <- ggplot(data=dplotMratio,aes(x=MAP_Estimate,y=id,color=flux)) + scale_y_continuous(breaks=1:nrow(dplotA_Mpart)) + facet_wrap(~comp) +
  geom_pointrange(aes(xmin=lower,xmax=upper,y=id)) + geom_vline(xintercept = 1, col="red", alpha=0.5) +
  xlab("ratio meta-d'/d' (95% HDI)") + ylab("Participant ID") + scale_color_grey(start=0.5,end=0.7)
ggsave(paste0("../figures/type2_",studyname,"_comp_Mratio_MAP_flux_A_B.pdf"),p2,width = 9, height = 7)


# Compute individual meta-d
data.mcmc.A1_MRule <- as.data.frame(do.call(rbind,mcmcA1_MRule))
dplotA_MRule <- data.mcmc.A1_MRule %>% select(starts_with(c("Mratio","d1")))
data.mcmc.B1_MRule <- as.data.frame(do.call(rbind,mcmcB1_MRule))
dplotB_MRule <- data.mcmc.B1_MRule %>% select(starts_with(c("Mratio","d1")))

data.mcmc.A1_Mpart <- as.data.frame(do.call(rbind,mcmcA1_Mpart))
dplotA_Mpart <- data.mcmc.A1_Mpart %>% select(starts_with(c("Mratio","d1")))
data.mcmc.B1_Mpart <- as.data.frame(do.call(rbind,mcmcB1_Mpart))
dplotB_Mpart <- data.mcmc.B1_Mpart %>% select(starts_with(c("Mratio","d1")))

nbid <- ncol(dplotA_Mpart)/2

indiv_metad <- lapply(1:nbid, FUN = function(i){
  shortname <- function(x){strsplit(x,"\\[")[[1]][1]}
  tempA_Mpart <- dplotA_Mpart %>% select(ends_with(paste0("[",i,"]"))) %>% 
    mutate(id = i) %>% 
    rename_with(shortname, starts_with("Mratio")) %>%
    rename_with(shortname, starts_with("d1")) %>%
    mutate(metad = Mratio/d1) %>% mutate(flux = "A", comp = "M vs part")
  
  tempB_Mpart <- dplotB_Mpart %>% select(ends_with(paste0("[",i,"]"))) %>% 
    mutate(id = i) %>% 
    rename_with(shortname, starts_with("Mratio")) %>%
    rename_with(shortname, starts_with("d1")) %>%
    mutate(metad = Mratio/d1) %>% mutate(flux = "B", comp = "M vs part")
  
  tempA_MRule <- dplotA_MRule %>% select(ends_with(paste0("[",i,"]"))) %>% 
    mutate(id = i) %>% 
    rename_with(shortname, starts_with("Mratio")) %>%
    rename_with(shortname, starts_with("d1")) %>%
    mutate(metad = Mratio/d1) %>% mutate(flux = "A", comp = "M vs Rule")
  
  tempB_MRule <- dplotB_MRule %>% select(ends_with(paste0("[",i,"]"))) %>% 
    mutate(id = i) %>% 
    rename_with(shortname, starts_with("Mratio")) %>%
    rename_with(shortname, starts_with("d1")) %>%
    mutate(metad = Mratio/d1) %>% mutate(flux = "B", comp = "M vs Rule")
  
  return(rbind(tempA_Mpart,tempA_MRule,tempB_Mpart,tempB_MRule))
})
indiv_metad <- do.call(rbind,indiv_metad)


## Boxplot of the posterior means and MAP for individual d-prime and Mratio
boxplotdprime <- ggplot(data=dplotdprime,aes(x=flux,y=mean,fill=comp)) + geom_boxplot(width=0.5) + 
  xlab("Flux") + ylab("Posterior mean of individual d'") + scale_fill_grey("Comparison",start=0.45,end=0.8)
ggsave(paste0("../figures/type2_",studyname,"_boxplot_postmean_dprime_flux_A_B.pdf"),boxplotdprime,width = 4, height = 4)
boxplotdprime <- ggplot(data=dplotdprime,aes(x=flux,y=MAP_Estimate,fill=comp)) + geom_boxplot(width=0.5) + 
  xlab("Flux") + ylab("Maximum a posteriori of individual d'") + scale_fill_grey("Comparison",start=0.45,end=0.8)
ggsave(paste0("../figures/type2_",studyname,"_boxplot_MAP_dprime_flux_A_B.pdf"),boxplotdprime,width = 4, height = 4)


boxplotMratio <- ggplot(data=dplotMratio,aes(x=flux,y=mean,fill=comp)) + geom_boxplot(width=0.5) + 
  xlab("Flux") + ylab("Posterior mean of individual ratio meta-d'/d'") + scale_fill_grey("Comparison",start=0.45,end=0.8)
ggsave(paste0("../figures/type2_",studyname,"_boxplot_postmean_Mratio_flux_A_B.pdf"),boxplotMratio,width = 4, height = 4)
boxplotMratio <- ggplot(data=dplotMratio,aes(x=flux,y=MAP_Estimate,fill=comp)) + geom_boxplot(width=0.5) + 
  xlab("Flux") + ylab("Maximum a posteriori of individual ratio meta-d'/d'") + scale_fill_grey("Comparison",start=0.45,end=0.8)
ggsave(paste0("../figures/type2_",studyname,"_boxplot_MAP_Mratio_flux_A_B.pdf"),boxplotMratio,width = 4, height = 4)


# Create file with individual posterior mean for Mratio, dprime and metad
listcomp <- unique(indiv_metad$comp)
dmetad <- lapply(1:nbid, FUN = function(i){
  byflux <- lapply(c("A","B"), FUN = function(j){
    bycomp <- lapply(listcomp, FUN = function(k){
      temp <- indiv_metad %>% filter(id==i & flux==j & comp == k) %>%
      summarise(meanratio = mean(Mratio),
                meandprime = mean(d1),
                meanmetad = mean(metad),
                mapratio = map_estimate(Mratio),
                mapdprime = map_estimate(d1),
                mapmetad = map_estimate(metad)) 
      temp %<>% mutate(id=i,flux=j,comp=k)
    })
    bycomp <- do.call(rbind,bycomp)
  })
  byflux <- do.call(rbind,byflux)
  return(byflux)
}) 
dmetad <- do.call(rbind,dmetad)


boxplotmetad <- ggplot(data=dmetad,aes(x=flux,y=meanmetad,fill=comp)) + geom_boxplot(width=0.5) + 
  xlab("Flux") + ylab("Posterior mean of individual meta-d''") + scale_fill_grey("Comparison",start=0.45,end=0.8)
ggsave(paste0("../figures/type2_",studyname,"_boxplot_postmean_metad_flux_A_B.pdf"),boxplotmetad,width = 8, height = 7)
boxplotmetad <- ggplot(data=dmetad,aes(x=flux,y=mapmetad,fill=comp)) + geom_boxplot(width=0.5) + 
  xlab("Flux") + ylab("Maximum a posteriori of individual meta-d''") + scale_fill_grey("Comparison",start=0.45,end=0.8)


dplotall <- melt(dmetad, id.vars = c("id","flux","comp"))
dplotall$type <- ifelse(grepl("mean",dplotall$variable),"Posterior mean","MAP")
dplotall$variable <- ifelse(grepl("ratio",dplotall$variable),"Mratio",ifelse(grepl("dprime",dplotall$variable),"d'","meta-d"))

boxplotall <- dplotall %>% filter(type=="Posterior mean") %>%
  ggplot(aes(x=flux,y=value,fill=comp)) + geom_boxplot(width=0.5) + facet_wrap(~variable,scales = "free") +
  xlab("Flux") + ylab("Posterior mean") + scale_fill_grey("Comparison",start=0.45,end=0.8)
ggsave(paste0("../figures/type2_",studyname,"_boxplot_postmean_allparams_flux_A_B.pdf"),boxplotall,width = 10, height = 4)

boxplotall <- dplotall %>% filter(type=="MAP") %>%
  ggplot(aes(x=flux,y=value,fill=comp)) + geom_boxplot(width=0.5) + facet_wrap(~variable,scales = "free") +
  xlab("Flux") + ylab("Maximum a posteriori") + scale_fill_grey("Comparison",start=0.45,end=0.8)
ggsave(paste0("../figures/type2_",studyname,"_boxplot_MAP_allparams_flux_A_B.pdf"),boxplotall,width = 10, height = 4)



# Plot posterior distribution
# mcmc values in df for plot posterior distributions
mcmc.sample.A1_Mpart <- ggs(mcmcA1_Mpart)
mcmc.sample.B1_Mpart <- ggs(mcmcB1_Mpart)
mcmc.sample.A1_MRule <- ggs(mcmcA1_MRule)
mcmc.sample.B1_MRule <- ggs(mcmcB1_MRule)


# Plot posterior distribution for mu_Mratio value
comp_posteriors_flux <- function(paramName){
  plotAr <- mcmc.sample.A1_MRule %>%
    filter(Parameter %in% paramName) %>% mutate(flux = "A", comp = "M vs Rule")
  plotBr <- mcmc.sample.B1_MRule %>%
    filter(Parameter %in% paramName) %>% mutate(flux = "B", comp = "M vs Rule")
  plotAp <- mcmc.sample.A1_Mpart %>%
    filter(Parameter %in% paramName)%>% mutate(flux = "A", comp = "M vs part")
  plotBp <- mcmc.sample.B1_Mpart %>%
    filter(Parameter %in% paramName) %>% mutate(flux = "B", comp = "M vs part")
  
  dplot <- rbind(plotAr,plotAp,plotBr,plotBp)
  
#  mean_hdi <- rbind(resultsA1,resultsB1) %>% filter(grepl(paramName,name))
#  mean_hdi$flux <- c("A","B")
#  mean_hdi$yforhdi <- c(50,100)
  
  dplot$fact <- paste0(dplot$flux,", ",dplot$comp)
  
  p <- ggplot(data=dplot,aes(value, fill=flux)) +
    geom_histogram(position = "identity", binwidth = 0.03, alpha = 0.5) +
    #geom_vline(data=mean_hdi,aes(xintercept = mean),linetype="dashed", size = 0.5) +
    #geom_segment(data=mean_hdi,aes(x = lower, y = yforhdi, xend = upper, yend = yforhdi), colour="white", size = 2.5) +
    ylab("Sample count") + scale_fill_grey(start=0.45,end=0.7) + facet_wrap(~comp, nrow = 2) + 
    xlab(paramName)
  return(p)
}

Mratio_plot <- comp_posteriors_flux("mu_logMratio")
ggsave(paste0("../figures/type2_",studyname,"_postdist_mu_logMratio_flux_A_B.pdf"),Mratio_plot,width = 8, height = 7)
mu_d1_plot <- comp_posteriors_flux("mu_d1")
ggsave(paste0("../figures/type2_",studyname,"_postdist_mu_d1_flux_A_B.pdf"),mu_d1_plot,width = 8, height = 7)

