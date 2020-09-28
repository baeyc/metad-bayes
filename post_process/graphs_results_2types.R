# Plots results

library(reshape)
library(ggplot2)
library(ggmcmc)
library(bayestestR)

studyname <- "ASSA_young_2types"
comparison <- c(s2="M",s1="NM")

flux_analyze <- "A1"
suffix <- ifelse(exists("flux_analyze"),paste0("flux_",flux_analyze),"all_fluxes")
resultsA1 <- readRDS(paste0("results/postmean_map_hdi_",suffix,"_",studyname,"_comp_",comparison["s2"],"_vs_",comparison["s1"],".rds"))
mcmcA1 <- readRDS(paste0("results/type2_",suffix,"_",studyname,"_comp_",comparison["s2"],"_vs_",comparison["s1"],".rds"))

flux_analyze <- "B1"
suffix <- ifelse(exists("flux_analyze"),paste0("flux_",flux_analyze),"all_fluxes")
resultsB1 <- readRDS(paste0("results/postmean_map_hdi_",suffix,"_",studyname,"_comp_",comparison["s2"],"_vs_",comparison["s1"],".rds"))
mcmcB1 <- readRDS(paste0("results/type2_",suffix,"_",studyname,"_comp_",comparison["s2"],"_vs_",comparison["s1"],".rds"))


# HDI for individuals dprime
# Comparison of languages
dplotA <- resultsA1 %>% filter(grepl("d1\\[",Parameter)) %>% select(mean,MAP_Estimate,lower,upper)
dplotA$id <- 1:nrow(dplotA)
dplotA$flux <- "A"
dplotB <- resultsB1 %>% filter(grepl("d1\\[",Parameter)) %>% select(mean,MAP_Estimate,lower,upper)
dplotB$id <- (1:nrow(dplotB)+0.2)
dplotB$flux <- "B"
dplotdprime <- rbind(dplotA,dplotB)


# Plot posterior mean and 95% HDI, and MAP and 95% HDI
p <- ggplot(data=dplotdprime,aes(x=mean,y=id,color=flux)) + scale_y_continuous(breaks=1:23) +
  geom_pointrange(aes(xmin=lower,xmax=upper,y=id)) + geom_vline(xintercept = 0, col="red", alpha=0.5) +
  xlab("d' (95% HDI)") + ylab("Participant ID") + scale_color_grey(start=0.5,end=0.7)
ggsave(paste0("figures/type2_",studyname,"_comp_dprime_postmean_flux_A_B.pdf"),p,width = 8, height = 7)

p <- ggplot(data=dplotdprime,aes(x=MAP_Estimate,y=id,color=flux)) + scale_y_continuous(breaks=1:23) +
  geom_pointrange(aes(xmin=lower,xmax=upper,y=id)) + geom_vline(xintercept = 0, col="red", alpha=0.5) +
  xlab("d' (95% HDI)") + ylab("Participant ID") + scale_color_grey(start=0.5,end=0.7)
ggsave(paste0("figures/type2_",studyname,"_comp_dprime_MAP_flux_A_B.pdf"),p,width = 8, height = 7)


# Idem for Mratio
dplotA <- resultsA1 %>% filter(grepl("Mratio\\[",Parameter)) %>% select(mean,MAP_Estimate,lower,upper)
dplotA$id <- 1:nrow(dplotA)
dplotA$flux <- "A"
dplotB <- resultsB1 %>% filter(grepl("Mratio\\[",Parameter)) %>% select(mean,MAP_Estimate,lower,upper)
dplotB$id <- (1:nrow(dplotB)+0.2)
dplotB$flux <- "B"
dplotMratio <- rbind(dplotA,dplotB)


p2 <- ggplot(data=dplotMratio,aes(x=mean,y=id,color=flux)) + scale_y_continuous(breaks=1:23) +
  geom_pointrange(aes(xmin=lower,xmax=upper,y=id)) + geom_vline(xintercept = 1, col="red", alpha=0.5) +
  xlab("ratio meta-d'/d' (95% HDI)") + ylab("Participant ID") + scale_color_grey(start=0.5,end=0.7)
ggsave(paste0("figures/type2_",studyname,"_comp_Mratio_postmean_flux_A_B.pdf"),p2,width = 8, height = 7)

p2 <- ggplot(data=dplotMratio,aes(x=MAP_Estimate,y=id,color=flux)) + scale_y_continuous(breaks=1:23) +
  geom_pointrange(aes(xmin=lower,xmax=upper,y=id)) + geom_vline(xintercept = 1, col="red", alpha=0.5) +
  xlab("ratio meta-d'/d' (95% HDI)") + ylab("Participant ID") + scale_color_grey(start=0.5,end=0.7)
ggsave(paste0("figures/type2_",studyname,"_comp_Mratio_MAP_flux_A_B.pdf"),p2,width = 8, height = 7)


# Compute individual meta-d
data.mcmc.A1 <- as.data.frame(do.call(rbind,mcmcA1))
dplotA <- data.mcmc.A1 %>% select(starts_with(c("Mratio","d1")))
data.mcmc.B1 <- as.data.frame(do.call(rbind,mcmcB1))
dplotB <- data.mcmc.B1 %>% select(starts_with(c("Mratio","d1")))

nbid <- ncol(dplotA)/2

indiv_metad <- lapply(1:nbid, FUN = function(i){
  shortname <- function(x){strsplit(x,"\\[")[[1]][1]}
  tempA <- dplotA %>% select(ends_with(paste0("[",i,"]"))) %>% 
    mutate(id = i) %>% 
    rename_with(shortname, starts_with("Mratio")) %>%
    rename_with(shortname, starts_with("d1")) %>%
    mutate(metad = Mratio/d1) %>% mutate(flux = "A")
  
  tempB <- dplotB %>% select(ends_with(paste0("[",i,"]"))) %>% 
    mutate(id = i) %>% 
    rename_with(shortname, starts_with("Mratio")) %>%
    rename_with(shortname, starts_with("d1")) %>%
    mutate(metad = Mratio/d1) %>% mutate(flux = "B")
  
  return(rbind(tempA,tempB))
})
indiv_metad <- do.call(rbind,indiv_metad)


## Boxplot of the posterior means for individual d-prime and Mratio
boxplotdprime <- ggplot(data=dplotdprime,aes(x=flux,y=mean,fill=flux)) + geom_boxplot(width=0.5) + 
  xlab("Flux") + ylab("Posterior mean of individual d'") + scale_fill_grey(start=0.5,end=0.7)
ggsave(paste0("figures/type2_",studyname,"_boxplot_postmean_dprime_flux_A_B.pdf"),boxplotdprime,width = 4, height = 4)
boxplotdprime <- ggplot(data=dplotdprime,aes(x=flux,y=MAP_Estimate,fill=flux)) + geom_boxplot(width=0.5) + 
  xlab("Flux") + ylab("Maximum a posteriori of individual d'") + scale_fill_grey(start=0.5,end=0.7)
ggsave(paste0("figures/type2_",studyname,"_boxplot_MAP_dprime_flux_A_B.pdf"),boxplotdprime,width = 4, height = 4)


boxplotMratio <- ggplot(data=dplotMratio,aes(x=flux,y=mean,fill=flux)) + geom_boxplot(width=0.5) + 
  xlab("Flux") + ylab("Posterior mean of individual ratio meta-d'/d'") + scale_fill_grey(start=0.5,end=0.7)
ggsave(paste0("figures/type2_",studyname,"_boxplot_postmean_Mratio_flux_A_B.pdf"),boxplotMratio,width = 4, height = 4)
boxplotMratio <- ggplot(data=dplotMratio,aes(x=flux,y=MAP_Estimate,fill=flux)) + geom_boxplot(width=0.5) + 
  xlab("Flux") + ylab("Maximum a posteriori of individual ratio meta-d'/d'") + scale_fill_grey(start=0.5,end=0.7)
ggsave(paste0("figures/type2_",studyname,"_boxplot_MAP_Mratio_flux_A_B.pdf"),boxplotMratio,width = 4, height = 4)


dmetad <- lapply(1:nbid, FUN = function(i){
  byflux <- lapply(c("A","B"), FUN = function(j){
    temp <- indiv_metad %>% filter(id==i & flux==j) %>%
      summarise(meanratio = mean(Mratio),
                meandprime = mean(d1),
                meanmetad = mean(metad),
                mapratio = map_estimate(Mratio),
                mapdprime = map_estimate(d1),
                mapmetad = map_estimate(metad)) 
    temp %<>% mutate(id=i,flux=j)
  })
  byflux <- do.call(rbind,byflux)
  return(byflux)
}) 
dmetad <- do.call(rbind,dmetad)


boxplotmetad <- ggplot(data=dmetad,aes(x=flux,y=meanmetad,fill=flux)) + geom_boxplot(width=0.5) + 
  xlab("Flux") + ylab("Posterior mean of individual meta-d'") + scale_fill_grey(start=0.5,end=0.7)
ggsave(paste0("figures/type2_",studyname,"_boxplot_postmean_metad_flux_A_B.pdf"),boxplotmetad,width = 4, height = 4)
boxplotmetad <- ggplot(data=dmetad,aes(x=flux,y=mapmetad,fill=flux)) + geom_boxplot(width=0.5) + 
  xlab("Flux") + ylab("Maximum a posteriori of individual meta-d'") + scale_fill_grey(start=0.5,end=0.7)
ggsave(paste0("figures/type2_",studyname,"_boxplot_MAP_metad_flux_A_B.pdf"),boxplotmetad,width = 4, height = 4)


dplotall <- melt(dmetad, id.vars = c("id","flux"))
dplotall$type <- ifelse(grepl("mean",dplotall$variable),"Posterior mean","MAP")
dplotall$variable <- ifelse(grepl("ratio",dplotall$variable),"Mratio",ifelse(grepl("dprime",dplotall$variable),"d'","meta-d"))

boxplotall <- dplotall %>% filter(type=="Posterior mean") %>%
  ggplot(aes(x=flux,y=value)) + geom_boxplot(width=0.5, fill=grey(0.7)) + facet_wrap(~variable,scales = "free") +
  xlab("Flux") + ylab("Posterior mean")
ggsave(paste0("figures/type2_",studyname,"_boxplot_postmean_allparams_flux_A_B.pdf"),boxplotall,width = 10, height = 4)
boxplotall <- dplotall %>% filter(type=="MAP") %>%
  ggplot(aes(x=flux,y=value)) + geom_boxplot(width=0.5, fill=grey(0.7)) + facet_wrap(~variable,scales = "free") +
  xlab("Flux") + ylab("Maximum a posteriori")
ggsave(paste0("figures/type2_",studyname,"_boxplot_MAP_allparams_flux_A_B.pdf"),boxplotall,width = 10, height = 4)


# Plot posterior distribution
# mcmc values in df for plot posterior distributions
mcmc.sample.A1 <- ggs(mcmcA1)
mcmc.sample.B1 <- ggs(mcmcB1)

# Plot posterior distribution for mu_Mratio value
comp_posteriors_flux <- function(paramName){
  plotA <- mcmc.sample.A1 %>%
    filter(Parameter %in% paramName)
  plotB <- mcmc.sample.B1 %>%
    filter(Parameter %in% paramName)
  plotA$flux <- "A"
  plotB$flux <- "B"
  dplot <- rbind(plotA,plotB)
  
  mean_hdi <- rbind(resultsA1,resultsB1) %>% filter(grepl(paramName,name))
  mean_hdi$flux <- c("A","B")
  mean_hdi$yforhdi <- c(50,100)
  
  p <- ggplot(data=dplot,aes(value, fill=flux)) +
    geom_histogram(position = "identity", binwidth = 0.03, alpha = 0.5) +
    #geom_vline(data=mean_hdi,aes(xintercept = mean),linetype="dashed", size = 0.5) +
    #geom_segment(data=mean_hdi,aes(x = lower, y = yforhdi, xend = upper, yend = yforhdi), colour="white", size = 2.5) +
    ylab("Sample count") + scale_fill_grey(start=0.2,end=0.5) +
    xlab(paramName)
  return(p)
}

Mratio_plot <- comp_posteriors_flux("mu_logMratio")
ggsave(paste0("figures/type2_",studyname,"_postdist_mu_logMratio_flux_A_B.pdf"),Mratio_plot,width = 6, height = 5)
mu_d1_plot <- comp_posteriors_flux("mu_d1")
ggsave(paste0("figures/type2_",studyname,"_postdist_mu_d1_flux_A_B.pdf"),mu_d1_plot,width = 6, height = 5)

