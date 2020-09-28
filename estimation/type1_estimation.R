# Estimation of dprime using type 1 SDT model

# libraries
library(ggplot2)
library(reshape2)

# import data
## Import data
studyname <- "ASSA_young_3types"
data <- read.csv(paste0("../data/",studyname,".csv"))

comparison <- c(s2="M",s1="part")

# n1, n2, n3 -> not a word
# n4, n5, n6 -> word

data$nbW <- data$n4+data$n5+data$n6
data$nbNW <- data$n1+data$n2+data$n3

estimates <- lapply(unique(data$id),FUN = function(i){
  dd <- data[data$id==i,]
  estflux <- lapply(unique(data$flux),FUN = function(j){
    ddf <- dd[dd$flux == j,]

    # type 1
    hr <- as.numeric(ddf %>% filter(type == comparison["s2"]) %>% mutate(hr=nbW/total) %>% select(hr))
    far <- as.numeric(ddf %>% filter(type == comparison["s1"]) %>% mutate(far=nbW/total) %>% select(far))

    dprime <- qnorm(hr) - qnorm(far)
    c <- -0.5 * (qnorm(far) + qnorm(hr))

    return(data.frame(hr=hr,far=far,dprime=dprime,c=c,flux=j,ind=i))
  })
  estflux <- do.call(rbind,estflux)
  return(estflux)
})
estimates <- do.call(rbind,estimates)

saveRDS(estimates,paste0("../results/type1_",studyname,"_comp_",comparison["s2"],"_vs_",comparison["s1"],"_flux_A_B.rds"))
