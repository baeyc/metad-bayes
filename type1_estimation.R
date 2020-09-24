# Estimation of dprime using type 1 SDT model

# libraries
library(ggplot2)
library(reshape2)

# import data
data <- read.csv("../data/ASSA_young_2types.csv")

id <- unique(data$id)
flux <- unique(data$flux)

# n1, n2, n3 -> not a word
# n4, n5, n6 -> word

data$nbW <- data$n4+data$n5+data$n6
data$nbNW <- data$n1+data$n2+data$n3

estimates <- lapply(id,FUN = function(i){
  dd <- data[data$id==i,]
  estflux <- lapply(flux,FUN = function(j){
    ddf <- dd[dd$flux == j,]

    # type 1
    hr <- ddf$nbW[ddf$type=="M"]/ddf$total[ddf$type=="M"]
    far <- ddf$nbW[ddf$type=="NM"]/ddf$total[ddf$type=="NM"]
    dprime <- qnorm(hr) - qnorm(far)
    c <- -0.5 * (qnorm(far) + qnorm(hr))

    # type 2
    hrW1 <- ddf$n6[ddf$type=="M"]/ddf$nbW[ddf$type=="M"]
    farW1 <- ddf$n6[ddf$type=="NM"]/ddf$nbW[ddf$type=="NM"]

    hrNW1 <- ddf$n1[ddf$type=="NM"]/ddf$nbW[ddf$type=="NM"]
    farNW1 <- ddf$n1[ddf$type=="NM"]/ddf$nbW[ddf$type=="NM"]

    hrW2 <- 1 - ddf$n4[ddf$type=="M"]/ddf$nbW[ddf$type=="M"]
    farW2 <- 1 - ddf$n4[ddf$type=="NM"]/ddf$nbW[ddf$type=="NM"]

    hrNW2 <- 1 - ddf$n3[ddf$type=="NM"]/ddf$nbW[ddf$type=="NM"]
    farNW2 <- 1 - ddf$n3[ddf$type=="NM"]/ddf$nbW[ddf$type=="NM"]


    return(data.frame(hr=hr,far=far,dprime=dprime,c=c,
                      hrW1=hrW1,hrW2=hrW2,
                      hrNW1=hrNW1,hrNW2=hrNW2,
                      farW1=farW1,farW2=farW2,
                      farNW1=farNW1,farNW2=farNW2,
                      flux=j,ind=i))
  })
  estflux <- do.call(rbind,estflux)
  return(estflux)
})
estimates <- do.call(rbind,estimates)


# Plot results
dplot <- melt(estimates[,c("hr","far","dprime","c","flux","ind")],id.vars = c("flux","ind"))
dplot$type <- ifelse(dplot$variable %in% c("hr","far"),"Rates","Estimates")

# compute the sample size for each boxplot
dplot <- dplot[is.finite(dplot$value),]
cts <- merge(aggregate(value ~ variable + flux + type, dplot, max),
             aggregate(value ~ variable + flux + type, dplot, length),
             by = c("variable","flux","type"))

p <- ggplot(data=dplot) + geom_boxplot(aes(y=value,x=variable,fill=variable), width = 0.5) +
  facet_wrap(type~flux, scales = "free") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  geom_text(data = cts, aes(x=variable,y=value.x*1.25,label = paste0("n=",value.y))) +
  scale_fill_grey(start=0.9, end=0.4, name="",labels=c("d'","c","Hit rate","False alarm rate"), breaks = c("dprime","c","hr","far"))
p
ggsave("../figures/type1_dprime_estimates.pdf",p,height = 5, width = 7)
