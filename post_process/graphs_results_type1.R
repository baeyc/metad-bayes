# libraries
library(ggplot2)
library(reshape2)

## Import data
studyname <- "ASSA_young_3types"

comparison <- c(s2="M",s1="part")
estimates <- readRDS(paste0("../results/type1_",studyname,"_comp_",comparison["s2"],"_vs_",comparison["s1"],"_flux_A_B.rds"))

# if 3 types, two results files are imported (comment if study on 2 types)
comparison2 <- c(s2="M",s1="Rule")
estimates2 <- readRDS(paste0("../results/type1_",studyname,"_comp_",comparison2["s2"],"_vs_",comparison2["s1"],"_flux_A_B.rds"))

# Plot results
dplot <- melt(estimates[,c("hr","far","dprime","c","flux","ind")],id.vars = c("flux","ind"))
dplot$type <- ifelse(dplot$variable %in% c("hr","far"),"Rates","Estimates")
dplot$comp <- paste0(comparison[1]," vs ",comparison[2])

# For 3 types only
dplot2 <- melt(estimates2[,c("hr","far","dprime","c","flux","ind")],id.vars = c("flux","ind"))
dplot2$type <- ifelse(dplot2$variable %in% c("hr","far"),"Rates","Estimates")
dplot2$comp <- paste0(comparison2[1]," vs ",comparison2[2])

dplot <- rbind(dplot,dplot2)

# compute the sample size for each boxplot
dplot <- dplot[is.finite(dplot$value),]
cts <- merge(aggregate(value ~ variable + flux + type + comp, dplot, max),
             aggregate(value ~ variable + flux + type + comp, dplot, length),
             by = c("variable","flux","type","comp"))

dplot$variable <- factor(dplot$variable,levels=c("hr","far","dprime","c"),labels=c("Hit rate","False alarm rate","d'","c"))
cts$variable <- factor(cts$variable,levels=c("hr","far","dprime","c"),labels=c("Hit rate","False alarm rate","d'","c"))
p <- ggplot(data=dplot,aes(y=value,x=comp)) + geom_boxplot(width = 0.5, fill=grey(0.7)) +
  facet_grid(variable~flux, scales = "free") +
  geom_text(data = cts, aes(x=comp,y=value.x*1.25,label = paste0("n=",value.y)))# +
  #scale_fill_grey(start=0.9, end=0.4, name="",labels=c("d'","c","Hit rate","False alarm rate"), breaks = c("dprime","c","hr","far"))
p
ggsave(paste0("../figures/type1_",studyname,"_boxplot_allparams_flux_A_B.pdf"),p,height = 10, width = 8)
