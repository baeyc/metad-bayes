# Transition probabilities

# Import libraries
library(markovchain)
library(xtable)
library(gtools)
library(ggplot2)
library(RColorBrewer)
library(patternplot)


# Import file
d <- read.csv("../data/fluxAB20191023.csv")

fluxA <- paste0(d$A1)
fluxB <- paste0(d$B1)

syllA <- c("mi","lo","ra","nou","fo","be","pou","ki")
syllB <- c("ba","ga","vo","pi","to","dou","li","fe")

for (i in 1:length(syllA)){
  fluxA <- gsub(syllA[i],paste0(syllA[i],"-"),fluxA)
}
cutA <- unlist(strsplit(fluxA,"-"))

for (i in 1:length(syllB)){
  fluxB <- gsub(syllB[i],paste0(syllB[i],"-"),fluxB)
}
cutB <- unlist(strsplit(fluxB,"-"))


# TP per syllable
mcFitA <- markovchainFit(data = cutA)
mcFitA$estimate
xtable(mcFitA$estimate@transitionMatrix)

mcFitB <- markovchainFit(data = cutB)
mcFitB$estimate
xtable(mcFitB$estimate@transitionMatrix[c(2,7,3,4,1,6,5,8),c(2,7,3,4,1,6,5,8)])

# TP per word
mcMotsA <- markovchainFit(data = fluxA)
mcMotsA$estimate
xtable(mcMotsA$estimate@transitionMatrix)

mcMotsB <- markovchainFit(data = fluxB)
mcMotsB$estimate
xtable(mcMotsB$estimate@transitionMatrix[c(5,6,1,2,3,4),c(5,6,1,2,3,4)])


# TP per group of three syllables
matItemsA <- as.data.frame(cbind(cutA[1:(length(cutA)-2)],cutA[2:(length(cutA)-1)],cutA[3:length(cutA)]))
matItemsA <- paste0(matItemsA$V1,matItemsA$V2,matItemsA$V3)

summaryA <- prop.table(table(matItemsA))
xtable(100*t(summaryA))

matItemsB <- as.data.frame(cbind(cutB[1:(length(cutB)-2)],cutB[2:(length(cutB)-1)],cutB[3:length(cutB)]))
matItemsB <- paste0(matItemsB$V1,matItemsB$V2,matItemsB$V3)

summaryB <- prop.table(table(matItemsB))
xtable(100*t(summaryB))

d <- as.data.frame(sort(summaryA,decreasing = T))
p <- ggplot(data=d,aes(x=matItemsA,y=Freq)) + geom_bar(stat="identity",fill="grey") + xlab("Three-syllable items") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) + ylab("Frequency")
p
ggsave("../figures/freq_3syll_fluxA.pdf",p,height = 4,width = 8)

d <- as.data.frame(sort(summaryB,decreasing = T))
p <- ggplot(data=d,aes(x=matItemsB,y=Freq)) + geom_bar(stat="identity",fill="grey") + xlab("Three-syllable items") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) + ylab("Frequency")
p
ggsave("../figures/freq_3syll_fluxB.pdf",p,height = 4,width = 8)


# Graphs for describing SDT model
mu0 <- -1.5
mu1 <- 1.5
x <- seq(-4,4,0.1)
y1 <- dnorm(x,mu0,1.25)
y2 <- dnorm(x,mu1,1.25)

gg_color_hue <- function(n) {
  #hues = seq(15, 375, length = n + 1)
  #hcl(h = hues, l = 65, c = 100)[1:n]
  brewer.pal(n,"Dark2")
  ###gray.colors(n, start = 0.3, end = 0.9)
}
coul <- c("Hit rate"=gg_color_hue(2)[2],"False alarm rate"=gg_color_hue(2)[1])
coul2 <- c("signal"=gg_color_hue(2)[2],"noise"=gg_color_hue(2)[1])
#coul2 <- c(coul,lines="black")

dplot <- data.frame(x=c(x,x),y=c(y1,y2),source=c(rep("noise",length(x)),rep("signal",length(x))))
p <- ggplot(data=dplot,aes(x,y,group=source,color=source)) + geom_line(lwd=1) + ylim(c(-0.02,0.5)) +
  geom_segment(x=mu0,xend=mu0,y=0,yend=0.4,linetype=2,color="black") +
  geom_segment(x=mu1,xend=mu1,y=0,yend=0.4,linetype=2,color="black") +
  geom_segment(x=0,xend=mu0,y=0.4,yend=0.4,color="black", arrow = arrow(length = unit(0.1, "inches"))) +
  geom_segment(x=0,xend=mu1,y=0.4,yend=0.4,color="black", arrow = arrow(length = unit(0.1, "inches"))) +
  annotate(geom="text",x=0,y=0.43,label="d'",color="black",family = "Helvetica",size=5) +
  annotate(geom="text",x=mu0,y=-0.02,parse=T,label=as.character(expression(mu[0])),size=5) +
  annotate(geom="text",x=mu1,y=-0.02,parse=T,label=as.character(expression(mu[1])),size=5) +
  geom_segment(x = 0.8, xend=0.8, y=0,yend=0.5, color="darkgrey",linetype=3) +
  annotate(geom="text",x=0.8,y=-
             0.015,label="c",size=5) +
  annotate(geom="text",x=2.2,y=0.475,label="Decision threshold",size=4,col="darkgrey") +
  scale_x_continuous(name="Latent decision variable", breaks=seq(-8,8,2)) +
  scale_y_continuous(name="Probability") +
  stat_function(fun = dnorm, args = list(mean=mu1,sd=1.25),
                xlim = c(0.8,4),
                geom = "area",
                aes(fill = "Hit rate"),
                color = NA,
                alpha = 0.3) +
  stat_function(fun = dnorm, args = list(mean=mu0,sd=1.25),
                xlim = c(0.8,4),
                geom = "area",
                aes(fill = "False alarm rate"),
                color = NA,
                alpha = 0.5) +
  scale_color_manual(name="Source",values=coul2) +
  scale_fill_manual(name="",values=coul) +
  guides(color = guide_legend(order=1),
         fill = guide_legend(order=2))
p
ggsave("../figures/type1_sdtmodel.pdf",p,height = 4, width = 7)

# graph with confidence level

coul <- c("Signal"=gg_color_hue(2)[2],"Noise"=gg_color_hue(2)[1])
alph <- c("High"=0.4,"Medium"=0.25,"Low"=0.1)
c <- 0.8
cNW3 <- -1.75
cNW2 <- -0.5
cW2 <- 2.1
cW3 <- 3
p <- ggplot(data=dplot,aes(x,y,group=source,color=source)) + geom_line(lwd=1) + ylim(c(-0.02,0.5)) +
  geom_segment(x = 0.8, xend=0.8, y=0,yend=0.5, color="darkgrey",linetype=1) +
  annotate(geom="text",x=0.8,y=-
             0.0125,label="c",size=5) +
  scale_x_continuous(name="Latent decision variable", breaks=seq(-8,8,2)) +
  scale_y_continuous(name="Probability") +
  stat_function(fun = dnorm, args = list(mean=mu1,sd=1.25),
                xlim = c(cW3,4),
                geom = "area",
                aes(fill = "Signal", alpha = "High"),
                color = NA) +
  stat_function(fun = dnorm, args = list(mean=mu0,sd=1.25),
                xlim = c(-4,cNW3),
                geom = "area",
                aes(fill = "Noise", alpha = "High"),
                color = NA) +
  stat_function(fun = dnorm, args = list(mean=mu1,sd=1.25),
                xlim = c(cW2,cW3),
                geom = "area",
                aes(fill = "Signal", alpha = "Medium"),
                color = NA) +
  stat_function(fun = dnorm, args = list(mean=mu0,sd=1.25),
                xlim = c(cNW3,cNW2),
                geom = "area",
                aes(fill = "Noise", alpha = "Medium"),
                color = NA) +
  stat_function(fun = dnorm, args = list(mean=mu1,sd=1.25),
                xlim = c(c,cW2),
                geom = "area",
                aes(fill = "Signal", alpha = "Low"),
                color = NA) +
  stat_function(fun = dnorm, args = list(mean=mu0,sd=1.25),
                xlim = c(cNW2,c),
                geom = "area",
                aes(fill = "Noise", alpha = "Low"),
                color = NA) +
  scale_color_manual(name="Source",values=coul2) +
  scale_fill_manual(name="Response",values=coul) +
  scale_alpha_manual(name="Confidence",values=alph,breaks = c("High","Medium","Low")) +
  guides(color = guide_legend(order=1),
         fill = guide_legend(order=2)) +
  geom_segment(x = cNW2, xend=cNW2, y=0,yend=0.5, color="darkgrey",linetype=3) +
  annotate(geom="text",x=cNW2,y=-
             0.015,label=expression(c[0][2]),size=5) +
  geom_segment(x = cNW3, xend=cNW3, y=0,yend=0.5, color="darkgrey",linetype=3) +
  annotate(geom="text",x=cNW3,y=-
             0.015,label=expression(c[0][3]),size=5) +
  geom_segment(x = cW2, xend=cW2, y=0,yend=0.5, color="darkgrey",linetype=3) +
  annotate(geom="text",x=cW2,y=-
             0.015,label=expression(c[12]),size=5) +
  geom_segment(x = cW3, xend=cW3, y=0,yend=0.5, color="darkgrey",linetype=3) +
  annotate(geom="text",x=cW3,y=-
             0.015,label=expression(c[13]),size=5)
p
ggsave("../figures/type2_sdtmodel.pdf",p,height = 4,width = 7)
