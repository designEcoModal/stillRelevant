####   Authors: Phillips, J. & Kominksy, J.                  ###
####                                                         ###
####   Title: Causation and norms of proper functioning:     ###
####           Counterfactuals are (still) relevant          ###
####                                                         ###
####   Contact: phillips01@g.harvard.edu                     ###

#### directory and packages #####
setwd("C:/Users/Jonathan/Dropbox/stillRelevant/stillRelevant")
#setwd("C:/Users/Jonathan/Documents/currentProjects/KominskyCFR_Response")

rm(list=ls())

library(tidyr)
library(mediation)
library(lsr)
library(plyr)
library(ggplot2)
require(wesanderson)# this loads a color palette for the graphs
jphilPalette <- c("darkorange3","lightblue","darkgreen","azure4")
se <- function(x) {sd(x)/sqrt(length(x))}

#### Study 1 ####
d.1 <- read.csv("data/study1.csv")

##demongraphics
d1.age <- matrix(c("mean",mean(d.1$Age,na.rm=T),"sd",sd(d.1$Age,na.rm=T),"n",length(d.1$Age)),ncol = 3)
print(d1.age)

d.1$Sex <- factor(c("Male","Female")[d.1$Sex])
d1.gender <- table(d.1$Sex, exclude=NULL)
print(d1.gender)

d.1$study <- "Study 1"

# Study 1 
d.1$condition <- factor(c("Deceived","Ignorant","Standard","Unintended")[d.1$DO.BR.FL_15])
d.1$question <- factor(c("Action","Agent","Object")[d.1$DO.BR.FL_22])

d.1 <- d.1[,-c(2:33,79:104)]

d.1$Control1_1_1 <- rowSums(d.1[,c(6,21,36)],na.rm=T)
d.1$Control1_1_2 <- rowSums(d.1[,c(7,22,37)],na.rm=T)
d.1$Control1_2_1 <- rowSums(d.1[,c(8,23,38)],na.rm=T)
d.1$Control1_2_2 <- rowSums(d.1[,c(9,24,39)],na.rm=T)

d.1$Control1_1 <- 0
d.1$Control1_1[d.1$Control1_1_1==1 & d.1$Control1_1_2==0] <- 1
d.1$Control1_2 <- 0
d.1$Control1_2[d.1$Control1_2_1==0 & d.1$Control1_2_2==1] <- 1

d.1$Control1 <- 0
d.1$Control1[d.1$Control1_1==1 & d.1$Control1_2==1] <- 1

d.1$Control2_1_1 <- rowSums(d.1[,c(10,25,40)],na.rm=T)
d.1$Control2_1_2 <- rowSums(d.1[,c(11,26,41)],na.rm=T)
d.1$Control2_2_1 <- rowSums(d.1[,c(12,27,42)],na.rm=T)
d.1$Control2_2_2 <- rowSums(d.1[,c(13,28,43)],na.rm=T)

d.1$Control2_1 <- 0
d.1$Control2_1[d.1$Control2_1_1==1 & d.1$Control2_1_2==0] <- 1
d.1$Control2_2 <- 0
d.1$Control2_2[d.1$Control2_2_1==1 & d.1$Control2_2_2==0] <- 1

d.1$Control2 <- 0
d.1$Control2[d.1$Control2_1==1 & d.1$Control2_2==1] <- 1

d.1$control <- 0
d.1$control[d.1$Control1==1 & d.1$Control2==1] <- 1

d.1$estimate1 <- rowSums(d.1[,c(14,29,44)],na.rm = T)
d.1$estimate2 <- rowSums(d.1[,c(15,30,45)],na.rm = T)
d.1$estimate3 <- rowSums(d.1[,c(16,31,46)],na.rm = T)

d.1$relevanceA <- rowSums(d.1[,c(2,17,32)],na.rm = T)
d.1$relevanceB <- rowSums(d.1[,c(3,18,33)],na.rm = T)

d.1$causeA <- rowSums(d.1[,c(4,19,34)],na.rm = T)
d.1$causeB <- rowSums(d.1[,c(5,20,35)],na.rm = T)

# For analyses that include all participants simply delete the d.1$control==1 fromt he line below
d.1 <- d.1[d.1$control==1,-c(2:46,50:63)]

length(unique(d.1$ResponseID))

## bringing in the Samland & Waldmann results
d.sw <- read.csv("Samland_data/mmc2_exp4.csv",stringsAsFactors = F)

d.sw$condition <- factor(c("Standard","Unintended","Ignorant","Deceived")[d.sw$IV1_Norm.Transgression+1])
d.sw$quesiton <- factor(c("Object","Action","Agent")[d.sw$IV2_Question.Type])
d.sw$study <- "S&W"
d.sw <- d.sw[d.sw$Correct.Answer.to.all.Checks.1..2...3...1.yes.0.no.==1,] 
d.sw1 <- d.sw[,c(1,6:7,22:24)] 
colnames(d.sw1) <- c("ResponseID","causeA","causeB","condition","question","study")

## correlation plots
d1.corrPlot1 <- gather(d.1[,-c(1:2,5:10)],causeQ, cause,-c(question,condition))
dsw.corrPlot1 <- gather(d.sw1[,-c(1,6)], causeQ, cause, -c(question,condition))

d1.corrPlot1 <- ddply(d1.corrPlot1, c("condition","question","causeQ"),summarise,
                      d1Mean = mean(cause,na.rm=T))
d1.corrPlotsw <- ddply(dsw.corrPlot1, c("condition","question","causeQ"),summarise,
                       swMean = mean(cause,na.rm=T))

d1.corrPlotR <- gather(d.1[,-c(1:2,5:8,11:12)],relevanceQ, relevance,-c(question,condition))

d1.corrPlotR <- ddply(d1.corrPlotR, c("condition","question","relevanceQ"),summarise,
                      relMean = mean(relevance,na.rm=T))

d1.corrPlotR$relMean <- (d1.corrPlotR$relMean*-1)+2

d1.corrPlot <- cbind(d1.corrPlot1,d1.corrPlotsw[,4],d1.corrPlotR[,4])

colnames(d1.corrPlot) <- c("Condition","Question","Agent","d1Cause","swCause","d1Relevance")

d1.corrPlot$Agent <- factor(d1.corrPlot$Agent) 
d1.corrPlot$Agent <- factor(c("C","V")[d1.corrPlot$Agent]) 

# d1.corrPlot <- melt(d1.corrPlot, id.vars = c(1:3,5))
# d1.corrPlot$variable <- factor(c("Mean Causal Judgment","Mean Relevance Judgment")[d1.corrPlot$variable])

library(ggplot2)
library(wesanderson)

fig1a <- ggplot(d1.corrPlot, aes(x=swCause, y=d1Cause)) +
  geom_point(aes(color=Condition,shape=Question),stat = "identity", position = "jitter",size=rel((6))) +
  geom_text(aes(label=Agent),stat = "identity",size=rel((4))) +
  stat_smooth(method=lm, formula= y ~ x) +
  #coord_cartesian(ylim=c(.15,1)) +
  theme_bw() +
  ylab("Average Causal Judgment") +
  xlab("S&W (2016) Causal Judgment") +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    #,legend.position=c(.85,.15)
    ,legend.title=element_blank()
    ,legend.text=element_text(size=rel(1.25))
    ,axis.text=element_text(size=rel(1.5))
    ,strip.text=element_text(size=rel(1.25))
    ,axis.title.y=element_text(vjust=.9)
    ,axis.ticks = element_blank()
    ,axis.title=element_text(size=rel(1.5))
    ,plot.title = element_text(face="bold",vjust=.75)
  )
fig1a

# ggsave("fig1a.jpg",fig1a,dpi=600)

## Relevance plots

fig1b <- ggplot(d1.corrPlot, aes(x= d1Relevance, y=d1Cause)) +
  geom_point(aes(color=Condition,shape=Question),stat = "identity", position = "jitter",size=rel((6))) +
  geom_text(aes(label=Agent),stat = "identity",size=rel((4))) +
  stat_smooth(method=lm, formula= y ~ x) +
  #coord_cartesian(ylim=c(.2,1)) +
  theme_bw() +
  ylab("Average Causal Judgment") +
  xlab("Average Relevance Judgment") +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    #,legend.position=c(.85,.15)
    ,legend.title=element_blank()
    ,legend.text=element_text(size=rel(1.25))
    ,axis.text=element_text(size=rel(1.5))
    ,strip.text=element_text(size=rel(1.25))
    ,axis.title.y=element_text(vjust=.9)
    ,axis.ticks = element_blank()
    ,axis.title=element_text(size=rel(1.5))
    ,plot.title = element_text(face="bold",vjust=.75)
  )
fig1b


# ggsave("fig1b.jpg",fig1b,dpi=600)
# 
# pdf(file="fig1.pdf",width=10,height = 9)
# print(fig1)
# dev.off()


## Replication of probability estimates 
summary(aov(lm(estimate1~condition, data=d.1)))
summary(aov(lm(estimate2~condition, data=d.1)))
summary(aov(lm(estimate3~condition, data=d.1)))


## Study 1 - primary analyses ####
d.1$cause <- NA
d.1$cause[d.1$causeA==1 & d.1$causeB==1] <- "Both"
d.1$cause[d.1$causeA==0 & d.1$causeB==1] <- "Norm violating"
d.1$cause[d.1$causeA==1 & d.1$causeB==0] <- "Norm conforming"
d.1$cause <- factor(d.1$cause)

d.1$relevance <- NA
d.1$relevance[d.1$relevanceA==1 & d.1$relevanceB==1] <- "Both"
d.1$relevance[d.1$relevanceA==2 & d.1$relevanceB==1] <- "Norm violating"
d.1$relevance[d.1$relevanceA==1 & d.1$relevanceB==2] <- "Norm conforming"
d.1$relevance <- factor(d.1$relevance)


olr1.0 <- polr(cause ~ condition * question,data=d.1)
#interaction effect
dropterm(olr1.0, test = "Chisq")

#main effects
olr1.1 <- polr(cause ~ condition + question,data=d.1)
dropterm(olr1.1,test="Chisq")

## relevance
olr1.4 <- polr(relevance ~ condition * question,data=d.1)
#interaction effect
dropterm(olr1.4, test = "Chisq")
#main effects
olr1.5 <- polr(relevance ~ condition + question,data=d.1)
dropterm(olr1.5,test="Chisq")

cor.test(d.1$causeB,d.1$relevanceB)
cor.test(d.1$causeA,d.1$relevanceA)

names <- c("relevance", "cause")

d1.temp <- d.1[,c(3:4,9:12)]
colnames(d1.temp) <- c("condition","question",rep(names,each=2))

d1.temp <- rbind(d1.temp[,c(1:3,5)],d1.temp[,c(1:2,4,6)])

cor.test(d1.temp$cause[d.1$question=="Agent"],d1.temp$relevance[d.1$question=="Agent"])
cor.test(d1.temp$cause[d.1$question=="Action"],d1.temp$relevance[d.1$question=="Action"])
cor.test(d1.temp$cause[d.1$question=="Object"],d1.temp$relevance[d.1$question=="Object"])


## Statistics for replication table ####

#Standard norm violation
table1a <- aggregate(cause~question, FUN=table, data=d.1[d.1$condition=="Standard",])
chisq.test(as.table(table1a[[2]])[,-2]) #NB: removed norm confirming bc of low values to follow S&W (2016)
cramersV(as.table(table1a[[2]])[,-2])

chisq.test(table(d.1$cause[d.1$condition=="Standard" & d.1$question=="Object"])[-2])
cramersV(table(d.1$cause[d.1$condition=="Standard" & d.1$question=="Object"])[-2])

chisq.test(table(d.1$cause[d.1$condition=="Standard" & d.1$question=="Action"])[-2])
cramersV(table(d.1$cause[d.1$condition=="Standard" & d.1$question=="Action"])[-2])

chisq.test(table(d.1$cause[d.1$condition=="Standard" & d.1$question=="Agent"])[-2])
cramersV(table(d.1$cause[d.1$condition=="Standard" & d.1$question=="Agent"])[-2])

#Unintentional norm violation
table1b <- aggregate(cause~question, FUN=table, data=d.1[d.1$condition=="Unintended",])
chisq.test(table1b[[2]]) 
cramersV(table1b[[2]])

#Ignorant norm violation
table1c <- aggregate(cause~question, FUN=table, data=d.1[d.1$condition=="Ignorant",])
chisq.test(table1c[[2]]) 
cramersV(table1c[[2]])

# Deception-based  
table1d <- aggregate(cause~question, FUN=table, data=d.1[d.1$condition=="Deceived",])
chisq.test(table1d[[2]])
cramersV(table1d[[2]])

# Chemical question 
table1e <- aggregate(cause~condition, FUN=table, data=d.1[d.1$question=="Object",])
chisq.test(table1e[[2]])
cramersV(table1e[[2]])

# Action question 
table1f <- aggregate(cause~condition, FUN=table, data=d.1[d.1$question=="Action",])
chisq.test(table1f[[2]])
cramersV(table1f[[2]])

# Agent question 
table1g <- aggregate(cause~condition, FUN=table, data=d.1[d.1$question=="Agent",])
chisq.test(table1g[[2]])
cramersV(table1g[[2]])

# Difference from standard violation 
table1h <- aggregate(cause~condition, FUN=table, data=d.1[d.1$question=="Agent",])
# unintended violation
chisq.test(as.table(table1h[[2]])[3:4,])
cramersV(as.table(table1h[[2]])[3:4,])
# ignorant violation
chisq.test(as.table(table1h[[2]])[2:3,])
cramersV(as.table(table1h[[2]])[2:3,])
# deception violation
chisq.test(as.table(table1h[[2]])[c(1,3),])
cramersV(as.table(table1h[[2]])[c(1,3),])


## relevance judgments

#Standard norm violation
table1i <- aggregate(relevance~question, FUN=table, data=d.1[d.1$condition=="Standard",])
chisq.test(as.table(table1i[[2]])[,-2]) #NB: removed norm confirming bc of low values to follow S&W (2016)
cramersV(as.table(table1i[[2]])[,-2])

#Unintentional norm violation
table1j <- aggregate(relevance~question, FUN=table, data=d.1[d.1$condition=="Unintended",])
chisq.test(table1j[[2]])
cramersV(table1j[[2]])

#Ignorant norm violation
table1k <- aggregate(relevance~question, FUN=table, data=d.1[d.1$condition=="Ignorant",])
chisq.test(table1k[[2]])
cramersV(table1k[[2]])

# Deception-based
table1l <- aggregate(relevance~question, FUN=table, data=d.1[d.1$condition=="Deceived",])
chisq.test(table1l[[2]])
cramersV(table1l[[2]])

# Chemical question
table1m <- aggregate(relevance~condition, FUN=table, data=d.1[d.1$question=="Object",])
chisq.test(table1m[[2]])
cramersV(table1m[[2]])

# Action question
table1n <- aggregate(relevance~condition, FUN=table, data=d.1[d.1$question=="Action",])
chisq.test(table1n[[2]])
cramersV(table1n[[2]])

# Agent question
table1o <- aggregate(relevance~condition, FUN=table, data=d.1[d.1$question=="Agent",])
chisq.test(table1o[[2]])
cramersV(table1o[[2]])

# Difference from standard violation
table1p <- aggregate(relevance~condition, FUN=table, data=d.1[d.1$question=="Agent",])
# unintended violation
chisq.test(as.table(table1p[[2]])[3:4,])
cramersV(as.table(table1p[[2]])[3:4,])
# ignorant violation
chisq.test(as.table(table1p[[2]])[2:3,])
cramersV(as.table(table1p[[2]])[2:3,])
# deception violation
chisq.test(as.table(table1p[[2]])[c(1,3),])
cramersV(as.table(table1p[[2]])[c(1,3),])


#### Norms of proper functioning study #####

d.2 <- read.csv("data/study2.csv")

##demongraphics
d2.age <- matrix(c("mean",mean(d.2$Age,na.rm=T),"sd",sd(d.2$Age,na.rm=T),"n",length(d.2$Age)),ncol = 3)
print(d2.age)

d.2$Sex <- factor(c("Male","Female")[d.2$Sex])
d2.gender <- table(d.2$Sex, exclude=NULL)
print(d2.gender)

d.2$study <- "Study 2"

d.2$condition <- factor(c("Malfunction","Immoral","Normal")[d.2$DO.BR.FL_15])
d.2$question <- factor(c("Agent","Object")[d.2$DO.BR.FL_22])
d.2$time <- rowSums(d.2[,c(17,22,27)],na.rm = T)
d.2$order <- "Cause First"
d.2$order[d.2$DO.BL.AgentQuestions=="Agent_rel|Agent_cause"] <- "Relevance First"
d.2$order[d.2$DO.BL.ObjectQuestions=="Object_Rel|Object_cause"] <- "Relevance First"

d.2 <- d.2[,-c(2:28,51:71)]

d.2$Control1_1 <- 0
d.2$Control1_1[d.2$condition=="Malfunction" & is.na(d.2$Control1_1_1) & d.2$Control1_1_2==1] <- 1
d.2$Control1_1[d.2$condition!="Malfunction" & d.2$Control1_1_1==1 & is.na(d.2$Control1_1_2)] <- 1
d.2$Control1_2 <- 0
d.2$Control1_2[d.2$Control1_2_1==1 & is.na(d.2$Control1_2_2)] <- 1

d.2$Control1 <- 0
d.2$Control1[d.2$Control1_1==1 & d.2$Control1_2==1] <- 1

d.2$Control2_1 <- 0
d.2$Control2_1[d.2$condition=="Malfunction" & d.2$Control2_1_1==1 & d.2$Control2_1_2==1] <- 1
d.2$Control2_1[d.2$condition!="Malfunction" & d.2$Control2_1_1==1 & is.na(d.2$Control2_1_2)] <- 1
d.2$Control2_2 <- 0
d.2$Control2_2[d.2$Control2_2_1==1 & is.na(d.2$Control2_2_2)] <- 1

d.2$Control2 <- 0
d.2$Control2[d.2$Control2_1==1 & d.2$Control2_2==1] <- 1

d.2$Control3_1 <- 0
d.2$Control3_1[d.2$condition=="Immoral" & is.na(d.2$Control3_1_1)] <- 1
d.2$Control3_1[d.2$condition!="Immoral" & d.2$Control3_1_1==1] <- 1
d.2$Control3_2 <- 0
d.2$Control3_2[d.2$Control3_2_1==1] <- 1

d.2$Control3 <- 0
d.2$Control3[d.2$Control3_1==1 & d.2$Control3_2==1] <- 1

d.2$control <- 0
d.2$control[d.2$Control1==1 & d.2$Control2==1 & d.2$Control3==1] <- 1

# Manipulation checks
##Moral check
d.2$morality <- d.2$morality-3 ## this was recoded incorrectly on Qualtrics and started at 4
aggregate(morality~condition, FUN=function(x) c(M = mean(x), SD = sd(x)), data=d.2)
## Immoral vs normal
var.test(d.2$morality[d.2$condition=="Immoral"],d.2$morality[d.2$condition=="Normal"])
t.test(d.2$morality[d.2$condition=="Immoral"],d.2$morality[d.2$condition=="Normal"])
cohensD(d.2$morality[d.2$condition=="Immoral"],d.2$morality[d.2$condition=="Normal"])
## Immoral vs. malfunction
var.test(d.2$morality[d.2$condition=="Immoral"],d.2$morality[d.2$condition=="Malfunction"])
t.test(d.2$morality[d.2$condition=="Immoral"],d.2$morality[d.2$condition=="Malfunction"],var.equal=T)
cohensD(d.2$morality[d.2$condition=="Immoral"],d.2$morality[d.2$condition=="Malfunction"])
## Normal vs. Malfunction
var.test(d.2$morality[d.2$condition=="Normal"],d.2$morality[d.2$condition=="Malfunction"])
t.test(d.2$morality[d.2$condition=="Normal"],d.2$morality[d.2$condition=="Malfunction"],var.equal=T)
cohensD(d.2$morality[d.2$condition=="Normal"],d.2$morality[d.2$condition=="Malfunction"])

##Malfunciton check
aggregate(probability_4~condition, FUN=function(x) c(M = mean(x), SD = sd(x)), data=d.2)
## Malfunction
t.test(d.2$probability_4[d.2$condition=="Malfunction"],mu=50, alternative="greater")
## Normal
t.test(d.2$probability_4[d.2$condition=="Normal"],mu=50, alternative="greater")
## Immoral
t.test(d.2$probability_4[d.2$condition=="Immoral"],mu=50, alternative="greater")

## Causal and Relevance Preference Scores
d.2$relevanceA <- rowSums(d.2[,c(2,6)],na.rm = T)
d.2$relevanceB <- rowSums(d.2[,c(3,7)],na.rm = T)
d.2$causeA <- rowSums(d.2[,c(4,8)],na.rm = T)
d.2$causeB <- rowSums(d.2[,c(5,9)],na.rm = T)

d.2$causeStr <- NA
d.2$causeStr[d.2$causeA==0 & d.2$causeB==0] <- 0 #Neither
d.2$causeStr[d.2$causeA==1 & d.2$causeB==1] <- 0 # Both
d.2$causeStr[d.2$causeA==1 & d.2$causeB==0] <- -1 # Admin only
d.2$causeStr[d.2$causeA==0 & d.2$causeB==1] <- 1 # Prof only
table(d.2$causeStr,exclude = NULL)

d.2$relevanceStr <- NA
d.2$relevanceStr[(d.2$relevanceA==2 | d.2$relevanceA==0) & (d.2$relevanceB==2 | d.2$relevanceB==0)] <- 0 #"Neither"
d.2$relevanceStr[d.2$relevanceA==1 & d.2$relevanceB==1] <- 0 #"Both"
d.2$relevanceStr[d.2$relevanceA==1 & (d.2$relevanceB==2 | d.2$relevanceB==0)] <- -1 #"Admin Only"
d.2$relevanceStr[(d.2$relevanceA==2 | d.2$relevanceA==0) & d.2$relevanceB==1] <- 1 #"Prof Only"
table(d.2$relevanceStr, exclude=NULL)

d.2 <- d.2[d.2$control==1,-c(2:21,29:37)]
length(d.2$ResponseID)


d.2l <- gather(d.2[,c(1,5:6,14:15)],judgment,strength,-c(1:3))  

d2.sum <- ddply(d.2l, c("condition","question","judgment"), summarise,
                N    = length(strength),
                mean = mean(strength, na.rm=TRUE),
                sd   = sd(strength,na.rm=TRUE),
                se   = sd / sqrt(N) )

d2.sum$judgment <- factor(d2.sum$judgment)
d2.sum$judgment <- factor(c("Cause","Relevance")[d2.sum$judgment])


d2.plot1 <- ggplot(d2.sum[d2.sum$judgment=="Cause",], aes(x=question, y=mean, fill=question)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=wes_palette("Royal1",3)) + 
  facet_wrap(~condition) +
  ylab("Causal Preference for Norm-violating Event") +
  xlab("") +
  scale_y_continuous(limits=c(-0.3,1.1),breaks = c(-0.25,0.00,0.25,0.50,1.00), labels = c(-0.25,0.00,0.25,0.50,1.00)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=position_dodge(.9)) +
  theme_bw() +
  theme(
     plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.position=c(.9,.9)
    ,legend.title=element_blank()
    #,legend.title=element_text(size=rel(1.75))
    ,legend.text=element_text(size=rel(1.5))
    ,axis.text.x=element_text(size=rel(1.75))
    ,axis.text.y=element_text(size=rel(1.25))
    ,axis.title=element_text(size=rel(1.5))
    ,strip.text = element_text(size = rel(1.7))
    ,axis.title.y = element_text(vjust = 0.75)
  )

d2.plot1

# ggsave(d2.plot1, file="fig2a.jpg",dpi=600)

d2.plot2 <- ggplot(d2.sum[d2.sum$judgment=="Relevance",], aes(x=question, y=mean, fill=question)) +
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=position_dodge(.9)) +
  facet_wrap(~condition) +
  scale_y_continuous(limits=c(-0.3,1.1),breaks = c(-0.25,0.00,0.25,0.50,1.00), labels = c(-0.25,0.00,0.25,0.50,1.00)) +
  scale_fill_manual(values=wes_palette("Royal1",3)) + 
  ylab("Relevance Preference for Norm-violating Event") +
  xlab("") +
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.position=c(.9,.9)
    ,legend.title=element_blank()
    #,legend.title=element_text(size=rel(1.75))
    ,legend.text=element_text(size=rel(1.5))
    ,axis.text.x=element_text(size=rel(1.75))
    ,axis.text.y=element_text(size=rel(1.25))
    ,axis.title=element_text(size=rel(1.5))
    ,strip.text = element_text(size = rel(1.7))
    ,axis.title.y = element_text(vjust = 0.75)
  )

d2.plot2

# ggsave(d2.plot2, file="fig2b.jpg",dpi=600)

#proportional odds logistic regression
## Causal judgments
d.2$causeStr <- factor(d.2$causeStr)

lm2.1 <- polr(causeStr ~condition+question,data=d.2)
dropterm(lm2.1, test="Chisq")
# interaction effect
lm2.0 <- polr(causeStr ~condition*question,data=d.2)
dropterm(lm2.0, test = "Chisq")

# Moral Norm Condition
aggregate(causeStr ~ question, FUN=table, data=d.2[d.2$condition=="Immoral",])
lm2.1i <- polr(causeStr ~ question, data=d.2[d.2$condition=="Immoral",])
dropterm(lm2.1i, test="Chisq")
# Functional Norm Condition 
aggregate(causeStr ~ question, FUN=table, data=d.2[d.2$condition=="Malfunction",])
lm2.1m <- polr(causeStr ~ question, data=d.2[d.2$condition=="Malfunction",])
dropterm(lm2.1m, test="Chisq")
# No Norm Condition
aggregate(causeStr ~ question, FUN=table, data=d.2[d.2$condition=="Normal",])
lm2.1n <- polr(causeStr ~ question, data=d.2[d.2$condition=="Normal",])
dropterm(lm2.1n, test="Chisq")


## Relevance judgments
d.2$relevanceStr <- factor(d.2$relevanceStr)
#main effects
lm2.3 <- polr(relevanceStr ~condition+question,data=d.2)
dropterm(lm2.3, test="Chisq")
# interaction effect
lm2.2 <- polr(relevanceStr ~condition*question,data=d.2)
dropterm(lm2.2, test = "Chisq")

# Moral Norm Condition
aggregate(relevanceStr ~ question, FUN=table, data=d.2[d.2$condition=="Immoral",])
lm2.3i <- polr(relevanceStr ~ question, data=d.2[d.2$condition=="Immoral",])
dropterm(lm2.3i, test="Chisq")
# Functional Norm Condition 
aggregate(relevanceStr ~ question, FUN=table, data=d.2[d.2$condition=="Malfunction",])
lm2.3m <- polr(relevanceStr ~ question, data=d.2[d.2$condition=="Malfunction",])
dropterm(lm2.3m, test="Chisq")
# No Norm Condition
aggregate(relevanceStr ~ question, FUN=table, data=d.2[d.2$condition=="Normal",])
lm2.3n <- polr(relevanceStr ~ question, data=d.2[d.2$condition=="Normal",])
dropterm(lm2.3n, test="Chisq")




#### Manipulation Study ####

d.3 <- read.csv("data/study3.csv",stringsAsFactors = F)

d.3$time <- rowSums(d.3[,c(17,23)],na.rm = T)
d.3$condition[d.3$DO.BL.Manipulation=="LeverManip|altTime|outcomeChange"] <- "Object CF"
d.3$condition[d.3$DO.BL.Manipulation=="ProfManip|altTime|outcomeChange"] <- "Agent CF"
d.3$order[d.3$DO.BL.CausationQuestions=="Object_cause|Agent_cause"] <- "Object first"

d.3$controlQ <- 0
d.3$controlQ[d.3$control1_1==1 & d.3$control1_2==1 & is.na(d.3$control1_3)  
            & d.3$control2_1==1 & d.3$control2_2==1 & is.na(d.3$control2_3)] <- 1
#d.3 <- d.3[d.3$control==1 & d.3$time>85,]

d.3 <- gather(d.3[,c(1,25:27,34:35,50:53)],judgment,cause,-c(1:2,5:10))

### control version with NO CF
d.3a <- read.csv("data/study3a.csv",stringsAsFactors = F)

d.3a$time <- rowSums(d.3a[,c(17,22)],na.rm = T)
d.3a$condition <- "No CF"
d.3a$order[d.3a$DO.BL.CausationQuestions=="Object_cause|Agent_cause"] <- "Object first"
d.3a$outcomeChange <- NA
d.3a$controlQ <- 0
d.3a$controlQ[d.3a$control1_1==1 & d.3a$control1_2==1 & is.na(d.3a$control1_3) &  
             d.3a$control2_1==1 & d.3a$control2_2==1 & is.na(d.3a$control2_3)] <- 1

d.3al <- gather(d.3a[,c(1,24:25,32:33,48:52)],judgment,cause,-c(1,4:10))


### version with the other order of questions
d.3b <- read.csv("data/study3b.csv")
d.3b$time <- rowSums(d.3b[,c(17,24)],na.rm = T)
d.3b$condition[d.3b$DO.BL.Manipulation=="control|altTime|outcomeChange"] <- "No CF"
d.3b$condition[d.3b$DO.BL.Manipulation=="LeverManip|altTime|outcomeChange"] <- "Object CF"
d.3b$condition[d.3b$DO.BL.Manipulation=="ProfManip|altTime|outcomeChange"] <- "Agent CF"
d.3b$order <- "Agent first"

d.3b$controlQ <- 0
d.3b$controlQ[d.3b$control1_1==1 & d.3b$control1_2==1 & is.na(d.3b$control1_3) &  
              d.3b$control2_1==1 & d.3b$control2_2==1 & is.na(d.3b$control2_3)] <- 1

d.3bl <- gather(d.3b[,c(1,26:28,35:36,51:54)],judgment,cause,-c(1:2,5:10))

d.3l <- rbind(d.3,d.3al,d.3bl)

d.3l$Age[d.3l$Age==199] <- NA
d3.age <- matrix(c("mean",mean(d.3l$Age[!duplicated(d.3l$ResponseID)],na.rm=T),
                   "sd",sd(d.3$Age[!duplicated(d.3l$ResponseID)],na.rm=T),
                   "n",length(d.3$Age[!duplicated(d.3l$ResponseID)])),ncol = 3)
print(d3.age)

d.3l$Sex <- factor(c("Male","Female")[d.3l$Sex])
d3.gender <- table(d.3l$Sex[!duplicated(d.3l$ResponseID)], exclude=NULL)
print(d3.gender)

d.3l <- d.3l[d.3l$controlQ==1,]

d.3l$responseN <- "Second answer"
d.3l$responseN[d.3l$order=="Agent first" & d.3l$judgment=="Agent_cause_8"] <-  "First answer"
d.3l$responseN[d.3l$order=="Object first" & d.3l$judgment=="Object_cause_8"] <-  "First answer"

d.3l$judgment <- factor(d.3l$judgment)
d.3l$judgment <- factor(c("Agent","Object")[d.3l$judgment])

d3.sum <- ddply(d.3l, c("condition","judgment"), summarise,
                  N    = length(cause),
                mean = mean(cause, na.rm=TRUE),
                sd   = sd(cause,na.rm=TRUE),
                se   = sd / sqrt(N) )

d3.plot <- ggplot(d3.sum, aes(x=judgment, y=mean, fill=condition)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=wes_palette("Royal1",3)) + 
  #facet_wrap(~responseN) +
  ylab("Agreement with Causal Statement") +
  xlab("") +
  #scale_y_continuous(limits=c(-0.3,1.1),breaks = c(-0.25,0.00,0.25,0.50,1.00), labels = c(-0.25,0.00,0.25,0.50,1.00)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=position_dodge(.9)) +
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.position=c(.9,.9)
    ,legend.title=element_blank()
    ,legend.text=element_text(size=rel(1.5))
    ,axis.text.x=element_text(size=rel(1.75))
    ,axis.text.y=element_text(size=rel(1.25))
    ,axis.title=element_text(size=rel(1.5))
    ,strip.text = element_text(size = rel(1.7))
    ,axis.title.y = element_text(vjust = 0.75)
  )

d3.plot

lm4.0 <- lmer(cause~condition * judgment + (1|ResponseID), data=d.3l)
## Interaction effect
lm4.1 <- lmer(cause~condition + judgment + (1|ResponseID), data=d.3l)
anova(lm4.0,lm4.1)
## main effect of question
lm4.2 <- lmer(cause~condition + (1|ResponseID), data=d.3l)
anova(lm4.1,lm4.2)
## main effect of condition
lm4.3 <- lmer(cause~judgment + (1|ResponseID), data=d.3l)
anova(lm4.1,lm4.3)

  
aggregate(cause~condition*judgment, FUN=function(x) c(M = mean(x), SD = sd(x)),data=d.3l)
#AGENT: agent cf vs object cf
var.test(d.3l$cause[d.3l$condition=="Agent CF" & d.3l$judgment=="Agent"],
         d.3l$cause[d.3l$condition=="Object CF" & d.3l$judgment=="Agent"])
t.test(d.3l$cause[d.3l$condition=="Agent CF" & d.3l$judgment=="Agent"],
       d.3l$cause[d.3l$condition=="Object CF" & d.3l$judgment=="Agent"],var.equal =T)
cohensD(d.3l$cause[d.3l$condition=="Agent CF" & d.3l$judgment=="Agent"],
        d.3l$cause[d.3l$condition=="Object CF" & d.3l$judgment=="Agent"])
#AgENT: agent cf vs. no cf
var.test(d.3l$cause[d.3l$condition=="Agent CF" & d.3l$judgment=="Agent"],
         d.3l$cause[d.3l$condition=="No CF" & d.3l$judgment=="Agent"])
t.test(d.3l$cause[d.3l$condition=="Agent CF" & d.3l$judgment=="Agent"],
         d.3l$cause[d.3l$condition=="No CF" & d.3l$judgment=="Agent"])
cohensD(d.3l$cause[d.3l$condition=="Agent CF" & d.3l$judgment=="Agent"],
        d.3l$cause[d.3l$condition=="No CF" & d.3l$judgment=="Agent"])


var.test(d.3l$cause[d.3l$condition=="Object CF" & d.3l$judgment=="Object"],
          d.3l$cause[d.3l$condition=="Agent CF" & d.3l$judgment=="Object"])
t.test(d.3l$cause[d.3l$condition=="Object CF" & d.3l$judgment=="Object"],
       d.3l$cause[d.3l$condition=="Agent CF" & d.3l$judgment=="Object"])
cohensD(d.3l$cause[d.3l$condition=="Object CF" & d.3l$judgment=="Object"],
        d.3l$cause[d.3l$condition=="Agent CF" & d.3l$judgment=="Object"])


var.test(d.3l$cause[d.3l$condition=="Object CF" & d.3l$judgment=="Object"],
         d.3l$cause[d.3l$condition=="No CF" & d.3l$judgment=="Object"])
t.test(d.3l$cause[d.3l$condition=="Object CF" & d.3l$judgment=="Object"],
       d.3l$cause[d.3l$condition=="No CF" & d.3l$judgment=="Object"])
cohensD(d.3l$cause[d.3l$condition=="Object CF" & d.3l$judgment=="Object"],
        d.3l$cause[d.3l$condition=="No CF" & d.3l$judgment=="Object"])

