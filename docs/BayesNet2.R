#PART 1
library(bnlearn)
library(simstudy)
library(tidyverse)

#generate some data
C <- defData(varname = "C",  formula = ".15;.15;.2;.1;.2;.2" , dist = "categorical", id = "Subject")
R <- defData(varname = "R",  formula = ".1;.1;.15;.15;.25;.15" , dist = "categorical", id = "Subject")
D <- defData(varname = "D",  formula = ".05;.05;.1;.05;.3;.25" , dist = "categorical", id = "Subject")
CP <- defData(varname = 'CP', formula = ".05;.1;.05;.05;.3;.25", dist = 'categorical', id = 'Subject' )

dC <- genData(1000, C)
dR <- genData(1000, R)
dD <- genData(1000, D)
dCP <- genData(1000, CP)

simdata <- dC%>%
  inner_join(dR, by = 'Subject')%>%
  inner_join(dD, by = 'Subject')%>%
  inner_join(dCP, by = 'Subject')%>%
  select(-Subject)%>%
  mutate(C = as_factor(C),
         R = as_factor(R),
         D = as_factor(D),
         CP = as_factor(CP))

#model data
net <- model2network("[C][R][D][CP|C:R:D]")
fitted <- bn.fit(net, data = simdata, method = 'bayes')


#PART 2
#Learn network structure from data using an applied example
#Data from Study 3 from Conflict Changes How People View God
#Paper: https://www.researchgate.net/publication/336749467_Conflict_Changes_How_People_View_God

GodData <- read_csv("~/Desktop/GodData.csv")
pdag <- iamb(GodData)

GodData2 <- GodData%>%
  select(c(WarCond, Punitive, SCTTotal))

#We will use hill climbing to learn network structure
bn.hc <- hc(GodData2)

#get fitted network strucutre
bn.hc$arcs
#visualize
#install.packages("BiocManager")
#BiocManager::install("Rgraphviz")
require(Rgraphviz)
graphviz.plot(bn.hc)


#model data
Godnet <- model2network("[WarCond][SCTTotal|Punitive:WarCond][Punitive|WarCond]")
fitted <- bn.fit(Godnet, data = GodData2)

#How should we assess model fit? Viz + model comparison


#But let's look a little closer at this bn model
fitted

#How do they compare simple lm?
lm(Punitive~WarCond, data = goddata)
lm(SCTTotal ~ Punitive + WarCond, data = goddata)
lm(SCTTotal ~ WarCond, data = goddata)

#compare to model in page 19 of paper

