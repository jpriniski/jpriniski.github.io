#bnlearn tutorial Part 1: CCD Lab Modeling Meeting
#5.15.2020
#Hunter Priniski

require(tidyverse)
require(bnlearn)
require(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

#First, simulate some data.

#Let's have 5 variables: A, B, C, D, E. E is our response variable - the one we wish to predict.
set.seed(420)

A <- round(rnorm(rbinom(250, 10, .2), .01), 2)
B <- round(rnorm(rpois(250, .2), .01), 2)
C <- round(rnorm(250, mean = 2*A, sd = .25), 2)
D <- round(rnorm(250, mean = (2*A - 4*B), sd = .25), 2)
E <- round(rnorm(250, mean = (3*C + 4*D), sd = .1), 2 )

df <- data.frame('A' = A,'B'= B, 'C' = C, 'D' = D, 'E' = E)
head(df)

#What variables depend on others?
#What are A, B, C, D, and E? Coefficents (i.e., betas) or ...
#What are the numbers 2 and 4, ...?
#Let's, write out the data generating process.

#Say we want to predict the response variable, in this case, E. 
#We, of course, don't know the strucutre of the data gnerating process when we're actually modeling data: ie., that C depends on A and D depnds on A and B, .... 
#This could be problematic when building a model. 
#To see why, think about some possible GLM we may fit to predict E, given we only have df (i.e., no knowledge about the DGP):
#e.g., E ~  A + B + C + D
#Such models don't capture how the variables depend on one another. What are some ways people generally go about finding these dependencies?

#One way we can try to uncover this strucutre is by using bayesian networks. We will use the bnlearn pacakge to do this. 

#So say we do a lit review and we realize that there are two competing theories. one predicts
#variable dependencies like those secified in dag1, the other specifies dag2. 

#Note on reading BN syntax. 
#[A] --> if the variable is by itself, no dependencies
#[B|A] ---> B is conditioned on A
#[C|A:B] --> C is conditioned on A and B

#Which of these two dags, given the data we generated above, is correct? Note, it's helpful to write these down on paper
dag1 <- model2network("[A][B][C|A][D|A:B][E|C:D]")
dag2 <- model2network("[A][B][C][D][E|A:B:C:D]")

fitted1 <- bn.fit(dag1, df)
fitted1

fitted2 <- bn.fit(dag2, df)
fitted2

#look at effect's estimates for fitted 1 and 2 and compare to the data that simulated the dataset. 
#in reality, it is difficult to know which strucutre is infact better. 
#How would we go about assessing which model is better?


#https://www.bnlearn.com/examples/xval/

#compare loss functions by cross validating
bn.cv(data = df, bn  = dag1)
bn.cv(data = df, bn = dag2)

#which one has lower loss, ie., is better fit? 

#In next tutorial, I will show how to use bnlearn to fit a structure (we specified structures here)
#and how they can be used to guide model development for GLMs. 

