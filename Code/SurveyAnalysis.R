#Project: A Darkening Spring: How Preexisting Distrust Shaped COVID-19 Skepticism
#Author: XXX
#required packages. 
#run install.packages() with the package name to install  
library(readr)
require(brms)
require(rstan)
require(Rcpp)
require(scales)
require(gridExtra)
require(loo)
require(tidyverse)
require(readxl)

#for viz
library(ggExtra)
require(cowplot)
require(bayesplot)

#partial correlations
require(ppcor)
library(ggcorrplot)

#run stan computations on multiple cores
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#Set wd to where you downloaded this folder. 
setwd("/Users/hunter/Desktop/Covid-19 Attitudes/Attitude Surveys")

#load in survey responses for three time points
data1_wide <- read_csv('prewave/Data/study1_data.csv')
data2_wide <- read_csv('firstwave/Phase 2 Data/study2_data.csv')
data3_wide <- read_csv('firstwave/Phase 3 Data/study3_data.csv')


#### Clean up WIDE DFs and calculate mean scale responses ####

### PREWAVE (STUDY 1) ###
data1_wide_clean <- data1_wide%>%
  #remove subjects who don't pass 7 out of 7 attention checks
  filter(xenoAC == 1)%>%
  filter(coverageAC ==1)%>%
  filter(antiAC == 1)%>%
  filter(postvaxAC == 1)%>%
  filter(POST_xenoAC == 1)%>%
  filter(POST_polAC == 1)%>%
  filter(POST_covAC == 1)%>%
  #make a categorical classification (for viz), rename political scale
  mutate(Politics = ifelse(Q78_1>4, 'Conservative',ifelse(Q78_1<4, 'Liberal','Moderate')))%>%
  rename(SocialPolitics = Q78_1)%>%
  #get scale averages
  rowwise()%>%
  mutate(SeverityAvg = mean(c(severity1, severity2, severity3, severity4, severity5)))%>%
  mutate(MedSkepAvg = mean(c(skep1, skep2, skep3, skep4)))%>%
  mutate(ForeignAvg = mean(c(xeno1, xeno2, xeno3, xeno4)))%>% 
  mutate(OriginAvg = mean(c(origin1, origin2, origin3, origin4)))%>% #fears surrounding origin of the virus
  mutate(PolSkepAvg = mean(c(politics1, politics2, politics3)))%>% #dems making a big deal 
  mutate(CovidVaxAvg = mean(c(antivax1, antivax2)))%>% #these scale items ask about anti covid-19 vax attitudes. we dropped antivax 3 to increase reliability of scale. however, the results are robust to droping this item too.
  mutate(MediaSkepAvg = mean(c(coverage1, coverage2, coverage3)))%>%
  ungroup()

### MAKE LONG FORMAT
study1_long <- data1_wide_clean %>%
  dplyr::select(c(ResponseId,
           MedSkepAvg,
           ForeignAvg,
           OriginAvg,
           PolSkepAvg,
           CovidVaxAvg,
           MediaSkepAvg,
           Politics, #categorical variable
           SocialPolitics, #response
           severity1, severity2, severity3, severity4, severity5))%>%
  gather('SeverityItem', 'SeverityResponse', severity1, severity2, severity3, severity4, severity5)%>%
  mutate(SeverityResponse = recode(SeverityResponse, '7' = 1, '6' = 2, '5' = 3, '4' = 4, '3' = 5, '2' = 6, '1' = 7))


### FIRSTWAVE T1 (STUDY 2) ###
data2_wide_clean <- data2_wide %>%
  #Remove Ss who don't pass both attention checks
  filter(AC1 == 1)%>%
  filter(AC2 == 1)%>%
  mutate(Politics = ifelse(SocialIssues_1>4, 'Conservative',ifelse(SocialIssues_1<4, 'Liberal','Moderate')))%>% #make a categorical politicals variable for viz
  rename(SocialPolitics = SocialIssues_1)%>%
  rowwise()%>%
  mutate(SeverityAvg = mean(c(severity1, severity2, severity3, severity4, severity5)))%>%
  mutate(MedSkepAvg = mean(c(skep1, skep2, skep3, skep4)))%>%
  mutate(ForeignAvg = mean(c(xeno1, xeno2, xeno3, xeno4)))%>%
  mutate(OriginAvg = mean(c(origin1, origin2, origin3, origin4)))%>%
  mutate(PolSkepAvg = mean(c(politics1, politics2, politics3)))%>%
  mutate(VaxSkepAvg = mean(c(vaxeff_1, vaxeff_2, vaxeff_3,vaxeff_4)))%>%
  mutate(EconAvg = mean(c(econ_1, econ_2)))%>%
  mutate(CovidVaxAvg = mean(c(covidvax1, covidvax2, covidvax3))) #we made scale item 3 better, so will keep here.
  

#make long DF
study2_long <- data2_wide_clean %>%
  dplyr::select(c(ResponseId,
           MedSkepAvg,
           ForeignAvg,
           OriginAvg,
           PolSkepAvg,
           VaxSkepAvg,
           EconAvg,
           CovidVaxAvg,
           Politics, #categorical variable
           SocialPolitics, #response
           severity1, severity2, severity3, severity4, severity5))%>%
  gather('SeverityItem', 'SeverityResponse', severity1, severity2, severity3, severity4, severity5)%>%
  mutate(SeverityResponse = recode(SeverityResponse, '7' = 1, '6' = 2, '5' = 3, '4' = 4, '3' = 5, '2' = 6, '1' = 7))


### FIRSTWAVE T2 (STUDY 3) ###
data3_wide_clean <- data3_wide %>%
  filter(AC1 == 1)%>%
  filter(AC2 == 1)%>%
  mutate(Politics = ifelse(SocialIssues_1>4, 'Conservative',ifelse(SocialIssues_1<4, 'Liberal','Moderate')))%>% #make a categorical politicals variable for viz
  rename(SocialPolitics = SocialIssues_1)%>%
  rowwise()%>%
  mutate(MedSkepAvg = mean(c(skep1, skep2, skep3, skep4)))%>%
  mutate(ForeignAvg = mean(c(xeno1, xeno2, xeno3, xeno4)))%>%
  mutate(OriginAvg = mean(c(origin1, origin2, origin3, origin4)))%>%
  mutate(PolSkepAvg = mean(c(politics1, politics2, politics3)))%>%
  mutate(VaxSkepAvg = mean(c(vaxeff_1, vaxeff_2, vaxeff_3,vaxeff_4)))%>%
  mutate(EconAvg = mean(c(econ_1, econ_2, econ_3)))%>%
  mutate(CovidVaxAvg = mean(c(covidvax1, covidvax2, covidvax3)))#we made scale item 3 better, so will keep

#make long DF
study3_long <- data3_wide_clean %>%
  dplyr::select(c(ResponseId,
           MedSkepAvg,
           ForeignAvg,
           OriginAvg,
           PolSkepAvg,
           VaxSkepAvg,
           EconAvg,
           CovidVaxAvg,
           Politics, #categorical variable
           SocialPolitics, #response
           severity1, severity2, severity3, severity4, severity5))%>%
  gather('SeverityItem', 'SeverityResponse', severity1, severity2, severity3, severity4, severity5)%>%
  mutate(SeverityResponse = recode(SeverityResponse, '7' = 1, '6' = 2, '5' = 3, '4' = 4, '3' = 5, '2' = 6, '1' = 7))


##### MAKE FINAL PREWAVE and FIRST WAVE LONG DF #####

#add phase info
study1_long <- study1_long %>%
  mutate("Month" = "March")%>%
  mutate('Wave' = 'pre')

study2_long <- study2_long %>%
  mutate("Month" = "April")%>%
  mutate('Wave' = 'first')

study3_long <- study3_long %>%
  mutate("Month" = "May")%>%
  mutate('Wave' = 'first')

#final dataframes are prewave and firstwave
prewave <- study1_long
firstwave <- bind_rows(study2_long, study3_long)%>%
  drop_na() #loo requires the same number of data points, so will drop_na for matching across predictors.


#will make a full dataframe so we can compute howCovid-19 skepticism shifted between prewave and firstwave

#we need to subset because the studies include some difference additional questions. only combine on matched cols
prewave_subset <- prewave %>%
  dplyr::select(c(SeverityItem, SeverityResponse, Politics, SocialPolitics, Wave, ResponseId))

firstwave_subset <- firstwave %>%
  dplyr::select(c(SeverityItem, SeverityResponse, Politics, SocialPolitics, Wave, ResponseId))

all_data <- bind_rows(prewave_subset, firstwave_subset)



###### MODEL RESPONSES USING BAYESIAN ME ORDINAL REGRESSION ######

#####
##### PREWAVE ATTITUDES
#####


#use distribution of raw responses to inform intercept priors
qlogis(cumsum(table(prewave$SeverityResponse)/length(prewave$SeverityResponse)))


#get prior information for full model
get_prior(SeverityResponse ~ MedSkepAvg + ForeignAvg + OriginAvg + PolSkepAvg + CovidVaxAvg + MediaSkepAvg + SocialPolitics + (1 |ResponseId) + (1 | SeverityItem), 
          data = prewave)

#FIT FULL MODEL, PREWAVE ATTITUDES
prewave_full <- brm(SeverityResponse ~ MedSkepAvg + ForeignAvg + OriginAvg + PolSkepAvg + CovidVaxAvg + MediaSkepAvg + SocialPolitics + (1 |ResponseId) + (1 | SeverityItem), 
                   data = prewave,
                   family="cumulative", 
                   file = 'prewave/Models/prewave_full',
                   chains=4, 
                   iter=3000, 
                   warmup=1500,
                   control = list(adapt_delta = .90),
                   prior = c(set_prior("normal(-2.8, .25)", class = "Intercept", coef = "1"),
                             set_prior("normal(-1.7, .25)", class = "Intercept", coef = "2"),
                             set_prior("normal(-1.0, .25)", class = "Intercept", coef = "3"),
                             set_prior("normal(-.67, .25)", class = "Intercept", coef = "4"),
                             set_prior("normal(0.07, .25)", class = "Intercept", coef = "5"),
                             set_prior("normal(1.06, .25)", class = "Intercept", coef = "6"),
                             set_prior("normal(0, 2)", class = "b", coef = "ForeignAvg"),
                             set_prior("normal(0, 2)", class = "b", coef = "MediaSkepAvg"),
                             set_prior("normal(0, 2)", class = "b", coef = "MedSkepAvg"),
                             set_prior("normal(0, 2)", class = "b", coef = "OriginAvg"),
                             set_prior("normal(0, 2)", class = "b", coef = "PolSkepAvg"),
                             set_prior("normal(0, 2)", class = "b", coef = "CovidVaxAvg"),
                             set_prior("normal(0, 2)", class = "b", coef = "SocialPolitics"),
                             set_prior("normal(1,2)", class= "sd")))

#DO LOO-IC model fitting. Get the best performing, parsimonious model. 

#to keep track of models, they will be labeled as follows
#full (ref)
#m2 full - politics
#m3 big 3 + pol
#m4 big 3 - pol
#m5 big 2 + pol
#m6 big 2 - pol
#m7 big 1 + pol
#m8 big 1 - pol
#m9 politics only

prewave_full_loo <- loo(prewave_full)
saveRDS(prewave_full_loo, file = 'prewave/Models/prewave_full_loo.rds')


#remove politics
prewave_m2 <- brm(SeverityResponse ~ MedSkepAvg + ForeignAvg + OriginAvg + PolSkepAvg + CovidVaxAvg + MediaSkepAvg  + (1 |ResponseId) + (1 | SeverityItem), 
                    data = prewave,
                    family="cumulative", 
                    file = 'prewave/Models/prewave_m2',
                    chains=4, 
                    iter=3000, 
                    warmup=1500,
                    control = list(adapt_delta = .90),
                    prior = c(set_prior("normal(-2.8, .25)", class = "Intercept", coef = "1"),
                              set_prior("normal(-1.7, .25)", class = "Intercept", coef = "2"),
                              set_prior("normal(-1.0, .25)", class = "Intercept", coef = "3"),
                              set_prior("normal(-.67, .25)", class = "Intercept", coef = "4"),
                              set_prior("normal(0.07, .25)", class = "Intercept", coef = "5"),
                              set_prior("normal(1.06, .25)", class = "Intercept", coef = "6"),
                              set_prior("normal(0, 2)", class = "b", coef = "ForeignAvg"),
                              set_prior("normal(0, 2)", class = "b", coef = "MediaSkepAvg"),
                              set_prior("normal(0, 2)", class = "b", coef = "MedSkepAvg"),
                              set_prior("normal(0, 2)", class = "b", coef = "OriginAvg"),
                              set_prior("normal(0, 2)", class = "b", coef = "PolSkepAvg"),
                              set_prior("normal(0, 2)", class = "b", coef = "CovidVaxAvg"),
                              set_prior("normal(1,2)", class= "sd")))


prewave_m2_loo <- loo(prewave_m2)
saveRDS(prewave_m2_loo, file = 'prewave/Models/prewave_m2_loo.rds')


#big 3 plus politics
prewave_m3 <- brm(SeverityResponse ~ ForeignAvg +  PolSkepAvg + CovidVaxAvg +  SocialPolitics + (1 |ResponseId) + (1 | SeverityItem), 
                    data = prewave,
                    family="cumulative", 
                    file = 'prewave/Models/prewave_m3',
                    chains=4, 
                    iter=3000, 
                    warmup=1500,
                    control = list(adapt_delta = .90),
                    prior = c(set_prior("normal(-2.8, .25)", class = "Intercept", coef = "1"),
                              set_prior("normal(-1.7, .25)", class = "Intercept", coef = "2"),
                              set_prior("normal(-1.0, .25)", class = "Intercept", coef = "3"),
                              set_prior("normal(-.67, .25)", class = "Intercept", coef = "4"),
                              set_prior("normal(0.07, .25)", class = "Intercept", coef = "5"),
                              set_prior("normal(1.06, .25)", class = "Intercept", coef = "6"),
                              set_prior("normal(0, 2)", class = "b", coef = "ForeignAvg"),
                              set_prior("normal(0, 2)", class = "b", coef = "PolSkepAvg"),
                              set_prior("normal(0, 2)", class = "b", coef = "CovidVaxAvg"),
                              set_prior("normal(0, 2)", class = "b", coef = "SocialPolitics"),
                              set_prior("normal(1,2)", class= "sd")))

prewave_m3_loo <- loo(prewave_m3)
saveRDS(prewave_m3_loo, file = 'prewave/Models/prewave_m3_loo.rds')

#big 3 drop pol
prewave_m4 <- brm(SeverityResponse ~ ForeignAvg +  PolSkepAvg + CovidVaxAvg  + (1 |ResponseId) + (1 | SeverityItem), 
                  data = prewave,
                  family="cumulative", 
                  file = 'prewave/Models/prewave_m4',
                  chains=4, 
                  iter=3000, 
                  warmup=1500,
                  control = list(adapt_delta = .90),
                  prior = c(set_prior("normal(-2.8, .25)", class = "Intercept", coef = "1"),
                            set_prior("normal(-1.7, .25)", class = "Intercept", coef = "2"),
                            set_prior("normal(-1.0, .25)", class = "Intercept", coef = "3"),
                            set_prior("normal(-.67, .25)", class = "Intercept", coef = "4"),
                            set_prior("normal(0.07, .25)", class = "Intercept", coef = "5"),
                            set_prior("normal(1.06, .25)", class = "Intercept", coef = "6"),
                            set_prior("normal(0, 2)", class = "b", coef = "ForeignAvg"),
                            set_prior("normal(0, 2)", class = "b", coef = "PolSkepAvg"),
                            set_prior("normal(0, 2)", class = "b", coef = "CovidVaxAvg"),
                            set_prior("normal(1,2)", class= "sd")))

prewave_m4_loo <- loo(prewave_m4)
saveRDS(prewave_m4_loo, file = 'prewave/Models/prewave_m4_loo.rds')

#big 2 plus politics
prewave_m5 <- brm(SeverityResponse ~  PolSkepAvg + CovidVaxAvg +  SocialPolitics + (1 |ResponseId) + (1 | SeverityItem), 
                  data = prewave,
                  family="cumulative", 
                  file = 'prewave/Models/prewave_m5',
                  chains=4, 
                  iter=3000, 
                  warmup=1500,
                  control = list(adapt_delta = .90),
                  prior = c(set_prior("normal(-2.8, .25)", class = "Intercept", coef = "1"),
                            set_prior("normal(-1.7, .25)", class = "Intercept", coef = "2"),
                            set_prior("normal(-1.0, .25)", class = "Intercept", coef = "3"),
                            set_prior("normal(-.67, .25)", class = "Intercept", coef = "4"),
                            set_prior("normal(0.07, .25)", class = "Intercept", coef = "5"),
                            set_prior("normal(1.06, .25)", class = "Intercept", coef = "6"),
                            set_prior("normal(0, 2)", class = "b", coef = "PolSkepAvg"),
                            set_prior("normal(0, 2)", class = "b", coef = "CovidVaxAvg"),
                            set_prior("normal(0, 2)", class = "b", coef = "SocialPolitics"),
                            set_prior("normal(1,2)", class= "sd")))

prewave_m5_loo <- loo(prewave_m5)
saveRDS(prewave_m5_loo, file = 'prewave/Models/prewave_m5_loo.rds')

#big 2 drop pol
prewave_m6 <- brm(SeverityResponse ~ PolSkepAvg + CovidVaxAvg + (1 |ResponseId) + (1 | SeverityItem), 
                  data = prewave,
                  family="cumulative", 
                  file = 'prewave/Models/prewave_m6',
                  chains=4, 
                  iter=3000, 
                  warmup=1500,
                  control = list(adapt_delta = .90),
                  prior = c(set_prior("normal(-2.8, .25)", class = "Intercept", coef = "1"),
                            set_prior("normal(-1.7, .25)", class = "Intercept", coef = "2"),
                            set_prior("normal(-1.0, .25)", class = "Intercept", coef = "3"),
                            set_prior("normal(-.67, .25)", class = "Intercept", coef = "4"),
                            set_prior("normal(0.07, .25)", class = "Intercept", coef = "5"),
                            set_prior("normal(1.06, .25)", class = "Intercept", coef = "6"),
                            set_prior("normal(0, 2)", class = "b", coef = "PolSkepAvg"),
                            set_prior("normal(0, 2)", class = "b", coef = "CovidVaxAvg"),
                            set_prior("normal(1,2)", class= "sd")))

prewave_m6_loo <- loo(prewave_m6)
saveRDS(prewave_m6_loo, file = 'prewave/Models/prewave_m6_loo.rds')

#big 1 plus politics
prewave_m7 <- brm(SeverityResponse ~  PolSkepAvg + SocialPolitics + (1 |ResponseId) + (1 | SeverityItem), 
                  data = prewave,
                  family="cumulative", 
                  file = 'prewave/Models/prewave_m7',
                  chains=4, 
                  iter=3000, 
                  warmup=1500,
                  control = list(adapt_delta = .90),
                  prior = c(set_prior("normal(-2.8, .25)", class = "Intercept", coef = "1"),
                            set_prior("normal(-1.7, .25)", class = "Intercept", coef = "2"),
                            set_prior("normal(-1.0, .25)", class = "Intercept", coef = "3"),
                            set_prior("normal(-.67, .25)", class = "Intercept", coef = "4"),
                            set_prior("normal(0.07, .25)", class = "Intercept", coef = "5"),
                            set_prior("normal(1.06, .25)", class = "Intercept", coef = "6"),
                            set_prior("normal(0, 2)", class = "b", coef = "PolSkepAvg"),
                            set_prior("normal(0, 2)", class = "b", coef = "SocialPolitics"),
                            set_prior("normal(1,2)", class= "sd")))

prewave_m7_loo <- loo(prewave_m7)
saveRDS(prewave_m7_loo, file = 'prewave/Models/prewave_m7_loo.rds')

#big 1 drop pol
prewave_m8 <- brm(SeverityResponse ~ PolSkepAvg + (1 |ResponseId) + (1 | SeverityItem), 
                  data = prewave,
                  family="cumulative", 
                  file = 'prewave/Models/prewave_m8',
                  chains=4, 
                  iter=3000, 
                  warmup=1500,
                  control = list(adapt_delta = .90),
                  prior = c(set_prior("normal(-2.8, .25)", class = "Intercept", coef = "1"),
                            set_prior("normal(-1.7, .25)", class = "Intercept", coef = "2"),
                            set_prior("normal(-1.0, .25)", class = "Intercept", coef = "3"),
                            set_prior("normal(-.67, .25)", class = "Intercept", coef = "4"),
                            set_prior("normal(0.07, .25)", class = "Intercept", coef = "5"),
                            set_prior("normal(1.06, .25)", class = "Intercept", coef = "6"),
                            set_prior("normal(0, 2)", class = "b", coef = "PolSkepAvg"),
                            set_prior("normal(1,2)", class= "sd")))

prewave_m8_loo <- loo(prewave_m8)
saveRDS(prewave_m8_loo, file = 'prewave/Models/prewave_m8_loo.rds')
 

#big pos + neg plus politics
prewave_m9 <- brm(SeverityResponse ~ ForeignAvg +  PolSkepAvg + SocialPolitics + (1 |ResponseId) + (1 | SeverityItem), 
                  data = prewave,
                  family="cumulative", 
                  file = 'prewave/Models/prewave_9',
                  chains=4, 
                  iter=3000, 
                  warmup=1500,
                  control = list(adapt_delta = .90),
                  prior = c(set_prior("normal(-2.8, .25)", class = "Intercept", coef = "1"),
                            set_prior("normal(-1.7, .25)", class = "Intercept", coef = "2"),
                            set_prior("normal(-1.0, .25)", class = "Intercept", coef = "3"),
                            set_prior("normal(-.67, .25)", class = "Intercept", coef = "4"),
                            set_prior("normal(0.07, .25)", class = "Intercept", coef = "5"),
                            set_prior("normal(1.06, .25)", class = "Intercept", coef = "6"),
                            set_prior("normal(0, 2)", class = "b", coef = "ForeignAvg"),
                            set_prior("normal(0, 2)", class = "b", coef = "PolSkepAvg"),
                            set_prior("normal(0, 2)", class = "b", coef = "SocialPolitics"),
                            set_prior("normal(1,2)", class= "sd")))

prewave_m9_loo <- loo(prewave_m9)
saveRDS(prewave_m9_loo, file = 'prewave/Models/prewave_m9_loo.rds')

#big pos+ neg drop pol
prewave_m10 <- brm(SeverityResponse ~ ForeignAvg +  PolSkepAvg + (1 |ResponseId) + (1 | SeverityItem), 
                  data = prewave,
                  family="cumulative", 
                  file = 'prewave/Models/prewave_m10',
                  chains=4, 
                  iter=3000, 
                  warmup=1500,
                  control = list(adapt_delta = .90),
                  prior = c(set_prior("normal(-2.8, .25)", class = "Intercept", coef = "1"),
                            set_prior("normal(-1.7, .25)", class = "Intercept", coef = "2"),
                            set_prior("normal(-1.0, .25)", class = "Intercept", coef = "3"),
                            set_prior("normal(-.67, .25)", class = "Intercept", coef = "4"),
                            set_prior("normal(0.07, .25)", class = "Intercept", coef = "5"),
                            set_prior("normal(1.06, .25)", class = "Intercept", coef = "6"),
                            set_prior("normal(0, 2)", class = "b", coef = "ForeignAvg"),
                            set_prior("normal(0, 2)", class = "b", coef = "PolSkepAvg"),
                            set_prior("normal(1,2)", class= "sd")))

prewave_m10_loo <- loo(prewave_m10)
saveRDS(prewave_m10_loo, file = 'prewave/Models/prewave_m10_loo.rds')

prewave_m11 <- brm(SeverityResponse ~ SocialPolitics + (1 |ResponseId) + (1 | SeverityItem), 
                  data = prewave,
                  family="cumulative", 
                  file = 'prewave/Models/prewave_m11',
                  chains=4, 
                  iter=3000, 
                  warmup=1500,
                  control = list(adapt_delta = .90),
                  prior = c(set_prior("normal(-2.8, .25)", class = "Intercept", coef = "1"),
                            set_prior("normal(-1.7, .25)", class = "Intercept", coef = "2"),
                            set_prior("normal(-1.0, .25)", class = "Intercept", coef = "3"),
                            set_prior("normal(-.67, .25)", class = "Intercept", coef = "4"),
                            set_prior("normal(0.07, .25)", class = "Intercept", coef = "5"),
                            set_prior("normal(1.06, .25)", class = "Intercept", coef = "6"),
                            set_prior("normal(0, 2)", class = "b", coef = "SocialPolitics"),
                            set_prior("normal(1,2)", class= "sd")))

prewave_m11_loo <- loo(prewave_m11)
saveRDS(prewave_m11_loo, file = 'prewave/Models/prewave_m11_loo.rds')

prewave_loo_compare <- loo_compare(prewave_full_loo,
            prewave_m2_loo,
            prewave_m3_loo,
            prewave_m4_loo,
            prewave_m5_loo,
            prewave_m6_loo,
            prewave_m7_loo,
            prewave_m8_loo,
            prewave_m9_loo,
            prewave_m10_loo,
            prewave_m11_loo)

#prewave_loo_compare <- as.array(prewave_loo_compare)
#prewave_loo_compare
#prewave_loo_compare$model_name <- rownames(prewave_loo_compare)

prewave_loo_compare <- data.frame(model_name = row.names(prewave_loo_compare), prewave_loo_compare)
prewave_loo_compare$elpd_diff <- abs(prewave_loo_compare$elpd_diff)

###
### Visualize loo model comparison
###
 
model_order <- rev(c('prewave_full',
                 'prewave_m2', 'prewave_m3', 'prewave_m4', 'prewave_m5', 'prewave_m6',
                 'prewave_m7', 'prewave_m8', 'prewave_m9', 'prewave_m10', 'prewave_m11'))


#full (ref)
#m2 full - politics
#m3 big 3 + pol
#m4 big 3 - pol
#m5 big 2 + pol
#m6 big 2 - pol
#m7 big 1 + pol
#m8 big 1 - pol
#m9 big pos + neg plus politics
#m10 big pos + neg w/0 politics
#m11 politics only

model_names <- rev(c('Full Model',
                 'Full Model - Politics', 
                 'Big 3 + Politics', 
                 'Big 3 - Politics', 
                 'Big 2 + Politics', 
                 'Big 2 - Politics',
                 'Political Distrust + Politics', 
                 'Political Distrust - Politics', 
                 'Political Distrust + Foreign + Politics',
                 'Political Distrust + Foreign - Politics',
                 'Politics Only'))

prewave_loo_plot <- ggplot(data = prewave_loo_compare, aes(x = factor(model_name, level = model_order), y= elpd_diff))+
  geom_point()+
  geom_errorbar(aes(ymin = elpd_diff - se_diff, ymax = elpd_diff + se_diff, width = .0))+
  coord_flip()+
  scale_x_discrete(name = "Model", 
                     labels = model_names)+
  scale_y_continuous(name = "loo Cross-Validation Difference", 
                     breaks = c(0, 15, 30, 45, 60, 75, 90, 105),
                     labels = c(0, 15, 30, 45, 60, 75, 90, 105))+
  theme_minimal()+
  theme(
    axis.text.y = element_text(size=10,face="bold"),
    axis.text.x = element_text(size=12,face="bold"),
    axis.title.x = element_text(size=14,face="bold"),
    axis.title.y = element_text(size=14,face="bold"))


prewave_loo_plot


###
### VISUALIZE PREWAVE ATTITUDES
###



###
#FIGURE 2: MAXIMAL MODEL POSTERIORS
###
color_scheme_set("blue")
bayesplot_theme_update(text = element_text(size = 16, family = "sans"))

prewave_posterior <- as.array(prewave_full)

prewave_post_plot <- mcmc_intervals(prewave_posterior, 
                                    pars = c("b_SocialPolitics", "b_PolSkepAvg", "b_CovidVaxAvg", "b_MedSkepAvg",   "b_MediaSkepAvg", "b_OriginAvg", "b_ForeignAvg"),
                                    prob = .95, #95% CI, wish i could make it 94, but ya know... 
                                    prob_outer = 0.99, 
                                    point_est = "mean")

#view posteriors as distributions here
#mcmc_areas(prewave_posterior, 
#           pars = c("b_PoliticsVal", "b_PolSkepAvg", "b_CovidVaxAvg", "b_MedSkepAvg", "b_MediaSkepAvg",   "b_OriginAvg", "b_ForeignAvg"),
#           prob = .94, #94% CI, 
#           prob_outer = 0.99, 
#           point_est = "mean")


prewave_post_plot +
  scale_y_discrete(
    labels = c("b_SocialPolitics" = 'Political Attitudes', 
               "b_PolSkepAvg" = 'Democrat Distrust', 
               "b_CovidVaxAvg" = 'COVID-19 Vax Fears', 
               "b_MediaSkepAvg" = 'Media Distrust',
               "b_MedSkepAvg" = 'Medical Distrust', 
               "b_OriginAvg" = 'Origin Suspicions', 
               "b_ForeignAvg" = 'Foreign Threat')
  )+
  xlab('Estimated Effect on COVID-19 Severity \n (More negative values imply a stronger effect)')+
  ylab('Belief')

###
### PLOT BIG 3 PREWAVE PREDICTORS
###

ce1 <- conditional_effects(prewave_full)

prewave_split <- prewave %>%
  filter(Politics != 'Moderate')


ce1_tib_pol <- tibble('Skepticism' = ce1["PolSkepAvg"]$PolSkepAvg$PolSkepAvg,
                      'lower' = ce1["PolSkepAvg"]$PolSkepAvg$lower__,
                      'upper' = ce1["PolSkepAvg"]$PolSkepAvg$upper__,
                      'Estimate' = ce1["PolSkepAvg"]$PolSkepAvg$estimate__
)

ce1_tib_vax <- tibble('Skepticism' = ce1["CovidVaxAvg"]$CovidVaxAvg$CovidVaxAvg,
                      'lower' = ce1["CovidVaxAvg"]$CovidVaxAvg$lower__,
                      'upper' = ce1["CovidVaxAvg"]$CovidVaxAvg$upper__,
                      'Estimate' = ce1["CovidVaxAvg"]$CovidVaxAvg$estimate__
)

ce1_tib_for <- tibble('Skepticism' = ce1["ForeignAvg"]$ForeignAvg$ForeignAvg,
                      'lower' = ce1["ForeignAvg"]$ForeignAvg$lower__,
                      'upper' = ce1["ForeignAvg"]$ForeignAvg$upper__,
                      'Estimate' = ce1["ForeignAvg"]$ForeignAvg$estimate__
)


#Believing Covid-19 is a Liberal Hoax Predicts Not Taking It Seriously
p1 <- ggplot()+
  geom_jitter(data = prewave_split, 
              aes(x = PolSkepAvg, y = SeverityResponse, colour = Politics, stroke = 0, alpha = .0))+
  geom_smooth(data = ce1_tib_pol, aes(x = Skepticism, y = Estimate), se= FALSE, colour = '#251d26')+
  geom_ribbon(data = ce1_tib_pol ,aes(x = Skepticism, ymin = lower, ymax = upper), alpha = .25, colour = NA)+
  theme_minimal()+
  scale_y_continuous(name = "COVID-19 Severity", 
                     breaks = c(1,4,7),
                     labels = c('Low', 'Unsure', 'High'))+
  scale_x_continuous(name = "Democrat Distrust", 
                     breaks = c(1,4,7),
                     labels = c('Low', 'Unsure', 'High'))+
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.75, vjust = .25))+
  scale_fill_manual(values = c('#F50000', '#0077ff'))+
  scale_color_manual(values = c('#F50000', '#0077ff'))+
  theme(
    axis.title.y = element_text(size=14,face="bold"),
    axis.text.x = element_text(size=12,face="bold"),
    axis.text.y = element_text(size=12,face="bold"),
    axis.title.x = element_text(size=14,face="bold"))


p1b <- ggMarginal(p1, groupColour = TRUE, groupFill =  TRUE, margin = 'x')#set margins to dicate which one's are drawn

p2 <- ggplot()+
  geom_jitter(data = prewave_split, 
              aes(x = ForeignAvg, y = SeverityResponse, colour = Politics, stroke = 0, alpha = .0))+
  geom_smooth(data = ce1_tib_for, aes(x = Skepticism, y = Estimate), se= FALSE, colour = '#251d26')+
  geom_ribbon(data = ce1_tib_for ,aes(x = Skepticism, ymin = lower, ymax = upper), alpha = .25, colour = NA)+
  theme_minimal()+
  scale_x_continuous(name = "Assessments of Foreign Threat", 
                     breaks = c(1,4,7),
                     labels = c('Low', 'Unsure', 'High'))+
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.75, vjust = .25))+
  scale_fill_manual(values = c('#F50000', '#0077ff'))+
  scale_color_manual(values = c('#F50000', '#0077ff'))+
  ylab(NULL)+
  theme(    
    axis.text.y = element_blank(),
    axis.text.x = element_text(size=12,face="bold"),
    axis.title.x = element_text(size=14,face="bold"))

p2b <- ggMarginal(p2, groupColour = TRUE,  groupFill =  TRUE)#set margins to dicate which one's are drawn

p3 <- ggplot()+
  geom_jitter(data = prewave_split, 
              aes(x = CovidVaxAvg, y = SeverityResponse, colour = Politics, stroke = 0, alpha = .0))+
  geom_smooth(data = ce1_tib_vax, aes(x = Skepticism, y = Estimate), se= FALSE, colour = '#251d26')+
  geom_ribbon(data = ce1_tib_vax, aes(x = Skepticism, ymin = lower, ymax = upper), alpha = .25, colour = NA)+
  theme_minimal()+
  ylab('COVID-19 Severity')+
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.75, vjust = .25))+
  scale_fill_manual(values = c('#F50000', '#0077ff'))+
  scale_color_manual(values = c('#F50000', '#0077ff'))+
  ylab(NULL)+
  scale_x_continuous(name = "COVID-19 Vax Fears", 
                     breaks = c(1,4,7),
                     labels = c('Low', 'Unsure', 'High'))+
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(size=12,face="bold"),
    axis.title.x = element_text(size=14,face="bold"))

p3b <- ggMarginal(p3, groupColour = TRUE, groupFill =  TRUE, margins = 'x')

#put into same fig
#prewave big 3 fig in publication
plot_grid(p1b, p3b,p2b , labels = c('a','b','c'), nrow = 1, ncol = 3)


### PARTIAL CORRELATIONS BETWEEN PREDICTORS, PREWAVE
#build partial correlation
prewave_pc_df <- prewave %>%
  dplyr::select(c(MedSkepAvg, ForeignAvg ,OriginAvg , PolSkepAvg , CovidVaxAvg , MediaSkepAvg, SocialPolitics))%>%
  rename(`Medical Distrust` = MedSkepAvg)%>%
  rename(`Foreign Threat` = ForeignAvg)%>%
  rename(`COVID-19 Origin` = OriginAvg)%>%
  rename(`Democrat Distrust` = PolSkepAvg)%>%
  rename(`COVID-19 Vax Fears` = CovidVaxAvg)%>%
  rename(`Media Distrust` = MediaSkepAvg)%>%
  rename(`Conservative Politics` = SocialPolitics)

ggcorrplot(pcor(prewave_pc_df)$estimate, 
           type = 'upper', 
           outline = 'white', 
           lab = TRUE, 
           show.legend = FALSE, 
           colors = c('#0077ff','white','#F50000' ))


#####
##### FIRSTWAVE ATTITUDES
#####
get_prior(SeverityResponse ~ Wave*SocialPolitics + (1 |ResponseId) + (1 | SeverityItem), 
          data = all_data) 


qlogis(cumsum(table(all_data$SeverityResponse)/length(all_data$SeverityResponse)))

shift <- brm(SeverityResponse ~ Wave*SocialPolitics + (1 |ResponseId) + (1 | SeverityItem), 
             data = all_data,
             family="cumulative", 
             file = 'firstwave/Models/shift',
             chains=4, 
             iter=3000, 
             warmup=1500,
             control = list(adapt_delta = .90),
             prior = c(set_prior("normal(-3., .25)", class = "Intercept", coef = "1"),
                       set_prior("normal(-1.9, .25)", class = "Intercept", coef = "2"),
                       set_prior("normal(-1.3, .25)", class = "Intercept", coef = "3"),
                       set_prior("normal(-1., .25)", class = "Intercept", coef = "4"),
                       set_prior("normal(-.25, .25)", class = "Intercept", coef = "5"),
                       set_prior("normal(.74, .25)", class = "Intercept", coef = "6"),
                       set_prior("normal(0, 2)", class = "b", coef = "Wavepre"),
                       set_prior("normal(0, .5)", class = "b", coef = "Wavepre:SocialPolitics"),
                       set_prior("normal(1,2)", class= "sd")))


shift_posterior <- as.array(shift)

shift_post_plot <- mcmc_intervals(shift_posterior, 
                                    pars = c("b_Wavepre", "b_SocialPolitics", "b_Wavepre:SocialPolitics"),
                                    prob = .95, #95% CI, wish i could make it 94, but ya know... 
                                    prob_outer = 0.99, 
                                    point_est = "mean")


#ty https://discourse.mc-stan.org/t/change-aesthetics-conditional-effects/13500
ce_shift <- conditional_effects(shift, spaghetti = FALSE, nsamples = 100)

ce_shift_plot <- plot(ce_shift, plot = FALSE)[[3]]+ 
  scale_x_continuous(name = "Political Orientation", 
                     breaks = c(1.5,4,6.5),
                     labels = c('Very\nLiberal', 'Moderate', 'Very\nConservative'))+
  scale_y_continuous(name = "COVID-19 Severity", 
                     breaks = c(3,4,5,6),
                     labels = c('Moderately Unsevere','Less Severe','Moderately\nSevere','More Severe'))+
  theme_minimal()+
  scale_fill_manual(values = c('#4a4063', '#4287f5'), labels = c('First-wave', 'Prewave'))+
  scale_color_manual(values = c('#4a4063', '#4287f5'), labels = c('First-wave', 'Prewave'))+
  theme(legend.title = element_blank(),
        legend.position = c(.8, .85),
        legend.key.size = unit(1, 'cm'),
        legend.text = element_text(size=14, face="bold"),
        axis.text.x = element_text(size=14,face="bold"),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(size=16,face="bold"),
        axis.title.y = element_text(size=16,face="bold"))#+
  #guides(color=guide_legend(override.aes=list(fill=NA)))

ce_shift_plot


### MODELS OF FIRSTWAVE ATTITUDES
get_prior(SeverityResponse ~ MedSkepAvg + ForeignAvg + OriginAvg + PolSkepAvg + VaxSkepAvg + EconAvg + CovidVaxAvg + SocialPolitics + (1 |ResponseId) + (1 | SeverityItem), 
          data = firstwave)

qlogis(cumsum(table(firstwave$SeverityResponse)/length(firstwave$SeverityResponse)))

firstwave_full <- brm(SeverityResponse ~ MedSkepAvg + ForeignAvg + OriginAvg + PolSkepAvg + VaxSkepAvg + EconAvg + CovidVaxAvg + SocialPolitics + (1 |ResponseId) + (1 | SeverityItem), 
                      data = firstwave,
                      family="cumulative", 
                      file = 'firstwave/Models/firstwave_full',
                      chains=4, 
                      iter=3000, 
                      warmup=1500,
                      control = list(adapt_delta = .90),
                      prior = c(set_prior("normal(-3.05, .25)", class = "Intercept", coef = "1"),
                                set_prior("normal(-1.97, .25)", class = "Intercept", coef = "2"),
                                set_prior("normal(-1.39, .25)", class = "Intercept", coef = "3"),
                                set_prior("normal(-1.02, .25)", class = "Intercept", coef = "4"),
                                set_prior("normal(-.38, .25)", class = "Intercept", coef = "5"),
                                set_prior("normal(.62, .25)", class = "Intercept", coef = "6"),
                                set_prior("normal(0, 2)", class = "b", coef = "ForeignAvg"),
                                set_prior("normal(0, 2)", class = "b", coef = "EconAvg"),
                                set_prior("normal(0, 2)", class = "b", coef = "MedSkepAvg"),
                                set_prior("normal(0, 2)", class = "b", coef = "OriginAvg"),
                                set_prior("normal(0, 2)", class = "b", coef = "PolSkepAvg"),
                                set_prior("normal(0, 2)", class = "b", coef = "VaxSkepAvg"),
                                set_prior("normal(0, 2)", class = "b", coef = "CovidVaxAvg"),
                                set_prior("normal(0, 2)", class = "b", coef = "SocialPolitics"),
                                set_prior("normal(1,2)", class= "sd")))


firstwave_posterior <- as.array(firstwave_full)

firstwave_post_plot <- mcmc_intervals(firstwave_posterior, 
                                    pars = c("b_SocialPolitics", "b_PolSkepAvg","b_EconAvg","b_CovidVaxAvg", "b_MedSkepAvg", "b_VaxSkepAvg","b_ForeignAvg", "b_OriginAvg"),
                                    prob = .95, #95% CI, wish i could make it 94, but ya know... 
                                    prob_outer = 0.99, 
                                    point_est = "mean")

#view posteriors as distributions here
#mcmc_areas(firstwave_posterior, 
#pars = c("b_SocialPolitics", "b_PolSkepAvg","b_EconAvg","b_CovidVaxAvg", "b_MedSkepAvg", "b_VaxSkepAvg", "b_OriginAvg", "b_ForeignAvg"),
#prob = .95, #95% CI, wish i could make it 94, but ya know... 
#prob_outer = 0.99, 
#point_est = "mean")

firstwave_post_plot +
  scale_y_discrete(
    labels = c("b_SocialPolitics" = 'Political Attitudes', 
               "b_PolSkepAvg" = 'Democrat Distrust', 
               "b_CovidVaxAvg" = 'COVID-19 Vax Fears', 
               "b_VaxSkepAvg" = 'General Vaccine Skepticism',
               "b_EconAvg" = "Open up the Economy",
               "b_MedSkepAvg" = 'Medical Distrust', 
               "b_OriginAvg" = 'Origin Suspicions', 
               "b_ForeignAvg" = 'Foreign Threat')
  )+
  xlab('Estimated Effect on COVID-19 Severity \n (More negative values imply a stronger effect)')+
  ylab('Belief')



###
### PLOT BIG 5 PREDICTORS OF PREWAVE PREDICTORS
###

ce2 <- conditional_effects(firstwave_full)

wave_longDR <- firstwave %>%
  filter(Politics != 'Moderate')

#PolSkepAvg
ce2_tib_pol <- tibble('Skepticism' = ce2["PolSkepAvg"]$PolSkepAvg$PolSkepAvg,
                      'lower' = ce2["PolSkepAvg"]$PolSkepAvg$lower__,
                      'upper' = ce2["PolSkepAvg"]$PolSkepAvg$upper__,
                      'Estimate' = ce2["PolSkepAvg"]$PolSkepAvg$estimate__
)

#CovidVaxAvg
ce2_tib_vax19 <- tibble('Skepticism' = ce2["CovidVaxAvg"]$CovidVaxAvg$CovidVaxAvg,
                        'lower' = ce2["CovidVaxAvg"]$CovidVaxAvg$lower__,
                        'upper' = ce2["CovidVaxAvg"]$CovidVaxAvg$upper__,
                        'Estimate' = ce2["CovidVaxAvg"]$CovidVaxAvg$estimate__
)
#EconAvg
ce2_tib_econ <- tibble('Skepticism' = ce2["EconAvg"]$EconAvg$EconAvg,
                       'lower' = ce2["EconAvg"]$EconAvg$lower__,
                       'upper' = ce2["EconAvg"]$EconAvg$upper__,
                       'Estimate' = ce2["EconAvg"]$EconAvg$estimate__
)

#OriginAvg
ce2_tib_org<- tibble('Skepticism' = ce2["OriginAvg"]$OriginAvg$OriginAvg,
                     'lower' = ce2["OriginAvg"]$OriginAvg$lower__,
                     'upper' = ce2["OriginAvg"]$OriginAvg$upper__,
                     'Estimate' = ce2["OriginAvg"]$OriginAvg$estimate__
)

#MedSkepAvg
ce2_tib_med <- tibble('Skepticism' = ce2["MedSkepAvg"]$MedSkepAvg$MedSkepAvg,
                      'lower' = ce2["MedSkepAvg"]$MedSkepAvg$lower__,
                      'upper' = ce2["MedSkepAvg"]$MedSkepAvg$upper__,
                      'Estimate' = ce2["MedSkepAvg"]$MedSkepAvg$estimate__
)



#Believing Covid-19 is a Liberal Hoax Predicts Not Taking It Seriously
wp1 <- ggplot()+
  geom_jitter(data = wave_longDR, 
              aes(x = PolSkepAvg, y = SeverityResponse, colour = Politics, stroke = 0, alpha = .0))+
  geom_smooth(data = ce2_tib_pol, aes(x = Skepticism, y = Estimate), se= FALSE, colour = '#251d26')+
  geom_ribbon(data = ce2_tib_pol ,aes(x = Skepticism, ymin = lower, ymax = upper), alpha = .25, colour = NA)+
  theme_minimal()+
  ylab('COVID-19 Severity')+
  scale_y_continuous(name = "COVID-19 Severity", 
                     breaks = c(1,4,7),
                     labels = c('Low', 'Unsure', 'High'))+
  scale_x_continuous(name = "Democrat Distrust", 
                     breaks = c(1,4,7),
                     labels = c('Low', 'Unsure', 'High'))+
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.75, vjust = .25))+
  scale_fill_manual(values = c('#F50000', '#0077ff'))+
  scale_color_manual(values = c('#F50000', '#0077ff'))+
  theme(
    axis.title.y = element_text(size=14,face="bold"),
    axis.text.x = element_text(size=12,face="bold"),
    axis.text.y = element_text(size=12,face="bold"),
    axis.title.x = element_text(size=14,face="bold"))


wp1b <- ggMarginal(wp1, groupColour = TRUE, groupFill =  TRUE, margins = 'x')#set margins to dicate which one's are drawn
#wp1b


#covid-19 vax fears
wp2 <- ggplot()+
  geom_jitter(data = wave_longDR, 
              aes(x = CovidVaxAvg, y = SeverityResponse, colour = Politics, stroke = 0, alpha = .0))+
  geom_smooth(data = ce2_tib_vax19, aes(x = Skepticism, y = Estimate), se= FALSE, colour = '#251d26')+
  geom_ribbon(data = ce2_tib_vax19 ,aes(x = Skepticism, ymin = lower, ymax = upper), alpha = .25, colour = NA)+
  theme_minimal()+
  ylab(NULL)+
  scale_x_continuous(name = "COVID-19 Vax Fears", 
                     breaks = c(1,4,7),
                     labels = c('Low', 'Unsure', 'High'))+
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.75, vjust = .25))+
  scale_fill_manual(values = c('#F50000', '#0077ff'))+
  scale_color_manual(values = c('#F50000', '#0077ff'))+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size=12,face="bold"),
    axis.title.x = element_text(size=14,face="bold"))

wp2b <- ggMarginal(wp2, groupColour = TRUE, groupFill =  TRUE)#set margins to dicate which one's are drawn


#econ
wp3 <- ggplot()+
  geom_jitter(data = wave_longDR, 
              aes(x = EconAvg, y = SeverityResponse, colour = Politics, stroke = 0, alpha = .0))+
  geom_smooth(data = ce2_tib_econ, aes(x = Skepticism, y = Estimate), se= FALSE, colour = '#251d26')+
  geom_ribbon(data = ce2_tib_econ, aes(x = Skepticism, ymin = lower, ymax = upper), alpha = .25, colour = NA)+
  theme_minimal()+
  xlab('Opening up Economy is Necessary')+
  ylab('COVID-19 Severity')+
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.75, vjust = .25))+
  scale_fill_manual(values = c('#F50000', '#0077ff'))+
  scale_color_manual(values = c('#F50000', '#0077ff'))+
  ylab(NULL)+
  scale_x_continuous(name = "Opening up Economy is Necessary", 
                     breaks = c(1,4,7),
                     labels = c('Low', 'Unsure', 'High'))+
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(size=12,face="bold"),
    axis.title.x = element_text(size=14,face="bold"))

wp3b <- ggMarginal(wp3, groupColour = TRUE, groupFill =  TRUE, margins = 'x')
#wp3b

#origin
wp4 <- ggplot()+
  geom_jitter(data = wave_longDR, 
              aes(x = OriginAvg, y = SeverityResponse, colour = Politics, stroke = 0, alpha = .0))+
  geom_smooth(data = ce2_tib_org, aes(x = Skepticism, y = Estimate), se= FALSE, colour = '#251d26')+
  geom_ribbon(data = ce2_tib_org, aes(x = Skepticism, ymin = lower, ymax = upper), alpha = .25, colour = NA)+
  theme_minima()l+
  xlab('Covid-19 has Uncertain Origins')+
  ylab(NULL)+
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.75, vjust = .25))+
  scale_fill_manual(values = c('#F50000', '#0077ff'))+
  scale_color_manual(values = c('#F50000', '#0077ff'))+
  scale_x_continuous(name = "COVID-19 has Uncertain Origins", 
                     breaks = c(1,4,7),
                     labels = c('Low', 'Unsure', 'High'))+
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(size=12,face="bold"),
    axis.title.x = element_text(size=14,face="bold"))

wp4b <- ggMarginal(wp4, groupColour = TRUE, groupFill =  TRUE)

#medical skepticism
wp5 <- ggplot()+
  geom_jitter(data = wave_longDR, 
              aes(x = MedSkepAvg, y = SeverityResponse, colour = Politics, stroke = 0, alpha = .0))+
  geom_smooth(data = ce2_tib_med, aes(x = Skepticism, y = Estimate), se= FALSE, colour = '#251d26')+
  geom_ribbon(data = ce2_tib_med, aes(x = Skepticism, ymin = lower, ymax = upper), alpha = .25, colour = NA)+
  theme_minimal()+
  xlab('Distrust of Large Medical Organizations')+
  ylab('Covid-19 Severity')+
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.75, vjust = .25))+
  scale_fill_manual(values = c('#F50000', '#0077ff'))+
  scale_color_manual(values = c('#F50000', '#0077ff'))+
  scale_y_continuous(name = "COVID-19 Severity", 
                     breaks = c(1,4,7),
                     labels = c('Low', 'Unsure', 'High'))+
  scale_x_continuous(name = "Distrust of Large Medical Organizations", 
                     breaks = c(1,4,7),
                     labels = c('Low', 'Unsure', 'High'))+
  theme(
    axis.title.y = element_text(size=14,face="bold"),
    axis.text.x = element_text(size=12,face="bold"),
    axis.text.y = element_text(size=12,face="bold"),
    axis.title.x = element_text(size=14,face="bold"))

wp5b <- ggMarginal(wp5, groupColour = TRUE, groupFill =  TRUE, margin = 'x')



plot_grid(wp1b, wp3b, wp2b, wp5b, wp4b, labels = c('a','b','c', 'd', 'e'), nrow = 2, ncol = 3)

#DO LOO-IC model fitting. Get the best performing, parsimonious model. 
#to keep track of models, they will be labeled as follows
  #full (ref)
  #m2 full - politics
  #m3 big 5 + pol (includes origin, positive effect on severity)
  #m4 big 5 - pol (includes origin, positive effect on severity)
  #m5 big 4 + pol (only factors that predict skepticism)
  #m6 big 4 - pol (only factors that predict skepticism)
  #m7 big 3 + pol (top 3 predictors of skepticism)
  #m8 big 3 - pol (top 3 predictors of skepticism)
  #m9 big 2 + pol (no medical science distrust in model)
  #m10 big 2 - pol (no medical science distrust in model)
  #m11 Dem + Covid-19 Vax Fears + pol (2 ontologies top predictor)
  #m12 Dem + COVID-19 Vax Fears - pol (2 ontologies top predictor)
  #m13 Dem hoax + pol (Biggest predictor)
  #m14 Dem hoax - pol (Biggest predictor)
  #m15 politics only

#firstwave_full_loo <- loo(firstwave_full)
#saveRDS(firstwave_full_loo, file = 'firstwave/Models/firstwave_full_loo.rds')
firstwave_full_loo <- readRDS(file = 'firstwave/Models/firstwave_full_loo.rds')

#m2 full - politics
firstwave_m2 <- brm(SeverityResponse ~ MedSkepAvg + ForeignAvg + OriginAvg + PolSkepAvg + VaxSkepAvg + EconAvg + CovidVaxAvg  + (1 |ResponseId) + (1 | SeverityItem), 
                      data = firstwave,
                      family="cumulative", 
                      file = 'firstwave/Models/firstwave_m2',
                      chains=4, 
                      iter=3000, 
                      warmup=1500,
                      control = list(adapt_delta = .90),
                      prior = c(set_prior("normal(-3.05, .25)", class = "Intercept", coef = "1"),
                                set_prior("normal(-1.97, .25)", class = "Intercept", coef = "2"),
                                set_prior("normal(-1.39, .25)", class = "Intercept", coef = "3"),
                                set_prior("normal(-1.02, .25)", class = "Intercept", coef = "4"),
                                set_prior("normal(-.38, .25)", class = "Intercept", coef = "5"),
                                set_prior("normal(.62, .25)", class = "Intercept", coef = "6"),
                                set_prior("normal(0, 2)", class = "b", coef = "ForeignAvg"),
                                set_prior("normal(0, 2)", class = "b", coef = "EconAvg"),
                                set_prior("normal(0, 2)", class = "b", coef = "MedSkepAvg"),
                                set_prior("normal(0, 2)", class = "b", coef = "OriginAvg"),
                                set_prior("normal(0, 2)", class = "b", coef = "PolSkepAvg"),
                                set_prior("normal(0, 2)", class = "b", coef = "VaxSkepAvg"),
                                set_prior("normal(0, 2)", class = "b", coef = "CovidVaxAvg"),
                                set_prior("normal(1,2)", class= "sd")))


#firstwave_m2_loo <- loo(firstwave_m2)
#saveRDS(firstwave_m2_loo, file = 'firstwave/Models/firstwave_m2_loo.rds')
firstwave_m2_loo <- readRDS(file = 'firstwave/Models/firstwave_m2_loo.rds')

#m3 big 5 + pol (includes origin, positive effect on severity)
firstwave_m3 <- brm(SeverityResponse ~ MedSkepAvg  + OriginAvg + PolSkepAvg  + EconAvg + CovidVaxAvg + SocialPolitics + (1 |ResponseId) + (1 | SeverityItem), 
                    data = firstwave,
                    family="cumulative", 
                    file = 'firstwave/Models/firstwave_m3',
                    chains=4, 
                    iter=3000, 
                    warmup=1500,
                    control = list(adapt_delta = .90),
                    prior = c(set_prior("normal(-3.05, .25)", class = "Intercept", coef = "1"),
                              set_prior("normal(-1.97, .25)", class = "Intercept", coef = "2"),
                              set_prior("normal(-1.39, .25)", class = "Intercept", coef = "3"),
                              set_prior("normal(-1.02, .25)", class = "Intercept", coef = "4"),
                              set_prior("normal(-.38, .25)", class = "Intercept", coef = "5"),
                              set_prior("normal(.62, .25)", class = "Intercept", coef = "6"),
                              set_prior("normal(0, 2)", class = "b", coef = "EconAvg"),
                              set_prior("normal(0, 2)", class = "b", coef = "MedSkepAvg"),
                              set_prior("normal(0, 2)", class = "b", coef = "OriginAvg"),
                              set_prior("normal(0, 2)", class = "b", coef = "PolSkepAvg"),
                              set_prior("normal(0, 2)", class = "b", coef = "CovidVaxAvg"),
                              set_prior("normal(0, 2)", class = "b", coef = "SocialPolitics"),
                              set_prior("normal(1,2)", class= "sd")))

#firstwave_m3_loo <- loo(firstwave_m3)
#saveRDS(firstwave_m3_loo, file = 'firstwave/Models/firstwave_m3_loo.rds')
firstwave_m3_loo <- readRDS(file = 'firstwave/Models/firstwave_m3_loo.rds')


#m4 big 5 - pol (includes origin, positive effect on severity)
firstwave_m4 <- brm(SeverityResponse ~ MedSkepAvg  + OriginAvg + PolSkepAvg  + EconAvg + CovidVaxAvg  + (1 |ResponseId) + (1 | SeverityItem), 
                    data = firstwave,
                    family="cumulative", 
                    file = 'firstwave/Models/firstwave_m4',
                    chains=4, 
                    iter=3000, 
                    warmup=1500,
                    control = list(adapt_delta = .90),
                    prior = c(set_prior("normal(-3.05, .25)", class = "Intercept", coef = "1"),
                              set_prior("normal(-1.97, .25)", class = "Intercept", coef = "2"),
                              set_prior("normal(-1.39, .25)", class = "Intercept", coef = "3"),
                              set_prior("normal(-1.02, .25)", class = "Intercept", coef = "4"),
                              set_prior("normal(-.38, .25)", class = "Intercept", coef = "5"),
                              set_prior("normal(.62, .25)", class = "Intercept", coef = "6"),
                              set_prior("normal(0, 2)", class = "b", coef = "EconAvg"),
                              set_prior("normal(0, 2)", class = "b", coef = "MedSkepAvg"),
                              set_prior("normal(0, 2)", class = "b", coef = "OriginAvg"),
                              set_prior("normal(0, 2)", class = "b", coef = "PolSkepAvg"),
                              set_prior("normal(0, 2)", class = "b", coef = "CovidVaxAvg"),
                              set_prior("normal(1,2)", class= "sd")))

#firstwave_m4_loo <- loo(firstwave_m4)
#saveRDS(firstwave_m4_loo, file = 'firstwave/Models/firstwave_m4_loo.rds')
firstwave_m4_loo <- readRDS(file = 'firstwave/Models/firstwave_m4_loo.rds')

#m5 big 4 + pol (only factors that predict skepticism)
firstwave_m5 <- brm(SeverityResponse ~ MedSkepAvg + PolSkepAvg  + EconAvg + CovidVaxAvg + SocialPolitics + (1 |ResponseId) + (1 | SeverityItem), 
                    data = firstwave,
                    family="cumulative", 
                    file = 'firstwave/Models/firstwave_m5',
                    chains=4, 
                    iter=3000, 
                    warmup=1500,
                    control = list(adapt_delta = .90),
                    prior = c(set_prior("normal(-3.05, .25)", class = "Intercept", coef = "1"),
                              set_prior("normal(-1.97, .25)", class = "Intercept", coef = "2"),
                              set_prior("normal(-1.39, .25)", class = "Intercept", coef = "3"),
                              set_prior("normal(-1.02, .25)", class = "Intercept", coef = "4"),
                              set_prior("normal(-.38, .25)", class = "Intercept", coef = "5"),
                              set_prior("normal(.62, .25)", class = "Intercept", coef = "6"),
                              set_prior("normal(0, 2)", class = "b", coef = "EconAvg"),
                              set_prior("normal(0, 2)", class = "b", coef = "MedSkepAvg"),
                              set_prior("normal(0, 2)", class = "b", coef = "PolSkepAvg"),
                              set_prior("normal(0, 2)", class = "b", coef = "CovidVaxAvg"),
                              set_prior("normal(0, 2)", class = "b", coef = "SocialPolitics"),
                              set_prior("normal(1,2)", class= "sd")))

#firstwave_m5_loo <- loo(firstwave_m5)
#saveRDS(firstwave_m5_loo, file = 'firstwave/Models/firstwave_m5_loo.rds')
firstwave_m5_loo <- readRDS(file = 'firstwave/Models/firstwave_m5_loo.rds')


#m6 big 4 - pol (only factors that predict skepticism)
firstwave_m6 <- brm(SeverityResponse ~ MedSkepAvg + PolSkepAvg  + EconAvg + CovidVaxAvg  + (1 |ResponseId) + (1 | SeverityItem), 
                    data = firstwave,
                    family="cumulative", 
                    file = 'firstwave/Models/firstwave_m6',
                    chains=4, 
                    iter=3000, 
                    warmup=1500,
                    control = list(adapt_delta = .90),
                    prior = c(set_prior("normal(-3.05, .25)", class = "Intercept", coef = "1"),
                              set_prior("normal(-1.97, .25)", class = "Intercept", coef = "2"),
                              set_prior("normal(-1.39, .25)", class = "Intercept", coef = "3"),
                              set_prior("normal(-1.02, .25)", class = "Intercept", coef = "4"),
                              set_prior("normal(-.38, .25)", class = "Intercept", coef = "5"),
                              set_prior("normal(.62, .25)", class = "Intercept", coef = "6"),
                              set_prior("normal(0, 2)", class = "b", coef = "EconAvg"),
                              set_prior("normal(0, 2)", class = "b", coef = "MedSkepAvg"),
                              set_prior("normal(0, 2)", class = "b", coef = "PolSkepAvg"),
                              set_prior("normal(0, 2)", class = "b", coef = "CovidVaxAvg"),
                              set_prior("normal(1,2)", class= "sd")))

#firstwave_m6_loo <- loo(firstwave_m6)
#saveRDS(firstwave_m6_loo, file = 'firstwave/Models/firstwave_m6_loo.rds')
firstwave_m6_loo <- readRDS(file = 'firstwave/Models/firstwave_m6_loo.rds')


#m7 big 3 + pol (top 3 predictors of skepticism)
firstwave_m7 <- brm(SeverityResponse ~ PolSkepAvg  + EconAvg + CovidVaxAvg + SocialPolitics + (1 |ResponseId) + (1 | SeverityItem), 
                    data = firstwave,
                    family="cumulative", 
                    file = 'firstwave/Models/firstwave_m7',
                    chains=4, 
                    iter=3000, 
                    warmup=1500,
                    control = list(adapt_delta = .90),
                    prior = c(set_prior("normal(-3.05, .25)", class = "Intercept", coef = "1"),
                              set_prior("normal(-1.97, .25)", class = "Intercept", coef = "2"),
                              set_prior("normal(-1.39, .25)", class = "Intercept", coef = "3"),
                              set_prior("normal(-1.02, .25)", class = "Intercept", coef = "4"),
                              set_prior("normal(-.38, .25)", class = "Intercept", coef = "5"),
                              set_prior("normal(.62, .25)", class = "Intercept", coef = "6"),
                              set_prior("normal(0, 2)", class = "b", coef = "EconAvg"),
                              set_prior("normal(0, 2)", class = "b", coef = "PolSkepAvg"),
                              set_prior("normal(0, 2)", class = "b", coef = "CovidVaxAvg"),
                              set_prior("normal(0, 2)", class = "b", coef = "SocialPolitics"),
                              set_prior("normal(1,2)", class= "sd")))

#firstwave_m7_loo <- loo(firstwave_m7)
#saveRDS(firstwave_m7_loo, file = 'firstwave/Models/firstwave_m7_loo.rds')
firstwave_m7_loo <- readRDS(file = 'firstwave/Models/firstwave_m7_loo.rds')

#m8 big 3 - pol (top 3 predictors of skepticism)
firstwave_m8 <- brm(SeverityResponse ~  PolSkepAvg  + EconAvg + CovidVaxAvg  + (1 |ResponseId) + (1 | SeverityItem), 
                    data = firstwave,
                    family="cumulative", 
                    file = 'firstwave/Models/firstwave_m8',
                    chains=4, 
                    iter=3000, 
                    warmup=1500,
                    control = list(adapt_delta = .90),
                    prior = c(set_prior("normal(-3.05, .25)", class = "Intercept", coef = "1"),
                              set_prior("normal(-1.97, .25)", class = "Intercept", coef = "2"),
                              set_prior("normal(-1.39, .25)", class = "Intercept", coef = "3"),
                              set_prior("normal(-1.02, .25)", class = "Intercept", coef = "4"),
                              set_prior("normal(-.38, .25)", class = "Intercept", coef = "5"),
                              set_prior("normal(.62, .25)", class = "Intercept", coef = "6"),
                              set_prior("normal(0, 2)", class = "b", coef = "EconAvg"),
                              set_prior("normal(0, 2)", class = "b", coef = "PolSkepAvg"),
                              set_prior("normal(0, 2)", class = "b", coef = "CovidVaxAvg"),
                              set_prior("normal(1,2)", class= "sd")))

#firstwave_m8_loo <- loo(firstwave_m8)
#saveRDS(firstwave_m8_loo, file = 'firstwave/Models/firstwave_m8_loo.rds')
firstwave_m8_loo <- readRDS(file = 'firstwave/Models/firstwave_m8_loo.rds')


#m9 big 2 + pol (no medical science distrust in model)
firstwave_m9 <- brm(SeverityResponse ~ PolSkepAvg + EconAvg + SocialPolitics + (1 |ResponseId) + (1 | SeverityItem), 
                    data = firstwave,
                    family="cumulative", 
                    file = 'firstwave/Models/firstwave_m9',
                    chains=4, 
                    iter=3000, 
                    warmup=1500,
                    control = list(adapt_delta = .90),
                    prior = c(set_prior("normal(-3.05, .25)", class = "Intercept", coef = "1"),
                              set_prior("normal(-1.97, .25)", class = "Intercept", coef = "2"),
                              set_prior("normal(-1.39, .25)", class = "Intercept", coef = "3"),
                              set_prior("normal(-1.02, .25)", class = "Intercept", coef = "4"),
                              set_prior("normal(-.38, .25)", class = "Intercept", coef = "5"),
                              set_prior("normal(.62, .25)", class = "Intercept", coef = "6"),
                              set_prior("normal(0, 2)", class = "b", coef = "EconAvg"),
                              set_prior("normal(0, 2)", class = "b", coef = "PolSkepAvg"),
                              set_prior("normal(0, 2)", class = "b", coef = "SocialPolitics"),
                              set_prior("normal(1,2)", class= "sd")))

#firstwave_m9_loo <- loo(firstwave_m9)
#saveRDS(firstwave_m9_loo, file = 'firstwave/Models/firstwave_m9_loo.rds')
firstwave_m9_loo <- readRDS(file = 'firstwave/Models/firstwave_m9_loo.rds') 



#m10 big 2 - pol (no medical science distrust in model)
firstwave_m10 <- brm(SeverityResponse ~ PolSkepAvg  + EconAvg  + (1 |ResponseId) + (1 | SeverityItem), 
                    data = firstwave,
                    family="cumulative", 
                    file = 'firstwave/Models/firstwave_m10',
                    chains=4, 
                    iter=3000, 
                    warmup=1500,
                    control = list(adapt_delta = .90),
                    prior = c(set_prior("normal(-3.05, .25)", class = "Intercept", coef = "1"),
                              set_prior("normal(-1.97, .25)", class = "Intercept", coef = "2"),
                              set_prior("normal(-1.39, .25)", class = "Intercept", coef = "3"),
                              set_prior("normal(-1.02, .25)", class = "Intercept", coef = "4"),
                              set_prior("normal(-.38, .25)", class = "Intercept", coef = "5"),
                              set_prior("normal(.62, .25)", class = "Intercept", coef = "6"),
                              set_prior("normal(0, 2)", class = "b", coef = "EconAvg"),
                              set_prior("normal(0, 2)", class = "b", coef = "PolSkepAvg"),
                              set_prior("normal(1,2)", class= "sd")))

#firstwave_m10_loo <- loo(firstwave_m10)
#saveRDS(firstwave_m10_loo, file = 'firstwave/Models/firstwave_m10_loo.rds')
firstwave_m10_loo <- readRDS(file = 'firstwave/Models/firstwave_m10_loo.rds')

#m11 Dem + Covid-19 Vax Fears + pol (2 ontologies top predictor)
firstwave_m11 <- brm(SeverityResponse ~ PolSkepAvg  + CovidVaxAvg + SocialPolitics + (1 |ResponseId) + (1 | SeverityItem), 
                    data = firstwave,
                    family="cumulative", 
                    file = 'firstwave/Models/firstwave_m11',
                    chains=4, 
                    iter=3000, 
                    warmup=1500,
                    control = list(adapt_delta = .90),
                    prior = c(set_prior("normal(-3.05, .25)", class = "Intercept", coef = "1"),
                              set_prior("normal(-1.97, .25)", class = "Intercept", coef = "2"),
                              set_prior("normal(-1.39, .25)", class = "Intercept", coef = "3"),
                              set_prior("normal(-1.02, .25)", class = "Intercept", coef = "4"),
                              set_prior("normal(-.38, .25)", class = "Intercept", coef = "5"),
                              set_prior("normal(.62, .25)", class = "Intercept", coef = "6"),
                              set_prior("normal(0, 2)", class = "b", coef = "PolSkepAvg"),
                              set_prior("normal(0, 2)", class = "b", coef = "CovidVaxAvg"),
                              set_prior("normal(0, 2)", class = "b", coef = "SocialPolitics"),
                              set_prior("normal(1,2)", class= "sd")))

#firstwave_m11_loo <- loo(firstwave_m11)
#saveRDS(firstwave_m11_loo, file = 'firstwave/Models/firstwave_m11_loo.rds')
firstwave_m11_loo <- readRDS(file = 'firstwave/Models/firstwave_m11_loo.rds')


#m12 Dem + COVID-19 Vax Fears - pol (2 ontologies top predictor)
firstwave_m12 <- brm(SeverityResponse ~  PolSkepAvg + CovidVaxAvg  + (1 |ResponseId) + (1 | SeverityItem), 
                    data = firstwave,
                    family="cumulative", 
                    file = 'firstwave/Models/firstwave_m12',
                    chains=4, 
                    iter=3000, 
                    warmup=1500,
                    control = list(adapt_delta = .90),
                    prior = c(set_prior("normal(-3.05, .25)", class = "Intercept", coef = "1"),
                              set_prior("normal(-1.97, .25)", class = "Intercept", coef = "2"),
                              set_prior("normal(-1.39, .25)", class = "Intercept", coef = "3"),
                              set_prior("normal(-1.02, .25)", class = "Intercept", coef = "4"),
                              set_prior("normal(-.38, .25)", class = "Intercept", coef = "5"),
                              set_prior("normal(.62, .25)", class = "Intercept", coef = "6"),
                              set_prior("normal(0, 2)", class = "b", coef = "PolSkepAvg"),
                              set_prior("normal(0, 2)", class = "b", coef = "CovidVaxAvg"),
                              set_prior("normal(1,2)", class= "sd")))

#firstwave_m12_loo <- loo(firstwave_m12)
#saveRDS(firstwave_m12_loo, file = 'firstwave/Models/firstwave_m12_loo.rds')
firstwave_m12_loo <- readRDS(file = 'firstwave/Models/firstwave_m12_loo.rds')


#m13 Dem hoax + pol (Biggest predictor)
firstwave_m13 <- brm(SeverityResponse ~ PolSkepAvg + SocialPolitics + (1 |ResponseId) + (1 | SeverityItem), 
                     data = firstwave,
                     family="cumulative", 
                     file = 'firstwave/Models/firstwave_m13',
                     chains=4, 
                     iter=3000, 
                     warmup=1500,
                     control = list(adapt_delta = .90),
                     prior = c(set_prior("normal(-3.05, .25)", class = "Intercept", coef = "1"),
                               set_prior("normal(-1.97, .25)", class = "Intercept", coef = "2"),
                               set_prior("normal(-1.39, .25)", class = "Intercept", coef = "3"),
                               set_prior("normal(-1.02, .25)", class = "Intercept", coef = "4"),
                               set_prior("normal(-.38, .25)", class = "Intercept", coef = "5"),
                               set_prior("normal(.62, .25)", class = "Intercept", coef = "6"),
                               set_prior("normal(0, 2)", class = "b", coef = "PolSkepAvg"),
                               set_prior("normal(0, 2)", class = "b", coef = "SocialPolitics"),
                               set_prior("normal(1,2)", class= "sd")))

#firstwave_m13_loo <- loo(firstwave_m13)
#saveRDS(firstwave_m13_loo, file = 'firstwave/Models/firstwave_m13_loo.rds')
firstwave_m13_loo <- readRDS(file = 'firstwave/Models/firstwave_m13_loo.rds')

#m14 Dem hoax - pol (Biggest predictor)
firstwave_m14 <- brm(SeverityResponse ~  PolSkepAvg + (1 |ResponseId) + (1 | SeverityItem), 
                     data = firstwave,
                     family="cumulative", 
                     file = 'firstwave/Models/firstwave_m14',
                     chains=4, 
                     iter=3000, 
                     warmup=1500,
                     control = list(adapt_delta = .90),
                     prior = c(set_prior("normal(-3.05, .25)", class = "Intercept", coef = "1"),
                               set_prior("normal(-1.97, .25)", class = "Intercept", coef = "2"),
                               set_prior("normal(-1.39, .25)", class = "Intercept", coef = "3"),
                               set_prior("normal(-1.02, .25)", class = "Intercept", coef = "4"),
                               set_prior("normal(-.38, .25)", class = "Intercept", coef = "5"),
                               set_prior("normal(.62, .25)", class = "Intercept", coef = "6"),
                               set_prior("normal(0, 2)", class = "b", coef = "PolSkepAvg"),
                               set_prior("normal(1,2)", class= "sd")))

#firstwave_m14_loo <- loo(firstwave_m14)
#saveRDS(firstwave_m14_loo, file = 'firstwave/Models/firstwave_m14_loo.rds')
firstwave_m14_loo <- readRDS(file = 'firstwave/Models/firstwave_m14_loo.rds')


#m15 politics only
firstwave_m15 <- brm(SeverityResponse ~ SocialPolitics + (1 |ResponseId) + (1 | SeverityItem), 
                     data = firstwave,
                     family="cumulative", 
                     file = 'firstwave/Models/firstwave_m15',
                     chains=4, 
                     iter=3000, 
                     warmup=1500,
                     control = list(adapt_delta = .90),
                     prior = c(set_prior("normal(-3.05, .25)", class = "Intercept", coef = "1"),
                               set_prior("normal(-1.97, .25)", class = "Intercept", coef = "2"),
                               set_prior("normal(-1.39, .25)", class = "Intercept", coef = "3"),
                               set_prior("normal(-1.02, .25)", class = "Intercept", coef = "4"),
                               set_prior("normal(-.38, .25)", class = "Intercept", coef = "5"),
                               set_prior("normal(.62, .25)", class = "Intercept", coef = "6"),
                               set_prior("normal(0, 2)", class = "b", coef = "SocialPolitics"),
                               set_prior("normal(1,2)", class= "sd")))

#firstwave_m15_loo <- loo(firstwave_m15)
#saveRDS(firstwave_m15_loo, file = 'firstwave/Models/firstwave_m15_loo.rds')
firstwave_m15_loo <- readRDS(file = 'firstwave/Models/firstwave_m15_loo.rds')



firstwave_loo_compare <- loo_compare(firstwave_full_loo,
                                   firstwave_m2_loo,
                                   firstwave_m3_loo,
                                   firstwave_m4_loo,
                                   firstwave_m5_loo,
                                   firstwave_m6_loo,
                                   firstwave_m7_loo,
                                   firstwave_m8_loo,
                                   firstwave_m9_loo,
                                   firstwave_m10_loo,
                                   firstwave_m11_loo,
                                   firstwave_m12_loo,
                                   firstwave_m13_loo,
                                   firstwave_m14_loo,
                                   firstwave_m15_loo)

#prewave_loo_compare <- as.array(prewave_loo_compare)
#prewave_loo_compare
#prewave_loo_compare$model_name <- rownames(prewave_loo_compare)

firstwave_loo_compare <- data.frame(model_name = row.names(firstwave_loo_compare), firstwave_loo_compare)
firstwave_loo_compare$elpd_diff <- abs(firstwave_loo_compare$elpd_diff)

###
### Visualize loo model comparison
###

fw_model_order <- rev(c('firstwave_full', 
                        'firstwave_m3',
                     'firstwave_m2', 'firstwave_m4', 'firstwave_m6', 'firstwave_m5',
                     'firstwave_m8', 'firstwave_m7', 'firstwave_m12', 'firstwave_m11', 'firstwave_m10',
                     "firstwave_m9", "firstwave_m13", "firstwave_m14", "firstwave_m15"))

#full (ref)
#m2 full - politics
#m3 big 5 + pol (includes origin, positive effect on severity)
#m4 big 5 - pol (includes origin, positive effect on severity)
#m5 big 4 + pol (only factors that predict skepticism)
#m6 big 4 - pol (only factors that predict skepticism)
#m7 big 3 + pol (top 3 predictors of skepticism)
#m8 big 3 - pol (top 3 predictors of skepticism)
#m9 big 2 + pol (no medical science distrust in model)
#m10 big 2 - pol (no medical science distrust in model)
#m11 Dem + Covid-19 Vax Fears + pol (2 ontologies top predictor)
#m12 Dem + COVID-19 Vax Fears - pol (2 ontologies top predictor)
#m13 Dem hoax + pol (Biggest predictor)
#m14 Dem hoax - pol (Biggest predictor)
#m15 politics only

fw_model_names <- rev(c('Full Model', 
                        'Big 5 + Politics',
                        'Full Model - Politics', 
                     'Big 5 - Politics', 
                     'Big 4 - Politics', 
                     'Big 4 + Politics',
                     'Big 3 - Politics', 
                     'Big 3 + Politics', 
                     'Dem + COVID-19 Vax Fears - pol',
                     'Dem + COVID-19 Vax Fears + pol',
                     'Big 2 - pol',
                     'Big 2 + pol',
                     'Dem Distrust + pol',
                     'Dem Distrust - pol',
                     'Politics Only'))



firstwave_loo_plot <- ggplot(data = firstwave_loo_compare, aes(x = factor(model_name, level = fw_model_order), y= elpd_diff))+
  geom_point()+
  geom_errorbar(aes(ymin = elpd_diff - se_diff, ymax = elpd_diff + se_diff, width = .0))+
  coord_flip()+
  scale_x_discrete(name = "Model", 
                   labels = fw_model_names)+
  scale_y_continuous(name = "loo Cross-Validation Difference", 
                     breaks = c(0, 50, 100, 150, 200, 250, 300),
                     labels = c(0, 50, 100, 150, 200, 250, 300))+
  theme_minimal()+
  theme(
    axis.text.y = element_text(size=10,face="bold"),
    axis.text.x = element_text(size=12,face="bold"),
    axis.title.x = element_text(size=14,face="bold"),
    axis.title.y = element_text(size=14,face="bold"))


firstwave_loo_plot

####
####Build partial correlation
####
firstwave_pc_df <- firstwave %>%
  drop_na()%>%
  dplyr::select(c(MedSkepAvg, ForeignAvg, OriginAvg, PolSkepAvg, CovidVaxAvg, VaxSkepAvg, EconAvg, SocialPolitics))%>%
  rename(`Medical Distrust` = MedSkepAvg)%>%
  rename(`Foreign Threat` = ForeignAvg)%>%
  rename(`Covid-19 Origin` = OriginAvg)%>%
  rename(`Democrat Hoax` = PolSkepAvg)%>%
  rename(`Covid-19 Vaccine Fears` = CovidVaxAvg)%>%
  rename(`General Vaccine Skepticism` = VaxSkepAvg)%>%
  rename(`Open up the Economy` = EconAvg)%>%
  rename(`Conservative Politics` = SocialPolitics)

ggcorrplot(pcor(firstwave_pc_df)$estimate, 
           type = 'upper', 
           outline = 'white', 
           lab = TRUE, 
           #insig = 'blank',
           #p.mat = pcor(firstwave_pc_df)$p.value,
           show.legend = FALSE, 
           colors = c('#0077ff','white','#F50000' ))


















