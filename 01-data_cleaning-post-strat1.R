#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from Census data: Team, M. (n.d.). U.S. CENSUS DATA FOR SOCIAL, ECONOMIC, AND HEALTH RESEARCH. Retrieved October 31, 2020, from https://usa.ipums.org/usa/index.shtml
# Author:  Jiahan Deng, Zhaonan Liu, Shijie Min, Tianyi Zhang
# Data: 22 October 2020
# Contact: jiahan.deng@mai.utoronto.ca, zhaonan.liu@mail.utoronto.ca, tiantianyi.zhang@mail.utoronto.ca,shijiequinn.min@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
setwd("C:/Users/Janet/Downloads/censa data")
reduced_data_cen <- read_dta("usa_00001.dta")

# Add the labels
reduced_data_cen <- labelled::to_factor(reduced_data_cen)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data_cen <- 
  reduced_data_cen %>% 
  select(perwt,
         stateicp,
         hhincome,
         race,
         educ,
         sex, 
         age)
         

##clean data
reduced_data_cen$age<-as.numeric(reduced_data_cen$age)
reduced_data_cen<-reduced_data_cen %>% filter(age>=18)
#special NA
reduced_data_cen<-reduced_data_cen %>% 
  filter(hhincome!=9999999)


#stateicp
unique(reduced_data_cen$stateicp)
reduced_data_cen<-reduced_data_cen %>% 
  mutate(state = case_when(stateicp=="alabama"~"AL",
                           stateicp=="alaska"~"AK",
                           stateicp=="arizona"~"AZ",
                           stateicp=="arkansas"~"AR",
                           stateicp=="california"~"CA",
                           stateicp=="colorado"~"CO",
                           stateicp=="connecticut"~"CT",
                           stateicp=="delaware"~"DE",
                           stateicp=="florida"~"FL",
                           stateicp=="georgia"~"GA",
                           stateicp=="hawaii"~"HI",
                           stateicp=="idaho"~"ID",
                           stateicp=="illinois"~"IL",
                           stateicp=="indiana"~"IN",
                           stateicp=="iowa"~"IA",
                           stateicp=="kansas"~"KS",
                           stateicp=="kentucky"~"KY",
                           stateicp=="louisiana"~"LA",
                           stateicp=="maine"~"ME",
                           stateicp=="maryland"~"MD",
                           stateicp=="massachusetts"~"MA",
                           stateicp=="michigan"~"MI",
                           stateicp=="minnesota"~"MN",
                           stateicp=="mississippi"~"MS",
                           stateicp=="missouri"~"MO",
                           stateicp=="montana"~"MT",
                           stateicp=="nebraska"~"NE",
                           stateicp=="nevada"~"NV",
                           stateicp=="new hampshire"~"NH",
                           stateicp=="new jersey"~"NJ",
                           stateicp=="new mexico"~"NM",
                           stateicp=="new york"~"NY",
                           stateicp=="north carolina"~"NC",
                           stateicp=="north dakota"~"ND",
                           stateicp=="ohio"~"OH",
                           stateicp=="oklahoma"~"OK",
                           stateicp=="oregon"~"OR",
                           stateicp=="pennsylvania"~"PA",
                           stateicp=="rhode island"~"RI",
                           stateicp=="south carolina"~"SC",
                           stateicp=="south dakota"~"SD",
                           stateicp=="tennessee"~"TN",
                           stateicp=="texas"~"TX",
                           stateicp=="utah"~"UT",
                           stateicp=="vermont"~"VT",
                           stateicp=="virginia"~"VA",
                           stateicp=="washington"~"WA",
                           stateicp=="west virginia"~"WV",
                           stateicp=="wisconsin"~"WI",
                           stateicp=="wyoming"~"WY",
                           stateicp=="district of columbia"~"DC"))

reduced_data_cen<- reduced_data_cen%>%select(-stateicp)
reduced_data_cen$state <- as.factor(reduced_data_cen$state)
summary(reduced_data_cen$state)
length(unique(reduced_data_cen$state))#51



#hhincome
reduced_data_cen$hhincome<-as.numeric(reduced_data_cen$hhincome)
max(reduced_data_cen$hhincome)
min(reduced_data_cen$hhincome)
reduced_data_cen<-reduced_data_cen %>% filter(hhincome>=0)
reduced_data_cen<-reduced_data_cen %>% 
  mutate(income = ifelse(reduced_data_cen$hhincome <20000,"<20000",
                          ifelse(reduced_data_cen$hhincome>=20000 & reduced_data_cen$hhincome<40000,"20000-40000",
                                 ifelse(reduced_data_cen$hhincome>=40000 & reduced_data_cen$hhincome<60000,"40000-60000",
                                        ifelse(reduced_data_cen$hhincome>=60000 & reduced_data_cen$hhincome<80000,"60000-80000",
                                               ifelse(reduced_data_cen$hhincome>=80000 & reduced_data_cen$hhincome<100000,"80000-100000",
                                                      ifelse(reduced_data_cen$hhincome>=100000 & reduced_data_cen$hhincome<150000,"100000-150000",
                                                             ifelse(reduced_data_cen$hhincome>=150000 & reduced_data_cen$hhincome<200000,"150000-200000",
                                                                    ifelse(reduced_data_cen$hhincome>=200000 & reduced_data_cen$hhincome<250000,"200000-250000",">250000")))))))))




reduced_data_cen$income <- as.factor(reduced_data_cen$income)
summary(reduced_data_cen$income)
length(unique(reduced_data_cen$income))#9
reduced_data_cen<- reduced_data_cen%>%select(-hhincome)
#race
unique(reduced_data_cen$race)
reduced_data_cen$race = ifelse(reduced_data_cen$race=="other asian or pacific islander","Other Asian or Pacific Island",
                                     ifelse(reduced_data_cen$race=="american indian or alaska native","American Indian or Alaska Native",
                                            ifelse(reduced_data_cen$race=="black/african american/negro","Black/African American/Negro",
                                                   ifelse(reduced_data_cen$race=="chinese","Chinese",
                                                          ifelse(reduced_data_cen$race=="japanese","Japanese",
                                                                 ifelse(reduced_data_cen$race=="white","White","Some other race"))))))

reduced_data_cen$race <- as.factor(reduced_data_cen$race)
summary(reduced_data_cen$race)
length(unique(reduced_data_cen$race))#7

#educ

unique(reduced_data_cen$educ)

reduced_data_cen$educ = ifelse(reduced_data_cen$educ=="grade 5, 6, 7, or 8" | reduced_data_cen$educ=="n/a or no schooling" | reduced_data_cen$educ=="grade 11"| reduced_data_cen$educ=="grade 10"| reduced_data_cen$educ=="grade 9","Middel School or less",
                                ifelse(reduced_data_cen$educ=="grade 12" | reduced_data_cen$educ=="1 year of college","High School",
                                       ifelse(reduced_data_cen$educ=="5+ years of college","5+ in collage","4 or less in collage")))


reduced_data_cen$educ <- as.factor(reduced_data_cen$educ)
summary(reduced_data_cen$educ)
length(unique(reduced_data_cen$educ))#4
#sex
unique(reduced_data_cen$sex)
reduced_data_cen$sex = ifelse(reduced_data_cen$sex=="female","Female","Male")
reduced_data_cen$sex <- as.factor(reduced_data_cen$sex)
summary(reduced_data_cen$sex)
length(unique(reduced_data_cen$sex))#2
#age
unique(reduced_data_cen$age)
reduced_data_cen$age<-as.numeric(reduced_data_cen$age)
reduced_data_cen$age = ifelse(reduced_data_cen$age <=20,"<20",
                          ifelse(reduced_data_cen$age>20 & reduced_data_cen$age<=40,"21-40",
                                 ifelse(reduced_data_cen$age>40 & reduced_data_cen$age<=60,"41-60",
                                        ifelse(reduced_data_cen$age>60 & reduced_data_cen$age<=80,"61-80",">80"))))

reduced_data_cen$age <- as.factor(reduced_data_cen$age)
summary(reduced_data_cen$age)
length(unique(reduced_data_cen$age))#5



#Drop NAs
reduced_data_cen<-na.omit(reduced_data_cen)


#rename
library(janitor)
reduced_data_cen <- reduced_data_cen %>% 
  clean_names() %>% 
  rename(Age_group=age, 
         Sex=sex,
         State=state,
         HHIncome=income,
         Race=race,
         Edu=educ)



reduced_data_cen$HHIncome <- as.factor(reduced_data_cen$HHIncome)
summary(reduced_data_cen$HHIncome)
length(unique(reduced_data_cen$HHIncome))


# Saving the census data as a csv file in my
# working directory
write_csv(reduced_data_cen, "census_data.csv")
         