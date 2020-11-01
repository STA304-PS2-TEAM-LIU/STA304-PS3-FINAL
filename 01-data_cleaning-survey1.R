#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from Survey data: Tausanovitch, Chris and Lynn Vavreck. 2020. 
#Democracy Fund + UCLA Nationscape, October 10-17, 2019 (version 20200814).Retrieved from https://www.voterstudygroup.org/downloads?key=21bf654b-4376-40e2-83c8-def0421aab5d.
# Author:  Jiahan Deng, Zhaonan Liu, Shijie Min, Tianyi Zhang
# Data: 22 October 2020
# Contact: jiahan.deng@mai.utoronto.ca, zhaonan.liu@mail.utoronto.ca, tiantianyi.zhang@mail.utoronto.ca,shijiequinn.min@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from https://www.voterstudygroup.org/downloads?key=21bf654b-4376-40e2-83c8-def0421aab5d and save the fold
# interested in to data .
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
setwd("C:/Users/Janet/Downloads/304ps3data/ns20200625")
# Read in the raw data (You might need to change this if you use a different dataset)
raw_data <- read_dta("ns20200625.dta")
# Add the labels
raw_data <- labelled::to_factor(raw_data)
# Just keep some variables
reduced_data <- 
  raw_data %>% 
  select(vote_2020,
          vote_intention,
          registration,
          age,
          gender,
          education,
          state,
          household_income,
          race_ethnicity)

##cleanning data
#vote_2020
unique(reduced_data$vote_2020)
reduced_data<-reduced_data %>% 
  filter(vote_2020=="Donald Trump"|vote_2020=="Joe Biden")
unique(reduced_data$vote_2020)
reduced_data$vote_2020 = factor(ifelse(reduced_data$vote_2020 == "Donald Trump", 1, 0))

#vote_intention
unique(reduced_data$vote_intention)
reduced_data<-reduced_data %>% 
  filter(vote_intention=="Yes, I will vote")
unique(reduced_data$vote_intention)

#registration
unique(reduced_data$registration)
reduced_data<-reduced_data %>% 
  filter(registration=="Registered")
unique(reduced_data$registration)

#age
reduced_data$age<-as.numeric(reduced_data$age)
unique(reduced_data$age)
reduced_data$age = ifelse(reduced_data$age <=20,"<20",
                          ifelse(reduced_data$age>20 & reduced_data$age<=40,"21-40",
                                 ifelse(reduced_data$age>40 & reduced_data$age<=60,"41-60",
                                        ifelse(reduced_data$age>60 & reduced_data$age<=80,"61-80",">80"))))
reduced_data$age <- as.factor(reduced_data$age)
summary(reduced_data$age)
length(unique(reduced_data$age))
#5


#gender
unique(reduced_data$gender)#no need to clean
reduced_data$gender <- as.factor(reduced_data$gender)
summary(reduced_data$gender)
length(unique(reduced_data$gender))#2


#education
unique(reduced_data$education)

reduced_data$education = ifelse(reduced_data$education=="Middle School - Grades 4 - 8" | reduced_data$education=="3rd Grade or less" | reduced_data$education=="Completed some high school","Middel School or less",
                          ifelse(reduced_data$education=="High school graduate" | reduced_data$education=="Completed some college, but no degree","High School",
                                 ifelse(reduced_data$education=="Doctorate degree"|reduced_data$education=="Masters degree","5+ in collage","4 or less in collage")))
reduced_data$education <- as.factor(reduced_data$education)
summary(reduced_data$education)
length(unique(reduced_data$education))
#4

#state
unique(reduced_data$state)#no need to clean
reduced_data$state <- as.factor(reduced_data$state)
summary(reduced_data$state)
length(unique(reduced_data$state))#51

#household_income
unique(reduced_data$household_income)

reduced_data$household_income = ifelse(reduced_data$household_income=='Less than $14,999','<20000',
                                       ifelse(reduced_data$household_income=='$15,000 to $19,999'|reduced_data$household_income=='$20,000 to $24,999'|reduced_data$household_income=='$25,000 to $29,999'|reduced_data$household_income=='$30,000 to $34,999'|reduced_data$household_income=='$35,000 to $39,999','20000-40000',
                                              ifelse(reduced_data$household_income=='$40,000 to $44,999'|reduced_data$household_income=='$45,000 to $49,999'|reduced_data$household_income=='$50,000 to $54,999'|reduced_data$household_income=='$55,000 to $59,999','40000-60000',
                                                     ifelse(reduced_data$household_income=='$60,000 to $64,999'|reduced_data$household_income=='$65,000 to $69,999'|reduced_data$household_income=='$70,000 to $74,999'|reduced_data$household_income=='$75,000 to $79,999','60000-80000',
                                                                   ifelse(reduced_data$household_income=='$80,000 to $84,999'|reduced_data$household_income=='$85,000 to $89,999'|reduced_data$household_income=='$90,000 to $94,999'|reduced_data$household_income=='$95,000 to $99,999','80000-100000',
                                                                          ifelse(reduced_data$household_income=='$125,000 to $149,999'|reduced_data$household_income=='$100,000 to $124,999','100000-150000',
                                                                                 ifelse(reduced_data$household_income=='$200,000 to $249,999','200000-250000',
                                                                                        ifelse(reduced_data$household_income=='$250,000 and above','>250000','150000-200000'))))))))
                                                                                 
unique(reduced_data$household_income)
reduced_data<-reduced_data %>% 
  filter(household_income!="NA")
reduced_data$household_income <- as.factor(reduced_data$household_income)
summary(reduced_data$household_income)

length(unique(reduced_data$household_income))#9

#race_ethnicity
unique(reduced_data$race_ethnicity)
reduced_data$race_ethnicity = ifelse(reduced_data$race_ethnicity=="Asian (Vietnamese)" | reduced_data$race_ethnicity=="Asian (Korean)" | reduced_data$race_ethnicity=="Asian (Other)"| reduced_data$race_ethnicity=="Asian (Filipino)"|reduced_data$race_ethnicity=="Asian (Asian Indian)"|reduced_data$race_ethnicity=="Pacific Islander (Other)"|reduced_data$race_ethnicity=="Pacific Islander (Native Hawaiian)"|reduced_data$race_ethnicity=="Pacific Islander (Samoan)" |reduced_data$race_ethnicity=="Pacific Islander (Guamanian)","Other Asian or Pacific Island",
                                ifelse(reduced_data$race_ethnicity=="American Indian or Alaska Native","American Indian or Alaska Native",
                                       ifelse(reduced_data$race_ethnicity=="Black, or African American","Black/African American/Negro",
                                              ifelse(reduced_data$race_ethnicity=="Asian (Chinese)","Chinese",
                                                     ifelse(reduced_data$race_ethnicity=="Asian (Japanese)","Japanese",
                                              ifelse(reduced_data$race_ethnicity=="White","White","Some other race"))))))
unique(reduced_data$race_ethnicity)

reduced_data$race_ethnicity <- as.factor(reduced_data$race_ethnicity)
summary(reduced_data$race_ethnicity)
length(unique(reduced_data$race_ethnicity))#7


#Drop NAs
reduced_data<-na.omit(reduced_data)


#rename
library(janitor)
reduced_data <- reduced_data %>% 
  clean_names() %>% 
  rename(Age_group=age, 
         Sex=gender,
         State=state,
         HHIncome=household_income, 
         Race=race_ethnicity,
         Edu=education)

#select variables
reduced_data <- 
  reduced_data %>% 
  select(vote_2020,
         Age_group, 
         Sex,
         State,
         HHIncome,
         Race,
         Edu)





summary(reduced_data$Age_group)
length(unique(reduced_data$Age_group))#5

summary(reduced_data$Sex)
length(unique(reduced_data$Sex))#2

summary(reduced_data$State)
length(unique(reduced_data$State))#51

summary(reduced_data$HHIncome)
length(unique(reduced_data$HHIncome))#9

summary(reduced_data$Race)
length(unique(reduced_data$Race))#7

summary(reduced_data$Edu)
length(unique(reduced_data$Edu))#4

# Saving the survey/sample data as a csv file in my
# working directory
write_csv(reduced_data, "survey_data.csv")


rm(raw_data,reduced_data)

