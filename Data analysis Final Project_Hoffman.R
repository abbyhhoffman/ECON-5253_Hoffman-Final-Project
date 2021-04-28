
#Open Libraries
library(ipumsr)
library(censusapi)
library(tidycensus)
library(stargazer)
library(magrittr)
library(tidyverse) 
library(skimr) 
library(broom) 
library(car)
library(estimatr)
#Load Data from ACS

ddi <- read_ipums_ddi("usa_00007.xml")
data <- read_ipums_micro(ddi)



#Filter Data 

data1 <- data %>% select(-RACED,-EDUC,-EMPSTATD,-SAMPLE,-PWSTATE2,-PWMETRO,-STATEFIP)

data1 %<>% filter(EDUCD<=116)
data1 %<>% filter(EDUCD>=002)
data1 %<>% filter(EDUCD>=01)
data1 %<>% filter(INCWAGE>=1)
data1 %<>% filter(INCTOT>=15000)
data1 %<>% filter(INCTOT<=300000)
data1 %<>% filter(EDUCD<999)

##Mutate Data 
####RACE Variable 
#White = 1
#All others 2

data1 %<>% mutate(RACENW = case_when(
  RACE==1 ~ 1,
  RACE==2 ~ 2,
  RACE==3 ~ 2,
  RACE==4 ~ 2,
  RACE==5 ~ 2,
  RACE==6 ~ 2,
  RACE==7 ~ 2,
  RACE==8 ~ 2,
  RACE==9 ~ 2,))

##Transform Variables to Binary variables

#Race
#0 if White 1 if Non-white
data1 %<>% mutate(RACENW = case_when(
  RACENW==1 ~ 0,
  RACENW==2 ~ 1))

#Mutate gender 
# 0 if Male and 1 if Female 
data1 %<>% mutate(SEX = case_when(
  SEX==1 ~ 0,
  SEX==2 ~ 1))

#Mutate Schooling Years
#create uniformity in schooling 
data1 %<>% mutate(EDUCYRS = case_when(
  EDUCD==999 ~ NA,
  EDUCD==000 ~ NA,
  EDUCD==001 ~ NA,
  EDUCD==011 ~ NA,
  EDUCD==012 ~ NA))

data1 %<>% mutate(EDUCYRS = case_when(
EDUCD==013 ~ 2.5,
EDUCD==020 ~ 6.5,
EDUCD==021 ~ 5.5,
EDUCD==024 ~ 7.5))

data1 %<>% mutate(EDUCYRS = case_when(
  EDUCD==002 ~ 0,
  EDUCD==014 ~ 1,
  EDUCD==015 ~ 2,
  EDUCD==016 ~ 3,
  EDUCD==017 ~ 4,))

data1 %<>% mutate(EDUCYRS = case_when(
  EDUCD==022 ~ 5,
  EDUCD==023 ~ 6,
  EDUCD==025 ~ 7,
  EDUCD==026 ~ 8,
  EDUCD==030 ~ 9,
  EDUCD==040 ~ 10,
  EDUCD==050 ~ 11,
  EDUCD==061 ~ 12,
  EDUCD==062 ~ 12,
  EDUCD==063 ~ 12,
  EDUCD==064 ~ 12))

data1 %<>% mutate(EDUCYRS = case_when(
  EDUCD==065 ~ 12.5,
  EDUCD==070 ~ 13,
  EDUCD==071 ~ 13,
  EDUCD==080 ~ 14,
  EDUCD==081 ~ 14,
  EDUCD==082 ~ 14,
  EDUCD==083 ~ 14,
  EDUCD==090 ~ 15,
  EDUCD==100 ~ 16,))

data1 %<>% mutate(EDUCYRS = case_when(
  EDUCD==101 ~ 16,
  EDUCD==110 ~ 17,
  EDUCD==111 ~ 18,
  EDUCD==114 ~ 18,
  EDUCD==112 ~ 19,
  EDUCD==113 ~ 20,
  EDUCD==114 ~ 20,
  EDUCD==116 ~ 20))

###The case_when function is a bit finicky 
###Run each filter seperately if errors occur



#Drop data that is Not Available in Education Years column
data1 <- data1 %>% drop_na(EDUCYRS)


#Marital Status Mutation 
#1 If Married/Married but separated 
#2 for all other relationship types
data1 %<>% mutate(MARSTD= case_when(
  MARST==1 ~ 1,
  MARST==2 ~ 1,
  MARST==3 ~ 2,
  MARST==4 ~ 2,
  MARST==5 ~ 2,
  MARST==6 ~ 2,))
 
#Binary Variable 
# 0 if Married and 1 if Single 
data1 %<>% mutate(MARSTD = case_when(
  MARSTD==1 ~ 0,
  MARSTD==2 ~ 1))


#Mutate Metro Status
data1 %<>% mutate(METROSTATUS = case_when(
  METRO==0 ~ NA))

data1 %<>% mutate(METROSTATUS = case_when(
  METRO==1 ~ 1,
  METRO==2 ~ 2,
  METRO==3 ~ 2,
  METRO==4 ~ 2))

#0 if in metro 1 if not in metro 
data1 %<>% mutate(METROSTATUS = case_when(
  METROSTATUS==1 ~ 0,
  METROSTATUS==2 ~ 1))

#Drop Not Available data 
data1 <- data1 %>% drop_na(METROSTATUS)



#Mutate Labor force participation
data1 %<>% mutate(LABFORCE = case_when(
  LABFORCE==1 ~ 1,
  LABFORCE==2 ~ 2,))

#0 if NILF;
#1 if in LF
data1 %<>% mutate(LABFORCE = case_when(
  LABFORCE==1 ~ 0,
  LABFORCE==2 ~ 1))

#Drop unncessary variables from data set 
data2<- data1 %>% select(-METRO,-EDUCD,-MARST,-RACE,-EMPSTAT)


##Mutate Wage into a logarithm 
data2 <-  data2 %>% mutate(logincwage = log(INCWAGE))

##Mutate an additional variable of Age squared to see diminishing returns
data2 %<>% mutate(AGE.squared = AGE^2)


#Create State Regions

data2 %<>% mutate(STATEREG= case_when(
  STATEICP==71 ~ 1,
  STATEICP==72 ~ 1,
  STATEICP==73 ~ 1,
  STATEICP==81 ~ 1,
  STATEICP==82 ~ 1,
  STATEICP==49 ~ 3,
  STATEICP==53 ~ 3,
  STATEICP==61 ~ 3,
  STATEICP==66 ~ 3,
  STATEICP==1 ~ 6,
  STATEICP==2 ~ 6,
  STATEICP==3 ~ 6,
  STATEICP==4 ~ 6,
  STATEICP==5 ~ 6,
  STATEICP==6 ~ 6,
  STATEICP==7 ~ 6,
  STATEICP==8 ~ 6,
  STATEICP==9 ~ 6,
  STATEICP==10 ~ 6,
  STATEICP==11 ~ 6,
  STATEICP==12 ~ 6,
  STATEICP==13 ~ 6,
  STATEICP==14 ~ 6,
  STATEICP==52 ~ 6,
  STATEICP==62~ 2,
  STATEICP==63 ~ 2,
  STATEICP==64 ~ 2,
  STATEICP==65 ~ 2,
  STATEICP==67 ~ 2,
  STATEICP==68 ~ 2,
  STATEICP==21 ~ 4,
  STATEICP==22 ~ 4,
  STATEICP==23 ~ 4,
  STATEICP==24~ 4,
  STATEICP==25~ 4,
  STATEICP==26~ 4,
  STATEICP==27 ~ 4,
  STATEICP==28 ~ 4,
  STATEICP==29 ~ 4,
  STATEICP==30 ~ 4,
  STATEICP==31~ 4,
  STATEICP==32~ 4,
  STATEICP==33 ~ 4,
  STATEICP==34 ~ 4,
  STATEICP==35 ~ 4,
  STATEICP==36 ~ 4,
  STATEICP==37 ~ 4,
  STATEICP==52 ~ 4,
  STATEICP==40~ 5,
  STATEICP==41 ~ 5,
  STATEICP==42 ~ 5,
  STATEICP==43 ~ 5,
  STATEICP==44 ~ 5,
  STATEICP==45 ~ 5,
  STATEICP==46 ~ 5,
  STATEICP==47 ~ 5,
  STATEICP==48 ~ 5,
  STATEICP==54 ~ 5,
  STATEICP==56 ~ 5,
  STATEICP==51 ~ 5,
))

#CREATE REGIONAL DATA SETS
#1. Pacific West 
data2 %<>% mutate(PACIFICWEST= case_when(
  STATEREG==1 ~ 1))


data_pacificwest <- data2 %>% drop_na(PACIFICWEST)
#drop unneccessary columns
#data_pacificwest1 <- data_pacificwest %>% select(-NORTHEAST, -ROCKYMTN, -SOUTHWEST, -MIDWEST, -SOUTHEAST)
##

#2. Rocky Mountain 
data2 %<>% mutate(ROCKYMTN= case_when(
  STATEREG==2 ~ 1))
data_rockymtn <- data2 %>% drop_na(ROCKYMTN)
#drop unneccessary columns
data_rockymtn1 <- data_rockymtn %>% select(-PACIFICWEST)

##
#3. Southwest Region
data2 %<>% mutate(SOUTHWEST= case_when(
  STATEREG==3 ~ 1))
data_southwest <- data2 %>% drop_na(SOUTHWEST)
#drop unneccessary columns
data_southwest1 <- data_southwest %>% select(-PACIFICWEST, -ROCKYMTN)

##

#4. Midwest Region 
data2 %<>% mutate(MIDWEST= case_when(
  STATEREG==4 ~ 1))
data_midwest<- data2 %>% drop_na(MIDWEST)
#drop unneccessary columns
data_midwest1 <- data_midwest %>% select(-PACIFICWEST, -ROCKYMTN, -SOUTHWEST)
##


#5. Southeast Region
data2 %<>% mutate(SOUTHEAST = case_when(
  STATEREG==5 ~ 1))
data_southeast <- data2 %>% drop_na(SOUTHEAST)

#drop unneccessary columns
data_southeast1 <- data_southeast %>% select(-PACIFICWEST, -ROCKYMTN, -SOUTHWEST, -MIDWEST)
##

#6. Northeast Region
data2 %<>% mutate(NORTHEAST = case_when(
  STATEREG==6 ~ 1))
data_northeast <- data2 %>% drop_na(NORTHEAST)
#drop unneccessary columns
data_northeast1 <- data_northeast %>% select(-PACIFICWEST, -ROCKYMTN, -SOUTHWEST, -MIDWEST, -SOUTHEAST)


#REGRESSIONS

#1. Pacific West 

est_pacificwest <- lm(logincwage ~ EDUCYRS, data = data_pacificwest)

est_pacificwest2 <- lm(logincwage ~ EDUCYRS + AGE + AGE.squared + SEX + RACENW + LABFORCE + MARSTD + METROSTATUS, data = data_pacificwest)

tidy(est_pacificwest)
tidy(est_pacificwest2)
glance(est_pacificwest)
glance(est_pacificwest2)


#correcting heteroskedasticity 
est_pacificwest3 <- lm_robust(logincwage ~ EDUCYRS, data = data_pacificwest)

est_pacificwest4 <- lm_robust(logincwage ~ EDUCYRS + AGE + AGE.squared + SEX + RACENW + LABFORCE + MARSTD + METROSTATUS, data = data_pacificwest)
tidy(est_pacificwest3)
tidy(est_pacificwest4)

#Multicollinearity check 
vif(est_pacificwest3)
vif(est_pacificwest4)




#
est_rockymtn <- lm(logincwage ~ EDUCYRS, data = data_rockymtn)

est_rockymtn2 <- lm(logincwage ~ EDUCYRS + AGE + AGE.squared + SEX + RACENW + LABFORCE + MARSTD + METROSTATUS, data = data_rockymtn)

tidy(est_rockymtn)
tidy(est_rockymtn2)
glance(est_rockymtn)
glance(est_rockymtn2)
#correcting heteroskedasticity 
est_rockymtn3 <- lm_robust(logincwage ~ EDUCYRS, data = data_rockymtn)

est_rockymtn4 <- lm_robust(logincwage ~ EDUCYRS + AGE + AGE.squared + SEX + RACENW + LABFORCE + MARSTD + METROSTATUS, data = data_rockymtn)
tidy(est_rockymtn3)
tidy(est_rockymtn4)


#Multicollinearity check 
vif(est_rockymtn3)
vif(est_rockymtn4)



#
est_southwest <- lm(logincwage ~ EDUCYRS, data = data_southwest)

est_southwest2 <- lm(logincwage ~ EDUCYRS + AGE + AGE.squared + SEX + RACENW + LABFORCE + MARSTD + METROSTATUS, data = data_southwest)
tidy(est_southwest)
tidy(est_southwest2)
glance(est_southwest)
glance(est_southwest2)


#correcting heteroskedasticity 

est_southwest3 <- lm_robust(logincwage ~ EDUCYRS, data = data_southwest)

est_southwest4 <- lm_robust(logincwage ~ EDUCYRS + AGE + AGE.squared + SEX + RACENW + LABFORCE + MARSTD + METROSTATUS, data = data_southwest)

tidy(est_southwest3)
tidy(est_southwest4)

#Multicollinearity check 
vif(est_southwest3)
vif(est_southwest4)

#

est_midwest <- lm(logincwage ~ EDUCYRS, data = data_midwest1)

est_midwest2 <- lm(logincwage ~ EDUCYRS + AGE + AGE.squared + SEX + RACENW + LABFORCE + MARSTD + METROSTATUS, data = data_midwest1)
tidy(est_midwest)
tidy(est_midwest2)
glance(est_midwest)
glance(est_midwest2)

#correcting heteroskedasticity 
est_midwest3 <- lm_robust(logincwage ~ EDUCYRS, data = data_midwest1)

est_midwest4 <- lm_robust(logincwage ~ EDUCYRS + AGE + AGE.squared + SEX + RACENW + LABFORCE + MARSTD + METROSTATUS, data = data_midwest1)

tidy(est_midwest3)
tidy(est_midwest4)

#Multicollinearity check 
vif(est_midwest)
vif(est_midwest2)



#
est_southeast <- lm(logincwage ~ EDUCYRS, data = data_southeast1)

est_southeast2 <- lm(logincwage ~ EDUCYRS + AGE + AGE.squared + SEX + RACENW + LABFORCE + MARSTD + METROSTATUS, data = data_southeast1)

tidy(est_southeast)
tidy(est_southeast2)
glance(est_southeast)
glance(est_southeast2)

#correcting heteroskedasticity

est_southeast3 <- lm_robust(logincwage ~ EDUCYRS, data = data_southeast1)

est_southeast4 <- lm_robust(logincwage ~ EDUCYRS + AGE + AGE.squared + SEX + RACENW + LABFORCE + MARSTD + METROSTATUS, data = data_southeast1)

tidy(est_southeast3)
tidy(est_southeast4)

#Multicollinearity check 
vif(est_southeast3)
vif(est_southeast4)

#

est_northeast <- lm(logincwage ~ EDUCYRS, data = data_northeast1)

est_northeast2 <- lm(logincwage ~ EDUCYRS + AGE + AGE.squared + SEX + RACENW + LABFORCE + MARSTD + METROSTATUS, data = data_northeast1)

tidy(est_northeast)
tidy(est_northeast2)
glance(est_northeast)
glance(est_northeast2)
#correcting heteroskedasticity

est_northeast3 <- lm_robust(logincwage ~ EDUCYRS, data = data_northeast)

est_northeast4 <- lm_robust(logincwage ~ EDUCYRS + AGE + AGE.squared + SEX + RACENW + LABFORCE + MARSTD + METROSTATUS, data = data_northeast)
tidy(est_northeas3)
tidy(est_northeast4)

#Multicollinearity check 
vif(est_northeast3)
vif(est_northeast4)

#Create tables

library(modelsummary)

modelsummary(list(est_pacificwest3,est_pacificwest4), output= "markdown")
modelsummary(list(est_pacificwest3,est_pacificwest4), output= "latex")
#
modelsummary(list(est_rockymtn3,est_rockymtn4), output= "markdown")
modelsummary(list(est_rockymtn3,est_rockymtn4), output= "latex")
#
modelsummary(list(est_southwest3,est_southwest4), output= "markdown")
modelsummary(list(est_southwest3,est_southwest4), output= "latex")
#
modelsummary(list(est_midwest3,est_midwest4), output= "markdown")
modelsummary(list(est_midwest3,est_midwest4), output= "latex")
#
modelsummary(list(est_southeast3,est_southeast4), output= "markdown")
modelsummary(list(est_southeast3,est_southeast4), output= "latex")
#
modelsummary(list(est_northeast3,est_northeast4), output= "markdown")
modelsummary(list(est_northeast3,est_northeast4), output= "latex")
########################################

modelsummary(list(est_pacificwest3,est_pacificwest4), output= "latex")
#

modelsummary(list(est_rockymtn3,est_rockymtn4), output= "latex")
#

modelsummary(list(est_southwest3,est_southwest4), output= "latex")
#

modelsummary(list(est_midwest3,est_midwest4), output= "latex")
#

modelsummary(list(est_southeast3,est_southeast4), output= "latex")
#
modelsummary(list(est_northeast3,est_northeast4), output= "latex")




#GET GRAPH From St. Louis Federal Reserve Bank 

#Load Libraries 
library(ggplot2)
library(fredr)
library(quantmod)

#Use code to obtain 
getSymbols('CUSR0000SAE1',src='FRED')
#plot graph 
plot(CUSR0000SAE1)

data_CPIeduc <-read.csv("CUSR0000SAE1.csv", header=TRUE)
head(data_CPIeduc)

#give graph time frame, title and theme 
chartSeries(CUSR0000SAE1, subset='2010-01-01::2019-12-31',
            line.type = "l",
            name= "CPI US EDUCATION",
            theme=chartTheme('white'),
            TA="addVo()")




