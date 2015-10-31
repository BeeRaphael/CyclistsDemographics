#  Public Use Microdata Sample (PUMS) contains a sample of actual responses to the American Community Survey (ACS)

# PERSON RECORD
# *************
# Serialno - person serial number
# SPORDER - person number
# ST - state
# AGEP Age
# PWGTP - weight
# ESR - Employment status recode
# WAGP - salary / wage  # Use ADJINC to adjust WAGP to constant dollars.
# PERNP - person earnings  (job)
# PINCP - income (total)
# RAC1P - race code :  1 .White alone   2 .Black or African American alone / 3 .American Indian alone  / 4 .Alaska Native alone                     
#           5 .American Indian and Alaska Native tribes / 6 .Asian alone / 7 .Native Hawaiian and Other Pacific Islander alone
#           8 .Some Other Race alone   / 9 .Two or More Races  
# SCIENGRLP / SCIENGP Field of Degree Science and Engineering Flag  / related: 1:yes
# COW - Class of worker
# CIT - citizenship status (1-born in US, 4 .U.S. citizen by naturalization 5 .Not a citizen of the U.S.)
# OCCP - occupation code 1*** includes scientists , engineers, computer experts, analysts
# SCHL - degree   22 .Master's degree 23 .Professional degree beyond a bachelor's degree  24 .Doctorate degree 
# SEX
# JWAP time of arrival at work
# JWDP time of departure from work
# LANP language spoken at home bbb - only english
# JWMNP - travel time to work (in min)
# JWTR - transportation to work
#            01 .Car, truck, or van / 02 .Bus or trolley bus
#       03 .Streetcar or trolley car (carro publico in Puerto Rico) 04 .Subway or elevated
#       05 .Railroad 06 .Ferryboat            07 .Taxicab 08 .Motorcycle
#       09 .Bicycle 10 .Walked 11 .Worked at home 12 .Other method

# WKW       Weeks worked during past 12 months

##   FOD1P  + FOD2P : Field of degree; COMPUTER:  21** - 2107; ENGINEERING 24** 25**;  MATH  37**


library(data.table)
library(lubridate)
library(dplyr)

setwd("US Census data/uscensus_population")
colClasses = sapply(read.csv("ss13pusa.csv",nrows=100),class)

pusa <-fread('ss13pusa.csv', header=TRUE, select=c("SERIALNO","SPORDER","ST","PWGTP","AGEP","SEX","RAC1P","SCHL","CIT","COW","WAGP","PERNP","PINCP","OCCP","ESR", "SCIENGP","SCIENGRLP","FOD1P","FOD2P","JWMNP","JWTR","JWAP","JWDP"),colClasses = colClasses)
# 1613672 rows
pusb <-fread('ss13pusb.csv', header=TRUE, select=c("SERIALNO","SPORDER","ST","PWGTP","AGEP","SEX","RAC1P","SCHL","CIT","COW","WAGP","PERNP","PINCP","OCCP","ESR", "SCIENGP","SCIENGRLP","FOD1P","FOD2P","JWMNP","JWTR","JWAP","JWDP"),colClasses = colClasses)
# 1519123

setwd('Documents/Academia & Career/Bewerbung Engl/Data Incubator/Challenge')

Data<-rbind(pusa, pusb) # 3132795 rows
rm(pusa,pusb)

Data = Data[!is.na(Data$JWTR) & Data$WAGP>0 ,] # 1.38 Mi


Data<-transform(Data, SEX = factor(SEX), Race = factor(RAC1P), degree = factor(SCHL), Citizenship = factor(CIT), Transport = factor(JWTR))
Data<-transform(Data, ScienceDeg = (Data$SCIENGRLP==1 | Data$SCIENGP==1))
Data<-transform(Data, Bike = ((Data$Transport==9)))

Data$SCIENGRLP<-NULL
Data$SCIENGP<-NULL
Data$RAC1P <- NULL
Data$SCHL <- NULL
Data$CIT <- NULL
Data$JWTR <- NULL

NTransport<-table(Data$Transport)
NTransport/sum(NTransport)*100

DataCW <- Data[Data$Transport==9,]


