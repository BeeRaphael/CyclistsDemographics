# This script analyzes the  Public Use Microdata Sample (PUMS) from the us census year 2013
# The question I try to answer is: How is the demographics of people who cycle to work 
# different than from the general public.
# The script generates 3 graphs and saves them as png files (in the working directory).
# It is assumed that the data files (ss13pusb.csv and ss13pusb.csv) are in the working directory
# The following packages are used: data.table, ggplot2, dplyr, waffle
#
# author: Sabine Raphael
# Nov 1st 2015

# dataset:
# Public Use Microdata Sample (PUMS) contains a sample of actual responses to the American Community Survey (ACS)
#  1 year data of 2013

# Variables in PERSON RECORD of PUMS
# **********************************
# Serialno - person serial number
# SPORDER - person number
# ST     - US state
# AGEP   -  Age
# SEX    - 1 male, 2 - female
# PWGTP  - weight
# ESR    - Employment status recode
# WAGP   - salary / wage  # Use ADJINC to adjust WAGP to constant dollars.
# PERNP  - person earnings  (job)
# PINCP  - income (total)
# RAC1P  - race code :  1 .White alone   2 .Black or African American alone / 3 .American Indian alone  / 4 .Alaska Native alone                     
#           5 .American Indian and Alaska Native tribes / 6 .Asian alone / 7 .Native Hawaiian and Other Pacific Islander alone
#           8 .Some Other Race alone   / 9 .Two or More Races  
# SCIENGRLP / SCIENGP Field of Degree Science and Engineering Flag  / related: 1:yes  -> a lot of missing values!! do not use
# COW  - Class of worker
# CIT  - citizenship status (1-born in US, 4 .U.S. citizen by naturalization 5 .Not a citizen of the U.S.)
# OCCP - occupation code 1*** includes scientists , engineers, computer experts, analysts
# SCHL - degree   22 .Master's degree 23 .Professional degree beyond a bachelor's degree  24 .Doctorate degree 
# JWAP - time of arrival at work
# JWDP - time of departure from work
# LANP - language spoken at home bbb - only english
# JWMNP - travel time to work (in min)
# JWTR  - transportation to work
#    1 .Car, truck, or van /    2 .Bus or trolley bus    3 .Streetcar or trolley car 
#    4 .Subway      5 .Railroad     6 .Ferryboat         7 .Taxicab       8 .Motorcycle
#    9 .Bicycle     10 .Walked      11 .Worked at home   12 .Other method
##FOD1P  + FOD2P - Field of degree; COMPUTER:  21** - 2107; ENGINEERING 24** 25**;  MATH  37**


library(data.table)
library(lubridate)
library(ggplot2)
library(dplyr)
library(waffle)

setwd("US Census data/uscensus_population")
colClasses = sapply(read.csv("ss13pusa.csv",nrows=100),class)

# load the two datasets & combine them
# ****************
pusa <-fread('ss13pusa.csv', header=TRUE, 
             select=c("SERIALNO","SPORDER","ST","PWGTP","AGEP","SEX","RAC1P","SCHL","CIT", "COW","WAGP",
                      "PERNP","PINCP","OCCP","ESR","FOD1P","FOD2P","JWMNP","JWTR","JWAP","JWDP"))
# 1613672 rows
pusb <-fread('ss13pusb.csv', header=TRUE, 
             select=c("SERIALNO","SPORDER","ST","PWGTP","AGEP","SEX","RAC1P","SCHL","CIT","COW","WAGP",
                      "PERNP","PINCP","OCCP","ESR", "FOD1P","FOD2P","JWMNP","JWTR","JWAP","JWDP"))
# 1519123


# combine the 2 datasets, and clean up some working memory
Data <-bind_rows(pusa,pusb) # 3132795 rows
rm(pusa,pusb)

Data <- filter(Data, !is.na(JWTR) & WAGP>0  ) # exclude NAs in transport to work and wages of 0

# transform some variables in factors, and rename some
Data<-transform(Data,  Race = factor(RAC1P), degree = SCHL, Transport = factor(JWTR), WorkTravelTime =as.double(JWMNP))
Data<-transform(Data, Bike = ((Data$Transport==9)))

Data$RAC1P <- NULL
Data$JWTR <- NULL

# Transport column in Data
#   1 .Car, truck, or van /    2 .Bus or trolley bus    3 .Streetcar or trolley car 
#   4 .Subway      5 .Railroad     6 .Ferryboat         7 .Taxicab       8 .Motorcycle
#    9 .Bicycle     10 .Walked      11 .Worked at home   12 .Other method



# generate a table with the number of respondents traveling with each transport mode
# ****************

# relabel subway + trolley + ferry + Taxi travels to train category
Data$Transport[Data$Transport==4| Data$Transport==3 |  Data$Transport==6 |  Data$Transport==7] <-5
NTransport  <-table(Data$Transport)
TranspNames <-c("Car","Bus"," "," ","Train/Ferry", " ", " ","Motorbike","Bike","Walk","Home","Others")
names(NTransport) <- TranspNames
percNTransp <- round(NTransport/sum(NTransport)*1000) # transform to 1=0.1%
percNTransp <-sort(percNTransp,decreasing=TRUE)

# generate a waffle plot & save as png
# ****************
png(filename = "plot1.png",width = 1000, height = 500, units = "px", pointsize = 12)
waffle(percNTransp/2, rows=12, size=0, pad=1, reverse=TRUE,
       colors=c("gray77", "steelblue2","olivedrab3", "indianred1","goldenrod1", "violetred2","plum2","royalblue1"), 
       title="Modes of transport to work\n", xlab="       Each square is ~0.2% ")
dev.off()


# retrieve some statistics about different commuters
# ****************
GroupStats<-Data %>%
  group_by(Transport) %>%
  select(WorkTravelTime,degree, CIT,SEX,WAGP,OCCP) %>%
  summarize(MedianTime=median(WorkTravelTime),
            MeanTime=round(mean(WorkTravelTime)), 
            N=n(),
            NSCI=sum(OCCP<2000 & OCCP>999)/N, # N with occupation as scientists, analysts, computer experts, engineers
            NHighDegr=sum(degree>21)/N, # N with highest degree master or higher
            NUSborn = sum(CIT==1)/N,    # N born in US
            NHighInc = sum(WAGP>80000)/N, # N income above 80000 
            NWomen = sum(SEX==2) /N     # N women
  )

GroupStats  <- GroupStats[order(GroupStats$Transport, decreasing=FALSE),]
GroupStats$TMode <- c("Car","Bus","Train/Ferry","Motorbike","Bike","Walk","Home","Others")

# Transport MedianTime MeanTime       N       NSCI NHighDegr   NUSborn   NHighInc    NWomen       TMode
# 1         1         20       25 1134599 0.05454262 0.1275570 0.8559421 0.13555450 0.4846787         Car
# 2         2         40       47   30413 0.06273633 0.1094269 0.6791832 0.09762273 0.5245783         Bus
# 3         5         45       53   32189 0.10093510 0.2503650 0.6622759 0.29118643 0.4823076 Train/Ferry
# 4         8         20       23    3031 0.10293632 0.1045859 0.8983834 0.17123062 0.1418674   Motorbike
# 5         9         15       20    7922 0.11714214 0.2254481 0.8052260 0.14188336 0.2931078        Bike
# 6        10          7       11   38347 0.04837406 0.1233995 0.8119540 0.07468642 0.4717188        Walk
# 7        11         NA       NA   47448 0.09939302 0.1847496 0.8576968 0.23554207 0.4957006        Home
# 8        12         20       37   10525 0.04446556 0.0879810 0.7847031 0.15581948 0.3835629      Others



# calculate some population statistics
# ****************
PopStats<-Data %>%
  select(WorkTravelTime,degree, CIT,SEX,WAGP,OCCP) %>%
  summarize(MedianTime=median(WorkTravelTime,na.rm=TRUE),
            MeanTime=round(mean(WorkTravelTime,na.rm=TRUE) ), 
            N=n(),
            NSCI=sum(OCCP<2000 & OCCP>999)/N, # N with occupation as scientists, analysts, computer experts, engineers
            NHighDegr=sum(degree>21)/N, # N with highest degree master or higher
            NUSborn = sum(CIT==1)/N,    # N born in US
            NHighInc = sum(WAGP>80000)/N, # N income above 80000 
            NWomen = sum(SEX==2) /N     # N women
  )

#   MedianTime MeanTime       N       NSCI NHighDegr   NUSborn  NHighInc    NWomen
# 1       20     26      1304474 0.05773975 0.1323445 0.8450287 0.1406429 0.4827946



# Bubble plot 
# *******************




# plot distribution of traveling times to work for the different modes of transportation
# ****************

# exclude home workers and travel times that were not given
Data <- filter(Data, !is.na(WorkTravelTime) | Transport!=11) 

png(filename = "violineplot.png",width = 700, height = 700, units = "px", pointsize = 12)
p <- ggplot(Data, aes(x=Transport, y=WorkTravelTime, fill=Transport)) +  geom_violin() +
 scale_fill_manual("Modes of Transport: \n (median is indicated \n by the black dot)", labels= c("Car","Bus","Train/Ferry","Motorbike","Bike","Walk","Others"), 
                   values=c("gray66","goldenrod1",  "indianred1","steelblue2","plum2", "olivedrab3", "violetred2")) +
  scale_x_discrete(labels = c("Car","Bus","Train/Ferry","M-bike","Bike","Walk","Others")) +
  theme(text = element_text(size=18))+ #axis.text.x = element_text(colour="grey20",size=20,angle=90,hjust=.5,vjust=.5,face="plain")
  coord_cartesian(ylim = c(0, 100)) +
 theme(legend.title = element_text(colour="gray56", size=12,  face="bold")) +
  stat_summary(fun.y=median, geom="point", size=4, color="black")   +  # add median / mean
  labs(title="Tavel time to work\n", x="Mode of transport", y="Travel duration in min") 
dev.off()



