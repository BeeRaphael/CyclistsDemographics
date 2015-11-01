# * This script analyzes the  Public Use Microdata Sample (PUMS) from the us census year 2013
# * The question I try to answer is: How is the demographics of people who cycle to work 
#   different than from the general public.
# * The script generates 3 graphs and saves them as png files (in the working directory).
# * It also calculates some statistics about the people who use different modes of transportation to work
# * People with Wage (WAGP) =0 and Transportation (JWTR) = NA were excluded
# * It is assumed that the data files (ss13pusb.csv and ss13pusb.csv) are in the working directory
# * The following packages are used: data.table, ggplot2, dplyr, waffle
#
# author: Sabine Raphael
# Nov 1st 2015

# dataset:
#   Public Use Microdata Sample (PUMS) contains a sample of actual responses 
#   to the American Community Survey (ACS)
#   1 year data of 2013
#   downloaded from: http://www2.census.gov/acs2013_1yr/pums/csv_pus.zip
#   Data dictionary: http://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMSDataDict13.txt
#
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
library(ggplot2)
library(dplyr)
library(waffle)


# load the two datasets & combine them & some preprocessing
# ****************
pusa <-fread('ss13pusa.csv', header=TRUE, 
             select=c("SERIALNO","SPORDER","ST","PWGTP","AGEP","SEX","RAC1P","SCHL","CIT", "COW","WAGP",
                      "PERNP","PINCP","OCCP","ESR","FOD1P","FOD2P","JWMNP","JWTR","JWAP","JWDP"))
# 1613672 rows
pusb <-fread('ss13pusb.csv', header=TRUE, 
             select=c("SERIALNO","SPORDER","ST","PWGTP","AGEP","SEX","RAC1P","SCHL","CIT","COW","WAGP",
                      "PERNP","PINCP","OCCP","ESR", "FOD1P","FOD2P","JWMNP","JWTR","JWAP","JWDP"))
# 1519123 rows


# combine the 2 datasets, and clean up some working memory
Data <-bind_rows(pusa,pusb) # 3132795 rows
rm(pusa,pusb)

Data <- filter(Data, !is.na(JWTR) & WAGP>0  ) # exclude NAs in transport to work and wages of 0

# transform some variables in factors, and rename some
Data<-transform(Data,  Race = factor(RAC1P), degree = SCHL, Transport = factor(JWTR), WorkTravelTime =as.double(JWMNP))
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
NTransport        <- table(Data$Transport)
TranspNames       <- c("Car","Bus"," "," ","Train/Ferry", " ", " ","Motorbike","Bike","Walk","Home","Others")
names(NTransport) <- TranspNames
percNTransp       <- round(NTransport/sum(NTransport)*1000) # transform to 1=0.1%
percNTransp       <- sort(percNTransp,decreasing=TRUE)


# generate a waffle plot & save as png
# ****************
png(filename = "Modes of Transport to Work.png",width = 900, height = 500, units = "px", pointsize = 12)
p1 <- waffle(percNTransp/2, rows=12, size=0, pad=1, reverse=TRUE,
       colors = c("gray77", "steelblue2","olivedrab3", "indianred1","goldenrod1", "violetred2","plum2","royalblue1"), 
       title = "Modes of transport to work\n", xlab="         Each square is ~0.2% ") +
       theme(text = element_text(size=16), axis.title.x=element_text(size=14))
p1
dev.off()



# retrieve some statistics about different commuters
# ****************
GroupStats<-Data %>%
  group_by(Transport) %>%
  select(WorkTravelTime,degree, CIT,SEX,WAGP,OCCP) %>%
  summarize(MedianTime = median(WorkTravelTime),
            MeanTime = round(mean(WorkTravelTime)), 
            N = n(),
            NSCI=sum(OCCP<2000 & OCCP>999)/N*100, # Perc with occupation as scientists, analysts, computer experts, engineers
            NHighDegr = sum(degree>21)/N*100, # Perc with highest degree master or higher
            NUSborn  = sum(CIT==1)/N*100,    # Perc born in US
            NHighInc = sum(WAGP>80000)/N*100, # Perc income above 80000 
            NWomen   = sum(SEX==2) /N*100     # Perc women
  )

GroupStats  <- GroupStats[order(GroupStats$Transport, decreasing=FALSE),]
GroupStats$TMode <- c("Car","Bus","Train/Ferry","Motorbike","Bike","Walk","Home","Others")

print(GroupStats)
#   Transport MedianTime  MeanTime   N      NSCI NHighDegr  NUSborn  NHighInc   NWomen       TMode
#1         1         20       25 1134599  5.454262  12.75570 85.59421 13.555450 48.46787         Car
#2         2         40       47   30413  6.273633  10.94269 67.91832  9.762273 52.45783         Bus
#3         5         45       53   32189 10.093510  25.03650 66.22759 29.118643 48.23076 Train/Ferry
#4         8         20       23    3031 10.293632  10.45859 89.83834 17.123062 14.18674   Motorbike
#5         9         15       20    7922 11.714214  22.54481 80.52260 14.188336 29.31078        Bike
#6        10          7       11   38347  4.837406  12.33995 81.19540  7.468642 47.17188        Walk
#7        11         NA       NA   47448  9.939302  18.47496 85.76968 23.554207 49.57006        Home
#8        12         20       37   10525  4.446556   8.79810 78.47031 15.581948 38.35629      Others



# calculate some population statistics
# ****************
PopStats<-Data %>%
  select(WorkTravelTime,degree, CIT,SEX,WAGP,OCCP) %>%
  summarize(MedianTime = median(WorkTravelTime,na.rm = TRUE),
            MeanTime = round(mean(WorkTravelTime,na.rm = TRUE) ), 
            N = n(),
            NSCI = sum(OCCP<2000 & OCCP>999)/N*100, # Perc with occupation as scientists, analysts, computer experts, engineers
            NHighDegr=sum(degree>21)/N*100, # Perc with highest degree master or higher
            NUSborn  = sum(CIT==1)/N*100,    # Perc born in US
            NHighInc = sum(WAGP>80000)/N*100, # Perc income above 80000 
            NWomen   = sum(SEX==2) /N*100     # Perc women
  )

print(PopStats)
#   MedianTime MeanTime       N   NSCI    NHighDegr   NUSborn  NHighInc    NWomen
# 1       20     26      1304474 5.773975  13.23445 84.50287  14.06429   48.27946



# Bubble plot: Percent high earners vs. % with MSc or higher degree - for all modes of transport 
# *******************
png(filename = "DegreeVsWage.png",width = 650, height = 600, units = "px", pointsize = 12)

# area of circle should reflect frequency of transport mode
ggplot(GroupStats, aes(x = NHighInc, y = NHighDegr, size=sqrt(N/pi)/sqrt(sum(N)/pi)*100, label=TMode),guide=FALSE) +
  geom_point( aes( size = sqrt(N/pi)/sqrt(sum(N)/pi)*100 ), color="plum2") + 
  scale_size_area("Percent of \n Population", max_size = 20) + xlim(0,32) + ylim(5,28) +
  theme(text = element_text(size=16), axis.title.x=element_text(size=14)) +
  geom_text( aes(label=TMode), hjust=1.1, vjust=-0.4) +
  xlab("% with yearly wage above $80000") +
  ylab("% with highest degree MSc or higher") +
  ggtitle("Yearly wage vs. \n % People with at least MSc degree \n for different transport modes to work")
dev.off()


# plot distribution of traveling times to work for the different modes of transportation
# ****************

# exclude home workers and travel times that were not given
Data <- filter(Data, !is.na(WorkTravelTime) | Transport!=11) 

png(filename = "DistributioOfTimeToWork.png",width = 1000, height = 700, units = "px", pointsize = 12)
p2 <- ggplot(Data, aes(x=Transport, y=WorkTravelTime, fill=Transport)) +  geom_violin() +
 scale_fill_manual("Modes of Transport: \n (median is indicated \n by the black dot)", 
                   labels= c("Car","Bus","Train/Ferry","Motorbike","Bike","Walk","Others"), 
                   values=c("gray66","goldenrod1","indianred1","steelblue2","plum2", "olivedrab3", "violetred2")) +
  scale_x_discrete(labels = c("Car","Bus","Train/Ferry","Motorbike","Bike","Walk","Others")) +
  theme(text = element_text(size=18),  axis.title.x=element_text(size=16)) + 
  coord_cartesian(ylim = c(0, 100)) +
  theme(legend.title = element_text(colour="gray56", size=12,  face="bold")) +
  stat_summary(fun.y=median, geom="point", size=4, color="black")   +  # add median / mean
  labs(title="Tavel time to work\n", x="Mode of transport", y="Travel duration in min") 
p2
dev.off()

