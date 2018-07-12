#Emily Kalah Gade
# Sarah/Jon/John Senate Religon Paper
#10 April 2018 - Messy Code
rm(list=ls())

#library(mlr)
library(foreign) # load tab delim data
library(tidyverse) # data organizing
library(plotly) # pretty plots
#library(dplyr)
library(Hmisc)  #cor plots
library(corrplot) # cor plots
library(PerformanceAnalytics) # cor plots
library(wordcloud) # word clouds
library(betareg) # beta regression analysis
library(stargazer) # tables in latex
library(xtable) #tables in latex
library(coefplot) # ropeladders
library(car) # analysis of regression results/fit
library(lmtest)  # analysis of regression results/fit
#library(rcompanion)  # analysis of regression results/fit
#library(multcompView)  # analysis of regression results/fit
#library(lsmeans)  # analysis of regression results/fit
library(ggplot2) # ropeladders for beta reg 

####################
##### Getting data from the cluster loaded
####################


setwd(("/Users/emilykalahgade/Desktop/PostDoc/pub_projects/Working/EKGJonJohn_SenateInsecruityPaper/"))

#load all data


# Chapp words on house members
dat_house<-read.delim("22JanOutputHouse", sep="\t", header=F, stringsAsFactors = F)

#LWIC words for senate
da <- read.delim("textAsDataFinal", sep="\t", header=F, stringsAsFactors = F)

# DHS words for Senate
dat <- read.delim("DHSwordsTextasData", sep="\t", header=F, stringsAsFactors = F)

#words we decided were missing from senate
d<- read.delim("extrawordsTextasData_right", sep = "\t", header=F, stringsAsFactors = F)

# LWIC, DHS and "extra" words for house
daHouseAll<-read.delim("final_HouseLWIC_DHS_Extra_concat", sep="\t", header=F, stringsAsFactors = F)

#Chapp words for Senate
daSenateChapp<-read.delim("finalSenateChapOpp_concat", sep="\t", header=F, stringsAsFactors = F)

#Optimsim words for House
daHouseOpt<-read.delim("final_HouseOpp_concat", sep="\t", header=F, stringsAsFactors = F)

# house member data 
dhouse<-read.delim("HouseMembers", sep = ",", header = T, stringsAsFactors = F)

#append all the house datasets and senate datasets into one
dataHouse<-data.frame(rbind(dat_house, daHouseAll, daHouseOpt))
dataSenate<-data.frame(rbind(da, dat, d, daSenateChapp))

names(dataHouse)<-c('year', 'month', 'URLs', 'word', 'count')
names(dataSenate)<-c('year', 'month', 'URLs', 'word', 'count')

#check to make sure you didn't run totals ten times

data2House<-aggregate(dataHouse$count, by= list(dataHouse$year, dataHouse$month, dataHouse$URLs, dataHouse$word), FUN= sum)
names(data2House)<-c('year', 'month', 'URLs', 'word', 'count')

data2Senate<-aggregate(dataSenate$count, by= list(dataSenate$year, dataSenate$month, dataSenate$URLs, dataSenate$word), FUN= sum)
names(data2Senate)<-c('year', 'month', 'URLs', 'word', 'count')


# generating a month-year var 
data2House$month_year<-paste( data2House$year, data2House$month, sep=" ")
data2Senate$month_year<-paste( data2Senate$year, data2Senate$month, sep=" ")


### HOUSE  ###
# i'm now going to go through and to the House analysis since that is a more minor part of this project; just evaluating whether 
# actually going to chruch is predictive of using more religous words on both measures


## getting right dates and excluding problem words 
data2House<-data2House[data2House$year >= 2006,]
data2House<-data2House[data2House$word != "need",]
data2House<-data2House[data2House$word != "saw",]
data2House<-data2House[data2House$word != "inadequate",]
data2House<-data2House[data2House$word != "help",]
data2House<-data2House[data2House$year != 2007,]
data2House<-data2House[data2House$year != 2009,]
data2House<-data2House[data2House$year != 2011,]
data2House<-data2House[data2House$year != 2013,]

#write.csv(data2House, "rawdataforJon.csv")

#removing wprds I am supsicious of for house project
#data2House<-data2House[data2House$word != "nun",]
#data2House<-data2House[data2House$word != "nuns",]
#data2House<-data2House[data2House$word != "pew",]
#data2House<-data2House[data2House$word != "pews",]
#data2House<-data2House[data2House$word != "halo",]
data2House<-data2House[data2House$word != "ark",]
data2House<-data2House[data2House$word != "flock",]
data2House<-data2House[data2House$word != "mass",]
#data2House<-data2House[data2House$word != "sin",]
data2House<-data2House[data2House$word != "pastor",] # there is a house member named pastor
data2House<-data2House[data2House$word != "bishop",] # there is a house member named bishop
data2House<-data2House[data2House$word != "minister",]  # as in "prime minister" 

data2<-data2House

## checking out specific house members (looking for outliers)

sepcificsenators<-aggregate(data2$count, by=list(data2$URLs, data2$word), FUN=sum)
tots<-filter(sepcificsenators, sepcificsenators$Group.2 %in% c("total"))
sepcificsenators<-filter(sepcificsenators, !sepcificsenators$Group.2 %in% c("total"))
names(sepcificsenators)<-c("senator", "word", "count")
names(tots)<-c("senator", "total", "totals")
sepcificsenators2<-merge(sepcificsenators, tots, 
                         by=intersect(names(sepcificsenators), names(tots)))
sepcificsenators2$freq<-sepcificsenators2$count/sepcificsenators2$totals

# we only have 418 house members, which seems weird. Not sure what happened? 
## follow up - undergrad john says that he just got that data and some had become senators but their webpages didn't transfer. 
# this makes me nervous - are these the ones only for the 112th congress or something? which list did he choose?
# because of this i'm only going to include the year 2012 for these for now?

data2<-data2[data2$year != 2008,]
data2<-data2[data2$year != 2010,]
data2<-data2[data2$year != 2006,]

# this should be only year 2012, now getting rid of months

data2<-aggregate(data2$count, by= list(data2$year, data2$URLs, data2$word), FUN= sum)
names(data2)<-c('year', 'URLs', 'word', 'count')


### getting ISLAM specific words

df2<-filter(data2, data2$word %in% c("islam","muslim","shia","shiite","sunni","quran","quran","muhammed","modammad",
                                     "mecca","koran","imam","fundamentalist","alla","allah","prophet"))

IslamWords<-aggregate(df2$count, by= list(df2$year, df2$URLs), FUN= sum)

head(IslamWords)
names(IslamWords)<-c("year", "URLs", "IslamWords")



#getting anxiouty words

df3<-filter(data2, data2$word %in% c("afraid", "alarm", "anguish", "anxiety", "apprehension", "aversion",
                                     "bewilderment", "confusion", "desperate", "discomfort", "distraught", 
                                     "distress", "disturb", "dread", "emotional", "fear", "feared", "fearing", 
                                     "fears", "frantic", "fright", "hesitant", "horrific", "horrible", 
                                     "humiliating", "impatient", "inadequate", "insecure", "irritation", 
                                     "misery", "numerous", "obsession", "obsess", "overwhelm", "panic", 
                                     "petrify", "pressure", "reluctant", "restless", "saw", "scare", "shake", 
                                     "shy", "sicken", "startle", "strain", "stress", "stunned", "stuns", "tense", 
                                     "tension", "terrified", "terrifying", "terror", "tremble", "turmoil", "uncertain", 
                                     "uncomfortable", "uneasy", "unsure", "upset", "vulnerable", "worry", "fearful", "worried",
                                     "scared",    "suffer",    "suffering",    "need", "help",
                                     "miserable",    "apprehensive",    "bewildered",    "confused",
                                     "disturbed",    "fearful",    "frightened",    "humiliated",
                                     "miserable",    "obsessed",    "overwhelmed",    "panicked",
                                     "petrified",    "scared",    "shaken",    "sickened",
                                     "startled",    "strained",    "stressed",    "tragic",
                                     "trembling",    "instability",    "upsetting",    "concerned", "instability", "panicked")) 

anx<-unique(df3$word)


anxioutyWords<-aggregate(df3$count, by= list(df3$year, df3$URLs), FUN= sum)
names(anxioutyWords)<-c("year", "URLs", "anxioutyWords")


anxi_noURL<-aggregate(df3$count, by= list(df3$word), FUN= sum)


## getting Chapp religon words 
head(dat_house)
chappwords<-unique(dat_house$V4)
chappwords<-chappwords[chappwords != "total"]

df4<-filter(data2, data2$word %in% chappwords)
chapp<-aggregate(df4$count, by= list(df4$year, df4$URLs), FUN= sum)
names(chapp)<-c("year", "URLs", "chappwords")


## getting optimsim words 

head(daHouseOpt)
optwords<-unique(daHouseOpt$V4)
optwords<-optwords[optwords != "total"]

df5<-filter(data2, data2$word %in% optwords)
opt<-aggregate(df5$count, by= list(df5$year, df5$URLs), FUN= sum)
names(opt)<-c("year", "URLs", "optwords")

# getting DHS words 


df8<-filter(data2, data2$word %in% c("hazmat", "bomb", "attack" , "recruitment", 
                                     "flood" , "threat", "bacteria", "avian" , "recovery", "response", "enriched", 
                                     "exercise", "deaths" , "hazardous" , "collapse" , "security", "magnitude", 
                                     "exposure" , "incident" , "storm" , "outbreak" , "emergency", "hurricane", 
                                     "relief" , "terror" , "earthquake" , "screening" , "quarantine", "toxic", 
                                     "radiation" , "biological" , "radioactive", "heroin" , "hostage" ,"violence", 
                                     "evacuation" , "fundamentalism", "breach" , "typhoon" , "disaster", "wildfire", 
                                     "gang" , "hamas" , "plague" , "influenza" , "salmonella", "warning", 
                                     "strain" , "trafficking", "assassination", "burn" , "crash",  "closure", 
                                     "militia" , "nogales" , "infection", "mitigation", "cocaine", "symptoms" ,
                                     "contamination", "plume" , "lockdown" , "pandemic", "explosive", "resistant", 
                                     "methamphetamine", "riot" , "epidemic" , "ebola",  "kidnap", "terrorism" ,
                                     "mutation" , "hezbollah" , "smuggling" , "farc", "jihad" , "anthrax" ,
                                     "h1n1" , "sonora" , "shooting" , "extremism", "virus",  "tremor" ,
                                     "taliban" , "tornado" , "tsunami" , "vaccine", "standoff", "explosion", 
                                     "gangs" , "execution" , "spillover" , "flu",  "cartel", "shootout" ,
                                     "avalanche" , "looting" , "pirates" , "narcotics", "yuma", "sarin" ,
                                     "juarez" , "blizzard" , "listeria" , "trojan", "h5n1", "tijuana" ,
                                     "antiviral" , "hacker" , "ricin" , "guzman", "radicals", "islamist", 
                                     "smugglers" , "spammer" , "temblor" , "matamoros", "malware", "torreon", 
                                     "mudslide" , "twister" , "calderon" , "decapitated", "tamiflu", "sinaloa", 
                                     "tamaulipas" , "gunfight" , "zetas" , "botnet", "keylogger", "rootkit" ,
                                     "michoacana" , "conficker" , "narcos")) 

DHSWords<-aggregate(df8$count, by= list(df8$year, df8$URLs), FUN= sum)
names(DHSWords)<-c("year", "URLs", "DHSWords")

## getting religion words - LIWC


head(da)
ReligLWICwords<-unique(da$V4)
ReligLWICwords<-ReligLWICwords[ReligLWICwords != "total"]

df6<-filter(data2, data2$word %in% ReligLWICwords, !data2$word %in% anx)

Relig<-aggregate(df6$count, by= list(df6$year, df6$URLs), FUN= sum)
names(Relig)<-c("year", "URLs", "ReligLWICwords")


## get total words

total<-filter(dat_house, dat_house$V4 %in% c("total"))
TotalWords<-aggregate(total$V5, by= list(total$V1, total$V3), FUN= sum)
names(TotalWords)<-c("year", "URLs", "totals")
TotalWords<-TotalWords[TotalWords$year == 2012,]



## totals by senator (all time periods) for religious words
tot_sen_religous<-merge(Relig, TotalWords, 
                        by = intersect(names(Relig), names(TotalWords)), stringsAsFactors=F, all.x = T, all.y = T)
ifelse(is.na(tot_sen_religous$ReligLWICwords), 0, tot_sen_religous$ReligLWICwords) -> tot_sen_religous$ReligLWICwords


tot_sen_religous$freqReligLWIC<-tot_sen_religous$ReligLWICwords/tot_sen_religous$totals
ifelse(is.na(tot_sen_religous$freqReligLWIC), 0, tot_sen_religous$freqReligLWIC) -> tot_sen_religous$freqReligLWIC

tot_sen_religous2<-merge(chapp, tot_sen_religous, 
                        by = intersect(names(chapp), names(tot_sen_religous)), stringsAsFactors=F, all.x = T, all.y = T)
ifelse(is.na(tot_sen_religous2$chappwords), 0, tot_sen_religous2$chappwords) -> tot_sen_religous2$chappwords

tot_sen_religous2$freqReligChapp<-tot_sen_religous2$chappwords/tot_sen_religous2$totals
ifelse(is.na(tot_sen_religous2$freqReligChapp), 0, tot_sen_religous2$freqReligChapp) -> tot_sen_religous2$freqReligChapp

tot_sen_religous2[order(-tot_sen_religous2$freqReligChapp),]


#### merging data together to evalute two measure of religion against the church going measure


relig_freq_1<-tot_sen_religous2
names(relig_freq_1)<-c("year", "url", "chappwords", "religouswordsLWIC", "totals", "frequency_religLWIC", "frequency_chapp")



#making blank dataframe to add things too

data2year<-aggregate(data2$count, by= list(data2$year, data2$URLs), FUN= sum) # as place holder for dataframe (unique obs)

df1<-as.data.frame(rep(unique(data2$URLs), each= length(unique(data2$year))))
names(df1)<-c("URLs")
df1['year']<-as.data.frame(rep(unique(data2$year), 
                               times= length(unique(data2$URLs))))
names(df1)<-c("URLs", "year")


## mergeing into one dataframe 

merge_1<-merge(df1, IslamWords, by = intersect(names(df1), names(IslamWords)), all.x = T)#, incomparables = NA)

merge_2<-merge(merge_1, TotalWords, by = intersect(names(merge_1), names(TotalWords)), all.x =T)#, incomparables = NA)

merge_3<-merge(merge_2, anxioutyWords, by = intersect(names(merge_2), names(anxioutyWords)), all.x =T)#, incomparables = NA)

merge_4 <- merge(merge_3, chapp, by = intersect(names(merge_3), names(chapp)), all.x =T)

merge_5 <- merge(merge_4, DHSWords, by = intersect(names(merge_4), names(DHSWords)), all.x =T)

merge_6 <- merge(merge_5, Relig, by = intersect(names(merge_5), names(Relig)), all.x =T)

#merge_6 <- merge(merge_5, religousWords_bad, by = intersect(names(merge_4), names(DHSWords)), all.x =T)

merge_7 <- merge(merge_6, opt, by = intersect(names(merge_6), names(opt)), all.x =T)

#merge_7 <- merge(merge_6, religousWords_good, by = intersect(names(merge_4), names(DHSWords)), all.x =T)


####

rightdates<-merge_7

#aggregate 
str(rightdates)
rightdates$URLs<-as.character(rightdates$URLs)
rightdates$year<-as.numeric(rightdates$year)

## getting rid of NAs, except totals
ifelse(is.na(rightdates$IslamWords), 0, rightdates$IslamWords) -> rightdates$IslamWords
ifelse(is.na(rightdates$anxioutyWords), 0, rightdates$anxioutyWords) -> rightdates$anxioutyWords
ifelse(is.na(rightdates$ReligLWICwords), 0, rightdates$ReligLWICwords) -> rightdates$ReligLWICwords
ifelse(is.na(rightdates$chappwords), 0, rightdates$chappwords) -> rightdates$chappwords
ifelse(is.na(rightdates$DHSWords), 0, rightdates$DHSWords) -> rightdates$DHSWords
ifelse(is.na(rightdates$optwords), 0, rightdates$optwords ) -> rightdates$optwords

rightdates_1<-filter(rightdates, !rightdates$totals == 0)  ## getting rid of ones that seem to have gotten lost in the cluster
rightdates_1<-filter(rightdates, !rightdates$totals <= 1000)  ## getting rid of ones that seem to have gotten lost in the cluster

## if total = NA, omit
rightdates3<-na.omit(rightdates_1)

#get frequencies 
rightdates3$frequency_anxiouty= rightdates3$anxioutyWords/rightdates3$totals
rightdates3$frequency_chapp= rightdates3$chappwords/rightdates3$totals
rightdates3$frequency_religLWIC= rightdates3$ReligLWICwords/rightdates3$totals
rightdates3$frequency_reli_Lwicnoislam= (rightdates3$frequency_religLWIC - rightdates3$IslamWords)/rightdates3$totals
rightdates3$frequency_dhs= rightdates3$DHSWords/rightdates3$totals
rightdates3$frequency_islam= rightdates3$IslamWords/rightdates3$totals
rightdates3$frequency_opt= rightdates3$optwords/rightdates3$totals

rightdates3$frequency_anxiouty<- rightdates3$frequency_anxiouty + 0.000000001
rightdates3$frequency_religLWIC<-rightdates3$frequency_religLWIC + 0.000000001
rightdates3$frequency_chapp<-rightdates3$frequency_chapp + 0.000000001
rightdates3$frequency_reli_Lwicnoislam<- rightdates3$frequency_reli_Lwicnoislam + 0.000000001
rightdates3$frequency_dhs<- rightdates3$frequency_dhs + 0.000000001
rightdates3$frequency_islam<- rightdates3$frequency_islam + 0.000000001
rightdates3$frequency_opt<- rightdates3$frequency_opt + 0.000000001






## finding missing senators - why are these not in this list??

#TotalWords[TotalWords$URLs=="adamsmith", ]
# adams 0 words
# adamsmith 0 words
# bachmann  0 words
# berman 41439 
# boren 9648 
# cardoza  0 words
# wu 40 words?
# weiner 0 words
# tonko 0 words
# tiberi 0 words
# susandavis 0 words
# ruppersberger 0 words
# roybal-allard 0 words
# ross 910
# ros-lehtinen 0 words
# ralphhall 0 words
# quigley 0 words
# pingree 0 words
# myrick 0 words
# mccotter 523
# lynnjenkins 0 words
# lindasanchez 0 words
# jerrylewis 56620
# inslee 1046
# hochul 0
# grimm  0 words
# graves 93731
# giffords 0 
# geoffdavis 1376
# garamendi  0 words
# ebjohnson 0 words
# dicks 15582
# denham  0 words
# creyes 0 words
# chrislee 0 words



names(rightdates3)<-c("url", "year",  "IslamWords" ,   "totals" ,           
                      "anxioutyWords", "chappwords" ,    "DHSWords" ,    "ReligLWICwords",       
                     "optwords",    "frequency_anxiouty", "frequency_chapp", "frequency_religLWIC",    
                     "frequency_reli_Lwicnoislam", "frequency_dhs", "frequency_islam","frequency_opt")


merge_J<-merge(dhouse, rightdates3, by = intersect(names(dhouse), names(rightdates3)), all.x = T)#, incomparables = NA)


m<-data.frame(merge_J$url, merge_J$cong, merge_J$state.x, 
              merge_J$dist, merge_J$party.x, merge_J$frequency_religLWIC, 
              merge_J$frequency_chapp, merge_J$frequency_dhs,  
              merge_J$frequency_opt, merge_J$frequency_anxiouty, 
              merge_J$districtparty,
              merge_J$gender, merge_J$openrelig, merge_J$evangel,
              merge_J$CDIRRELG, merge_J$CDIRCHURCH, merge_J$totmention,
              merge_J$nutheo, merge_J$nuactive)

m2<-m

a<-lm(m2$merge_J.frequency_religLWIC~ m2$merge_J.nuthe) #m$merge_J.CDIRRELG + m$merge_J.openrelig 
#m2$merge_J.districtparty + m2$merge_J.evangel + 
#m2$merge_J.gender + 

b<-lm(m2$merge_J.frequency_religLWIC~ m2$merge_J.nuactive) #m$merge_J.CDIRRELG + m$merge_J.openrelig 
#m2$merge_J.districtparty + m2$merge_J.evangel + 
#m2$merge_J.gender + 

c<-lm(m2$merge_J.frequency_chapp~ $merge_J.nuthe)

d<-lm(m2$merge_J.frequency_chapp~ $merge_J.nuactive)


c<-lm(m2$merge_J.frequency_dhs~ m2$merge_J.nuactive) 

d<-lm(m2$merge_J.frequency_anxiouty ~ m2$merge_J.nuactive) 

e<-lm(m2$merge_J.frequency_opt ~ m2$merge_J.nuactive) 


summary(a)
summary(b)
summary(c)
summary(d)
summary(e)
stargazer(a, b, c, d, e)


a2<-lm(m2$merge_J.frequency_religLWIC~ m2$merge_J.nuactive + m2$merge_J.districtparty + m2$merge_J.evangel + m2$merge_J.gender) 
b2<-lm(m2$merge_J.frequency_chapp~ m2$merge_J.nuactive + m2$merge_J.districtparty + m2$merge_J.evangel + m2$merge_J.gender) 
c2<-lm(m2$merge_J.frequency_dhs~ m2$merge_J.nuactive + m2$merge_J.districtparty + m2$merge_J.evangel + m2$merge_J.gender) 
d2<-lm(m2$merge_J.frequency_anxiouty~ m2$merge_J.nuactive + m2$merge_J.districtparty + m2$merge_J.evangel + m2$merge_J.gender) 
e2<-lm(m2$merge_J.frequency_opt~ m2$merge_J.nuactive + m2$merge_J.districtparty + m2$merge_J.evangel + m2$merge_J.gender) 


summary(a2)
summary(b2)
summary(c2)
summary(d2)
summary(e2)
stargazer(a2, b2, c2, d2, e2)

e3<-lm(m2$merge_J.frequency_chapp~ m2$merge_J.frequency_opt + m2$merge_J.frequency_dhs+ 
         m2$merge_J.frequency_anxiouty + m2$merge_J.nuactive + m2$merge_J.districtparty + 
         m2$merge_J.evangel + m2$merge_J.gender) 

e4<-lm(m2$merge_J.frequency_religLWIC~ m2$merge_J.frequency_opt + m2$merge_J.frequency_dhs+ 
         m2$merge_J.frequency_anxiouty + m2$merge_J.nuactive + m2$merge_J.districtparty + 
         m2$merge_J.evangel + m2$merge_J.gender) 

stargazer(e3, e4)


m3<-m2[m2$merge_J.frequency_religLWIC > 0,]


betaregA<-betareg(m3$merge_J.frequency_religLWIC ~ m3$merge_J.nuactive, data=m3)
beta<-betareg(m3$merge_J.frequency_religLWIC ~ m3$merge_J.nutheo, data=m3)
betaregA2<-betareg(m3$merge_J.frequency_religLWIC ~ m3$merge_J.nuactive + m3$merge_J.evangel + 
                     m3$merge_J.districtparty + 
                     m3$merge_J.gender, data=m3)

m3<-m3[m3$merge_J.frequency_chapp > 0,]

betaregB<-betareg(m3$merge_J.frequency_chapp ~ m3$merge_J.nuactive, data=m3)
beta2<-betareg(m3$merge_J.frequency_chapp ~ m3$merge_J.nutheo, data=m3)
betaregB2<-betareg(m3$merge_J.frequency_chapp ~ m3$merge_J.nuactive + m3$merge_J.evangel + 
                     m3$merge_J.districtparty + 
                     m3$merge_J.gender, data=m3)

summary(a)
summary(b)
summary(a2)
summary(b2)
summary(betaregA)
summary(betaregA2)
summary(betaregB)
summary(beta)
summary(beta2)
summary(betaregB2)
stargazer(a, b, a2, b2)

stargazer(betaregA, beta, betaregB, beta2)

stargazer(betaregA2, betaregB2)


### changing to check other measures of rehetoric 


m3<-m2[m2$merge_J.frequency_dhs > 0,]


betaregA<-betareg(m3$merge_J.frequency_dhs ~ m3$merge_J.nuactive, data=m3)
beta<-betareg(m3$merge_J.frequency_opt ~ m3$merge_J.nuactive, data=m3)
betaregB<-betareg(m3$merge_J.frequency_anxiouty ~ m3$merge_J.nuactive, data=m3)

betaregA2<-betareg(m3$merge_J.frequency_dhs ~ m3$merge_J.nuactive + m3$merge_J.evangel + 
                     m3$merge_J.districtparty + 
                     m3$merge_J.gender, data=m3)
betaregB2<-betareg(m3$merge_J.frequency_opt ~ m3$merge_J.nuactive + m3$merge_J.evangel + 
                     m3$merge_J.districtparty + 
                     m3$merge_J.gender, data=m3)
betaregC2<-betareg(m3$merge_J.frequency_anxiouty ~ m3$merge_J.nuactive + m3$merge_J.evangel + 
                     m3$merge_J.districtparty + 
                     m3$merge_J.gender, data=m3)

stargazer(betaregA, betaregA2, beta, betaregB2, betaregB, betaregC2)


### T test to confirm the deletions from some house members shouldn't bias results. 

t.test(dhouse$nuactive,m2$merge_J.nuactive)


m3<-na.omit(m3)
###### MAKING SCATTER PLOTS WITH NAMES AND LINES

b_forplot <- betareg(m3$merge_J.frequency_chapp ~ m3$merge_J.frequency_anxiouty | m3$merge_J.frequency_anxiouty,
                     data = m3, hessian = TRUE)



dvData<-data.frame(as.character(m3$merge_J.url), as.numeric(m3$merge_J.frequency_chapp), 
                   as.numeric(m3$merge_J.frequency_anxiouty), as.numeric(m3$merge_J.frequency_opt))
colnames(dvData)<-c("url", "frequency_religous", "frequency_anxiouty", "freq_opt")  #, "frequency_dhs")
dvData$url<-as.character(dvData$url)

agDVdat<-aggregate(list(dvData$frequency_religous, dvData$frequency_anxiouty, dvData$freq_opt), by= list(dvData$url), FUN= mean) # dvData$frequency_dhs, 
colnames(agDVdat)<-c("last", "frequency_religous","frequency_anxiouty", "freq_opt") #"frequency_dhs",

#test<-agDVdat[agDVdat$last == "Burris",]
#plot(jitter(test$frequency_religous)~ jitter(test$frequency_anxiouty),
#     xlim = c(0, 0.002), ylim = c(0, 0.002))
#with(dvData, text(jitter(test$frequency_religous)~ jitter(test$frequency_anxiouty), 
#                  labels = test$last, pos = 4))



agDVdat2<-agDVdat[agDVdat$frequency_anxiouty >= 2.0e-03 | agDVdat$frequency_religous >= 3.0e-03, ]
agDVdat3<-agDVdat[agDVdat$freq_opt >= 3.5e-03 | agDVdat$frequency_religous >= 3.0e-03, ]

pdf(file="HousescatterWithRegLines_anxietyOpt.pdf", paper="letter",width = 10,height = 7)
par(mfrow=c(2,1))

plot(jitter(agDVdat$frequency_religous)~ jitter(agDVdat$frequency_anxiouty),
     xlim = c(0, 0.004), ylim = c(0, 0.006), ylab = "Freq. Religious Terms", 
     xlab = "Freq. Anxiety Terms", 
     main = "Freq. Religious Terms v. Freq. Anxiety Terms by House Member")
with(dvData, text(jitter(agDVdat2$frequency_religous)~ jitter(agDVdat2$frequency_anxiouty),
                  labels = agDVdat2$last, pos = 4))
lines(lowess(m3$merge_J.frequency_chapp ~ m3$merge_J.frequency_anxiouty))
lines(fitted(betareg(m3$merge_J.frequency_chapp ~  m3$merge_J.frequency_anxiouty))~ m3$merge_J.frequency_anxiouty, lty=2)
lines(fitted(lm(m3$merge_J.frequency_chapp ~ m3$merge_J.frequency_anxiouty)) ~ m3$merge_J.frequency_anxiouty, lty = 3)
legend("topright", c("LOWESS (smoothing)", "Beta Reg. (Coef + Phi Coef)", "Linear"), lty = 1:3, bty = "n")



plot(jitter(agDVdat$frequency_religous)~ jitter(agDVdat$frequency_anxiouty),
     xlim = c(0, .0065), ylim = c(0, 0.0055), ylab = "Freq. Religious Terms", 
     xlab = "Freq. Optimsim Terms", 
     main = "Freq. Religious Terms v. Freq. Optimism Terms by House Member")
with(dvData, text(jitter(agDVdat3$frequency_religous)~ jitter(agDVdat3$freq_opt),
                  labels = agDVdat2$last, pos = 4))
lines(lowess(m3$merge_J.frequency_chapp ~ m3$merge_J.frequency_opt))
lines(fitted(betareg(m3$merge_J.frequency_chapp ~  m3$merge_J.frequency_opt))~ m3$merge_J.frequency_opt, lty=2)
lines(fitted(lm(m3$merge_J.frequency_chapp ~ m3$merge_J.frequency_opt)) ~ m3$merge_J.frequency_opt, lty = 3)
legend("topright", c("LOWESS (smoothing)", "Beta Reg. (Coef + Phi Coef)", "Linear"), lty = 1:3, bty = "n")

dev.off()





# some quick plots for house members appendix 


corsN<-na.omit(data.frame(m2$merge_J.freqLWICRelig, m2$merge_J.freqChapp, 
                          m2$merge_J.nuactive, m2$merge_J.evangel, m2$merge_J.districtparty))
names(corsN)<- c("freq relig LWIC", "freq relig Chapp", "active-ness", 
                 "evangel", "dist party")
res2N<-rcorr(as.matrix(corsN))

#png(file= "cor_house_13April.png")
par(mfrow = c(1, 1))#,  mai=c(1,0.5,0.5,0.25), pin=c(2,2))
# Insignificant correlations are leaved blank
corrplot(res2N$r, type="upper", order="original", 
         p.mat = res2N$P, sig.level = 0.001, insig = "blank")#, title = "Term ")
dev.off()

### making word clouds

df4<-df4[df4$URLs != "other",]
df6<-df6[df6$URLs != "other",]
#wc_religLWIC<-aggregate(df4$count, by= list(df4$word), FUN= sum)


rel_forplotLWIC<-aggregate(df6$count, by= list(df6$word, df6$URLs), FUN= sum)

names(rel_forplotLWIC)<-c("word", "URLs", "count")
subforplot_relFREQLWIC<-merge(rel_forplotLWIC, Totalbysenator, by = intersect(names(rel_forplotLWIC), names(Totalbysenator)) )

totFreqForTable_relLWIC<-aggregate(list(subforplot_relFREQLWIC$count, subforplot_relFREQLWIC$totals), 
                                   by= list(subforplot_relFREQLWIC$word), FUN= sum)
names(totFreqForTable_relLWIC)<-c("word", "counts", "totals")
totFreqForTable_relLWIC$freq<-totFreqForTable_relLWIC$count/totFreqForTable_relLWIC$totals


freqs2LWIC<-na.omit(data.frame(totFreqForTable_relLWIC$word, 
                                totFreqForTable_relLWIC$freq, stringsAsFactors = F))
names(freqs2LWIC)<-c("word", "freq")

## now chapp words

wc_religChapp<-aggregate(df4$count, by= list(df4$word), FUN= sum)

rel_forplotChapp<-aggregate(df4$count, by= list(df4$word, df4$URLs), FUN= sum)

names(rel_forplotChapp)<-c("word", "URLs", "count")
subforplot_relFREQChapp<-merge(rel_forplotChapp, Totalbysenator, by = intersect(names(rel_forplotChapp), names(Totalbysenator)) )

totFreqForTable_relChapp<-aggregate(list(subforplot_relFREQChapp$count, subforplot_relFREQChapp$totals), 
                                    by= list(subforplot_relFREQChapp$word), FUN= sum)
names(totFreqForTable_relChapp)<-c("word", "counts", "totals")
totFreqForTable_relChapp$freq<-totFreqForTable_relChapp$count/totFreqForTable_relChapp$totals

freqs2chapp<-na.omit(data.frame(totFreqForTable_relChapp$word, 
                                      totFreqForTable_relChapp$freq, stringsAsFactors = F))
names(freqs2chapp)<-c("word", "freq")


#pdf(file="wordClouds_house_religx2.pdf", paper="letter",width = 7,height = 5)
par(mfrow=c(1,2))


wordcloud(freqs2chapp$word, freqs2chapp$freq, scale=c(3, .3), 
          random.order = FALSE, random.color = FALSE, 
          colors= c("indianred1","indianred2","indianred3","indianred"))

wordcloud(freqs2LWIC$word, freqs2LWIC$freq, scale=c(3, .3), 
          random.order = FALSE, random.color = FALSE, colors= c("lightsteelblue1","lightsteelblue2","lightsteelblue3","lightsteelblue"))

dev.off()



####################################
####################################
##### NOW FOR SENATORS - drop the house members :) 
####################################
####################################



data2<-data2Senate

## getting right dates and excluding problem words 
data2<-data2[data2$year >= 2006,]
data2<-data2[data2$word != "need",]
data2<-data2[data2$word != "saw",]
data2<-data2[data2$word != "inadequate",]
data2<-data2[data2$word != "help",]
data2<-data2[data2$year != 2007,]
data2<-data2[data2$year != 2009,]
data2<-data2[data2$year != 2011,]
data2<-data2[data2$year != 2013,]

#write.csv(data2, "rawdataforJon.csv")

#removing wprds I am supsicious of for house project
#data2<-data2[data2$word != "nun",]
#data2<-data2[data2$word != "nuns",]
#data2<-data2[data2$word != "pew",]
#data2<-data2[data2$word != "pews",]
#data2<-data2[data2$word != "halo",]
#data2<-data2[data2$word != "ark",]
#data2<-data2[data2$word != "flock",]
#data2<-data2[data2$word != "mass",]
#data2<-data2[data2$word != "sin",]
data2<-data2[data2$word != "pastor",] # there is a house member named pastor
data2<-data2[data2$word != "bishop",] # there is a house member named bishop
data2<-data2[data2$word != "minister",]  # as in "prime minister" 
data2<-data2[data2$word!= "prayer",]


## checking out specific house members (looking for outliers)

sepcificsenators<-aggregate(data2$count, by=list(data2$URLs, data2$word), FUN=sum)
tots<-filter(sepcificsenators, sepcificsenators$Group.2 %in% c("total"))
sepcificsenators<-filter(sepcificsenators, !sepcificsenators$Group.2 %in% c("total"))
names(sepcificsenators)<-c("senator", "word", "count")
names(tots)<-c("senator", "total", "totals")
sepcificsenators2<-merge(sepcificsenators, tots, 
                         by=intersect(names(sepcificsenators), names(tots)))
sepcificsenators2$freq<-sepcificsenators2$count/sepcificsenators2$totals

# 


data2<-aggregate(data2$count, by= list(data2$year, data2$URLs, data2$word), FUN= sum)
names(data2)<-c('year', 'URLs', 'word', 'count')


### getting ISLAM specific words

df2<-filter(data2, data2$word %in% c("islam","muslim","shia","shiite","sunni","quran","quran","muhammed","modammad",
                                     "mecca","koran","imam","fundamentalist","alla","allah","prophet"))

IslamWords<-aggregate(df2$count, by= list(df2$year, df2$URLs), FUN= sum)

head(IslamWords)
names(IslamWords)<-c("year", "URLs", "IslamWords")

#IslamWords<-aggregate(df2$count, by= list( df2$URLs), FUN= sum)

#names(IslamWords)<-c( "URLs", "IslamWords")


#getting anxiouty words

df3<-filter(data2, data2$word %in% c("afraid", "alarm", "anguish", "anxiety", "apprehension", "aversion",
                                     "bewilderment", "confusion", "desperate", "discomfort", "distraught", 
                                     "distress", "disturb", "dread", "emotional", "fear", "feared", "fearing", 
                                     "fears", "frantic", "fright", "hesitant", "horrific", "horrible", 
                                     "humiliating", "impatient", "inadequate", "insecure", "irritation", 
                                     "misery", "numerous", "obsession", "obsess", "overwhelm", "panic", 
                                     "petrify", "pressure", "reluctant", "restless", "saw", "scare", "shake", 
                                     "shy", "sicken", "startle", "strain", "stress", "stunned", "stuns", "tense", 
                                     "tension", "terrified", "terrifying", "terror", "tremble", "turmoil", "uncertain", 
                                     "uncomfortable", "uneasy", "unsure", "upset", "vulnerable", "worry", "fearful", "worried",
                                     "scared",    "suffer",    "suffering",    "need", "help",
                                     "miserable",    "apprehensive",    "bewildered",    "confused",
                                     "disturbed",    "fearful",    "frightened",    "humiliated",
                                     "miserable",    "obsessed",    "overwhelmed",    "panicked",
                                     "petrified",    "scared",    "shaken",    "sickened",
                                     "startled",    "strained",    "stressed",    "tragic",
                                     "trembling",    "instability",    "upsetting",    "concerned", "instability", "panicked")) 

anx<-unique(df3$word)


anxioutyWords<-aggregate(df3$count, by= list(df3$year, df3$URLs), FUN= sum)
names(anxioutyWords)<-c("year", "URLs", "anxioutyWords")
#anxioutyWords<-aggregate(df3$count, by= list(df3$URLs), FUN= sum)
#names(anxioutyWords)<-c("URLs", "anxioutyWords")


anxi_noURL<-aggregate(df3$count, by= list(df3$word), FUN= sum)


## getting Chapp religon words 
head(dat_house)
chappwords<-unique(dat_house$V4)
chappwords<-chappwords[chappwords != "total"]

df4<-filter(data2, data2$word %in% chappwords)
chapp<-aggregate(df4$count, by= list(df4$year, df4$URLs), FUN= sum)
names(chapp)<-c("year", "URLs", "chappwords")

#chapp<-aggregate(df4$count, by= list( df4$URLs), FUN= sum)
#names(chapp)<-c("URLs", "chappwords")

## getting optimsim words 

head(daHouseOpt)
optwords<-unique(daHouseOpt$V4)
optwords<-optwords[optwords != "total"]

df8<-filter(data2, data2$word %in% optwords)
opt<-aggregate(df8$count, by= list(df8$year, df8$URLs), FUN= sum)
names(opt)<-c("year", "URLs", "optwords")
#opt<-aggregate(df8$count, by= list(df8$URLs), FUN= sum)
#names(opt)<-c("URLs", "optwords")
# getting DHS words 


df5<-filter(data2, data2$word %in% c("hazmat", "bomb", "attack" , "recruitment", 
                                     "flood" , "threat", "bacteria", "avian" , "recovery", "response", "enriched", 
                                     "exercise", "deaths" , "hazardous" , "collapse" , "security", "magnitude", 
                                     "exposure" , "incident" , "storm" , "outbreak" , "emergency", "hurricane", 
                                     "relief" , "terror" , "earthquake" , "screening" , "quarantine", "toxic", 
                                     "radiation" , "biological" , "radioactive", "heroin" , "hostage" ,"violence", 
                                     "evacuation" , "fundamentalism", "breach" , "typhoon" , "disaster", "wildfire", 
                                     "gang" , "hamas" , "plague" , "influenza" , "salmonella", "warning", 
                                     "strain" , "trafficking", "assassination", "burn" , "crash",  "closure", 
                                     "militia" , "nogales" , "infection", "mitigation", "cocaine", "symptoms" ,
                                     "contamination", "plume" , "lockdown" , "pandemic", "explosive", "resistant", 
                                     "methamphetamine", "riot" , "epidemic" , "ebola",  "kidnap", "terrorism" ,
                                     "mutation" , "hezbollah" , "smuggling" , "farc", "jihad" , "anthrax" ,
                                     "h1n1" , "sonora" , "shooting" , "extremism", "virus",  "tremor" ,
                                     "taliban" , "tornado" , "tsunami" , "vaccine", "standoff", "explosion", 
                                     "gangs" , "execution" , "spillover" , "flu",  "cartel", "shootout" ,
                                     "avalanche" , "looting" , "pirates" , "narcotics", "yuma", "sarin" ,
                                     "juarez" , "blizzard" , "listeria" , "trojan", "h5n1", "tijuana" ,
                                     "antiviral" , "hacker" , "ricin" , "guzman", "radicals", "islamist", 
                                     "smugglers" , "spammer" , "temblor" , "matamoros", "malware", "torreon", 
                                     "mudslide" , "twister" , "calderon" , "decapitated", "tamiflu", "sinaloa", 
                                     "tamaulipas" , "gunfight" , "zetas" , "botnet", "keylogger", "rootkit" ,
                                     "michoacana" , "conficker" , "narcos")) 

DHSWords<-aggregate(df5$count, by= list(df5$year, df5$URLs), FUN= sum)
names(DHSWords)<-c("year", "URLs", "DHSWords")

#DHSWords<-aggregate(df5$count, by= list( df5$URLs), FUN= sum)
#names(DHSWords)<-c("URLs", "DHSWords")
## getting religion words - LIWC


head(da)
ReligLWICwords<-unique(da$V4)
ReligLWICwords<-ReligLWICwords[ReligLWICwords != "total"]

df6<-filter(data2, data2$word %in% ReligLWICwords, !data2$word %in% anx)

Relig<-aggregate(df6$count, by= list(df6$year, df6$URLs), FUN= sum)
names(Relig)<-c("year", "URLs", "ReligLWICwords")

#Relig<-aggregate(df6$count, by= list(df6$URLs), FUN= sum)
#names(Relig)<-c("URLs", "ReligLWICwords")

## get total words

total<-filter(da, da$V4 %in% c("total"))
TotalWords<-aggregate(total$V5, by= list(total$V1, total$V3), FUN= sum)
names(TotalWords)<-c("year", "URLs", "totals")
TotalWords<-TotalWords[TotalWords$year >= 2006,]
TotalWords<-TotalWords[TotalWords$year != 2007,]
TotalWords<-TotalWords[TotalWords$year != 2009,]
TotalWords<-TotalWords[TotalWords$year != 2011,]
TotalWords<-TotalWords[TotalWords$year != 2013,]

#TotalWords<-aggregate(total$V5, by= list(total$V3), FUN= sum)
#names(TotalWords)<-c("URLs", "totals")

## totals by senator (year periods) for religious words
tot_sen_religous<-merge(Relig, TotalWords, 
                        by = intersect(names(Relig), names(TotalWords)), stringsAsFactors=F, all.x = T, all.y = T)
ifelse(is.na(tot_sen_religous$ReligLWICwords), 0, tot_sen_religous$ReligLWICwords) -> tot_sen_religous$ReligLWICwords


tot_sen_religous$freqReligLWIC<-tot_sen_religous$ReligLWICwords/tot_sen_religous$totals
ifelse(is.na(tot_sen_religous$freqReligLWIC), 0, tot_sen_religous$freqReligLWIC) -> tot_sen_religous$freqReligLWIC

tot_sen_religous2<-merge(chapp, tot_sen_religous, 
                         by = intersect(names(chapp), names(tot_sen_religous)), stringsAsFactors=F, all.x = T, all.y = T)
ifelse(is.na(tot_sen_religous2$chappwords), 0, tot_sen_religous2$chappwords) -> tot_sen_religous2$chappwords

tot_sen_religous2$freqReligChapp<-tot_sen_religous2$chappwords/tot_sen_religous2$totals
ifelse(is.na(tot_sen_religous2$freqReligChapp), 0, tot_sen_religous2$freqReligChapp) -> tot_sen_religous2$freqReligChapp

tot_sen_religous2[order(-tot_sen_religous2$freqReligChapp),]

###################
### descriptive stats via word cloud
#####################
### making word clouds

df4<-df4[df4$URLs != "other",]
df6<-df6[df6$URLs != "other",]
df5<-df5[df5$URLs != "other", ]
df8<-df8[df8$URLs != "other",]
df2<-df2[df2$URLs != "other",]
df3<-df3[df3$URLs != "other",]

#wc_religLWIC<-aggregate(df4$count, by= list(df4$word), FUN= sum)


Totalbysenator<-aggregate(TotalWords$totals, by= list(TotalWords$URLs), FUN= sum)
names(Totalbysenator)<-c("URLs", "totals")
Totalbysenator<-Totalbysenator[Totalbysenator$URLs != "other",]


rel_forplotLWIC<-aggregate(df6$count, by= list(df6$word, df6$URLs), FUN= sum)

names(rel_forplotLWIC)<-c("word", "URLs", "count")
subforplot_relFREQLWIC<-merge(rel_forplotLWIC, Totalbysenator, by = intersect(names(rel_forplotLWIC), names(Totalbysenator)) )

totFreqForTable_relLWIC<-aggregate(list(subforplot_relFREQLWIC$count, subforplot_relFREQLWIC$totals), 
                                   by= list(subforplot_relFREQLWIC$word), FUN= sum)
names(totFreqForTable_relLWIC)<-c("word", "counts", "totals")
totFreqForTable_relLWIC$freq<-totFreqForTable_relLWIC$count/totFreqForTable_relLWIC$totals


freqs2LWIC<-na.omit(data.frame(totFreqForTable_relLWIC$word, 
                               totFreqForTable_relLWIC$freq, stringsAsFactors = F))
names(freqs2LWIC)<-c("word", "freq")

## now chapp words

rel_forplotChapp<-aggregate(df4$count, by= list(df4$word, df4$URLs), FUN= sum)

names(rel_forplotChapp)<-c("word", "URLs", "count")
subforplot_relFREQChapp<-merge(rel_forplotChapp, Totalbysenator, by = intersect(names(rel_forplotChapp), names(Totalbysenator)) )

totFreqForTable_relChapp<-aggregate(list(subforplot_relFREQChapp$count, subforplot_relFREQChapp$totals), 
                                    by= list(subforplot_relFREQChapp$word), FUN= sum)
names(totFreqForTable_relChapp)<-c("word", "counts", "totals")
totFreqForTable_relChapp$freq<-totFreqForTable_relChapp$count/totFreqForTable_relChapp$totals

freqs2chapp<-na.omit(data.frame(totFreqForTable_relChapp$word, 
                                totFreqForTable_relChapp$freq, stringsAsFactors = F))
names(freqs2chapp)<-c("word", "freq")

# now for opp words


Opt_forplot<-aggregate(df8$count, by= list(df8$word, df8$URLs), FUN= sum)

names(Opt_forplot)<-c("word", "URLs", "count")
subforplot_optFREQ<-merge(Opt_forplot, Totalbysenator, by = intersect(names(Opt_forplot), names(Totalbysenator)) )

totFreqForTable_opt<-aggregate(list(subforplot_optFREQ$count, subforplot_optFREQ$totals), 
                               by= list(subforplot_optFREQ$word), FUN= sum)
names(totFreqForTable_opt)<-c("word", "counts", "totals")
totFreqForTable_opt$freq<-totFreqForTable_opt$count/totFreqForTable_opt$totals

freqs2opt<-na.omit(data.frame(totFreqForTable_opt$word, 
                              totFreqForTable_opt$freq, stringsAsFactors = F))
names(freqs2opt)<-c("word", "freq")


# now for dhs words

DHS_forplot<-aggregate(df5$count, by= list(df5$word, df5$URLs), FUN= sum)

names(DHS_forplot)<-c("word", "URLs", "count")
subforplot_dhsFREQ<-merge(DHS_forplot, Totalbysenator, by = intersect(names(DHS_forplot), names(Totalbysenator)) )

totFreqForTable_dhs<-aggregate(list(subforplot_dhsFREQ$count, subforplot_dhsFREQ$totals), 
                               by= list(subforplot_dhsFREQ$word), FUN= sum)
names(totFreqForTable_dhs)<-c("word", "counts", "totals")
totFreqForTable_dhs$freq<-totFreqForTable_dhs$count/totFreqForTable_dhs$totals

freqs2dhs<-na.omit(data.frame(totFreqForTable_dhs$word, 
                              totFreqForTable_dhs$freq, stringsAsFactors = F))
names(freqs2dhs)<-c("word", "freq")

# now for anxiety words

anx_forplot<-aggregate(df3$count, by= list(df3$word, df3$URLs), FUN= sum)

names(anx_forplot)<-c("word", "URLs", "count")
subforplot_anxFREQ<-merge(anx_forplot, Totalbysenator, by = intersect(names(anx_forplot), names(Totalbysenator)) )

totFreqForTable_anx<-aggregate(list(subforplot_anxFREQ$count, subforplot_anxFREQ$totals), 
                               by= list(subforplot_anxFREQ$word), FUN= sum)
names(totFreqForTable_anx)<-c("word", "counts", "totals")
totFreqForTable_anx$freq<-totFreqForTable_anx$count/totFreqForTable_anx$totals

freqs2anx<-na.omit(data.frame(totFreqForTable_anx$word, 
                              totFreqForTable_anx$freq, stringsAsFactors = F))
names(freqs2anx)<-c("word", "freq")

## now for islam words 

islam_forplot<-aggregate(df2$count, by= list(df2$word, df2$URLs), FUN= sum)

names(islam_forplot)<-c("word", "URLs", "count")
subforplot_islameFREQ<-merge(islam_forplot, Totalbysenator, by = intersect(names(islam_forplot), names(Totalbysenator)) )

totFreqForTable_islam<-aggregate(list(subforplot_islameFREQ$count, subforplot_islameFREQ$totals), 
                                 by= list(subforplot_islameFREQ$word), FUN= sum)
names(totFreqForTable_islam)<-c("word", "counts", "totals")
totFreqForTable_islam$freq<-totFreqForTable_islam$count/totFreqForTable_islam$totals

freqs2islam<-na.omit(data.frame(totFreqForTable_islam$word, 
                                totFreqForTable_islam$freq, stringsAsFactors = F))
names(freqs2islam)<-c("word", "freq")



### making clouds
#pdf(file="wordClouds_senate_all.pdf", paper="letter",width = 7,height = 5)
par(mfrow=c(3,2))


wordcloud(freqs2chapp$word, freqs2chapp$freq, scale=c(4, .25), 
          random.order = FALSE, random.color = FALSE, 
          colors= c("indianred1","indianred2","indianred3","indianred"))

wordcloud(freqs2LWIC$word, freqs2LWIC$freq, scale=c(4, .4), 
          random.order = FALSE, random.color = FALSE, 
          colors= c("lightsteelblue1","lightsteelblue2","lightsteelblue3","lightsteelblue"))


wordcloud(freqs2dhs$word, freqs2dhs$freq, scale=c(4, .3), 
          random.order = FALSE, random.color = FALSE, 
          colors= c("goldenrod","goldenrod1","goldenrod2","goldenrod3"))

wordcloud(freqs2islam$word, freqs2islam$freq, scale=c(4, .5), 
          random.order = FALSE, random.color = FALSE, 
          colors= c("tomato","tomato1","tomato2","tomato3"))

wordcloud(freqs2anx$word, freqs2anx$freq, scale=c(4, .3), 
          random.order = FALSE, random.color = FALSE, 
          colors= c("seagreen1","seagreen2","seagreen3","seagreen4"))


wordcloud(freqs2opt$word, freqs2opt$freq, scale=c(4, .5), 
          random.order = FALSE, random.color = FALSE, 
          colors= c("cadetblue1","cadetblue","cadetblue3","cadetblue"))


dev.off()


####################
# moving to regression
####################


#making blank dataframe to add things too

data2year<-aggregate(data2$count, by= list(data2$year, data2$URLs), FUN= sum) # as place holder for dataframe (unique obs)
#data2year<-aggregate(data2$count, by= list(data2$URLs), FUN= sum) # as place holder for dataframe (unique obs)

df1<-as.data.frame(rep(unique(data2$URLs), each= length(unique(data2$year))))
names(df1)<-c("URLs")
df1['year']<-as.data.frame(rep(unique(data2$year), 
                                     times= length(unique(data2$URLs))))
names(df1)<-c("URLs", "year")


#df1<-as.data.frame(unique(data2$URLs))
#names(df1)<-c("URLs")
#df1['year']<-as.data.frame(rep(unique(data2$year), 
 #                                    times= length(unique(data2$URLs))))
#names(df1)<-c("URLs", "year")

## mergeing into one dataframe 

merge_1<-merge(df1, IslamWords, by = intersect(names(df1), names(IslamWords)), all.x = T)#, incomparables = NA)

merge_2<-merge(merge_1, TotalWords, by = intersect(names(merge_1), names(TotalWords)), all.x =T)#, incomparables = NA)

merge_3<-merge(merge_2, anxioutyWords, by = intersect(names(merge_2), names(anxioutyWords)), all.x =T)#, incomparables = NA)

merge_4 <- merge(merge_3, chapp, by = intersect(names(merge_3), names(chapp)), all.x =T)

merge_5 <- merge(merge_4, DHSWords, by = intersect(names(merge_4), names(DHSWords)), all.x =T)

merge_6 <- merge(merge_5, Relig, by = intersect(names(merge_4), names(Relig)), all.x =T)

#merge_6 <- merge(merge_5, religousWords_bad, by = intersect(names(merge_4), names(DHSWords)), all.x =T)

merge_7 <- merge(merge_6, opt, by = intersect(names(merge_4), names(opt)), all.x =T)

#merge_7 <- merge(merge_6, religousWords_good, by = intersect(names(merge_4), names(DHSWords)), all.x =T)


####

rightdates<-merge_7

#aggregate 
str(rightdates)
rightdates$URLs<-as.character(rightdates$URLs)
rightdates$year<-as.numeric(rightdates$year)

## getting rid of NAs, except totals
ifelse(is.na(rightdates$IslamWords), 0, rightdates$IslamWords) -> rightdates$IslamWords
ifelse(is.na(rightdates$anxioutyWords), 0, rightdates$anxioutyWords) -> rightdates$anxioutyWords
ifelse(is.na(rightdates$ReligLWICwords), 0, rightdates$ReligLWICwords) -> rightdates$ReligLWICwords
ifelse(is.na(rightdates$chappwords), 0, rightdates$chappwords) -> rightdates$chappwords
ifelse(is.na(rightdates$DHSWords), 0, rightdates$DHSWords) -> rightdates$DHSWords
ifelse(is.na(rightdates$optwords), 0, rightdates$optwords ) -> rightdates$optwords


### getting NA omitted dataset
rightdates_1<-rightdates

#get rid of month-year before aggregate 
#rightdates_1<-rightdates_1[,-2]
#rightdates_2<- rightdates_1 %>%
#  group_by(URLs, year) %>% 
#  summarise_each(funs(sum))


## if total = NA, omit
rightdates3<-na.omit(rightdates_1)

#get frequencies 
rightdates3$frequency_anxiouty= rightdates3$anxioutyWords/rightdates3$totals
rightdates3$frequency_chapp= rightdates3$chappwords/rightdates3$totals
rightdates3$frequency_religLWIC= rightdates3$ReligLWICwords/rightdates3$totals
rightdates3$frequency_reli_Lwicnoislam= (rightdates3$ReligLWICwords - rightdates3$IslamWords)/rightdates3$totals
rightdates3$frequency_dhs= rightdates3$DHSWords/rightdates3$totals
rightdates3$frequency_islam= rightdates3$IslamWords/rightdates3$totals
rightdates3$frequency_opt= rightdates3$optwords/rightdates3$totals

rightdates3$frequency_anxiouty<- rightdates3$frequency_anxiouty + 0.000000001
rightdates3$frequency_religLWIC<-rightdates3$frequency_religLWIC + 0.000000001
rightdates3$frequency_chapp<-rightdates3$frequency_chapp + 0.000000001
rightdates3$frequency_reli_Lwicnoislam<- rightdates3$frequency_reli_Lwicnoislam + 0.000000001
rightdates3$frequency_dhs<- rightdates3$frequency_dhs + 0.000000001
rightdates3$frequency_islam<- rightdates3$frequency_islam + 0.000000001
rightdates3$frequency_opt<- rightdates3$frequency_opt + 0.000000001




#### merging in controls for senators and states

controls<-read.csv("ReligionControls&Independents2.csv", header = T, stringsAsFactors = F)

## getting rid of NAs

ifelse(is.na(controls$unafilated), 0, controls$unafilated) -> controls$unafilated
ifelse(is.na(controls$other), 0, controls$other) -> controls$other
ifelse(is.na(controls$jew), 0, controls$jew) -> controls$jew
ifelse(is.na(controls$mormon), 0, controls$mormon) -> controls$mormon
ifelse(is.na(controls$othodoxchristain), 0, controls$othodoxchristain) -> controls$othodoxchristain
ifelse(is.na(controls$protestantdemonation), 0, controls$protestantdemonation) -> controls$protestantdemonation
ifelse(is.na(controls$catholic), 0, controls$catholic) -> controls$catholic

controls$URLs<-tolower(controls$last)

veryR<-read.csv("Veryreligious.csv", stringsAsFactors = F, header = F)
veryR<-unique(veryR)
names(veryR)<-c("state", "veryReligous")

controls$nonwhite[controls$aa ==1]<-1
controls$nonwhite[controls$hisp ==1]<-1
ifelse(is.na(controls$nonwhite), 0, controls$nonwhite) -> controls$nonwhite

staterelig<-read.csv("State_Religion_glenmary.csv", stringsAsFactors = F, header=T)

names(staterelig)<-c("state",  "abrvState", "catholic_state",  "jewish_state",                      
                     "evang_state", "orthodox_state" ,  "bkprot_state",    "mainline_state",          
                     "conservative.non.traditional_state", "liberal.protestant_state",  "secular_state"   )

c<-merge(controls, staterelig, by=intersect(names(controls), names(staterelig)), all.x = T)

controls2<-merge(c, veryR, by=intersect(names(c), names(veryR)), all=T)


### recode vars
controls2$upforElection[controls2$class == 1 & controls2$year =="2006"]<-1
controls2$upforElection[controls2$class == 2 & controls2$year =="2008"]<-1
controls2$upforElection[controls2$class == 3 & controls2$year =="2010"]<-1
controls2$upforElection[controls2$class == 1 & controls2$year =="2012"]<-1
ifelse(is.na(controls2$upforElection), 0, controls2$upforElection) -> controls2$upforElection

controls2$Female<-ifelse(controls2$gender == "M", 0, ifelse(controls2$gender == "F", 1, 2))
controls2$Republican<-ifelse(controls2$party == "Republican", 1, ifelse(controls2$party == "Democrat", 0, 0))


names(controls2)[42]<-c("percentwhite")
names(controls2)[44]<-c("veryconservative") 
names(controls2)[45]<-c("don'tattendchurch")
names(controls2)[46]<-c("don'tpray") 

#require(data.table)
#dt <- data.table(df)
#dt.out <- dt[, list(s.v1=sum(v1), m.v2=mean(v2)), 
#             by=c("c1","c2")]

## use rightdates3 if you want to do a beta regression (have to na omit, rather than put in zeros)
IVDV<- merge(rightdates3, controls2, by = intersect(names(rightdates3), names(controls2)))

# where we don't na omit - haven't done this yet
#IVDV_2 <- merge(rightdates2, controls2, by = intersect(names(rightdates2), names(controls))) 




#### descriptive stats of vars

Vars<- c("Freq Religious LWIC", "Freq Relig Chapp",  "Freq Relig LWIC No Islam", "Freq DHS", "State Terror Attacks", "FEMA Decs",
         "% Very Religious (State)", "% Very Conservative (State)", "% Evangelical (State)", "Don't Pray",
         "Conservatism (Senator)", "Jewish Faith (Senator)", "Mormon Faith (Senator)", "Female (Senator)", 
         "Up For Election", "Republican (Senator)")
Range<-list(range(IVDV$frequency_religLWIC), 
            range(IVDV$frequency_chapp), 
            range(IVDV$frequency_reli_Lwicnoislam), 
            range(IVDV$frequency_dhs), 
            range(IVDV$terroristattack), range(IVDV$femadec),
            range(IVDV$veryReligous), range(IVDV$veryconservative), 
            range(na.omit(IVDV$evang_state)), range(IVDV$`don'tpray`),
            range(IVDV$dw1), range(IVDV$jew), range(IVDV$mormon), 
            range(IVDV$Female), range(IVDV$upforElection), range(IVDV$Republican))
Mean <- c(mean(IVDV$frequency_religLWIC), 
          mean(IVDV$frequency_chapp),
          mean(IVDV$frequency_reli_Lwicnoislam), 
          mean(IVDV$frequency_dhs), 
          mean(IVDV$terroristattack), mean(IVDV$femadec),
          mean(IVDV$veryReligous), mean(IVDV$veryconservative), 
          mean(na.omit(IVDV$evang_state)), mean(IVDV$`don'tpray`),
          mean(IVDV$dw1), mean(IVDV$jew), mean(IVDV$mormon), 
          mean(IVDV$Female), mean(IVDV$upforElection), mean(IVDV$Republican))
SD<-  c(sd(IVDV$frequency_religLWIC), 
        sd(IVDV$frequency_chapp),
        sd(IVDV$frequency_reli_Lwicnoislam), sd(IVDV$frequency_dhs), 
        sd(IVDV$terroristattack), sd(IVDV$femadec),
        sd(IVDV$veryReligous), sd(IVDV$veryconservative), sd(na.omit(IVDV$evang_state)), sd(IVDV$`don'tpray`),
        sd(IVDV$dw1), sd(IVDV$jew), sd(IVDV$mormon), sd(IVDV$Female), sd(IVDV$upforElection), sd(IVDV$Republican))

a<-unlist(lapply(Range, `[[`, 1))
b<-unlist(lapply(Range, `[[`, 2))
descr_stats<-data.frame(Vars, a, b,  Mean, SD)
xtable(descr_stats, digits = 12)

IVDV_2<-IVDV
barplots_senator<- aggregate(list(IVDV_2$unafilated, IVDV_2$other, IVDV_2$jew, IVDV_2$mormon, IVDV_2$catholic, IVDV_2$protestantdemonation, IVDV_2$Republican),
                             by=list(IVDV_2$URLs), FUN = sum)
names(barplots_senator)<-c("URLS", "Unafiliated", "other", "jewish", "mormon", "catholic", "protestant", "republican")
barplots_senator[barplots_senator>0]<-1

Unaffil<-table(barplots_senator$Unafiliated, barplots_senator$republican)
Catholic<-table(barplots_senator$catholic, barplots_senator$republican)
Jewish<-table(barplots_senator$jewish, barplots_senator$republican)
Other<-table(barplots_senator$other, barplots_senator$republican)
Protestant<-table(barplots_senator$protestant, barplots_senator$republican)
Mormon<-table(barplots_senator$mormon, barplots_senator$republican)

Unaffil<-Unaffil[2,]
Catholic<-Catholic[2,]
Jewish<-Jewish[2,]
Other<-Other[2,]
Mormon<-Mormon[2,]
Protestant<-Protestant[2,]

religtab<-data.frame(rbind(Unaffil, Catholic, Jewish, Other, Mormon, Protestant))
names(religtab)<-c("Dems", "Repub")
religtab$relig <- row.names(religtab)

library(reshape2)
mdfr <- melt(religtab, id.vars = "relig")
names(mdfr)<-c("Religion", "Party", "Count")

#mdfr2 <- melt.data.frame(mdfr, id.vars=c("relig", "variable"))
library(scales)
p <- ggplot(mdfr, aes(Religion, Count, fill = Party)) +
  geom_col() #colour= c("#F0E442")) ) #position = "fill")) #, stat = "identity"))
p + scale_fill_manual(values=c( "blue", "red")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                        panel.background = element_blank(), axis.line = element_line(colour = "black"))
#scale_fill_hue(l=40, c=35)
# +
scale_y_continuous(labels = percent)




######
 #### regression
###
#IVDV_2<-read.csv("IVDV_2_26April2018.csv", header = T, stringsAsFactors = F)
#pdata<-read.csv("pdata26April2018.csv", header= T, stringsAsFactors = F)

b_anx<-betareg(IVDV_2$frequency_religLWIC~ IVDV_2$frequency_anxiouty, data=IVDV_2)
#bc_anx<-betareg(IVDV_2$frequency_chapp~ IVDV_2$frequency_anxiouty, data=IVDV_2)

b_dhs<-betareg(IVDV_2$frequency_religLWIC~ IVDV_2$frequency_dhs, data=IVDV_2)
#bc_dhs<-betareg(IVDV_2$frequency_chapp~ IVDV_2$frequency_dhs, data=IVDV_2)

b_opt<-betareg(IVDV_2$frequency_religLWIC~ IVDV_2$frequency_opt, data=IVDV_2)
#bc_opt<-betareg(IVDV_2$frequency_chapp~ IVDV_2$frequency_opt, data=IVDV_2)

#bDhsAnxLwic<-betareg(IVDV_2$frequency_dhs ~ IVDV_2$terroristattack + IVDV_2$frequency_anxiouty, data=IVDV_2)
#bDhsAnxChapp<-betareg(IVDV_2$frequency_dhs~ IVDV_2$frequency_chapp + IVDV_2$frequency_anxiouty, data=IVDV_2)

bDhsAnxOptLwic<-betareg(IVDV_2$frequency_religLWIC~ IVDV_2$frequency_dhs + IVDV_2$frequency_anxiouty + IVDV_2$frequency_opt, data=IVDV_2)
bDhsAnxOptChapp<-betareg(IVDV_2$frequency_chapp~ IVDV_2$frequency_dhs + IVDV_2$frequency_anxiouty + IVDV_2$frequency_opt, data=IVDV_2)





summary(b_anx)
summary(bc_anx)
summary(b_dhs)
summary(bc_dhs)
summary(b_opt)
summary(bc_opt)
summary(bDhsAnxLwic)
summary(bDhsAnxChapp)
summary(bDhsAnxOptLwic)
summary(bDhsAnxOptChapp)

stargazer(b_anx, b_dhs, b_opt, bDhsAnxOptLwic) #, bc_opt, bc_anx, bDhsAnxOptChapp)

bLwicControlsSEn<-betareg(IVDV_2$frequency_religLWIC~  IVDV_2$Female 
                                   + IVDV_2$upforElection + IVDV_2$dw1 + IVDV_2$veryReligous  
                                   +  IVDV_2$veryconservative, 
                                   data=IVDV_2)


bDhsAnxOptLwicControlsSEn<-betareg(IVDV_2$frequency_religLWIC~ IVDV_2$frequency_dhs + 
                                  IVDV_2$frequency_anxiouty + IVDV_2$frequency_opt+ IVDV_2$Female 
                                + IVDV_2$upforElection + IVDV_2$dw1, 
                                data=IVDV_2)
bDhsAnxOptChappControlsSen<-betareg(IVDV_2$frequency_chapp~ IVDV_2$frequency_dhs + 
                                   IVDV_2$frequency_anxiouty + IVDV_2$frequency_opt+ IVDV_2$Female 
                                 + IVDV_2$upforElection + IVDV_2$dw1, 
                                 data=IVDV_2)

bDhsAnxOptLwicControlsDistric<-betareg(IVDV_2$frequency_religLWIC~ IVDV_2$frequency_dhs + 
                                  IVDV_2$frequency_anxiouty + IVDV_2$frequency_opt+ IVDV_2$veryReligous  
                                  +  IVDV_2$veryconservative, 
                                data=IVDV_2)
bDhsAnxOptChappControlsDistrict<-betareg(IVDV_2$frequency_chapp~ IVDV_2$frequency_dhs + 
                                   IVDV_2$frequency_anxiouty + IVDV_2$frequency_opt + IVDV_2$veryReligous  
                                   +  IVDV_2$veryconservative, 
                                 data=IVDV_2)

summary(bDhsAnxOptLwicControlsSEn)
summary(bDhsAnxOptChappControlsSen)
summary(bDhsAnxOptLwicControlsDistric)
summary(bDhsAnxOptChappControlsDistrict)

stargazer(bDhsAnxOptLwicControlsSEn, bDhsAnxOptLwicControlsDistric, bDhsAnxOptChappControlsSen, bDhsAnxOptChappControlsDistrict)


ballChapp<-betareg(IVDV_2$frequency_chapp~ IVDV_2$frequency_dhs + 
                     IVDV_2$frequency_anxiouty + IVDV_2$frequency_opt + IVDV_2$Female 
                   + IVDV_2$upforElection + IVDV_2$dw1+ IVDV_2$veryReligous  
                   +  IVDV_2$veryconservative, 
                   data=IVDV_2)

ballreliLWIC<-betareg(IVDV_2$frequency_religLWIC~ IVDV_2$frequency_dhs + 
                        IVDV_2$frequency_anxiouty + IVDV_2$frequency_opt+ IVDV_2$Female 
                      + IVDV_2$upforElection + IVDV_2$dw1+ IVDV_2$veryReligous  
                      +  IVDV_2$veryconservative, 
                      data=IVDV_2)


stargazer(ballChapp, ballreliLWIC)

summary(ballChapp)
summary(ballreliLWIC)




brealeventsChapp<-betareg( IVDV_2$frequency_chapp~ IVDV_2$terroristattack + IVDV_2$femadec, data=IVDV_2)
brealeventsLWIC<-betareg( IVDV_2$frequency_religLWIC~ IVDV_2$terroristattack + IVDV_2$femadec, data=IVDV_2)

summary(brealeventsChapp)
summary(brealeventsLWIC)


brealeventsChappControlsnotwords<-betareg( IVDV_2$frequency_chapp~ IVDV_2$terroristattack + IVDV_2$femadec +
                                     IVDV_2$Female 
                                   + IVDV_2$upforElection + IVDV_2$dw1+ IVDV_2$veryReligous  
                                   +  IVDV_2$veryconservative, data=IVDV_2)
brealeventsLWICControlsnotwords<-betareg( IVDV_2$frequency_religLWIC~ IVDV_2$terroristattack + IVDV_2$femadec+
                                    IVDV_2$Female 
                                  + IVDV_2$upforElection + IVDV_2$dw1+ IVDV_2$veryReligous  
                                  +  IVDV_2$veryconservative, data=IVDV_2)
summary(brealeventsChappControlsnotwords)
summary(brealeventsLWICControlsnotwords)

brealeventsChappControls<-betareg( IVDV_2$frequency_chapp~ IVDV_2$terroristattack + IVDV_2$femadec +
                                     IVDV_2$frequency_dhs + 
                                     IVDV_2$frequency_anxiouty + IVDV_2$frequency_opt+ IVDV_2$Female 
                                   + IVDV_2$upforElection + IVDV_2$dw1+ IVDV_2$veryReligous  
                                   +  IVDV_2$veryconservative, data=IVDV_2)
brealeventsLWICControls<-betareg( IVDV_2$frequency_religLWIC~ IVDV_2$terroristattack + IVDV_2$femadec+
                                    IVDV_2$frequency_dhs + 
                                    IVDV_2$frequency_anxiouty + IVDV_2$frequency_opt+ IVDV_2$Female 
                                  + IVDV_2$upforElection + IVDV_2$dw1+ IVDV_2$veryReligous  
                                  +  IVDV_2$veryconservative, data=IVDV_2)

summary(brealeventsChappControls)
summary(brealeventsLWICControls)

stargazer(brealeventsChapp, brealeventsChappControlsnotwords, brealeventsChappControls, 
          brealeventsLWIC, brealeventsLWICControlsnotwords, brealeventsLWICControls)

## one senator has a weird count for this, will run later

bnoIslam<-betareg(IVDV_2$frequency_reli_Lwicnoislam~IVDV_2$frequency_anxiouty + IVDV_2$frequency_dhs + IVDV_2$terroristattack + IVDV_2$femadec +
                 IVDV_2$veryReligous + IVDV_2$Female +
                 IVDV_2$upforElection + IVDV_2$dw1, data=IVDV_2) 

bnoislamcontrols_all<-betareg( IVDV_2$frequency_reli_Lwicnoislam~IVDV_2$frequency_anxiouty + IVDV_2$frequency_dhs + IVDV_2$terroristattack + IVDV_2$femadec +
                 IVDV_2$veryReligous + IVDV_2$conservative.non.traditional_state + IVDV_2$Female + 
                 IVDV_2$upforElection + IVDV_2$dw1, data=IVDV_2) 




predicts<-cbind(
  predict(brealeventsChappControls, type = "response"),
  predict(brealeventsChappControls, type = "link"),
  predict(brealeventsChappControls, type = "precision"),
  predict(brealeventsChappControls, type = "variance"),
  predict(brealeventsChappControls, type = "quantile", at = c(0.25, 0.5, 0.75))
)

#library(ggplot2)
ggplot(IVDV_2, aes(x = frequency_anxiouty, y = frequency_chapp)) +
  geom_point(size = 4,  shape = 21) + #aes(fill = URLs),
  scale_fill_grey() +
  geom_line(aes(y = predict(brealeventsChappControls, IVDV_2),
                colour = "log-log", linetype = "log-log")) +
  # geom_line(aes(y = predict(gy_logit, GasolineYield), 
  #                colour = "logit", linetype = "logit")) +
  scale_colour_manual("", values = c("red"))+#, "blue")) +
  scale_linetype_manual("", values = c("solid")) + #, "dashed")) +
  theme_bw()


###### machine learning
#install.packages("mlr")
library(mlr)
#install.packages("caret")
library(caret)


#makeClassifTask(data=IVDV_2, target= "frequency_religLWIC")
mlrtest<-read.csv("test8July.csv", stringsAsFactors = T, header = T)
findLinearCombos(mlrtest)

library(stats)




############ On to OLS with senator fixed effects 

#install.packages("pglm")
#install.packages("simcf")
library(pglm)
#library(simcf) 

selectdata2<-IVDV_2[ order(IVDV_2$URLs, IVDV_2$year) ,]
selectdata2<-na.omit(selectdata2)
pdata <- pdata.frame(selectdata2, index=c("URLs", "year"))

ab<-plm(pdata$frequency_chapp~ pdata$frequency_anxiouty, data = pdata, start= NULL, model="pooling")
a<-plm(pdata$frequency_religLWIC~ pdata$frequency_anxiouty, data=pdata, start= NULL, model="pooling")
b<-plm(pdata$frequency_religLWIC~ pdata$frequency_dhs, data=pdata, start= NULL, model="pooling")
c<-plm(pdata$frequency_religLWIC~ pdata$frequency_opt, data=pdata, start= NULL, model="pooling")


basics<-plm(pdata$frequency_religLWIC~ pdata$frequency_anxiouty + pdata$frequency_dhs + pdata$frequency_opt +
              pdata$dw1 + pdata$upforElection + pdata$Female + pdata$veryReligous + pdata$veryconservative, data= pdata, 
            start=NULL, model = "pooling")


#pdata$frequency_reli_Lwicnoislam<- abs(pdata$frequency_reli_Lwicnoislam + 0.00000001)

### do we have a theory of time??

pdata$frequency_chapp_lag1 <- lag(pdata$frequency_chapp, 1)
pdata$frequency_religLWIC_lag1 <- lag(pdata$frequency_religLWIC, 1)
pdata$frequency_anxiouty_lag1 <- lag(pdata$frequency_anxiouty, 1)
pdata$frequency_opt_lag1 <- lag(pdata$frequency_opt, 1)
pdata$terroristattack_lag1 <- lag(pdata$terroristattack, 1)
pdata$frequency_dhs_lag1 <- lag(pdata$frequency_dhs, 1)
pdata$femadec_lag1 <- lag(pdata$femadec, 1)
pdata$upforElection_lag1 <- lag(pdata$upforElection, 1)
pdata$frequency_reli_Lwicnoislam_lag1 <- lag(pdata$frequency_reli_Lwicnoislam, 1)

pdata<-na.omit(pdata)

test<-betareg(pdata$frequency_religLWIC~ pdata$frequency_religLWIC_lag1 + pdata$frequency_anxiouty + 
                pdata$frequency_opt + pdata$frequency_dhs + pdata$dw1 + pdata$Female + pdata$veryconservative + pdata$veryReligous
              + pdata$upforElection, data = pdata)

atest<-predict(test, type = "quantile", at = c(0.05, 0.5, 0.95))
coeftest(test)
PredProb=predict(test,type='response') #predicting probabilities

par(mfrow=c(5,5))
for(i in names(pdata)){
  plot(pdata[,i],PredProb,xlab=i)
}
test1<-betareg(pdata$frequency_religLWIC~ pdata$frequency_religLWIC_lag1 + pdata$dw1 + pdata$Female + pdata$veryconservative + pdata$veryReligous
              + pdata$upforElection, data = pdata)

test2<-betareg(pdata$frequency_religLWIC ~ pdata$frequency_religLWIC_lag1 + pdata$frequency_anxiouty +
                 pdata$frequency_dhs + pdata$frequency_opt, data= pdata)

test3<-betareg(pdata$frequency_religLWIC ~ pdata$frequency_religLWIC_lag1 + pdata$femadec + pdata$terroristattack + 
                 pdata$frequency_anxiouty + 
                 pdata$frequency_opt + pdata$frequency_dhs + pdata$dw1 + pdata$Female + pdata$veryconservative + pdata$veryReligous
               + pdata$upforElection, data = pdata)
#model<-pdata$frequency_religLWIC ~ pdata$frequency_religLWIC_lag1 + pdata$femadec + pdata$terroristattack + 
#                pdata$frequency_anxiouty + 
 #                pdata$frequency_opt + pdata$frequency_dhs + pdata$dw1 + pdata$Female + pdata$veryconservative + pdata$veryReligous
   #            + pdata$upforElection
stargazer(test2, test1, test, test3)

### removing ISLAM words

test1<-betareg(pdata$frequency_reli_Lwicnoislam ~ pdata$frequency_reli_Lwicnoislam_lag1 +  pdata$dw1 + pdata$Female + pdata$veryconservative + pdata$veryReligous
              + pdata$upforElection, data = pdata)

test2<-betareg(pdata$frequency_reli_Lwicnoislam ~ pdata$frequency_reli_Lwicnoislam_lag1 + pdata$frequency_anxiouty +
                 pdata$frequency_dhs + pdata$frequency_opt, data= pdata)

test3<-betareg(pdata$frequency_reli_Lwicnoislam ~ pdata$frequency_reli_Lwicnoislam_lag1 + pdata$femadec + pdata$terroristattack + 
                 pdata$frequency_anxiouty + 
                 pdata$frequency_opt + pdata$frequency_dhs + pdata$dw1 + pdata$Female + pdata$veryconservative + pdata$veryReligous
               + pdata$upforElection, data = pdata)
               
 stargazer(test1, test2, test3)              

t<-betareg(pdata$frequency_religLWIC ~  pdata$frequency_anxiouty, data= pdata)
 
data("pdata", package = "betareg")

gy_logit <- betareg(pdata$frequency_religLWIC ~ pdata$frequency_anxiouty, data = pdata)#, subset = batch == 6)
gy_loglog <- betareg(pdata$frequency_religLWIC ~ pdata$frequency_opt, data = pdata)#, subset = batch == 6,
                   #  link = "loglog")
#Second, plot the data:
  coeff=coefficients(t)
# Equation of the line : 
eq = paste0("y = ", round(coeff[2],1), "*x + ", round(coeff[1],1))


  library(ggplot2)
g<- ggplot(pdata, aes(x= frequency_anxiouty, y= frequency_religLWIC)) + geom_point() + 
g + geom_abline(yintercept=t$coefficients[1], slope =t$coefficients[2])
  ggtitle(eq)
#geom_smooth(method="lm", formula = y ~ splines::bs(x, 3),)


g1 <- g + coord_cartesian(xlim=c(0,0.0025), ylim=c(0, 0.01))  # zooms in
g1 + ggtitle("Predicted Prob", subtitle="Religon and Anxiety") + xlab("Anxiety Words (% of Total)") + ylab("Religous Words (% of Total)")



p <- ggplot(pdata, aes(frequency_anxiouty, frequency_religLWIC)) +
  geom_point() +
  facet_wrap(~ frequency_opt)

mean_wt <- data.frame(frequency_opt = c(4, 6, 8), wt = c(2.28, 3.11, 4.00))
p + geom_hline(aes(yintercept = wt), mean_wt)


plot(g1)

plot(g)


# plot 
plot(betareg(pdata$frequency_religLWIC ~ pdata$frequency_anxiouty, data = pdata)) 

# generate linear model: 
l <- lm(y ~ x) 

# generate sequence along x-var 
# and predictions 
d <- data.frame(x.new=seq(from=-1.5, to=1.2, by=0.1)) 
d$y.new <- predict(l, data.frame(x=d$x.new)) 

# add to plot: 
lines(y.new ~ x.new, data=d, lwd=2) 


t<-betareg(pdata$frequency_dhs~ pdata$frequency_dhs_lag1 + pdata$terroristattack + pdata$globalterrorism + pdata$femadec +pdata$dw1 + pdata$Female + pdata$veryconservative + pdata$veryReligous
           + pdata$upforElection, data = pdata)

t2<-betareg(pdata$frequency_anxiouty~ pdata$frequency_anxiouty_lag1 + pdata$terroristattack + pdata$globalterrorism + pdata$femadec + pdata$dw1 + pdata$Female + pdata$veryconservative + pdata$veryReligous
            + pdata$upforElection, data = pdata)


t3<-betareg(pdata$frequency_opt~ pdata$frequency_opt_lag1+ pdata$terroristattack + pdata$globalterrorism + pdata$femadec + pdata$dw1 + pdata$Female + pdata$veryconservative + pdata$veryReligous
            + pdata$upforElection, data = pdata)


stargazer(t, t2, t3)
### setting up diff and diff

pdata$freqChapp_diff<-diff(pdata$frequency_chapp)
pdata$frequency_religLWIC_diff<-diff(pdata$frequency_religLWIC)
pdata$frequency_anxiouty_diff<-diff(pdata$frequency_anxiouty)
pdata$frequency_opt_diff<-diff(pdata$frequency_opt)
pdata$terroristattack_diff<-diff(pdata$terroristattack)
pdata$femadec_diff<-diff(pdata$femadec)
pdata$frequency_dhs_diff<-diff(pdata$frequency_dhs)
pdata$upforElection_diff<-diff(pdata$upforElection)

pdata<-na.omit(pdata)

#### chapp bivariate 
model1<-freqChapp_diff~frequency_anxiouty_diff
model1a<-freqChapp_diff~frequency_opt_diff
model1b<-freqChapp_diff~frequency_dhs_diff
model1c<-freqChapp_diff~terroristattack_diff
model1d<-freqChapp_diff~femadec_diff
model1e<-freqChapp_diff~upforElection_diff

plm.res1<-plm(model1, data=pdata, model="within")
plm.res2<-plm(model1a, data=pdata, model="within")
plm.res3<-plm(model1b, data=pdata, model="within")
plm.res4<-plm(model1c, data=pdata, model="within")
plm.res5<-plm(model1d, data=pdata, model="within")
plm.res6<-plm(model1e, data=pdata, model="within")

stargazer(plm.res1, plm.res2, plm.res3, plm.res4, plm.res5, plm.res6)
### lwic bivariteat
model1<-frequency_religLWIC_diff~frequency_anxiouty_diff
model1a<-frequency_religLWIC_diff~frequency_opt_diff
model1b<-frequency_religLWIC_diff~frequency_dhs_diff
model1c<-frequency_religLWIC_diff~terroristattack_diff
model1d<-frequency_religLWIC_diff~femadec_diff
model1e<-frequency_religLWIC_diff~upforElection_diff

plm.res1b<-plm(model1, data=pdata, model="within")
plm.res2b<-plm(model1a, data=pdata, model="within")
plm.res3b<-plm(model1b, data=pdata, model="within")
plm.res4b<-plm(model1c, data=pdata, model="within")
plm.res5b<-plm(model1d, data=pdata, model="within")
plm.res6b<-plm(model1e, data=pdata, model="within")

stargazer(plm.res1b, plm.res2b, plm.res3b, plm.res4b, plm.res5b, plm.res6b)

## adding senator controls 
model1_sencontrols<-freqChapp_diff~frequency_anxiouty_diff + Female + dw1 
model1a_sencontrols<-freqChapp_diff~frequency_opt_diff + Female + dw1 
model1b_sencontrols<-freqChapp_diff~frequency_dhs_diff+ Female + dw1 
model1c_sencontrols<-freqChapp_diff~terroristattack_diff+ Female + dw1 
model1d_sencontrols<-freqChapp_diff~femadec_diff+ Female + dw1 
model1e_sencontrols<-freqChapp_diff~upforElection_diff+ Female + dw1 

plm.res1_sencontrols<-plm(model1_sencontrols, data=pdata, model="within")
plm.res2_sencontrols<-plm(model1a_sencontrols, data=pdata, model="within")
plm.res3_sencontrols<-plm(model1b_sencontrols, data=pdata, model="within")
plm.res4_sencontrols<-plm(model1c_sencontrols, data=pdata, model="within")
plm.res5_sencontrols<-plm(model1d_sencontrols, data=pdata, model="within")
plm.res6_sencontrols<-plm(model1e_sencontrols, data=pdata, model="within")

stargazer(plm.res1_sencontrols, plm.res2_sencontrols, plm.res3_sencontrols, 
          plm.res4_sencontrols, plm.res5_sencontrols, plm.res6_sencontrols)



### lwic senator controls 
model1_sencontrols<-frequency_religLWIC_diff~frequency_anxiouty_diff + Female + dw1 
model1a_sencontrols<-frequency_religLWIC_diff~frequency_opt_diff + Female + dw1 
model1b_sencontrols<-frequency_religLWIC_diff~frequency_dhs_diff+ Female + dw1 
model1c_sencontrols<-frequency_religLWIC_diff~terroristattack_diff+ Female + dw1 
model1d_sencontrols<-frequency_religLWIC_diff~femadec_diff+ Female + dw1 
model1e_sencontrols<-frequency_religLWIC_diff~upforElection_diff+ Female + dw1 

plm.res1b_sencontrols<-plm(model1_sencontrols, data=pdata, model="within")
plm.res2b_sencontrols<-plm(model1a_sencontrols, data=pdata, model="within")
plm.res3b_sencontrols<-plm(model1b_sencontrols, data=pdata, model="within")
plm.res4b_sencontrols<-plm(model1c_sencontrols, data=pdata, model="within")
plm.res5b_sencontrols<-plm(model1d_sencontrols, data=pdata, model="within")
plm.res6b_sencontrols<-plm(model1e_sencontrols, data=pdata, model="within")

stargazer(plm.res1b_sencontrols, plm.res2b_sencontrols, plm.res3b_sencontrols, 
          plm.res4b_sencontrols, plm.res5b_sencontrols, plm.res6b_sencontrols)




#### adding state controls

model1__state<-freqChapp_diff~frequency_anxiouty_diff  + veryReligous  +  veryconservative
model1a__state<-freqChapp_diff~frequency_opt_diff  + veryReligous  +  veryconservative
model1b__state<-freqChapp_diff~frequency_dhs_diff + veryReligous  +  veryconservative
model1c__state<-freqChapp_diff~terroristattack_diff + veryReligous  +  veryconservative
model1d__state<-freqChapp_diff~femadec_diff + veryReligous  +  veryconservative
model1e__state<-freqChapp_diff~upforElection_diff + veryReligous  +  veryconservative

plm.res1__state<-plm(model1__state, data=pdata, model="within")
plm.res2__state<-plm(model1a__state, data=pdata, model="within")
plm.res3__state<-plm(model1b__state, data=pdata, model="within")
plm.res4__state<-plm(model1c__state, data=pdata, model="within")
plm.res5__state<-plm(model1d__state, data=pdata, model="within")
plm.res6__state<-plm(model1e__state, data=pdata, model="within")

stargazer(plm.res1__state, plm.res2__state, plm.res3__state, plm.res4__state, 
          plm.res5__state, plm.res6__state)

### lwic state controls 
model1__state<-frequency_religLWIC_diff~frequency_anxiouty_diff  + veryReligous  +  veryconservative
model1a__state<-frequency_religLWIC_diff~frequency_opt_diff  + veryReligous  +  veryconservative
model1b__state<-frequency_religLWIC_diff~frequency_dhs_diff + veryReligous  +  veryconservative
model1c__state<-frequency_religLWIC_diff~terroristattack_diff + veryReligous  +  veryconservative
model1d__state<-frequency_religLWIC_diff~femadec_diff + veryReligous  +  veryconservative
model1e__state<-frequency_religLWIC_diff~upforElection_diff + veryReligous  +  veryconservative

plm.res1b__state<-plm(model1__state, data=pdata, model="within")
plm.res2b__state<-plm(model1a__state, data=pdata, model="within")
plm.res3b__state<-plm(model1b__state, data=pdata, model="within")
plm.res4b__state<-plm(model1c__state, data=pdata, model="within")
plm.res5b__state<-plm(model1d__state, data=pdata, model="within")
plm.res6b__state<-plm(model1e__state, data=pdata, model="within")


stargazer(plm.res1b__state, plm.res2b__state, plm.res3b__state, plm.res4b__state, 
          plm.res5b__state, plm.res6b__state)

### combining 
model1__state<-freqChapp_diff~frequency_anxiouty_diff  + frequency_opt_diff + dw1 + veryReligous  +  veryconservative
model1a__state<-freqChapp_diff~  frequency_dhs_diff+ frequency_anxiouty_diff  + frequency_opt_diff + dw1 + veryReligous  +  veryconservative
model1b__state<-freqChapp_diff~ terroristattack_diff+ upforElection_diff + femadec_diff + frequency_dhs_diff+ frequency_anxiouty_diff  + frequency_opt_diff + dw1 + veryReligous  +  veryconservative
model1c__state<-freqChapp_diff~terroristattack_diff + frequency_dhs_diff+ frequency_anxiouty_diff  + frequency_opt_diff + dw1 + veryReligous  +  veryconservative
model1d__state<-freqChapp_diff~femadec_diff + frequency_dhs_diff+ frequency_anxiouty_diff  + frequency_opt_diff + dw1 + veryReligous  +  veryconservative
model1e__state<-freqChapp_diff~upforElection_diff + femadec_diff + frequency_dhs_diff+ frequency_anxiouty_diff  + frequency_opt_diff + dw1 + veryReligous  +  veryconservative


plm.res1__state<-plm(model1__state, data=pdata, model="within")
plm.res2__state<-plm(model1a__state, data=pdata, model="within")
plm.res3__state<-plm(model1b__state, data=pdata, model="within")
plm.res4__state<-plm(model1c__state, data=pdata, model="within")
plm.res5__state<-plm(model1d__state, data=pdata, model="within")
plm.res6__state<-plm(model1e__state, data=pdata, model="within")

stargazer(plm.res1__state, plm.res2__state, plm.res3__state, plm.res4__state, 
          plm.res5__state, plm.res6__state)


### combining 
model1__state<-frequency_religLWIC_diff~ frequency_anxiouty_diff  + frequency_opt_diff + dw1 + veryReligous  +  veryconservative
model1a__state<-frequency_religLWIC_diff~  frequency_dhs_diff+ frequency_anxiouty_diff  + frequency_opt_diff + dw1 + veryReligous  +  veryconservative
model1b__state<-frequency_religLWIC_diff~ terroristattack_diff+ upforElection_diff + femadec_diff + frequency_dhs_diff+ frequency_anxiouty_diff  + frequency_opt_diff + dw1 + veryReligous  +  veryconservative
model1c__state<-frequency_religLWIC_diff~terroristattack_diff + frequency_dhs_diff+ frequency_anxiouty_diff  + frequency_opt_diff + dw1 + veryReligous  +  veryconservative
model1d__state<-frequency_religLWIC_diff~femadec_diff + frequency_dhs_diff+ frequency_anxiouty_diff  + frequency_opt_diff + dw1 + veryReligous  +  veryconservative
model1e__state<-frequency_religLWIC_diff~upforElection + frequency_dhs_diff+ frequency_anxiouty_diff  + frequency_opt_diff + dw1 + veryReligous  +  veryconservative


plm.res1__state<-plm(model1__state, data=pdata, model="within")
plm.res2__state<-plm(model1a__state, data=pdata, model="within")
plm.res3__state<-plm(model1b__state, data=pdata, model="within")
plm.res4__state<-plm(model1c__state, data=pdata, model="within")
plm.res5__state<-plm(model1d__state, data=pdata, model="within")
plm.res6__state<-plm(model1e__state, data=pdata, model="within")

stargazer(plm.res1__state, plm.res2__state, plm.res3__state, plm.res4__state, 
          plm.res5__state, plm.res6__state)

model1c__state<-frequency_religLWIC_diff~frequency_dhs_diff + dw1 + veryReligous  +  veryconservative

plm.res4__state<-plm(model1c__state, data=pdata, model="within")



## adding other contorls
IVDV_2$terroristattack + IVDV_2$femadec +
  IVDV_2$frequency_dhs + 
  IVDV_2$frequency_anxiouty + IVDV_2$frequency_opt+ IVDV_2$ 
+ IVDV_2$upforElection + IVDV_2$+ IVDV_2$

