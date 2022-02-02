#Interested in seeing if there is a tagger effect because 2020 had such a high shed tagging rate
#using a contingency table and chi-squared test to 

TagEffect <- read.csv("data/2020ShedTags - contingencyTableData.csv")
#using this link to guide this code http://www.r-tutor.com/elementary-statistics/goodness-fit/chi-squared-test-independence and https://www.r-bloggers.com/2020/12/contingency-tables-in-r/ accessed 1/11/2022

#null hypothesis: the distribution into the two tag effect categories is homogenous across taggers

library(MASS)
library(ISLR)
library(tidyverse)
library(Rfast)

str(TagEffect)
TagEffect$Shed_NotShed
table(TagEffect$Shed_NotShed)
table(TagEffect$Tagger)

#table(TagEffect$Shed_NotShed,TagEffect$Tagger)
table(TagEffect$Tagger,TagEffect$Shed_NotShed)
#decided to use this format because I want the tagger as the rows and the shedding as the columns

chisq.test(TagEffect$Tagger,TagEffect$Shed_NotShed)
#Pearson's Chi-squared test
#data:  TagEffect$Tagger and TagEffect$Shed_NotShed
#X-squared = 1.782, df = 3, p-value = 0.6189
#There is a 75-50% chance of exceeding the critical value
#RB says this case is testing for homogeneity. We have a high p-value therefore there is little eveidence AGAINST the null for homogeneity 


# downstream detections for tagger effects --------------------------------
#1/12/2022

#interested in seeing the average downstream arrival time separated by tagger and number of tags detected downstream
# sites I am interested in Hills, Grayson, and Mossdale for now. If I need to look further downstream then I will pool together a bunch of sites. 

library(tidyverse)
library(dplyr)
library(ggplot2)
head(ALLRel_visits2020_firstlast_nofilter)
dat <- read.csv("data_output/DetectionFiles/ALLRel_visits2020_firstonly_All_NOAAdata_01092022_NoPredFilter.csv")
#using only the first detections at each site because I am interested in how far fish/Tag IDs travelled per tagger to see if there is a tagger effect (ex: shedding)

ALLDets_byTagger<-dat[,c(2:5,13,15,21)]

#change surgeon,location and Tag ID to as.factor
ALLDets_byTagger$Surgeon<-as.factor(ALLDets_byTagger$Surgeon)
ALLDets_byTagger$GeneralLocation<-as.factor(ALLDets_byTagger$GeneralLocation)
ALLDets_byTagger$TagID_Hex<-as.factor(ALLDets_byTagger$TagID_Hex)
ALLDets_byTagger$GPS.Names<-as.factor(ALLDets_byTagger$GPS.Names)
# need to figure out how convert the date/time to POSIXct 1/12/2022
ALLDets_byTagger$DetectDateTime<-as.POSIXct(ALLDets_byTagger$DetectDateTime,format="%m/%d/%Y %H:%M:%S")

#create a new df separated by tag at its furthest distance or minimum rkm
rkm<-ALLDets_byTagger %>% 
  group_by(TagID_Hex, rkm)
#for CH data
rkm_CH = rkm[rkm$Surgeon=="CH", ]
rkm_CH_min<- rkm_CH %>% 
  group_by(TagID_Hex)%>%
  summarize(
    MaxRKMbyTagID = max(rkm, na.rm = T),
    MinRKMbyTagID = min(rkm, na.rm = T)
  ) %>%
  arrange(TagID_Hex)

range(rkm_CH_min[,3]) #52.04-270.57 (smaller number means it is closer to ocean)
mean(rkm_CH_min$MinRKMbyTagID) #206.0061

13/27 #48.48% shed in tag effects

#for AA data
rkm_AA = rkm[rkm$Surgeon=="AA", ]
rkm_AA_min<- rkm_AA %>% 
  group_by(TagID_Hex)%>%
  summarize(
    MaxRKMbyTagID = max(rkm, na.rm = T),
    MinRKMbyTagID = min(rkm, na.rm = T)
  ) %>%
  arrange(TagID_Hex)

range(rkm_AA_min[,3]) #0.80-270.57
mean(rkm_AA_min$MinRKMbyTagID) #187.82

5/16 #31.25% shed in tag effects

#for WX data
rkm_WX = rkm[rkm$Surgeon=="WX", ]
rkm_WX_min<- rkm_WX %>% 
  group_by(TagID_Hex)%>%
  summarize(
    MaxRKMbyTagID = max(rkm, na.rm = T),
    MinRKMbyTagID = min(rkm, na.rm = T)
  ) %>%
  arrange(TagID_Hex)

range(rkm_WX_min[,3]) #108.92-270.52
mean(rkm_WX_min$MinRKMbyTagID) #180.2424

2/7 #28.29% shed in tag effects

#for GS data
rkm_GS = rkm[rkm$Surgeon=="GS", ]
rkm_GS_min<- rkm_GS %>% 
  group_by(TagID_Hex)%>%
  summarize(
    MaxRKMbyTagID = max(rkm, na.rm = T),
    MinRKMbyTagID = min(rkm, na.rm = T)
  ) %>%
  arrange(TagID_Hex)

range(rkm_GS_min[,3]) #?-270.57   71.73 NOT ACCURATE BECAUSE IT IS FOR 2/5/2022.  CANT FIGURE OUT HOW TO REMOVE IT
mean(rkm_GS_min$MinRKMbyTagID) #245.0658

2/4 #50.00% shed in tag effects

#max and min for all taggers
all_surgeons<-rkm %>% 
  group_by(TagID_Hex,Surgeon)%>%
  summarize(
    MaxRKMbyTagID = max(rkm, na.rm = T),
    MinRKMbyTagID = min(rkm, na.rm = T)
  ) %>%
  arrange(Surgeon)

#det_bysite_CH$time_spent = difftime(det_bysite_CH$last,det_bysite_CH$first)



# ANOVA analysis -----------------------------------------------------------
#1-way ANOVA on the distance to last detection with tagger as the independent factor (from RB email 1/13/2022) NULL = distance for last detection is not affected by surgeon (basic version is the means of the different groups is the same)

#get "all_surgeon from above code to pull in csv and format it and all that good good
str(all_surgeons)
#change surgeon to factor
#all_surgeons$Surgeon<-as.factor(all_surgeons$Surgeon)
levels(all_surgeons$Surgeon)
library(dplyr)
group_by(all_surgeons,Surgeon) %>% 
  summarize(
    cound = n(),
    mean = mean(MinRKMbyTagID, na.rm = TRUE),
    sd = sd(MinRKMbyTagID, na.rm = TRUE)
  )
# A tibble: 4 x 4
#Surgeon count  mean  sd
#<fct>   <int>  <dbl>  <dbl>
#1 AA     198   188.   64.4
#2 CH     310   206.   57.3
#3 GS     64    245.   46.2
#4 WX     86    180.   42.6

#one_way<-aov(yield~MinRKMbyTagID,data=)
aov1 <- aov(MinRKMbyTagID~Surgeon, data = all_surgeons)
summary(aov1)

#RESULTS
#              Df  Sum      Sq Mean  Sq F value   Pr(>F)    
#Surgeon       3   203493   67831    20.92        5.93e-13 ***
#Residuals   654   2120164  3242                     
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#this seems wildly overwhelming. but I realized that not everyone tagged evenly between the release groups so I need to separate by release groups also!!! then run this again. 1/15/2022


######1/17/2022 separating by release groups
library(tidyverse)
library(dplyr)
library(ggplot2)
dat <- read.csv("data_output/DetectionFiles/ALLRel_visits2020_firstonly_All_NOAAdata_01092022_NoPredFilter.csv")
ALLDets_byrelease<-dat[,c(2:5,7,13,15,21)]

ALLDets_byrelease$Surgeon<-as.factor(ALLDets_byrelease$Surgeon)
ALLDets_byrelease$GeneralLocation<-as.factor(ALLDets_byrelease$GeneralLocation)
ALLDets_byrelease$TagID_Hex<-as.factor(ALLDets_byrelease$TagID_Hex)
ALLDets_byrelease$GPS.Names<-as.factor(ALLDets_byrelease$GPS.Names)
ALLDets_byrelease$Rel_group<-as.factor(ALLDets_byrelease$Rel_group)
# need to figure out how convert the date/time to POSIXct 1/12/2022
ALLDets_byrelease$DetectDateTime<-as.POSIXct(ALLDets_byrelease$DetectDateTime,format="%m/%d/%Y %H:%M:%S")

rkm_withrelease<-ALLDets_byrelease %>% 
  group_by(TagID_Hex, Rel_group)

#release 1: upper release site Fremont ford
rkm_Rel1 = rkm_withrelease[rkm_withrelease$Rel_group=="UpperRel", ] #created the new df to include only the upper release data

#setting up for the ANOVA info
levels(rkm_Rel1$Surgeon)
library(dplyr)
rkm_Rel1_maxmin<-rkm_Rel1 %>% 
  group_by(TagID_Hex,Surgeon)%>%
  summarize(
    MaxRKMbyTagID = max(rkm, na.rm = T),
    MinRKMbyTagID = min(rkm, na.rm = T)
  ) %>%
  arrange(Surgeon)#creating df with max and mins per surgeon

group_by(rkm_Rel1_maxmin,Surgeon) %>% 
    summarize(
      cound = n(),
      mean = mean(MinRKMbyTagID, na.rm = TRUE),
      sd = sd(MinRKMbyTagID, na.rm = TRUE)) 
#resulting table for anova 
# A tibble: 4 x 4
#Surgeon cound  mean    sd
#  <fct>   <int> <dbl> <dbl>
#1 AA         93  235.  58.5
#2 CH        169  245.  45.1
#3 GS         64  245.  46.2
#4 WX         17  255.  30.7

#1-way ANOVA for Upper release - release 1
aov_Rel1 <- aov(MinRKMbyTagID~Surgeon, data = rkm_Rel1_maxmin)
summary(aov_Rel1)
#the travel distances are not significantly different for Release 1: upper release. P = 0.295
  
#Release 2: Delta release 
rkm_Rel2 = rkm_withrelease[rkm_withrelease$Rel_group=="LowerRel", ]
#created the new df to include only the Lower release data

#setting up for the ANOVA info
levels(rkm_Rel2$Surgeon)
rkm_Rel2_maxmin<-rkm_Rel2 %>% 
  group_by(TagID_Hex,Surgeon)%>%
  summarize(
    MaxRKMbyTagID = max(rkm, na.rm = T),
    MinRKMbyTagID = min(rkm, na.rm = T)
  ) %>%
  arrange(Surgeon)#creating df with max and mins per surgeon

group_by(rkm_Rel2_maxmin,Surgeon) %>% 
  summarize(
    cound = n(),
    mean = mean(MinRKMbyTagID, na.rm = TRUE),
    sd = sd(MinRKMbyTagID, na.rm = TRUE)) 
#resulting table for anova
# A tibble: 3 x 4
#Surgeon cound  mean    sd
#<fct>   <int> <dbl> <dbl>
#1 AA         79  159.  22.1
#2 CH        131  163.  21.7
#3 WX         69  162.  17.4

#1-way ANOVA for Lower release - release 2
aov_Rel2 <- aov(MinRKMbyTagID~Surgeon, data = rkm_Rel2_maxmin)
summary(aov_Rel2)
#the travel distances are not significantly different for Release 1: upper release  p = 0.394

#Release 3: Franks Tract
rkm_Rel3 = rkm_withrelease[rkm_withrelease$Rel_group=="FrankTract", ]
#created the new df to include only the Franks Tract release data

#setting up for the ANOVA info
levels(rkm_Rel3$Surgeon)
rkm_Rel3_maxmin<-rkm_Rel3 %>% 
  group_by(TagID_Hex,Surgeon)%>%
  summarize(
    MaxRKMbyTagID = max(rkm, na.rm = T),
    MinRKMbyTagID = min(rkm, na.rm = T)
  ) %>%
  arrange(Surgeon)#creating df with max and mins per surgeon

group_by(rkm_Rel3_maxmin,Surgeon) %>% 
  summarize(
    cound = n(),
    mean = mean(MinRKMbyTagID, na.rm = TRUE),
    sd = sd(MinRKMbyTagID, na.rm = TRUE)) 
#resulting table for anova
# A tibble: 2 x 4
#Surgeon    cound  mean    sd
#<fct>     <int> <dbl> <dbl>
#1 AA         26 104.   12.2
#2 CH         10  99.5  18.0

#1-way ANOVA for Lower release - release 2
aov_Rel3 <- aov(MinRKMbyTagID~Surgeon, data = rkm_Rel3_maxmin)
summary(aov_Rel3)
#the travel distances are not significantly different for Release 1: upper release  p = 0.436

#####2-way ANOVA to account for Surgeon and release
#need to set up the dataframe for this
levels(rkm_withrelease$Surgeon)
levels(rkm_withrelease$Rel_group)
rkm_all_maxmin<-rkm_withrelease %>% 
  group_by(TagID_Hex,Surgeon,Rel_group)%>%
  summarize(
    MaxRKMbyTagID = max(rkm, na.rm = T),
    MinRKMbyTagID = min(rkm, na.rm = T)
  ) %>%
  arrange(Surgeon)

#2-way ANOVA with tukey
#lm1 = lm(MinRKMbyTagID~Rel_group, data = rkm_all_maxmin)
#summary(lm1)
#library(emmeans)
#emmeans(lm1,list(pairwise~Rel_group),adjust="tukey")
###RESULTS
#$`emmeans of Rel_group`
#Rel_group  emmean   SE  df lower.CL upper.CL
#FrankTract    102 6.32 655     90.1      115
#LowerRel      162 2.27 655    157.5      166
#UpperRel      243 2.05 655    239.0      247

#Confidence level used: 0.95 

#$`pairwise differences of Rel_group`
#1                     estimate   SE  df t.ratio p.value
#FrankTract - LowerRel    -59.5 6.72 655  -8.856  <.0001
#FrankTract - UpperRel   -140.6 6.64 655 -21.160  <.0001
#LowerRel - UpperRel      -81.1 3.06 655 -26.529  <.0001

#P value adjustment: tukey method for comparing a family of 3 estimates 

##there is a significant difference in the travel distance between releases. this did not include the surgeons. Now want to test for surgeon effect

rkm_all_maxmin$Rel_group<-as.factor(rkm_all_maxmin$Rel_group)
lm2 = lm(MinRKMbyTagID~Rel_group*Surgeon, data = rkm_all_maxmin)
summary(lm2)
library(emmeans)
emmeans(lm2,list(pairwise~Rel_group*Surgeon),adjust="tukey")
#significant difference in stance travelled between all taggers but AA-WX. need to test this with release as a comarison now
####*=interaction effect which is what I did for this one

lm3 = lm(MinRKMbyTagID~Rel_group+Surgeon, data = rkm_all_maxmin)
summary(lm3)
library(emmeans)
emmeans(lm3,list(pairwise~Rel_group+Surgeon),adjust="tukey")

#aov_1 <- aov(MinRKMbyTagID~Surgeon, data = rkm_all_maxmin)
#summary(aov_1) 
#not what we want because we are interested in both rel group and surgeon
aov_2<-aov(MinRKMbyTagID~Rel_group+Surgeon, data=rkm_all_maxmin)
summary(aov_2)

# PLOTS -------------------------------------------------------------------

library(ggplot2)
#PDF name "BoxWhisker_Rel1_rkm_withOutliars"
#ggplot(rkm_Rel1_maxmin, aes(x=Surgeon, y=MinRKMbyTagID))+
#  geom_boxplot()

#ggplot(rkm_Rel2_maxmin, aes(x=Surgeon, y=MinRKMbyTagID))+
#  geom_boxplot()

#ggplot(rkm_Rel3_maxmin, aes(x=Surgeon, y=MinRKMbyTagID))+
#  geom_boxplot()

ggplot(rkm_all_maxmin, aes(x=Surgeon, y=MinRKMbyTagID))+
  geom_boxplot()+
  scale_fill_brewer(palette="Paired")+
  xlab("Surgeon")+
  ylab("River km from ocean")+
  facet_wrap(~Rel_group)+
  theme(axis.title=element_text(size=12), axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))

#could create some of the plots in this URL too http://www.sthda.com/english/wiki/two-way-anova-test-in-r