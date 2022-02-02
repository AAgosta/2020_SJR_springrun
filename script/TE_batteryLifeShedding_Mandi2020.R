
# FailCompare for Battery Life study 1/24/20222 AEP (or AA) ---------------

te_begin<-read.csv("data/TEDetsMarch2020.csv") 
te_middle<-read.csv("data/TEDetsMay2020.csv")
te_end<-read.csv("data/TEDetsJan14_2012.csv")
#merge these files into one
merge1<-rbind(te_begin, te_middle)
#merge2<-rbind(te_begin, te_middle, te_end) #giving me a hard with adding the third/end dataframe
#merge_all<-rbind(merge1,te_end)            #giving me a hard with adding the third/end dataframe

#create a dataframe that will work in the model (tag ID and days on)\
unique(te_begin$TagID_HEX)
unique(te_end$TagID_HEX)
#IDEA: use only the end dates. pull out the last detection date. and add the tag date per tag. 
batt<-read.csv("data/2020BattLifeDays.csv")
unique(batt$TagID_HEX)#50 unique tags, this dataframe includes the number of days the tags are active

battlife=batt$DaysEnd #create vector for model

n_obs=nrow(batt) #count number of observations
S_t=c() # empty vector
for(i in 1:n_obs){
  n_greater_t=sum(battlife>battlife[i])  # temporarily store count of failures after t[i]
  S_t[i]=n_greater_t/n_obs
}

hist(x=battlife,
     xlim=c(0,80),  # scale set at zero
     main="Battery Life Distribution",xlab="Days")

# Sample survival function
plot(x=taglife,y=S_t,
     xlim=c(0,80),   # scale set at zero
     main="Survival Function for Tag Battery Life",
     xlab="Days",ylab="S(t)")

#do not see a need to run a model because all the tags survived till the last day


# FailCOmpare for Tag Shedding 1/24/2022 AEP (AA) -------------------------

shed<-read.csv("data/2020ShedTags.csv")
library(failCompare)
library(tidyverse)
library(lubridate)

#need to convert the data to inlcude only days from data tagged to date shed and tagger
shed_days<- shed[,c(2:6)] #days2Shed is what we need for analysis

taglife=shed_days$Days2Shed #create vector for model

n_obs=nrow(shed_days) #count number of observations
S_t=c() # empty vector
for(i in 1:n_obs){
  n_greater_t=sum(taglife>taglife[i])  # temporarily store count of failures after t[i]
  S_t[i]=n_greater_t/n_obs
}

# Histogram 
hist(x=taglife,
     xlim=c(0,120),  # scale set at zero
     main="tag shedding distribution",xlab="Days")

# Sample survival function
plot(x=taglife,y=S_t,
     xlim=c(0,120),   # scale set at zero
     main="survival function for tag shedding",
     xlab="Days",ylab="S(t)")

###now seeing which model the data fits best
?fc_fit
weib_mod <- fc_fit(taglife,"weibull")
weib_mod
plot(weib_mod) #does not fit at all. 2-parameter model
#RESULTS: weibull failure model object 
#Parameter estimates:
#       est        se
#shape  1.857345 0.2281932
#scale 95.371573 7.2768381

weib_mod3 <- fc_fit(taglife,"weibull3") #make sure 2-parameter weibull model is the best fit model
weib_mod3
plot(weib_mod3)#looks exact same as 2-parameter 
#RESULTS:weibull3 failure model object 
#Parameter estimates:
#       params       std
#shape  1.8573 0.2245966
#thrsh  0.0000 0.0000000
#scale 95.3712 7.2003596

# do not run this. it keeps crashing R
#gom_mod <- fc_fit(taglife,"gompertz") #make sure 2-parameter weibull model is the best fit model
#gom_mod
#plot(gom_mod)  

gamma=fc_fit(time=taglife,model="gamma")
gamma
plot(gamma)
#REULTS: gamma failure model object 
#Parameter estimates:
#     est          se
#shape 2.11963799 0.380204234
#rate  0.02483411 0.005023067

log_n=fc_fit(time=taglife,model="lognormal")
log_n
plot(log_n)
#REULTS:lognormal failure model object 
#Parameter estimates:
#  est         se
#meanlog 4.1927222 0.11223523
#sdlog   0.8247572 0.07936224

log_logist=fc_fit(time=taglife,model="llogis")
log_logist
plot(log_logist)
#REULTS:llogis failure model object 
#Parameter estimates:

#      est        se
#shape  2.073355 0.2361655
#scale 75.385644 8.8403976

gen_gam=fc_fit(time=taglife,model="gengamma")
gen_gam
plot(gen_gam) 
#REULTS:Parameter estimates:
#      est           se
#mu     4.78752694  0.007953997
#sigma  0.01748765  0.040102554
#Q     34.76727803 79.507021512

vit.ku=fc_fit(time=taglife,model="vitality.ku")
vit.ku
plot(vit.ku,km = T,km.ci = T)
plot(vit.ku,type = "resid",km = T,km.ci = T) # does not fit
#RESULTS: vitality.ku failure model object 
#Parameter estimates:
#     params        std
#[1,] 0.00927510 7.2453e+00
#[2,] 0.00015368        NaN
#[3,] 0.00591010 9.4784e-03
#[4,] 0.01845100 1.4095e+02

vit.4p=fc_fit(time=taglife,model="vitality.4p")
vit.4p
plot(vit.4p,km = T,km.ci = T)
plot(vit.4p,type = "resid",km = T,km.ci = T)
#results:vitality.4p failure model object 
#Parameter estimates:
#     params        std
#[1,] 0.00834210 1.6968e-05
#[2,] 0.00001776 4.6477e-03
#[3,] 0.00589050 1.3313e-02
#[4,] 2.90490000 1.8759e+01

KapMer <- fc_fit(taglife,"kaplan-meier")
KapMer
plot(KapMer)#fit perfectly to observed 


#ranking the fit of the different models
fmods=fc_combine(list(weib_mod,vit.ku))
fmods
fmods=fc_fit(taglife,c(weib_mod,vit.ku))

fmods_all=fc_combine(list("weib_mod","vit.ku","vit.4p","weib_mod3","gen_gam","log_logist","gamma","log_n"))
fmods_all1=fc_fit(taglife,c("weibull","vitality.ku","vitality.4p","weibull3","gengamma","llogis","gamma","lognormal"))
fmods

fc_rank(fmods)

plot(fmods,km=T) #add km=T to add the dash lines for the K-M model connecting the estimates

#to fit all 9 models in the package use this
#fmods_all=fc_combine(list("weib_mod","weib_mod3","weibull3","gamma","log_n","log_logis","gen_gam","vit.ku","vit.4p","kaplan-meier"))




# need to run tag shedding with right-censoring becuase a lot of t --------
shed<-read.csv("data/2020ShedTags.csv")
library(failCompare)
library(tidyverse)
library(lubridate)

#need to convert the data to inlcude only days from data tagged to date shed and tagger
shed_days<- shed[,c(2:6)] #days2Shed is what we need for analysis

taglife=shed_days$Days2Shed

?fc_fit
weib_mod_censor <- fc_fit(taglife,"weibull",rc.value=120)
weib_mod_censor
plot(weib_mod_censor) #
#RESULTS: Parameter estimates:
#      est        se
#shape   0.766941  0.1543247
#scale 279.683662 99.8518614

###can not run a right censor with this model
#weib_mod3_censor <- fc_fit(taglife,"weibull3",rc.value=120) 
#weib_mod3_censor
#plot(weib_mod3_censor)

gamma_censor=fc_fit(time=taglife,model="gamma",rc.value=120)
gamma_censor
plot(gamma_censor)
#RESULTS: Parameter estimates:
#      est          se
#shape 0.764359345 0.191622305
#rate  0.002773024 0.001545653

#gen_gam_censor=fc_fit(time=taglife,model="gengamma",rc.value=120)
#gen_gam_censor
#plot(gen_gam_censor)
#Error(s) in gengamma model fitting:
#non-finite finite-difference value 

log_n_censor=fc_fit(time=taglife,model="lognormal",rc.value=120)
log_n_censor
plot(log_n_censor)
#RESULTS: Parameter estimates:
#        est        se
#meanlog 5.154268 0.3519311
#sdlog   1.794365 0.3253201

log_logist_censor=fc_fit(time=taglife,model="llogis",rc.value=120)
log_logist_censor
plot(log_logist_censor)
# Parameter estimates:
#       est         se
#shape   0.9066776  0.1729642
#scale 168.4074786 57.7935346

#gen_gam_censor=fc_fit(time=taglife,model="gengamma",rc.value=120)
#gen_gam_censor
#plot(gen_gam_censor)
#Error(s) in gengamma model fitting:
#non-finite finite-difference value


#rejects the censoring. 
#vit.ku_censor=fc_fit(time=taglife,model="vitality.ku",rc.value=120)
#vit.ku_censor
#plot(vit.ku_censor,km = T,km.ci = T)
#plot(vit.ku_censor,type = "resid",km = T,km.ci = T)
#type="resid" is stating that you want to plot the difference between K-M estimates and prediction from a parametric model ("resid")
#km = show the kaplan-meier estimates
#km.ci is stating whether you want the 95% confidence limits or not

#rejects the censoring. 
#vit.4p_censor=fc_fit(time=taglife,model="vitality.4p",rc.value=120)
#vit.4p_censor
#plot(vit.4p_censor,km = T,km.ci = T)
#plot(vit.4p_censor,type = "resid",km = T,km.ci = T)




# comparing the models that worked ----------------------------------------
# models that worked are:
#log_logist_censor, log_n_censor, gamma_censor, weib_mod3, weib_mod_censor, vit.4p, vit.ku

#fmods1=fc_combine(list(log_logist_censor, log_n_censor))
#fmods1
#fc_rank(fmods1)
#Ranked list
#  model     SSE_KM     n    npars  denom    GOF
#1    llogis 0.07388439 54     2     51     0.0014
#2 lognormal 0.09000097 54     2     51     0.0018

#fmods2(fc_combine(list(gamma_censor, weib_mod3)))
#cannot combine because of different censoring

fmods2=fc_combine(list(log_logist_censor, log_n_censor,gamma_censor,weib_mod_censor))
fmods2
fc_rank(fmods2)
#Ranked list
#  model      SSE_KM      n    npars denom    GOF
#1    llogis  0.07388439  54   2     51       0.0014
#2 lognormal  0.09000097  54   2     51       0.0018
#3   weibull  0.09521627  54   2     51       0.0019
#4     gamma  0.11384555  54   2     51       0.0022

fmods3=fc_combine(list(weib_mod3,vit.4p, vit.ku))
fmods3
fc_rank(fmods3)
#Ranked list
#  model         SSE_KM      n    npars  denom   GOF
#1 vitality.ku   0.08393626  54     4    49      0.0017
#2 vitality.4p   0.19866431  54     4    49      0.0041
#3    weibull3   1.79444562  54     3    50      0.0359

fmod_fit1 <- fc_fit(time=taglife,c("llogis", "lognormal","gamma","weibull"),SE=TRUE, censorID = NULL,rc.value=120)
plot(fmod_fit1)

fmod_fit2 <- fc_fit(time=taglife,c("weibull3", "vitality.4p","vitality.ku"),SE=TRUE, censorID = NULL)
plot(fmod_fit2)

fc__test(times=taglife,model=fmod_fit1,iters=500)
#check with Steve if there is still an fc_test in the new failCompare package