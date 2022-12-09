       ## Importing data into R-Studio
stidata_unclean <- read_dta("C:/Users/DELL/OneDrive/Desktop/STATA TRAINING/stidata_unclean.dta")
attach(stidata_unclean)
### Installing packages into the current working environment
install.packages("tidyverse")
install.packages("haven")
install.packages("foreign")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("table1")
install.packages("tinytex")
install.packages("bit64")
install.packages("labelled")
install.packages("kableExtra")
install.packages("readxl")
install.packages("stargazer")

##install.packages("rtools")
### Loading the packages into the current working environment
library(readxl)
library(kableExtra)
library(tinytex)
library(haven)
library(foreign)
library(dplyr)
library(table1)
library(bit64)
library(labelled)
library(ggplot2)
library(star)
library(labelled)
library(tidyverse)
library(stargazer)
### DATA CLEANING/WRANGLING
  
 ##Checking for the inconsistency in the variable(casestatus)

# Extracking the inconsistent values in the var casestatus 
# Extracking the duplicated variables from the idnumbers(1&31)& age(30&23) respectively
attach(stidata_unclean)

stidata_unclean$casestatus[stidata_unclean$idnumber==1 & stidata_unclean$a1age==30]=1
stidata_unclean$casestatus[stidata_unclean$idnumber==31 & stidata_unclean$a1age==23]=1
table(stidata_unclean$casestatus)
#View(stidata_unclean$casestatus)
# Drop duplicated record with idnumber =51 & a1age=23
stidata_unclean<-stidata_unclean[!(stidata_unclean$idnumber==51 & stidata_unclean$a1age==23) ,]


#### 1. Caseststus
#### Converting casestatus variable into binary(0,1)
sti<-NA
sti[casestatus==1]=1
sti[casestatus==2]=0
sti[casestatus==3]=0
table(sti)

#### Labeling variables and assigning labels

sti<-factor(sti,levels=c(1,0),labels=c("Positive","Negative"))

var_label(sti)<-"Sti casestatus"

table(sti)

table1(~sti)


#### 2.Viewing Church/a3church/religion

table(stidata_unclean$a3church)

#creating a dummy variable~a3church_new

##converting the var into binary(0,1)

stidata_unclean$a3church_new<-NA

stidata_unclean$a3church_new[stidata_unclean$a3church=="7 roman catholic"]=1

stidata_unclean$a3church_new[stidata_unclean$a3church=="2 apostolic"|stidata_unclean$a3church=="3 methodist"|stidata_unclean$a3church=="4 anglican"|stidata_unclean$a3church=="5 pentecostal"|stidata_unclean$a3church=="6 atheist"| stidata_unclean$a3church=="8 other"]=0

table(stidata_unclean$a3church_new)


stidata_unclean$a3church_new<-factor(stidata_unclean$a3church_new,levels=c(1,0),labels=c("Roman Catholic","Non Catholic"))

table(stidata_unclean$a3church_new)

var_label(stidata_unclean$a3church_new)<-"Religion"



#### **3.View occupation/a2occupation**

#View(stidata_unclean$a2occupation)

table(stidata_unclean$a2occupation)

stidata_unclean$a2occupation_new<-NA

stidata_unclean$a2occupation_new[stidata_unclean$a2occupation=="1 unemployed"|stidata_unclean$a2occupation=="4 student"]=1

stidata_unclean$a2occupation_new[stidata_unclean$a2occupation=="2 informal"|stidata_unclean$a2occupation=="3 formal"]=0

table(stidata_unclean$a2occupation_new)

#### View stidata_unclean$a2occupation_new)

### labelling variables & assingning levels labels

stidata_unclean$a2occupation_new<-factor(stidata_unclean$a2occupation_new,levels=c(1,0),labels=c("Unemployed","Employed"))

var_label(stidata_unclean$a2occupation_new)<-"Occupation Status"

table(stidata_unclean$a2occupation_new)

table1(~stidata_unclean$a2occupation_new)

## checking for the inconsistency in casestatus

stidata_unclean<-stidata_unclean[order(stidata_unclean$casestatus), ]

#View(stidata_unclean$casestatus)
#View(stidata_unclean[stidata_unclean$casestatus==3,])
## Extracking the inconsistent values in the var casestatus 

stidata_unclean$casestatus[stidata_unclean$idnumber==1 & stidata_unclean$a1age==30]=1
stidata_unclean$casestatus[stidata_unclean$idnumber==31 & stidata_unclean$a1age==23]=1
table(stidata_unclean$casestatus)
#View(stidata_unclean$casestatus)
# Drop duplicated record with idnumber =51 & a1age=23

## 4.Marital status variable

table(stidata_unclean$a5maritalstatus)
stidata_unclean$a5maritalstatus_new<-NA
stidata_unclean$a5maritalstatus_new[stidata_unclean$a5maritalstatus=="1 single"|stidata_unclean$a5maritalstatus=="4 divorcee"|stidata_unclean$a5maritalstatus=="5 widowed"]=1
stidata_unclean$a5maritalstatus_new[stidata_unclean$a5maritalstatus=="2 married"|stidata_unclean$a5maritalstatus=="3 co-habiting"]=0
table(stidata_unclean$a5maritalstatus_new)

stidata_unclean$a5maritalstatus_new<-factor(stidata_unclean$a5maritalstatus_new,levels=c(1,0),labels=c("Un-married","Married"))
table(stidata_unclean$a5maritalstatus_new)
## labelling variable marital status
var_label(stidata_unclean$a5maritalstatus_new)<-"Marital Status"
table1(~stidata_unclean$a5maritalstatus_new)


## 5.Education/a4levelofeducation

table(stidata_unclean$a4levelofeducation) 

stidata_unclean$education_new<-NA                                 
stidata_unclean$education_new[stidata_unclean$a4levelofeducation=="1 none"| stidata_unclean$a4levelofeducation=="2 primary"]=1
stidata_unclean$education_new[stidata_unclean$a4levelofeducation=="3 secondary"| stidata_unclean$a4levelofeducation=="4 tertiary"]=0
table(stidata_unclean$education_new)


#View(stidata_unclean$education_new)
stidata_unclean$education_new<-factor(stidata_unclean$education_new,levels=c(1,0),labels=c("Primary and below","Secondary and above"))
##View(stidata_unclean$education_new)
var_label(stidata_unclean$education_new)<-"Education level"

table1(~stidata_unclean$education_new)

## 6.Sex/sex1
table(stidata_unclean$sex1)
##View(stidata_unclean$sex1)
stidata_unclean$sex1_new<-NA
stidata_unclean$sex1_new[stidata_unclean$sex1==1]=1
stidata_unclean$sex1_new[stidata_unclean$sex1==2]=0
table(stidata_unclean$sex1_new)

stidata_unclean$sex1_new<-factor(stidata_unclean$sex1_new,levels=c(1,0),labels=c("Female","Male"))
var_label(stidata_unclean$sex1_new)<-"Gender/Sex"

table1(~stidata_unclean$sex1_new)
### 7.Use of Condom

table(stidata_unclean$n11usedcondom)
str(stidata_unclean$n11usedcondom)
stidata_unclean$n11usedcondom_New<-NA
stidata_unclean$n11usedcondom_New[stidata_unclean$n11usedcondom=="1 yes"]=1
stidata_unclean$n11usedcondom_New[stidata_unclean$n11usedcondom=="2 No"]=0
table(stidata_unclean$n11usedcondom_New)
stidata_unclean$n11usedcondom_New<-factor(stidata_unclean$n11usedcondom_New,levels=c(1,0),labels=c("Yes","No"))
var_label(stidata_unclean$n11usedcondom_New)<-("Condom use")
table(stidata_unclean$n11usedcondom_New)
table1(~stidata_unclean$n11usedcondom_New)



### 8.Age/a1age
table(stidata_unclean$a1age)
stidata_unclean$agecat<-NA
stidata_unclean$agecat[stidata_unclean$a1age<=35]=1
stidata_unclean$agecat[stidata_unclean$a1age>35]=0
table(stidata_unclean$agecat)
stidata_unclean$agecat<-factor(stidata_unclean$agecat,levels=c(1,0),labels=c("Aged below 35","Aged above 35"))
var_label(stidata_unclean$agecat)<-"Age category"

table(stidata_unclean$agecat)
table1(~stidata_unclean$agecat)

### 9.No of sex partners in one year
table(stidata_unclean$sexpartner1year)

##View(stidata_unclean$sexpartner1year)
stidata_unclean$sexpartner1year_new<-NA
stidata_unclean$sexpartner1year_new[stidata_unclean$sexpartner1year==0]=0
stidata_unclean$sexpartner1year_new[stidata_unclean$sexpartner1year==1]=1
table(stidata_unclean$sexpartner1year_new)
stidata_unclean$sexpartner1year_new<-factor(stidata_unclean$sexpartner1year_new,levels=c(1,0),labels=c("Yes","No"))
var_label(stidata_unclean$sexpartner1year_new)<-"Number of sex partners per year"
table(stidata_unclean$sexpartner1year_new)
var_label(stidata_unclean$sexpartner1year_new)<-"Had Sex partner in a year "
table1(~stidata_unclean$sexpartner1year_new)

#View(stidata_unclean)


#1.Descriptive Statistics
attach(stidata_unclean)

##
model2<-glm(sti~factor(agecat)+factor(a5maritalstatus_new)+factor(sex1_new),family=binomial,data=stidata_unclean)
model2

stargazer(model2,type="text")

##model2<-lm(~sti+sex1_new,data=stidata_unclean)
model2

model_1<-lm(sti~sex1_new+a5maritalstatus_new,data=stidata_unclean)
table1(~sti+sex1_new+sexpartner1year_new+a5maritalstatus_new)




table1(~ agecat+sex1_new+education_new+a2occupation_new+a3church_new+a5maritalstatus_new+n11usedcondom_New+
         
         sexpartner1year_new|sti, caption = " Sexually Transmitted Infections Prevalence",
       
       overall = 'Total', data =stidata_unclean)


pvalue<-function(x,...){
  # Construct vectors of dependent y, and groups (strata) g
  y<-unlist(x)
  g<- factor(rep(1:length(x),times=sapply(x, length)))
  if(is.numeric(y)){
    p<-t.test(y~g)$p.value
  }
  else{
    p<-chisq.test(table(y,g))$p.value
  }
  c("", sub("<", "<", format.pval(p, digits = 3,eps = 0.001)))
}
library(table1)
library(stargazer)
library( stargazer)

table1(~agecat+sex1_new+education_new+a2occupation_new+a3church_new+a5maritalstatus_new+n11usedcondom_New+
         sexpartner1year_new|sti, caption = " Testing the null hypothesis that socio-demographic factors does not influence the sti prevalence",
       overall = 'Total', extra.col=list("P-value"=pvalue),data =stidata_unclean)


#View(sexpartner1year_new)
















