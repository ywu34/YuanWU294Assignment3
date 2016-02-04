#Econ 294A2 Assignment 3
print("Name:Yuan Wu
      Student ID:1307193
      Email:ywu34@ucs.edu")
#Question 1

library(foreign)
df.ex<-read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")
#loading up data set.

install.packages("dplyr")
library(dplyr)
#installing package dplyr
##CAUTION:INSTALLING NEW PACKAGE WILL CAUSE SCRIPT TO STOP RUNNING AND REBOOT RSTUDIO

dplyr::filter
#opening filter functions from dplyr package


#Question 2

sub2013<-filter(df.ex, year=="2013"&month=="12")
#creating subset of year 2013 and month 12 using filter

print(nrow(sub2013))
print("There are 13261 observations in the last month of 2013.")
#printing remaining number of observations in December 2013

subsummer<-filter(df.ex, year=="2013"&month %in% c("7","8","9"))
#creating subset of year 2013 and month 7,8 and 9 as Summer, using filter

print(nrow(subsummer))
print("There are 39657 observations in Summer 2013.")
#prtingting number of observations in Summer 2013

#Question 3

df.ex.3a<-df.ex %>% arrange(year, month)
#creating data frame sorted with year and month ascending

#Question 4

df.ex.4a<-select(df.ex, year:age)
#creating data frame with only columns year through age

df.ex.4b<-select(df.ex, year, month, starts_with("i"))
#creating data drame with only columns year, month and columns that start with i

distinct(select(df.ex, state))
print("There are 51 distinct values.")
#printing number of distinct values in state

#Question 5

stndz <- function(x){
  (x - mean(x, na.rm = T))  /  sd(x, na.rm = T)
}
#creating function for standard score

nrmlz <- function(x){
  (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
}
#creating function for feature scaling

dplyr::mutate
#opening matate functions from dplyr package

df.ex.5a<-
df.ex %>% mutate(
    rw.stndz=stndz(rw),
    rw_nrmlz=nrmlz(rw)
    )
#creating data frame with standard scores and feature scaled values

df.ex.5b<-
  df.ex %>% 
  group_by(year, month)%>%
  mutate(
    rw.stndz=stndz(rw),
    rw_nrmlz=nrmlz(rw),
    count       = n()
  )
#creating data frame with standard scores, featured scaled values and count at year month groupings.

#Question 6

df.ex.6<-
  df.ex %>% 
  group_by(year, month, state)%>%
  summarise(
    rw_min=min(rw, na.rm=T),
    rw_1stq=quantile(rw,0.25, na.rm=T),
    rw_mean=mean(rw, na.rm=T),
    rw_median=median(rw, na.rm=T),
    rw_3rdq=quantile(rw,0.75, na.rm=T),
    rw_max=max(rw, na.rm=T),
    count       = n()
  )%>%
  select(state,starts_with("rw_"), count)
#creating data frame with min, 1st and 3rd quantiles, mean, median, max, standar scores and feature scaled values.

df.ex.6b<-filter(df.ex.6, rw_mean==max(df.ex.6$rw_mean))
#creating a subset contains the row with the highest mean real wage

print(df.ex.6b)
print("In December 2013 in Washinton DC has the highest mean real wage.")
#prain and report the row with the combinition yields the highest mean real wage.

#Question 7

df.ex$state.char<- as.character(df.ex$state)
#creating new column that converts state from factor data to character data

df.ex.7a<-
  df.ex %>% arrange(year, month, desc(state.char))
#creating new data frame that is arranged by ascending year and month, and by state in descending alphabetical order