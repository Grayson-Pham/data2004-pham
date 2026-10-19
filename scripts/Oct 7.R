# weekly assignment review 

# now let's review the reading together, briefly 

## what does is.na() do? 

#it returns a Boolean value of whether or not there is an NA value

## what does na.rm = TRUE mean?

#it removes the missing values from a column of data allowing you to use functions like mean or median 

## what is the differnece between an explicit and implicit missing value?

#explicit values are labeled as missing, like NA
#implicit values just not in the data or absent

# brief introduction to statistical understanding of missingness: 
## there are (basically) three kinds of missingness that statisticians evaluate 

#M.A.R Missing at random
#MNAR Missing not at random
#MCAR missing completely at random


# packages 
library(tidyverse)

# we will get our data from this package today 
# install.packages("NHANES")
library(NHANES)

# this is the CDC's National Health and Nutrition Examination Survey; 
# interviews, physical exams, and lab tests, from 2009 to 2012. 
# use ?NHANESraw for documentation

# note: make sure you use NHANESraw NOT NHANES. NHANES is a weighted survey to 
# give inferences about the US population; some people are repeated to weight 
# their respective groups heavier. 


glimpse(NHANESraw)

#one Row represents one participant at health and nutriton survey 2009-2012

# again we'll start with a simple question: what share of adults currently smoke? 
NHANESraw |> 
  summarise(pct_smoke = mean(SmokeNow == "Yes", na.rm = TRUE))

table(NHANESraw$SmokeNow)

# is that reasonable? is something else going on here? 
# let's look at how many people that answer actually used, and out of how many

#~47% of people smoke apparently- could be missing values, and survey takers confused


# check the documentation for SmokeNow. is there an explanation of the missing 
# values?

#NA was given to everyone that had not smoked over 100 cigarettes 

# given that, what did na.rm = TRUE actually do? it literally removed missing values, 
# yes, but changed the question that we're asking. think about where the NA values 
# would systematically be located. 

# the question ..."Yes", na.rm = TRUE)) is asking isn't "what share of adults currently smoke?"
# it's asking "Among adults who've smoked 100 cigarettes, what share still smoke?"

# how should we deal with these missing values?
# R4DS says that we can use coalesce() when a missing value stands for a value 
# that you actually know. 

NHANESraw |> 
  summarise(pct_smoke = mean(coalesce(SmokeNow, "No") == "Yes"))

# is that reasonable? what did coalesce() assume?

#it assumed that 

# who else is missing in SmokeNow? look at Age. 
# create a column called age_group and define the groups as 
# 20+ and under 20. 

nhanesage<- NHANESraw |> 
  mutate(age_group= if_else(Age>19, "20+", "Under 20"))

# after that, keep only adults *first*, then use coalesce() again. 

nhanesage |> 
  filter(age_group== "20+") |> 
  summarise(percent_smoke= mean(coalesce(SmokeNow, "NO")== "Yes" ))

# so how can we create a column called 'smokes' that handles these missing values 
# correctly? 
# hint: we know Age matters here, and we know that participants who responded with 
# "No" in Smoke100 are actually "No"

nhanesage<- nhanesage |> 
  filter(age_group== "20+") |> 
  mutate(smokes= if_else(Smoke100=="No", "No", SmokeNow))

sum(is.na(nhanesage$smokes))


  


# do we have any missing in our smokes column?



# should they become "No"? 
# with that, does NA mean actually one thing in this column? 

# your turn :) 

# for each of the following variables: 
## 1. what percent is missing?
## 2. who was eligible to have a value? check ?NHANESraw.
## 3. is the missingness mostly outside that group?
## 4. answer the question, and say exactly who your answer describes.

# A. SleepHrsNight 
## how many hours do people usually sleep on a weeknight? 

# B. Testosterone 
## what's the average testosterone? does it differ between years? 
## hint: after you drop the missing values, count by SurveyYr. where did 2009_10 go?


# let's co one more thing: income 
# let's focus on adults 
adults <- NHANESraw |> 
  filter(Age >= 20)

adults |> 
  summarise(pct_missing = mean(is.na(HHIncomeMid)))

# does the doc give us anything at all here? 

# let's see if the missingness is systematically related to another variable 
adults |> 
  group_by(Education) |> 
  summarise(
    pct_missing = mean(is.na(HHIncomeMid)),
    mean_income = mean(HHIncomeMid, na.rm = TRUE)
  ) |> 
  arrange(Education)

mean(adults$HHIncomeMid, na.rm = TRUE)

# how does the pattern of missigness bias the results? 
# what happens if we just estimate the mean of HHIncomeMid and remove NAs?
