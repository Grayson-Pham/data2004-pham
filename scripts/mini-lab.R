# Lab 2

# We'll start by working with the actual crashes and persons fully 
library(tidyverse)

crashes <- read_csv("data/raw/crashes.csv")
persons <- read_csv("data/raw/person.csv")

# 1: Make a table of person records for female pedestrians 

pedestrians_female<- persons |> 
  filter(PERSON_SEX== "F", PERSON_TYPE== "Pedestrian")

## what does one row represent? 

#one row represents a record of a female pedestrian that was involved in a crash

# 2: Keep only the crashes that involved at least one female pedestrian. 
# Keep COLLISION_ID, BOROUGH, and the five vehicle type columns. 
# how can we select every variable that starts with "VEHICLE TYPE CODE"?

crashes_fem_peds<- crashes |> 
  semi_join(pedestrians_female, join_by(COLLISION_ID)) |> 
  select(COLLISION_ID, BOROUGH, starts_with("VEHICLE TYPE"))

# does one row still represent one crash? check it. 

#yes.

# why a filtering join instead of a mutating join? 

#There is no extra data that was added into the table

# 3: Right now the vehicle types are columns. We want one row per vehicle. 
# before writing your code, how many rows should we have? 

#It should be 5 times the amount because there are 5 vehicle types

crashes_female_pedestrians<- crashes_fem_peds |> 
  pivot_longer(starts_with("VEHICLE TYPE"))

# how many missing values are in the new dataframe? 

sum(is.na(crashes_female_pedestrians$value))

#20203 NA values

# why do we think that slots 3, 4, and 5 have so many more missing values? 

#Most crashes only involve one or two cars

# does every crash have a first vehicle recorded? 
crashes_female_pedestrians |> 
  filter(name== "VEHICLE TYPE CODE 1", is.na(value)) |> 
  count(value)

#there are 601 NA values in the first vehicle recorded

# are we safe to drop missing values?
#If we are just looking for the types of cars involved yes but not if we are looking at all crashes

# 5: what kinds of vehicles are involved in crashes with a female pedestrian? 
crashes_female_pedestrians |> 
  count(value, sort = TRUE) |> 
  print(n = 57)

##The most common was a sedans and then station wagons, the third highest was taxis
