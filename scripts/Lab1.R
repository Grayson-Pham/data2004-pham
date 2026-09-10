library(tidyverse)

#What is the us population in 2025
#which Kentucky counties grew the most in 2025 from 2024
#how many counties


data<- read_csv(
  "data/raw/co-est2025-alldata.csv",
  locale = locale(encoding = "Latin1"),
  col_types = cols(.default = col_character())
)

glimpse(data)

#We have annual resident population data from the U.S. census bureau

data_trimmed<- data |> 
  select(1:7, POPESTIMATE2025, POPESTIMATE2024, NPOPCHG2025)

##No, we have some state data and some county data
##SUMLEV coulumn 040 is a state and 050 is a county

data_trimmed |>  
  head(n=10)


#One row is an observation of either a U.S. state or county.
#One row represets a mixed state-county grain

data_trimmed_numeric<- data_trimmed |> 
  mutate(
    pop2025= as.numeric(POPESTIMATE2025),
    pop2024= as.numeric(POPESTIMATE2024),
    popchg2025= as.numeric(NPOPCHG2025)
  )

data_trimmed_numeric |> 
  filter(SUMLEV== "040") |> 
  summarise(
    total_pop_2025 = sum(pop2025)
  )

data_trimmed_numeric |> 
  filter(SUMLEV== "050") |> 
  summarise(
    total_pop_2025 = sum(pop2025)
  )

#there are 341,784,857 people in the US

data_trimmed_numeric |> 
  filter(STNAME=="Kentucky", SUMLEV== "050") |>
  select(CTYNAME, popchg2025) |> 
  arrange(desc(popchg2025)) |> 
  print()

data_trimmed_numeric |>  
  filter(SUMLEV== "050", STNAME=="Kentucky") |> 
  filter(popchg2025>0) |> 
  mutate(popchgcalc= pop2025- pop2024) |> 
  select(CTYNAME, popchgcalc) |> 
  arrange(desc(popchgcalc))
#Warren, Fayette, and Madison county grew the most from 2024 to 2025

data_trimmed_numeric |> 
  filter(SUMLEV=="050") |> 
  count()

#3144 counties in the US
