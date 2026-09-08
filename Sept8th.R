library(tidyverse)

chocolate<- read_csv("data/raw/chocolate.csv")

glimpse(chocolate)

chocolate |> 
  select(ref, company_manufacturer, company_location, rating, cocoa_percent)

chocolate |>
  filter(company_location=="U.S.A.")

chocolate |> 
  filter(company_location== "U.S.A.", rating>=3.5)

chocolate |> 
  filter(company_location== "U.S.A."| rating>=3.5)

chocolate |> 
  filter(company_location %in% c("U.S.A.", "France", "Canada"))

chocolate |> 
  filter(company_location== c("U.S.A.", "Vietnam"), rating>=3.5, review_date==2021)

chocolate |> 
  select(rating, country_of_bean_origin) |> 
  arrange(desc(rating))|>
  head(n=10)

chocolate |> 
  select(cocoa_percent) |> 
  mutate(
    cocoa_num= parse_number(cocoa_percent)
  )

chocolate<-chocolate |> 
  mutate(
    cocoa_num= parse_number(cocoa_percent)
  )

mean(chocolate$cocoa_num)

chocolate |> 
  group_by(company_location) |> 
  summarise(
    n=n(),
    avg_rating= mean(rating, na.rm= TRUE),
    avg_cocoa= mean(cocoa_num, na.rm= TRUE)
  )

sum(is.na(chocolate$review_date))

chocolate |> 
  ggplot(aes(rating)) +
  geom_bar()

chocolate |> 
  filter(company_location %in% c("U.S.A.", "Canada", "France")) |> 
  ggplot(aes(x= rating, y= company_location, color= company_location))+
  geom_boxplot()+
  coord_flip()
