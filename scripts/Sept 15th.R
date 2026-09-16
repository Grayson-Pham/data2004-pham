library(tidyverse)
fishing<-read_xlsx("data/raw/commercial.xlsx", sheet = "Erie")

glimpse(fishing)

#one row represnts region year weight in rounded pounds of fish caught

#how can we look at our grain over time by region

fishing |> 
  ggplot(aes(x= Year, y = `Grand Total`))+
  geom_line()

nrow(fishing)


fishing_long<-fishing |> 
  pivot_longer(
    cols = !c(Year, Lake, Species, Comments),
    names_to = "region", 
    values_to = "values"
  )

nrow(fishing_long)

fishing_long |> 
  distinct(region)

fishing_long |> 
  filter(Year== 1885, Species== "Lake Whitefish") |> 
  select(region, values)

133+30+1249+2120

133+30+1249+2120+186

fishing_long |> 
  filter(!region %in% c("U.S. Total", "Grand Total")) |> 
  mutate(species= fct_lump_n(Species, n= 6)) |> 
  ggplot(aes(x= Year, y= values, color= species))+
  scale_color_manual(values = palette.colors(7, "Okabe-Ito"))+
  geom_line()

fishing_long |>  
  select(Year, Species, region, values) |> 
  pivot_wider(names_from = region, values_from = values) |> 
  print(width = Inf)


fishing_superior<- read_xlsx("data/raw/commercial.xlsx", sheet = "Superior")

#what is the grain?
#the grain is a the weight of fish cought per year in rounded pounds in a region

#what is the total catch of the lake

fishing_superior |> 
  sum(`Grand Total`)

fishing_superior_long<- fishing_superior |> 
  pivot_longer(
    cols = !c(Year, Lake, Species, Comments),
    names_to= "region",
    values_to = "value"
)

