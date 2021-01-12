#Load in the required packages
library(tidyverse)
library(dplyr)

#load in the data
gapminder_data <- read_csv("data/gapminder_data.csv")

#get stats fast with summarize()
summarize(gapminder_data, mean(lifeExp))
summarize(gapminder_data, averagelifeExp = mean(lifeExp)) #renames mean heading to averagelifeExp
gapminder_data %>% summarize(averagelifeExp = mean(lifeExp)) #introduces piping function: %>%
gapminder_data %>% summarize("Average Life Expectancy" = mean(lifeExp)) #changed variable to have spaces
gapminder_data %>% summarize("Average Population" = mean(pop), 
                             recent_year = max(year))

#filter rows where year is 2007
recent_year <- gapminder_data %>% filter(year == 2007)
recent_year %>% summarise("Average Life Expectancy" = mean(lifeExp))

#this is another way to do this without creating a new dataframe
gapminder_data %>% filter(year == 2007) %>% summarise("Average Life Expectancy" = mean(lifeExp),
                                                      recent_year = max(year))

#Exercise: Find the average GDP per capita for the first year in the dataset
gapminder_data %>% summarize(first_year = min(year))
gapminder_data %>% 
  filter(year == 1952) %>% 
  summarise("Average GDP per Capita" = mean(gdpPercap), first_year = min(year))

#group_by

gapminder_data %>% 
  group_by(year) %>% 
  summarize(averagelifeExp = mean(lifeExp))

gapminder_data %>% 
  group_by(continent) %>% 
  summarize(averagelifeExp = mean(lifeExp))

# mutate
gapminder_data %>%
  mutate(gdp = gdpPercap * pop)

#Exercise: make a new column using mutate that is population in millions
gapminder_data %>%
  mutate(popMil = pop/1000000)

#select() - specify columns to keep
gapminder_data %>%
  select(year,pop)

#drop continent column
gapminder_data %>% 
  select(-continent)

#Exercise: select() country, continent, year, and lifeExp columns
gapminder_data %>% 
  select(-pop, -gdpPercap)

#manipulating or rotating data
#arrange(year) - arrange rows
#long vs wide
#pivot_longer and pivot_wider

gapminder_data %>% 
  select(-pop, -gdpPercap) %>% pivot_wider(names_from = year, values_from = lifeExp)

#rename() - rename columns

#Exercise: create a new dataset with only data from the Americas and 2007

Americas_2007 <- gapminder_data %>% 
  filter(year == 2007, continent == "Americas") %>%
  select(-continent, -year)


