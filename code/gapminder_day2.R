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

#Data analysis session 2
#read in new data
read_csv("data/co2-un-data.csv") #error first row is not column names
read_csv("data/co2-un-data.csv", skip = 1) #skip and make column names first row
#column 2 didn't have a name so now we have to assign column names
read_csv("data/co2-un-data.csv", skip = 1,
         col_names = c("region", "country", "year", 
                       "series", "value", "footnotes", "source")) 
#since we assigned column names we want to skip the row of misnamed columns
read_csv("data/co2-un-data.csv", skip = 2,
         col_names = c("region", "country", "year", 
                       "series", "value", "footnotes", "source")) 

##Goal of data cleaning: data from a year close to 2007
#a column for country and we want columns for different types of co2 emissions (total, per capita)

#Exercise: select only the country, year, series, value columns
read_csv("data/co2-un-data.csv", skip = 2,
         col_names = c("region", "country", "year", 
                       "series", "value", "footnotes", "source")) %>%
  select(-region, -footnotes, -source)

#data cleaning
co2_emissions <- read_csv("data/co2-un-data.csv", skip = 2,
         col_names = c("region", "country", "year", 
                       "series", "value", "footnotes", "source")) %>% 
  select(-region, -footnotes, -source) %>% 
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% #simplifying the series column 
  pivot_wider(names_from = series, values_from = value) %>% #split series entries into columns of total and per capita
  filter(year == 2005) %>% #only want data from 2005
  select(-year) %>% 
  mutate(country = recode(country, "Bolivia (Plurin. State of)" = "Bolivia",
                          "United States of America" = "United States",
                          "Venezuela (Boliv. Rep. of)" = "Venezuela"))
view(co2_emissions)

#change PR to be a part of the US
Americas_2007 <- gapminder_data %>% 
  filter(year == 2007, continent == "Americas") %>%
  select(-continent, -year) %>% 
  mutate(country = recode(country, "Puerto Rico" = "United States")) %>%
  group_by(country) %>% 
  summarize(lifeExp = sum(lifeExp*pop)/sum(pop),
            gdpPercap = sum(gdpPercap * pop)/sum(pop),
            pop = sum(pop))

##joining the datasets
#inner_join() only keeps info from both
#outer_join() keeps all

gapminder_co2 <- inner_join(Americas_2007, co2_emissions, by = "country")

anti_join(Americas_2007, co2_emissions) #checks to see what from the first dataset was not included in the join

#mutate and the if_else
gap_co2_region <- gapminder_co2 %>% 
  mutate(region = if_else(country == "Canada" | 
                            country == "United States" | 
                            country == "Mexico", "north", 
                          "south"))

#another version
gapminder_co2 %>% 
  mutate(region = if_else(country %in% c("Canada", "United States", "Mexico"), "north", 
                          "south"))

view(gap_co2_region)

#is there a relationship between gdp and co2 emissions
ggplot(gap_co2_region, aes(x = gdpPercap, y = per_capita_emissions)) +
  geom_point(aes(color = region), size = 3) +
  scale_color_brewer(palette = "Set1") +
  labs(y = "Emissions per Capita", x = "GDP per Capita") +
  theme_classic()

write_csv(gap_co2_region, "data/gap_co2_region.csv")
