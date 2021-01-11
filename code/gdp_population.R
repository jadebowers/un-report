library(tidyverse)
library(readr)

#read in data
gapminder_1997 <- read_csv("data/gapminder_1997.csv")
View(gapminder_1997)
gapminder_data <- read_csv("data/gapminder_data.csv")
View(gapminder_data)

#learn more about a function
?read_csv

ggplot(data = gapminder_1997) +
  aes(x = gdpPercap, y = lifeExp) +
  geom_point(aes(color = continent, size = pop/1000000)) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "GDP per Capita", y = "Life Expectancy", 
       title = "Do people in wealthy countries live longer?", 
       size = "Population (in millions)")

RColorBrewer::display.brewer.all()

ggplot(data = gapminder_data) +
  geom_line(aes(x = year, y = lifeExp, color = continent, group = country))

#adding a group makes it so the lines connect by contry instead of by color/continent

ggplot(data = gapminder_data, aes(x = continent, y = lifeExp)) +
  geom_jitter(aes(size = pop)) +
  geom_violin(alpha = 0.5, fill = "dodgerblue") +
  labs(x = 'Population (per million)', y = 'Life Expectancy')

ggplot(gapminder_data) +
  aes(x = lifeExp) +
  geom_histogram(bins = 20) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

ggsave("figures/awesome_plot.jpg", width = 6, height = 4)

violin_plot <- ggplot(data = gapminder_data, aes(x = continent, y = lifeExp)) +
  geom_jitter(aes(size = pop)) +
  geom_violin(alpha = 0.5, fill = "dodgerblue") +
  labs(x = 'Population (per million)', y = 'Life Expectancy')

violin_plot <- violin_plot + theme_bw()

ggsave("figures/awesome_violin_plot.jpg", plot = violin_plot, width = 6, height = 4)

#faceting plots
ggplot(gapminder_1997, aes(x = gdpPercap, y = lifeExp)) + 
  geom_point() +
  facet_wrap(vars(continent)) +
  labs(x = "GDP per Capita", y = "Life Expectancy") +
  theme_bw()

ggsave("figures/my_awesome_plot.jpg", width = 6, height = 4)
