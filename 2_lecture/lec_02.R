rm(list = ls())
gc()

setwd('d:/DS/mlcourse_open/2_lecture/')

library(data.table)
library(tidyverse)
library(GGally)
library(ggExtra)

dt <- fread('video_games_sales.csv', na.strings = '')
dt <- na.omit(dt)
dt[, User_Score := as.numeric(User_Score)]

dt.sales <- dt %>% select(grep('Sales', names(dt)), Year_of_Release)

useful_cols = c('Name', 'Platform', 'Year_of_Release', 'Genre', 
               'Global_Sales', 'Critic_Score', 'Critic_Count',
               'User_Score', 'User_Count', 'Rating')
dt <- dt %>% select(useful_cols)
rm(useful_cols)

dt.sales %>% 
  group_by(Year_of_Release) %>% 
  summarise_all(sum) %>% 
  gather(Region, Sales, -Year_of_Release) %>% 
  ggplot(aes(x = Year_of_Release, y = Sales, group = Region, col = Region)) + 
  geom_line(size = 1.2)
  
  
cols = c('Global_Sales', 'Critic_Score', 'Critic_Count', 'User_Score', 'User_Count')
dt %>% 
  select(cols) %>% 
  ggpairs()


dt %>% ggplot(aes(Critic_Score)) + 
  geom_bar(stat = 'bin', fill = 'green', aes(y = ..density..)) +
  geom_density(col = 'blue', size = 1.2)


g <- dt %>% ggplot(aes(Critic_Score, User_Score)) +
  geom_point(size = 2, col = 'blue', alpha = 0.5)
ggMarginal(g, type = "histogram")


top.platform <- dt[, .N, by = Platform] %>% 
  arrange(-N) %>% 
  head(5)
top.platform <- top.platform[, 1]
dt %>% 
  filter(Platform == top.platform) %>% 
  ggplot(aes(x = Platform, y = Critic_Score, col = Platform)) +
  geom_boxplot(size = 2) +
  coord_flip()


dt %>% 
  select(Genre, Platform, Global_Sales) %>% 
  group_by(Genre, Platform) %>% 
  summarise_all(funs(sum)) %>% 
  ggplot(aes(Genre, Platform, fill = Global_Sales)) +
  geom_tile() +
  #coord_equal() +
  scale_fill_gradient(low = "white", high = "red") +
  geom_text(aes(label = Global_Sales))
  






