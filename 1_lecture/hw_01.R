rm(list = ls())
gc()

setwd('d:/DS/mlcourse_open/1_lecture/')

library(data.table)
library(tidyverse)
library(stringr)

dt <- fread('adult.data.csv', stringsAsFactors = T)


# 1. Сколько мужчин и женщин (признак sex) представлено в этом наборе данных?
dt[, .N, by = sex]
dt %>% 
  group_by(sex) %>% 
  summarise(n = n())


# 2. Каков средний возраст (признак age) женщин?
dt[sex == 'Female', mean(age)]
dt %>% 
  filter(sex == 'Female') %>% 
  summarise(mean(age))


# 3. Какова доля граждан Германии (признак native-country)?
dt[`native-country` == 'Germany', .N] / nrow(dt)


# 4. Постройте гистограмму распределения (bar plot) образования людей (признак education).
dt %>% 
  ggplot(aes(education)) +
  geom_bar()


# 5. Каковы средние значения и среднеквадратичные отклонения возраста тех, 
# кто получает более 50K в год (признак salary) и тех, кто получает менее 50K в год?
dt %>% 
  select(salary, age) %>% 
  group_by(salary) %>% 
  summarise_each(funs(mean, sd))


# 6. Правда ли, что люди, которые получают больше 50k, имеют как минимум высшее образование? 
# (признак education - Bachelors, Prof-school, Assoc-acdm, Assoc-voc, Masters или Doctorate)
unique(dt[salary == '>50K', education])


# 7. Выведите статистику возраста для каждой расы (признак race) и каждого пола. 
# Используйте groupby и describe. 
# Найдите таким образом максимальный возраст мужчин расы Amer-Indian-Eskimo.
dt %>% 
  select(age, sex, race) %>% 
  group_by(sex, race) %>% 
  summarise_each(funs(max, mean))
dt %>% 
  select(age, sex, race) %>% 
  psych::describeBy(c('sex', 'race'))


# 8. Среди кого больше доля зарабатывающих много (>50K): среди женатых или холостых мужчин 
# (признак marital-status)? Женатыми считаем тех, у кого marital-status начинается с 
# Married (Married-civ-spouse, Married-spouse-absent или Married-AF-spouse), остальных считаем холостыми.
dt[, married := ifelse(regexpr('Married', dt$`marital-status`) == 1, T, F)]
dt[, married := grepl('Married', dt$`marital-status`)]
dt %>% 
  filter(salary == '>50K') %>%
  select(salary, married) %>% 
  group_by(married) %>% 
  table()
  

# 9. Какое максимальное число часов человек работает в неделю (признак hours-per-week)?
# Сколько людей работают такое количество часов и каков среди них процент зарабатывающих много?
dt %>% 
  filter(`hours-per-week` == max(dt$`hours-per-week`)) %>% 
  summarise(n = n())
dt %>% 
  filter(`hours-per-week` == max(dt$`hours-per-week`)) %>% 
  group_by(salary) %>% 
  summarise(n = n())
  

# 10. Посчитайте среднее время работы (hours-per-week) 
# зарабатывающих мало и много (salary) для каждой страны (native-country).
dt %>% 
  select(salary, `hours-per-week`, `native-country`) %>% 
  group_by(salary, `native-country`) %>% 
  summarise_each(funs(mean))


