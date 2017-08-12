rm(list = ls())
gc()

setwd('d:/DS/mlcourse_open/1_lecture/')

library(data.table)
library(tidyverse)

dt <- fread('telecom_churn.csv', stringsAsFactors = T)
Hmisc::describe(dt)

# какова доля людей нелояльных пользователей в нашем датафрейме?
dt[, mean(Churn)]

# каковы средние значения числовых признаков среди нелояльных пользователей?
sapply(select_if(dt[Churn == 1], is.numeric), mean)

# сколько в среднем в течение дня разговаривают по телефону нелояльные пользователи?
dt[Churn == 1, `Total day minutes`] %>% 
  mean()

# Какова максимальная длина международных звонков среди лояльных пользователей (Churn == 0), 
# не пользующихся услугой международного роуминга ('International plan' == 'No')?
dt %>% 
  filter(Churn == 0, `International plan` == 'No') %>% 
  select(`Total intl minutes`) %>% 
  max()


dt[1:5, State:`Area code`]

# Применение функции к каждому столбцу: apply
sapply(select_if(dt, is.numeric), max)
select_if(dt, is.numeric) %>% map_dbl(max)

# замена значений
dt[, `International plan` := ifelse(`International plan` == 'Yes', T, F)]
dt %>% mutate(`International plan` := ifelse(`International plan` == 'Yes', T, F))

# группировка
columns_to_show = c('Total day minutes', 'Total eve minutes', 'Total night minutes', 'Churn')
dt %>% 
  select(columns_to_show) %>% 
  group_by(Churn) %>% 
  summarise(mean(`Total day minutes`))

dt %>% 
  select(columns_to_show) %>% 
  psych::describeBy('Churn')

dt %>% 
  select(columns_to_show) %>% 
  summarise_each(funs(min, max, mean), columns_to_show)

# сводные таблицы
dt %>% 
  select(Churn, `International plan`) %>% 
  table()
dt[, .N, by = c('Churn', 'International plan')]

dt %>% 
  select(Churn, `International plan`) %>% 
  table() %>% 
  prop.table()
dt[, .N, by = c('Churn', 'International plan')][, Churn, N/sum(N)]

# добавление столбцов
dt %>% 
  mutate('Total calls' = `Total day charge` + `Total eve charge` + `Total night charge` + `Total intl charge`)
dt[, 'Total calls' := `Total day charge` + `Total eve charge` + `Total night charge` + `Total intl charge`]

# удаление столбцов
dt[, `Total calls` := NULL]
dt %>% 
  select(-one_of('Total calls'))

# первый прогноз
dt %>% 
  select(Churn, `International plan`) %>% 
  table()

dt %>% 
  select(Churn, `Customer service calls`) %>% 
  table()

dt[, Many_service_calls := ifelse(`Customer service calls` > 3, 1L, 0L)]
dt %>% 
  select(Churn, `Many_service_calls`) %>% 
  table()

dt %>% 
  select(Churn, `Many_service_calls`, `International plan`) %>% 
  table()





