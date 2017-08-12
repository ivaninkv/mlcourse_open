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


