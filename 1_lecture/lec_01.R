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
sapply(dt, mean)


