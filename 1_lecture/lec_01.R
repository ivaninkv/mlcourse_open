rm(list = ls())
gc()

setwd('d:/DS/mlcourse_open/1_lecture/')

library(data.table)
library(tidyverse)

dt <- fread('telecom_churn.csv', stringsAsFactors = T)
Hmisc::describe(dt)
