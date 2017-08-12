rm(list = ls())
gc()

setwd('d:/DS/mlcourse_open/2_lecture/')

library(data.table)
library(tidyverse)

dt <- fread('video_games_sales.csv')
dt <- na.omit(dt)
