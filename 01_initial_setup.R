# load libraries
library(tidyverse)
library(zoo)

general_elections_2016 <- 
  read_csv("data/2016_gen_poll.csv") |> 
  janitor::clean_names() |> 
  mutate(across(everything(), as.character)) |> 
  mutate(across(everything(), ~ na_if(., "—"))) |> 
  mutate(moe = as.numeric(moe),
         clinton_d = as.numeric(clinton_d),
         trump_r = as.numeric(trump_r)) |> 
  mutate(date_column = as.Date(date, format = "%m/%d") |> update(year = 2016)) |> 
  filter(date_column >= as.Date("2016-06-01") & date_column <= as.Date("2016-11-07"))

seven_day_avg <- 
  general_elections_2016 |> 
  group_by(date_column) |> 
  summarize(trump_mean = mean(trump_r),
            clinton_mean = mean(clinton_d)) |> 
  mutate(week = cut(date_column, breaks = seq(min(date_column), max(date_column) + 6, by = 7))) |> 
  group_by(week) |> 
  summarize(trump_mean = mean(trump_mean),
            clinton_mean = mean(clinton_mean))
