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

polling_data <- 
  general_elections_2016 |> 
  arrange(date_column) |> 
  mutate(Clinton_7day_avg = rollapply(clinton_d, width = 7, FUN = mean, align = "right", fill = NA),
         Trump_7day_avg = rollapply(trump_r, width = 7, FUN = mean, align = "right", fill = NA))

ggplot(polling_data |> filter(str_detect(pollster, "^LA")), aes(x = date_column)) +
  geom_line(aes(y = Clinton_7day_avg, color = "Clinton")) +
  geom_line(aes(y = Trump_7day_avg, color = "Trump")) +
  labs(title = "7-Day Rolling Average of Polling Results (June 1, 2016 - November 7, 2016)",
       x = "Date",
       y = "Support (%)",
       color = "Candidate") +
  theme_minimal()
