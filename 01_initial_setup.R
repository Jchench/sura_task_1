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

# method 1
rolling_average_1 <- 
  general_elections_2016 |> 
  group_by(date_column) |> 
  summarize(trump_mean = mean(trump_r),
            clinton_mean = mean(clinton_d)) |> 
  arrange(date_column) |>
  mutate(Clinton_7day_avg = rollapply(clinton_mean, width = 7, 
                                      FUN = mean, align = "right", fill = NA),
         Trump_7day_avg = rollapply(trump_mean, width = 7, 
                                    FUN = mean, align = "right", fill = NA))

ggplot(rolling_average_2, aes(x = date_column)) +
  geom_line(aes(y = Trump_7day_avg, color = "Trump")) +
  geom_line(aes(y = Clinton_7day_avg, color = "Clinton")) +
  labs(title = "7-Day Rolling Average of Polling Results (June 1, 2016 - November 7, 2016)",
       x = "Date",
       y = "Support (%)",
       color = "Candidate") +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal()

# method 2
rolling_average_2 <- 
  general_elections_2016 |> 
  arrange(date_column) |>
  mutate(Clinton_7day_avg = rollapply(clinton_d, width = 7, 
                                      FUN = mean, align = "right", fill = NA),
         Trump_7day_avg = rollapply(trump_r, width = 7, 
                                    FUN = mean, align = "right", fill = NA))

ggplot(rolling_average_2 |> filter(str_detect(pollster, "^LA")), aes(x = date_column)) +
  geom_line(aes(y = Trump_7day_avg, color = "Trump")) +
  geom_line(aes(y = Clinton_7day_avg, color = "Clinton")) +
  labs(title = "7-Day Rolling Average of Polling Results (USC/Dornsife only)",
       x = "Date",
       y = "Support (%)",
       color = "Candidate") +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal()
