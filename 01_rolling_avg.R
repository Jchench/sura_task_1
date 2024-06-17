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
  mutate(date_column = as.Date(date, format = "%m/%d") |> update(year = 2016))

# save out
save(general_elections_2016, file = "data/cleaned_polls.rda")

# RealClearPolitics
rolling_average_1 <- 
  general_elections_2016 |> 
  group_by(date_column) |> 
  summarize(trump_mean = mean(trump_r),
            clinton_mean = mean(clinton_d)) |> 
  arrange(date_column) |>
  mutate(Clinton_7day_avg = rollapply(clinton_mean, width = 7, 
                                      FUN = mean, align = "right", fill = NA),
         Trump_7day_avg = rollapply(trump_mean, width = 7, 
                                    FUN = mean, align = "right", fill = NA)) |> 
  filter(date_column >= as.Date("2016-06-01") & date_column <= as.Date("2016-11-07")) |> 
  select(-trump_mean, -clinton_mean)

# USC/Dornsife
rolling_average_2 <- 
  general_elections_2016 |> 
  arrange(date_column) |>
  mutate(Clinton_7day_avg = rollapply(clinton_d, width = 7, 
                                      FUN = mean, align = "right", fill = NA),
         Trump_7day_avg = rollapply(trump_r, width = 7, 
                                    FUN = mean, align = "right", fill = NA)) |> 
  filter(date_column >= as.Date("2016-06-01") & date_column <= as.Date("2016-11-07"),
         str_detect(pollster, "^LA")) |> 
  select(date_column, Clinton_7day_avg, Trump_7day_avg)

# Create the combined line graph
rolling_avgerage <- 
  ggplot(rolling_average_1, aes(x = date_column)) +
  geom_line(data = rolling_average_2, aes(y = Trump_7day_avg, color = "Trump", 
                                          linetype = "USD/Dornsife")) +
  geom_line(data = rolling_average_2, aes(y = Clinton_7day_avg, color = "Clinton", 
                                          linetype = "USD/Dornsife")) +
  geom_line(aes(y = Trump_7day_avg, color = "Trump", 
                linetype = "RealClearPolitics")) +
  geom_line(aes(y = Clinton_7day_avg, color = "Clinton", 
                linetype = "RealClearPolitics")) +
  coord_cartesian(ylim = c(30, 70)) +
  labs(title = "7-Day Rolling Average of Polling Results",
       subtitle = "(June 1, 2016 - November 7, 2016)",
       x = "Date",
       y = "Support (%)",
       color = "Candidate",
       linetype = "Poll Data") +
  scale_linetype_manual(values = c("dashed", "solid")) + 
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal()

# save out
ggsave(rolling_avgerage, filename = "2016_rolling_average.jpeg")
