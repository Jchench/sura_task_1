# load libraries
library(tidyverse)
load("data/cleaned_polls.rda")

general_elections_2016_voters <- 
  general_elections_2016 |> 
  separate(sample, into = c("sample_size", "voter_type"), sep = " ") |> 
  mutate(sample_size = as.numeric(sample_size)) |> 
  mutate(pollster = ifelse(str_detect(pollster, "^LA"), "USC/Dornsife", "other"))

# histogram
general_elections_2016_voters |> 
  drop_na(voter_type) |> 
  ggplot(aes(x = sample_size, fill = pollster)) +
  geom_histogram(bins = 100) +
  facet_wrap(~ voter_type) +
  labs(title = "Poll Data Sample Size",
       subtitle = "Likely vs. Registered Voters",
       y = NULL,
       x = "sample size") +
  theme_minimal()
