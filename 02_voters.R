# load libraries
library(tidyverse)
load("data/cleaned_polls.rda")

general_elections_2016_voters <- 
  general_elections_2016 |> 
  separate(sample, into = c("sample_size", "voter_type"), sep = " ") |> 
  mutate(sample_size = as.numeric(sample_size)) |> 
  mutate(pollster = ifelse(str_detect(pollster, "^LA"), "USC-LA Times", "Other"))

# histogram
sample_size_facet <- 
  general_elections_2016_voters |> 
  drop_na(voter_type) |> 
  ggplot(aes(x = sample_size, fill = pollster)) +
  geom_histogram(bins = 300) +
  facet_grid(~ voter_type) + 
  labs(title = "Poll Data Sample Size",
       subtitle = "Likely vs. Registered Voters",
       y = NULL,
       x = "sample size") +
  coord_cartesian(xlim = c(0, 5000)) +
  guides(fill = guide_legend(title = "Pollster")) +
  theme_minimal()

ggsave(sample_size_facet, filename = "2016_sample_size_facet.jpeg")

# one graph
sample_size_dist <- 
  general_elections_2016_voters |> 
  drop_na(voter_type) |> 
  ggplot(aes(x = sample_size, fill = voter_type)) +
  geom_histogram(bins = 300) +
  labs(title = "Poll Data Sample Size",
       subtitle = "Likely vs. Registered Voters",
       y = NULL,
       x = "sample size") +
  coord_cartesian(xlim = c(0, 5000)) +
  guides(fill = guide_legend(title = "Voter Type")) +
  theme_minimal()

ggsave(sample_size_dist, filename = "2016_sample_size_dist.jpeg")
