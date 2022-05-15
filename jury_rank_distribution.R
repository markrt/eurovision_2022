### get overall distribution of rankings
# both jury and televote
# for each country

# load packages
library(tidyverse)
library(janitor)

# load data

eurovision_2022 <- 
  read_csv("eurovision_2022_clean_with_points.csv")

# check overall allocation looks OK
# eurovision_2022 %>% 
#   select(jury_rank, televote_rank, points_to) %>% 
#   ggplot() + 
#   aes(x = jury_rank) + 
#   geom_histogram(bins = 25)

# yes looks fine

eurovision_2022 %>% 
  select(points_to) %>% 
  distinct() %>%
  # mutate(points_to = 
  #          str_sub(points_to,
  #                  2,
  #                  -1)) %>% 
  pull()


# plot again
eurovision_2022 %>% 
  select(jury_rank, televote_rank, points_to) %>% 
  ggplot() + 
  aes(x = jury_rank) + 
  geom_histogram(bins = 25) + 
  geom_vline(xintercept = 10.5,
             linetype = "dashed",
             colour = "gray") + 
  facet_wrap(~ points_to,
             nrow = 5)  +
  theme_minimal() + 
  theme(panel.grid = element_blank()) + 
  scale_y_continuous(breaks = c(0, 5, 10)) + 
  labs(x = "Jury rank",
       y = "")

# of the countries without UK in the top 10,
# where was it?
eurovision_2022 %>% 
  filter(points_to == "United Kingdom" &
           jury_rank > 10)
# Armenia and Australia: 15th
# Croatia: 19th
# Greece: 20th

# for whom was Germany a near miss?
eurovision_2022 %>% 
  filter(points_to == "Germany" &
           jury_rank == 11)
# Australia and Austria
