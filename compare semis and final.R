### look at how different acts did 
# in the semi and in the final

# load packages
library(tidyverse)

# load data
semi_1 <- 
  read_csv("eurovision_2022_semi_1_clean_with_points.csv")
semi_2 <- 
  read_csv("eurovision_2022_semi_2_clean_with_points.csv")
final <- 
  read_csv("eurovision_2022_clean_with_points.csv")

# add ranks
semi_1 <- 
  semi_1  %>% 
  mutate(jury_points = 
           replace_na(jury_points, 0)) %>% 
  mutate(televote_points = 
           replace_na(televote_points, 0)) %>% 
  mutate(total_points = 
           jury_points + 
           televote_points) %>% 
  group_by(points_to) %>% 
  summarise(total_points = sum(total_points)) %>% 
  arrange(-total_points) %>% 
  mutate(rank = 1:n())
semi_2 <- 
  semi_2  %>% 
  mutate(jury_points = 
           replace_na(jury_points, 0)) %>% 
  mutate(televote_points = 
           replace_na(televote_points, 0)) %>% 
  mutate(total_points = 
           jury_points + 
           televote_points) %>% 
  group_by(points_to) %>% 
  summarise(total_points = sum(total_points)) %>% 
  arrange(-total_points) %>% 
  mutate(rank = 1:n())
final <- 
  final  %>% 
  mutate(jury_points = 
           replace_na(jury_points, 0)) %>% 
  mutate(televote_points = 
           replace_na(televote_points, 0)) %>% 
  mutate(total_points = 
           jury_points + 
           televote_points) %>% 
  group_by(points_to) %>% 
  summarise(total_points = sum(total_points)) %>% 
  arrange(-total_points) %>% 
  mutate(rank = 1:n())

# combine datasets
semis_and_final <- 
  semi_1 %>% 
  mutate(source = "semi 1") %>% 
  bind_rows(semi_2 %>% 
              mutate(source = "semi 2")) %>% 
  bind_rows(final %>% 
              mutate(source = "final"))

# add flag for qualifiers or not

# creat three categories:
# qualifiers
# NQs
# big five
semi_1_qualifiers <- 
  c("Lithuania",
    "Switzerland",
    "Ukraine",
    "Netherlands",
    "Moldova",
    "Portugal",
    "Iceland",
    "Greece",
    "Norway",
    "Armenia")
semi_1_nq <- 
  c("Albania",
    "Latvia",
    "Slovenia",
    "Bulgaria",
    "Croatia",
    "Denmark",
    "Austria")
semi_2_qualifiers <- 
  c("Finland",
    "Serbia",
    "Azerbaijan",
    "Australia",
    "Estonia",
    "Romania",
    "Poland",
    "Belgium",
    "Sweden",
    "Czech Republic")
semi_2_nq <- 
  c("Israel",
    "Georgia",
    "Malta",
    "San Marino",
    "Cyprus",
    "Ireland",
    "North Macedonia",
    "Montenegro")
big_five <- 
  c("United Kingdom",
    "France",
    "Italy",
    "Germany",
    "Spain")

# look at change in performance, if applicable
semis_and_final %>% 
  mutate(which_semi = 
           case_when(points_to %in% semi_1_qualifiers ~ "semi 1",
                     points_to %in% semi_2_qualifiers ~ "semi 2")) %>% 
  filter(points_to %in% semi_1_qualifiers |
           points_to %in% semi_2_qualifiers) %>% 
  group_by(source, which_semi) %>% 
  mutate(rank_final = rank(rank)) %>% 
  mutate(is_final = 
           case_when(source == "semi 1" ~ "semi",
                     source == "semi 2" ~ "semi",
                     source == "final" ~ "final")) %>%
  ungroup %>% 
  mutate(is_final = 
           fct_relevel(is_final, 
                       "semi",
                       "final")) %>% 
  ggplot() + 
  aes(x = is_final,
      y = rank_final, 
      group = points_to,
      label = points_to) + 
  geom_point() +
  geom_line() + 
  scale_y_reverse() + 
  facet_wrap(~ which_semi) + 
  geom_text(data = . %>% 
              filter(is_final == "semi"),
            hjust = 1,
            position = position_nudge(x = -.1)) + 
  geom_text(data = . %>% 
              filter(is_final == "final"),
            hjust = 0,
            position = position_nudge(x = .1)) + 
  theme_minimal() + 
  labs(x = "",
       y = "") + 
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank())

