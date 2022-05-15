# i've copied and pasted a bunch of data from here
# https://eurovision.tv/event/turin-2022/grand-final/results/united-kingdom
# (and equivalents for all other countries)
# into an excel sheet

# nb there are a few countries where the format is different:
# Azerbaijan, Georgia, Montenegro, Poland, Romania, San Marino
# due to "irregularities"

# i've done my best to provide something usable, but there may be issues

# there are also issues with trailing blanks in the country names
# on account of the way that Eurovision includes little flag circles
# this may present issues with merges, sorry

# i've tried to deal with this in the str_sub bit but who knows what other 
# problems doing so will introduce

# anyway, this script converts the copied+pasted data into a long csv for anyone to use

# load packages
library(tidyverse)
library(readxl)

# define path
path <- "results_2022.xlsx"

# load data
mad <- path %>%
  excel_sheets() %>%
  set_names() %>%
  map_df(read_excel,
         path = path,
         .id = "points_from")

# rename columns, export
mad %>% 
  select(-Juror)  %>%
  rename(juror_a_ranking = 
           A,
         juror_b_ranking = 
           B,
         juror_c_ranking = 
           C,
         juror_d_ranking = 
           D,
         juror_e_ranking = 
           E,
         jury_rank = 
           `Jury rank`,
         televoting_rank = 
           `Televoting rank`,
         points_to = 
           "Country"
  ) %>% 
  mutate(points_to = 
           str_sub(points_to,
                   2,
                   -1)) %>% 
  write_csv("eurovision_2022_clean.csv")


# clean up the jury_rank and televoting_rank columns,
# add points
mad %>% 
  select(-Juror) %>% 
  rename(juror_a_ranking = 
           A,
         juror_b_ranking = 
           B,
         juror_c_ranking = 
           C,
         juror_d_ranking = 
           D,
         juror_e_ranking = 
           E,
         jury_rank = 
           `Jury rank`,
         televote_rank = 
           `Televoting rank`,
         points_to = 
           "Country"
  ) %>%
  mutate(points_to = 
           str_sub(points_to,
                   2,
                   -1))%>% 
  mutate(jury_rank = as.numeric(str_squish(str_sub(jury_rank, -4, -3)))) %>% 
  mutate(televote_rank = as.numeric(str_squish(str_sub(televote_rank, -4, -3)))) %>% 
  mutate(jury_points = if_else(jury_rank == 1,
                               12,
                               if_else(jury_rank == 2,
                                       10,
                                       if_else(jury_rank == 3,
                                               8,
                                               if_else(jury_rank == 4,
                                                       7,
                                                       if_else(jury_rank == 5,
                                                               6,
                                                               if_else(jury_rank == 6,
                                                                       5,
                                                                       if_else(jury_rank == 7,
                                                                               4,
                                                                               if_else(jury_rank == 8,
                                                                                       3,
                                                                                       if_else(jury_rank == 9,
                                                                                               2,
                                                                                               if_else(jury_rank == 10,
                                                                                                       1,
                                                                                                       0)))))))))))%>% 
  mutate(televote_points = if_else(televote_rank == 1,
                                   12,
                                   if_else(televote_rank == 2,
                                           10,
                                           if_else(televote_rank == 3,
                                                   8,
                                                   if_else(televote_rank == 4,
                                                           7,
                                                           if_else(televote_rank == 5,
                                                                   6,
                                                                   if_else(televote_rank == 6,
                                                                           5,
                                                                           if_else(televote_rank == 7,
                                                                                   4,
                                                                                   if_else(televote_rank == 8,
                                                                                           3,
                                                                                           if_else(televote_rank == 9,
                                                                                                   2,
                                                                                                   if_else(televote_rank == 10,
                                                                                                           1,
                                                                                                           0))))))))))) %>% 
  write_csv("eurovision_2022_clean_with_points.csv")



### rerun for semifinal 1


# define path
path_semi_1 <- "semi_final_1_2022.xlsx"

# load data
mad_semi_1 <- path_semi_1 %>%
  excel_sheets() %>%
  set_names() %>%
  map_df(read_excel,
         path = path_semi_1,
         .id = "points_from")

# rename columns, export
mad_semi_1 %>% 
  select(-Juror)  %>%
  rename(juror_a_ranking = 
           A,
         juror_b_ranking = 
           B,
         juror_c_ranking = 
           C,
         juror_d_ranking = 
           D,
         juror_e_ranking = 
           E,
         jury_rank = 
           `Jury rank`,
         televoting_rank = 
           `Televoting rank`,
         points_to = 
           "Country"
  ) %>% 
  mutate(points_to = 
           str_sub(points_to,
                   2,
                   -1)) %>% 
  write_csv("eurovision_2022_semi_1_clean.csv")


# clean up the jury_rank and televoting_rank columns,
# add points
mad_semi_1 %>% 
  select(-Juror) %>% 
  rename(juror_a_ranking = 
           A,
         juror_b_ranking = 
           B,
         juror_c_ranking = 
           C,
         juror_d_ranking = 
           D,
         juror_e_ranking = 
           E,
         jury_rank = 
           `Jury rank`,
         televote_rank = 
           `Televoting rank`,
         points_to = 
           "Country"
  ) %>%
  mutate(points_to = 
           str_sub(points_to,
                   2,
                   -1))%>% 
  mutate(jury_rank = as.numeric(str_squish(str_sub(jury_rank, -4, -3)))) %>% 
  mutate(televote_rank = as.numeric(str_squish(str_sub(televote_rank, -4, -3)))) %>% 
  mutate(jury_points = if_else(jury_rank == 1,
                               12,
                               if_else(jury_rank == 2,
                                       10,
                                       if_else(jury_rank == 3,
                                               8,
                                               if_else(jury_rank == 4,
                                                       7,
                                                       if_else(jury_rank == 5,
                                                               6,
                                                               if_else(jury_rank == 6,
                                                                       5,
                                                                       if_else(jury_rank == 7,
                                                                               4,
                                                                               if_else(jury_rank == 8,
                                                                                       3,
                                                                                       if_else(jury_rank == 9,
                                                                                               2,
                                                                                               if_else(jury_rank == 10,
                                                                                                       1,
                                                                                                       0)))))))))))%>% 
  mutate(televote_points = if_else(televote_rank == 1,
                                   12,
                                   if_else(televote_rank == 2,
                                           10,
                                           if_else(televote_rank == 3,
                                                   8,
                                                   if_else(televote_rank == 4,
                                                           7,
                                                           if_else(televote_rank == 5,
                                                                   6,
                                                                   if_else(televote_rank == 6,
                                                                           5,
                                                                           if_else(televote_rank == 7,
                                                                                   4,
                                                                                   if_else(televote_rank == 8,
                                                                                           3,
                                                                                           if_else(televote_rank == 9,
                                                                                                   2,
                                                                                                   if_else(televote_rank == 10,
                                                                                                           1,
                                                                                                           0))))))))))) %>% 
  write_csv("eurovision_2022_semi_1_clean_with_points.csv")



### same again, semi 2

# define path
path_semi_2 <- "semi_final_2_2022.xlsx"

# load data
mad_semi_2 <- path_semi_2 %>%
  excel_sheets() %>%
  set_names() %>%
  map_df(read_excel,
         path = path_semi_2,
         .id = "points_from")

# rename columns, export
mad_semi_2 %>% 
  select(-Juror)  %>%
  rename(juror_a_ranking = 
           A,
         juror_b_ranking = 
           B,
         juror_c_ranking = 
           C,
         juror_d_ranking = 
           D,
         juror_e_ranking = 
           E,
         jury_rank = 
           `Jury rank`,
         televoting_rank = 
           `Televoting rank`,
         points_to = 
           "Country"
  ) %>% 
  mutate(points_to = 
           str_sub(points_to,
                   2,
                   -1)) %>% 
  write_csv("eurovision_2022_semi_2_clean.csv")


# clean up the jury_rank and televoting_rank columns,
# add points
mad_semi_2 %>% 
  select(-Juror) %>% 
  rename(juror_a_ranking = 
           A,
         juror_b_ranking = 
           B,
         juror_c_ranking = 
           C,
         juror_d_ranking = 
           D,
         juror_e_ranking = 
           E,
         jury_rank = 
           `Jury rank`,
         televote_rank = 
           `Televoting rank`,
         points_to = 
           "Country"
  ) %>%
  mutate(points_to = 
           str_sub(points_to,
                   2,
                   -1))%>% 
  mutate(jury_rank = as.numeric(str_squish(str_sub(jury_rank, -4, -3)))) %>% 
  mutate(televote_rank = as.numeric(str_squish(str_sub(televote_rank, -4, -3)))) %>% 
  mutate(jury_points = if_else(jury_rank == 1,
                               12,
                               if_else(jury_rank == 2,
                                       10,
                                       if_else(jury_rank == 3,
                                               8,
                                               if_else(jury_rank == 4,
                                                       7,
                                                       if_else(jury_rank == 5,
                                                               6,
                                                               if_else(jury_rank == 6,
                                                                       5,
                                                                       if_else(jury_rank == 7,
                                                                               4,
                                                                               if_else(jury_rank == 8,
                                                                                       3,
                                                                                       if_else(jury_rank == 9,
                                                                                               2,
                                                                                               if_else(jury_rank == 10,
                                                                                                       1,
                                                                                                       0)))))))))))%>% 
  mutate(televote_points = if_else(televote_rank == 1,
                                   12,
                                   if_else(televote_rank == 2,
                                           10,
                                           if_else(televote_rank == 3,
                                                   8,
                                                   if_else(televote_rank == 4,
                                                           7,
                                                           if_else(televote_rank == 5,
                                                                   6,
                                                                   if_else(televote_rank == 6,
                                                                           5,
                                                                           if_else(televote_rank == 7,
                                                                                   4,
                                                                                   if_else(televote_rank == 8,
                                                                                           3,
                                                                                           if_else(televote_rank == 9,
                                                                                                   2,
                                                                                                   if_else(televote_rank == 10,
                                                                                                           1,
                                                                                                           0))))))))))) %>% 
  write_csv("eurovision_2022_semi_2_clean_with_points.csv")

