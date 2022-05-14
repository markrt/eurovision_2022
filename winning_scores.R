### an interesting discussion on Twitter recently
# has been around the number of points you need to win the contest
# and how these are distributed across the televote and jury votes

# so i've decided to look at this 
# purely over the 2016-2021 period (it's faffy to go earlier)

# this hasn't worked at all
# but maybe people will be interested to play with it and improve it

# load packages
library(tidyverse)
library(Runuran)
library(fitdistrplus)

# load data
previous_scores <- 
  read_csv("previous_contest_scores.csv")

# add ranking per year
previous_scores <- 
  previous_scores %>% 
  group_by(Year) %>% 
  mutate(Rank = rank(-Total))

# add winner flag
previous_scores <- 
  previous_scores %>% 
  mutate(is_winner = 
           case_when(Rank == 1 ~ "Winner",
                     Rank != 1 ~ "Not Winner"))

# add lots of variables
previous_scores <- 
  previous_scores %>% 
  group_by(Year) %>% 
  mutate(points_that_year = 
           sum(Total)) %>% # total points available that year
  mutate(sd_year_total = 
           sd(Total)) %>% # sd of points that year
  mutate(sd_year_televote = 
           sd(Televote)) %>% # sd of televote 
  mutate(sd_year_jury = 
           sd(Jury)) %>% # sd of jury
  mutate(winning_score_that_year = 
           max(Total)) %>% 
  mutate(mean_score_that_year = 
           mean(Total))

# illustrate relevance of SD
ggplot(previous_scores) + 
  aes(x = sd_year_total,
      y = Total) + 
  geom_point(aes(y = mean_score_that_year),
             size = 10,
             colour = "skyblue") +
  geom_point(data = . %>% 
               filter(is_winner == "Winner"),
             position = position_jitter(height = NULL,
                                        width = 1),
             colour = "black",
             fill = "gold",
             size = 5,
             shape = 23) + 
  geom_point(data = . %>% 
               filter(is_winner == "Not Winner"),
             position = position_jitter(height = NULL,
                                        width = 1)) + 
  theme_minimal()

# compare distributions for last few years
ggplot(previous_scores) +
  aes(x = Total,
      colour = as.factor(Year)) + 
  geom_density()

# what's the range of values within SD?
table(previous_scores$sd_year_total,
      previous_scores$Year)
# goes from about 124 to about 184

# ok so imagine we're going from 100 to 200

# there are 40 countries, meaning the number of points to be awarded is
(1+2+3+4+5+6+7+8+10+12) * 2 * 40
# 4640
# over 25 countries...
4640/25
# mean score will be 185.6

# max possible score?
12*2*39
# 936

# look at distributions that might fit ok
fit_w  <- fitdist(previous_scores$sd_year_total, "weibull")
fit_g  <- fitdist(previous_scores$sd_year_total, "gamma")
fit_ln <- fitdist(previous_scores$sd_year_total, "lnorm")
gofstat(list(fit_w, fit_g, fit_ln), fitnames = c("Weilbull", "Gamma", "Lnorm"))
# lognormal is best of these

# simulate based on the lognormal distribution
m <- 185.6
s <- 124
location <- log(m^2 / sqrt(s^2 + m^2))
shape <- sqrt(log(1 + (s^2 / m^2)))
print(paste("location:", location))
print(paste("shape:", shape))
draws3 <- rlnorm(n=25000000, location, shape)

winning_scores_when_its_close <- 
  draws3 %>% 
  as_tibble() %>% 
  mutate(contest_number = 
           (rep(c(1:1000000), 25))) %>% 
  group_by(contest_number) %>% 
  filter(value == max(value))

# do it again, higher variance

m2 <- 185.6
s2 <- 190
location2 <- log(m2^2 / sqrt(s2^2 + m2^2))
shape2 <- sqrt(log(1 + (s2^2 / m2^2)))
print(paste("location:", location2))
print(paste("shape:", shape2))
draws3_2 <- rlnorm(n=25000000, location2, shape2)


winning_scores_when_its_not_close <- 
  draws3_2 %>% 
  as_tibble() %>% 
  mutate(contest_number = 
           (rep(c(1:1000000), 25))) %>% 
  group_by(contest_number) %>% 
  filter(value == max(value))


# eyeball distributions
winning_scores_when_its_not_close %>% 
  mutate(variety = "Runaway winner (like 2017)") %>% 
  bind_rows(winning_scores_when_its_close %>% 
              mutate(variety = "Close contest (like 2018)")) %>% 
  filter(value < 937) %>% 
  ggplot() + 
  aes(x = value,
      colour = variety) + 
  geom_density() +
  labs(x = "Total points won by the simulated winner",
       y = "",
       colour = "") + 
  theme_minimal() + 
  theme(legend.position = "bottom")
ggsave("overall_simulation.png")

# get the numbers for when it's close...
winning_scores_when_its_close %>% 
  ungroup() %>% 
  filter(value < 937)  %>% 
  summarise(quantile = quantile(value, c(.1, 0.25, 0.5, 0.75, .9)), 
            q = c(.1, 0.25, 0.5, 0.75, .9))

#... and when it's not
winning_scores_when_its_not_close %>% 
  ungroup() %>% 
  filter(value < 937)  %>% 
  summarise(quantile = 
              quantile(value, c(.1, 0.25, 0.5, 0.75, .9)), 
            q = c(.1, 0.25, 0.5, 0.75, .9))
