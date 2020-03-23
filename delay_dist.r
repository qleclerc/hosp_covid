## get linelist
library("tidyverse")

ll <- NCoVUtils::get_linelist()

## filter and format
df <- ll %>%
  dplyr::filter(!is.na(date_admission_hospital),
         !is.na(date_death_or_discharge)) %>%
  select(age, country, date_admission_hospital, outcome,
         date_death_or_discharge) %>%
  mutate(date_death = as.Date(date_death_or_discharge, format = "%d.%m.%Y"),
         delay = date_death-date_admission_hospital) %>%
  filter(country == "China" & delay < 30) %>%
  select(-country)

## delay distribution
delays <- as.integer(df$delay)
saveRDS(delays, "delay_dist_linelist.rds")

## sample 10 delays
sample(delays, 10, replace = TRUE)

h <- hist(delays)


## filter and format
df = ll %>%
  select(ID, age, date_admission_hospital,
           outcome, date_death_or_discharge) %>%
  filter(!is.na(age), !is.na(outcome)) %>%
  mutate(date_death = as.Date(date_death_or_discharge, format = "%d.%m.%Y"),
         delay = date_death - date_admission_hospital) %>%
  select(-date_death_or_discharge) %>%
  mutate(age = as.integer(age)) %>%
  filter(!is.na(age), !is.na(delay)) %>%
  mutate(delay = as.integer(delay)) %>%
  mutate(age = cut(age, breaks = seq(0,100,10)))

ggplot(df, aes(age,delay)) +
  geom_boxplot() +
  geom_point()
