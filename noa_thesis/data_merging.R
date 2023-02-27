# Packages

library(tidyverse)
library(lubridate)

# Data collection

names_dat <- read_csv("names_only.csv") %>% 
  janitor::clean_names()

neurotrax_1 <- read_csv("neurotrax_1.csv") %>% 
  janitor::clean_names()
  
neurotrax_2 <- read_csv("neurotrax_2.csv") %>% 
  janitor::clean_names()
  
neurotrax_3 <- read_csv("neurotrax_3.csv") %>% 
  janitor::clean_names()

neurotrax_4 <- read_csv("neurotrax_4.csv") %>% 
  janitor::clean_names()

neurotrax_5 <- read_csv("neurotrax_5.csv") %>% 
  janitor::clean_names()

neurotrax_full_dat <- rbind(neurotrax_1, neurotrax_2, neurotrax_3, neurotrax_4, neurotrax_5) %>% 
  mutate(good_date = mdy(start_date))

neurotrax_dat <- neurotrax_full_dat %>% 
  group_by(name) %>% 
  arrange(good_date) %>%
  slice(1) %>%
  ungroup()  

united_dat <- left_join(names_dat, neurotrax_dat, by = "name") %>% 
  rowwise() %>%
  mutate(score = round(mean(c_across(memory:motor_skills), na.rm = TRUE), 1))
  # mutate(across(tbi:gynecology, as.logical))
  # unite("symptoms", tbi:gynecology, remove = FALSE)

bla <- united_dat %>% 
  filter(is.na(battery)) %>% 
  select(name)

length(unique(united_dat$symptoms))
table(united_dat$symptoms)

write_csv(bla, "missings.csv")

write_csv(neurotrax_full_dat, "neuro_united.csv")

write_csv(united_dat, "names_and_neurotrax.csv")
