# Packages

library(tidyverse)
library(lubridate)

# Data collection

grouping_dat <- read_csv("grouping_dat.csv") %>% 
  janitor::clean_names() %>% 
  # mutate(across(tbi:hypertension, as.logical)) %>% 
  rowwise() %>% 
  mutate(n_etiologies = sum(c_across(tbi:anesthesia))) %>% 
  unite("symptoms", tbi:anesthesia, remove = FALSE)

write_csv(as.data.frame(table(grouping_dat$symptoms)), "bla.csv")

