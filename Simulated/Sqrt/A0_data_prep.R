rm(list=ls()) 

library(dplyr)
library(tidyr)

dataAll = read.csv("sims_0006_02_350_0_sqrt.csv", header = TRUE)

# Format the data for model fitting 
# choice: 1 = left, 0 = right 
# xi = 1 (left), -1 (right), 0 otherwise
data_for_model = dataAll %>%
  pivot_longer(!c(X, choice, rt), 
               names_to = "tmpName", 
               values_to = "item_idx") %>%
  mutate(pos_idx = ifelse(tmpName == "leftval", 1, -1)) %>% 
  pivot_wider(!tmpName, 
              names_from = item_idx, 
              names_prefix = "x",
              values_from = pos_idx,
              values_fill = 0) %>% 
  rename(response = choice) %>%
  mutate(rt = rt / 1000, 
         subj_idx = 1) %>%
  select(subj_idx, response, rt, sprintf("x%d", 1:10))
  
write.csv(data_for_model, 
          'sims_0006_02_350_0_sqrt_hddm.csv', row.names = FALSE)


## dataset2: 10 subjects
dataAll = read.csv("sims_sqrt_10subs_params.csv", header = TRUE)

# Format the data for model fitting 
# choice: 1 = left, 0 = right 
# xi = 1 (left), -1 (right), 0 otherwise
data_for_model = dataAll %>%
  select(X, choice, rt, leftval, rightval) %>%
  pivot_longer(!c(X, choice, rt), 
               names_to = "tmpName", 
               values_to = "item_idx") %>%
  mutate(pos_idx = ifelse(tmpName == "leftval", 1, -1)) %>% 
  pivot_wider(!tmpName, 
              names_from = item_idx, 
              names_prefix = "x",
              values_from = pos_idx,
              values_fill = 0) %>% 
  rename(response = choice) %>%
  mutate(rt = rt / 1000, 
         subj_idx = 1) %>%
  select(subj_idx, response, rt, sprintf("x%d", 1:10))

write.csv(data_for_model, 'sims_sqrt_10subs_hddm.csv', row.names = FALSE)
