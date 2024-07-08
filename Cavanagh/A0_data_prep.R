rm(list=ls()) 

library(dplyr)
library(tidyr)

load("cavanagh_data_full.rdata")

sort(unique(c(unique(sims$vc), unique(sims$vi))))

# Format the data for model fitting 
# choice: 1 = correct, 0 = incorrect
# xi = 1 (correct option value), -1 (incorrect option value), 0 otherwise
data_for_model = sims %>%
  select(!subject) %>%
  mutate(X = row_number()) %>%
  pivot_longer(!c(X, rt, choice), 
               names_to = "tmpName", 
               values_to = "item_idx") %>%
  mutate(pos_idx = ifelse(tmpName == "vc", 1, -1)) %>% 
  pivot_wider(id_cols = !tmpName, 
              names_from = item_idx, 
              names_prefix = "x",
              values_from = pos_idx,
              values_fill = 0) %>% 
  mutate(rt = rt/1000,
         subj_idx = 1) %>%
  rename(response = choice) %>%
  select(subj_idx, response, rt, sprintf("x%d", 1:6))
  
write.csv(data_for_model, 'CavanaghDataset_full_hddm.csv', row.names = FALSE)


