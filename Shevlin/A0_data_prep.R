rm(list=ls()) 

library(dplyr)
library(tidyr)

dataAll = read.csv("boxData_mixed_only.csv", header = TRUE)

v = sort(unique(c(unique(dataAll$ValL), unique(dataAll$ValR))))
v_r = setNames(1:27, v)
# Format the data for model fitting 
# choice: 1 = correct, 0 = incorrect
# xi = 1 (correct option value), -1 (incorrect option value), 0 otherwise
data_for_model = dataAll %>%
  mutate(X = 1:n(), 
         vc = ifelse(ValL > ValR, ValL, ValR),# higher value 
         vi = ifelse(ValL > ValR, ValR, ValL)) %>%
  mutate(vc2 = recode(vc, !!!v_r), 
         vi2 = recode(vi, !!!v_r)) %>%
  select(X, choice, RT, vc2, vi2) %>%
  pivot_longer(!c(X, choice, RT), 
               names_to = "tmpName", 
               values_to = "item_idx") %>%
  mutate(pos_idx = ifelse(tmpName == "vc2", 1, -1)) %>% 
  pivot_wider(!tmpName, 
              names_from = item_idx, 
              names_prefix = "x",
              values_from = pos_idx,
              values_fill = 0) %>% 
  rename(response = choice) %>% 
  mutate(rt = RT/1000,
         subj_idx = 1) %>%
  select(subj_idx, response, rt, sprintf("x%d", 1:27))
  
write.csv(data_for_model, 'Shevlin_hddm.csv', row.names = FALSE)


