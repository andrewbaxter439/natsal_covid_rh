library(tidyverse)
library(ggmosaic)

wave2_data %>% 
  group_by(qsg) %>% 
  summarise(CondomAcc_perc = sum(CondomAcc_w2=="Yes")/n()) %>% 
  ggplot(aes(qsg, y = CondomAcc_perc)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent)

wave2_data %>% 
  group_by(qtarget) %>% 
  summarise(CondomAcc_perc = sum(CondomAcc_w2=="Yes")/n()) %>% 
  mutate(recr = str_wrap(qtarget, 20)) %>% 
  ggplot(aes(recr, y = CondomAcc_perc)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent)

wave2_data %>% 
  mutate(missing_IMD = ifelse(CombinedIMD == "Missing in IPSOS data", "Yes", "No"), 
         recr = str_wrap(qtarget, 20)) %>% 
  ggplot() +
  geom_mosaic(aes(product(missing_IMD, recr), fill = missing_IMD))


wave2_data %>% 
  filter(CondomAcc_w2 %in% c("Yes", "No")) %>% 
  mutate(recr = str_wrap(qtarget, 20),
         CondomAcc_w2 = fct_drop(CondomAcc_w2)) %>% 
  ggplot() +
  geom_mosaic(aes(product(CondomAcc_w2, recr), fill = CondomAcc_w2))

wave2_data %>% 
  filter(CondomAcc_w2 %in% c("Yes", "No")) %>% 
  mutate(missing_IMD = ifelse(CombinedIMD == "Missing in IPSOS data", "Yes", "No"), 
         CondomAcc_w2 = fct_drop(CondomAcc_w2)) %>% 
  glm(CondomAcc_w2 ~ missing_IMD, family = "binomial", data = .) %T>% 
  {cat("Odds Ratio:"); coefficients(.)[[2]] %>% exp() %>% cat()} %>% 
  {confint(.)[2,] %>% exp()}
