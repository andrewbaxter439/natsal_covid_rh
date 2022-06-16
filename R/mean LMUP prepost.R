source("R/import and convert.R")
source("R/functions.R")
library(survey)

wave2_data %>% 
  filter(as.integer(PregWhen_w2) %in% 3:4 | D_Preg1yr_w2 == "Yes") %>%
  group_by(D_Preg1yr_w2) %>% 
  summarise(mean_lmup = weighted.mean(D_LMUPScore_w2, weight2),
            sd_lmup = sd(D_LMUPScore_w2))


