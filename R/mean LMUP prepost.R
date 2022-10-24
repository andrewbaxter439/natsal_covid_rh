source("R/import and convert.R")
source("R/functions.R")
library(survey)

wave2_data %>% 
  filter(as.integer(PregWhen_w2) %in% 3:4 | D_Preg1yr_w2 == "Yes") %>%
  group_by(D_Preg1yr_w2) %>% 
  summarise(mean_lmup = weighted.mean(D_LMUPScore_w2, weight2),
            sd_lmup = sd(D_LMUPScore_w2),
            perc_unpl = sum((D_LMUPCat_w2 == "Unplanned") * weight2)/sum(weight2),
            ll = perc_ci(perc_unpl, n = sum(weight2)),
            ul = perc_ci(perc_unpl, "u", n = sum(weight2)),
            n = n())


