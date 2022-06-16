library(tidyverse)
source("R/import and convert.R")
source("R/functions.R")


wave2_data %>% 
  filter(!is.na(D_LMUPScore_w2)) %>% 
  mutate(preg_when = if_else(D_Preg1yr_w2 == "Yes", "last year", "1-5 yrs")) %>% 
  group_by(preg_when) %>% 
  summarise(mean_sc = weighted.mean(D_LMUPScore_w2, w = weight2, na.rm = TRUE),
            # sd_sc = sd(D_LMUPScore_w2, na.rm = TRUE),
            sd_sc = sqrt(Hmisc::wtd.var(D_LMUPScore_w2, weights = weight2, na.rm = TRUE)))

# What % of pregnancies were unplanned?
wave2_data %>% 
  filter(!is.na(D_LMUPScore_w2)) %>% 
  mutate(preg_when = if_else(D_Preg1yr_w2 == "Yes", "last year", "1-5 yrs"),
         unplanned = if_else(D_LMUPCat_w2 == "Unplanned", 1, 0)) %>%
  group_by(preg_when) %>% 
  summarise(prop = sum(unplanned)/n())


  glm(unplanned ~ preg_when, family = "binomial", data = ., weights = weight2) %>% 
  coefficients %>% 
  exp
  
df <-   wave2_data %>% 
    filter(!is.na(D_LMUPScore_w2)) %>% 
    mutate(preg_when = if_else(D_Preg1yr_w2 == "Yes", "last year", "1-5 yrs"),
           unplanned = if_else(D_LMUPCat_w2 == "Unplanned", 1, 0)) 

library(survey)

surv <- svydesign(id = ~NatSal_serial_A, weights = ~weight2, data = df)

mod1 <- svyglm(unplanned ~ preg_when, family = "binomial", design = surv)
mod2 <- svyglm(unplanned ~ preg_when + D_Age5Cat_w2, family = "binomial", design = surv)

anova(mod1, mod2)
