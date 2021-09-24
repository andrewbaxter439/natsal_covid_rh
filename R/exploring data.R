summary(wave2_data$qsg)
summary(wave2_data$qethnicity)

wave2_data %>%
  mutate(
    ethnicity_lumped = fct_explicit_na(fct_collapse(
      qethnicity,
      White = "White",
      Other = "Prefer not to say",
      other_level = "Minority"
    ), "Other")
  ) %>% 
  filter(ethnicity_lumped != "Other") %>% 
  ggplot(aes(ethnicity_lumped)) +
  geom_bar()


wave2_data %>% 
  mutate(
    deprivation = fct_collapse(
      qsg,
      high = "D Working class/ E Lower level of subsistence",
      other_level = "low"
    )
  ) %>% 
  ggplot(aes(deprivation)) +
  geom_bar()

rowAny <- function(x) rowSums(x) > 0 

wave2_data %>%
  filter(rowAny(across(ConPre6_w2:ConPre19_w2, ~ .x == "Yes"))) %>%
  filter(across(SwitchStopPan1_w2:SwitchStopPan4_w2, ~ .x %in% c("Yes", "No"))) %>%
  mutate(contr_stop = ifelse(
    SwitchStopPan1_w2 == "Yes" |
      SwitchStopPan2_w2 == "Yes",
    "Yes",
    "No"
  ),
  deprivation = fct_collapse(
    qsg,
    high = "D Working class/ E Lower level of subsistence",
    other_level = "low"
  )) %>% 
  group_by(deprivation, contr_stop) %>% 
  summarise(n = sum(weight2)) %>%
  ggplot(aes(deprivation, n, fill = contr_stop)) +
  geom_col(position = "fill")
# ggplot() +
# geom_mosaic(aes(product(contr_stop, deprivation), fill = contr_stop))

?glm


depr_conStop_mod1 <- wave2_data %>%
  filter(rowAny(across(ConPre6_w2:ConPre19_w2, ~ .x == "Yes"))) %>%
  filter(across(SwitchStopPan1_w2:SwitchStopPan4_w2, ~ .x %in% c("Yes", "No"))) %>%
  mutate(contr_stop = ifelse(
    SwitchStopPan1_w2 == "Yes" |
      SwitchStopPan2_w2 == "Yes",
    1,
    0
  ),
  deprivation = fct_collapse(
    qsg,
    high = "D Working class/ E Lower level of subsistence",
    other_level = "low"
  )) %>% 
  glm(contr_stop ~ deprivation, weights = .$weight2, data = ., family = binomial)

exp(coef(depr_conStop_mod1)["deprivationlow"])
exp(confint(depr_conStop_mod1)["deprivationlow",])