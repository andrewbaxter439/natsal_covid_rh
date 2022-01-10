source(file.path(old_wd, "R/import and convert.R"))
source(file.path(old_wd, "R/functions.R"))
library(SPHSUgraphs)
library(patchwork)
library(sandwich)
library(rlang)
library(gt)


### Filter for pregnant in last year!!! ###

wave2_data %>%
  filter(!is.na(D_LMUPScore_w2), D_Preg1yr_w2 == "Yes") %>%
  group_by(D_LMUPScore_w2, D_LMUPCat_w2) %>%
  summarise(tot = sum(weight2)) %>%
  ungroup() %>%
  mutate(prop = tot / sum(tot)) %>%
  ggplot(aes(
    factor(D_LMUPScore_w2, levels = 0:12),
    y = prop,
    fill = D_LMUPCat_w2
  )) +
  geom_bar(stat = "identity") +
  stat_summary(
    geom = "text",
    aes(
      label = after_stat(label),
      group = D_LMUPCat_w2,
      x = c(rep(2.5, 3), rep(7.5, 6), rep(12, 3))
    ),
    fun.data = function(x) {
      tibble(y = max(x) + 0.02,
             label = scales::percent(sum(x), accuracy = 0.1))
    }
  ) +
  stat_summary(
    geom = "segment",
    size = 1,
    aes(colour = D_LMUPCat_w2,
        x = c(rep(0, 3), rep(4, 6), rep(10, 3)) + 1,
        xend = c(rep(3, 3), rep(9, 6), rep(12, 3)) + 1),
    fun.data = function(x) {
      tibble(
        y = max(x) + 0.01,
        yend = y
      )
    }
  ) +
  scale_y_continuous("Proportion of pregnancies", labels = scales::percent) +
  scale_fill_sphsu(name = "Category of\npregnancy planning") +
  scale_colour_sphsu(name = "Category of\npregnancy planning") +
  scale_x_discrete("London Measure of Unplanned Pregnancy score", drop = FALSE) +
  theme(panel.background = element_blank(),
        legend.key = element_blank())

ggsave(file.path(old_wd, "graphs/LMUP_scores.png"), width = 250, height = 120, units = "mm", dpi = 300)

# going glms --------------------------------------------------------------



wave2_data %>% 
  glm(D_Planned_preg_1yr ~ D_Age5Cat_w2, data = ., family = binomial, weights = weight2) %T>%
  {broom::tidy(.) %>% print()} %>% 
  {cat("Robust SEs\n"); print(sqrt(diag(vcovHC(., type = "HC0"))))}


robust_glm(wave2_data, D_Planned_preg_1yr ~ D_Age5Cat_w2, weight2)
robust_glm(wave2_data, D_Planned_preg_1yr ~ qsg + D_Age5Cat_w2, weight2)
robust_glm(wave2_data, D_Planned_preg_1yr ~ qethnicity + D_Age5Cat_w2, weight2)
robust_glm(wave2_data, D_Planned_preg_1yr ~ D_relstatcatv7_w2 + D_Age5Cat_w2, weight2)
robust_glm(wave2_data, D_Planned_preg_1yr ~ D_Edu3Cat_w2 + D_Age5Cat_w2, weight2)


# by relationship status - remove Single ----------------------------------
# No longer required - don't know why!
# wave2_data %>% 
#   filter(D_relstatcatv7_w2 != "Single") %>% 
#   mutate(D_relstatcatv7_w2 = fct_drop(D_relstatcatv7_w2)) %>% 
#   robust_glm(D_Planned_preg_1yr ~ D_relstatcatv7_w2, weight2)
# 
# wave2_data %>% 
#   filter(D_relstatcatv7_w2 != "Single") %>% 
#   mutate(D_relstatcatv7_w2 = fct_drop(D_relstatcatv7_w2)) %>% 
#   robust_glm(D_Planned_preg_1yr ~ D_relstatcatv7_w2 + D_Age5Cat_w2, weight2)
# 


# Proportions of pregnancies with known outcomes --------------------------


preg_dataset <- wave2_data %>% 
  select(D_Preg1yr_w2,
         D_LMUPScore_w2,
         D_LMUPCat_w2,
         D_Age5Cat_w2,
         D_EthnicityCombined_w2,
         D_SexIDL_w2,
         qsg,
         D_Edu3Cat_w2,
         D_relstatcatv7_w2,
         EconActChg4_w2,
         EconActChg5_w2,
         D_drinkGrp_w2,
         SDSdrinkchangeW2_w2,
         Smokenow_w2,
         D_PHQ2Cat_w2,
         D_GAD2Cat_w2,
         weight2) %>% 
  mutate(Preg_unpl_amb = if_else(D_LMUPScore_w2 < 10 & D_Preg1yr_w2 == "Yes", 1, 0, 0),
         # RelStat_nosing = factor(D_relstatcatv7_w2, exclude = "Single")  # no longer required
         )

preg_perc <-
  preg_dataset %>% 
  select(- D_LMUPScore_w2) %>% 
  mutate(Total = "Total") %>% 
  pivot_longer(- c(Preg_unpl_amb, D_Preg1yr_w2, weight2), names_to = "Comparison", values_to = "Cat") %>% 
  filter(!is.na(Cat)) %>%
  group_by(Comparison, Cat) %>%
  summarise(preg_p = sum(weight2[D_Preg1yr_w2 == "Yes"])/sum(weight2),
            unpl_p = sum(weight2[Preg_unpl_amb == 1])/sum(weight2),
            preg_p_ll = perc_ci(preg_p, "l", sum(weight2)),
            preg_p_ul = perc_ci(preg_p, "u", sum(weight2)),
            unpl_p_ll = perc_ci(unpl_p, "l", sum(weight2)),
            unpl_p_ul = perc_ci(unpl_p, "u", sum(weight2)),
            ) %>% 
    mutate(`Pregnancy in last year_%` = scales::percent(preg_p, accuracy =  0.1),
           `Pregnancy in last year_CI` = paste0("(", round(preg_p_ll*100, 1), ", ", round(preg_p_ul*100, 1), ")"),
           `Unplanned/ambivalent pregnancy_%` = scales::percent(unpl_p, accuracy =  0.1),
           `Unplanned/ambivalent pregnancy_CI` = paste0("(", round(unpl_p_ll*100, 1), ", ", round(unpl_p_ul*100, 1), ")"))
  
unadj_ors <- preg_dataset %>% 
  select(- D_LMUPScore_w2) %>% 
  pivot_longer(- c(Preg_unpl_amb, D_Preg1yr_w2, weight2), names_to = "Comparison", values_to = "Cat") %>% 
  filter(!is.na(Cat)) %>%
  group_by(Comparison) %>% 
  nest() %>% 
  mutate(mod = map(data, ~ return_ORs(.x, Preg_unpl_amb ~ Cat, weight2))) %>% 
  unnest(mod) %>% 
  mutate(OR = aOR, aOR = NULL)
  
adj_ors <- preg_dataset %>% 
  select(- D_LMUPScore_w2) %>% 
  pivot_longer(- c(Preg_unpl_amb, D_Age5Cat_w2, D_Preg1yr_w2, weight2), names_to = "Comparison", values_to = "Cat") %>% 
  filter(!is.na(Cat)) %>%
  group_by(Comparison) %>% 
  nest() %>% 
  mutate(mod = map(data, ~ return_ORs(.x, Preg_unpl_amb ~ Cat + D_Age5Cat_w2, weight2))) %>% 
  unnest(mod)



comp_labels <- tibble(
  Comparison = c(
  "D_Age5Cat_w2",
  "qsg",
  "D_EthnicityCombined_w2",
  "D_Edu3Cat_w2",
  "EconActChg4_w2",
  "EconActChg5_w2",
  "D_drinkGrp_w2",
  "SDSdrinkchangeW2_w2",
  "Smokenow_w2",
  "D_relstatcatv7_w2",
  "D_PHQ2Cat_w2",
  "D_GAD2Cat_w2")
) %>%
  mutate(label = map_chr(Comparison, function(exp_var) {
    wave2_data %>%
      pull(exp_var) %>%
      attr("label")
  }))

# tribble(
#   ~Comparison, ~label,
#   "D_Age5Cat_w2", "Age group",
#   "qsg", "Social grade",
#   "D_EthnicityCombined_w2", "Ethnicity",
#   "D_relstatcatv7_w2", "Relationship/Cohabiting status",
#   "D_Edu3Cat_w2", "Education"
# )



preg_perc %>% select(
  Comparison,
  `Pregnancy in last year_%`,
  `Pregnancy in last year_CI`,
  `Unplanned/ambivalent pregnancy_%`,
  `Unplanned/ambivalent pregnancy_CI`,
  Cat
) %>% 
  left_join(
    adj_ors %>%
      select(
        Comparison,
        Cat,
        `Age-adjusted Odds ratio_aOR` = aOR,
        `Age-adjusted Odds ratio_CI` = CI,
        `Age-adjusted Odds ratio_p-value` = P
      ),
    by = c("Comparison", "Cat")
  ) %>%
  left_join(
    unadj_ors %>%
      select(
        Comparison,
        Cat,
        `Unadjusted Odds ratio_OR` = OR,
        `Unadjusted Odds ratio_CI` = CI,
        `Unadjusted Odds ratio_p-value` = P
      ),
    by = c("Comparison", "Cat")
  ) %>%
  left_join(comp_labels, by = "Comparison") %>%
  ungroup() %>%
  filter(!is.na(Cat)) %>% 
    mutate(` ` = " ") %>%
  select(
    ` `,
    "  " = Cat,
    `Pregnancy in last year_%`,
    `Pregnancy in last year_CI`,
    `Unplanned/ambivalent pregnancy_%`,
    `Unplanned/ambivalent pregnancy_CI`,
    `Unadjusted Odds ratio_OR`,
    `Unadjusted Odds ratio_CI`,
    `Unadjusted Odds ratio_p-value`,
    `Age-adjusted Odds ratio_aOR`,
    `Age-adjusted Odds ratio_CI`,
    `Age-adjusted Odds ratio_p-value`,
    # `p-value`,
    label
  ) %>%
  mutate(across(everything(), .fns = replace_na, "-")) %>%
  pivot_longer(
    3:12,
    names_to = c("outcome", "met"),
    values_to = "val",
    names_sep = "_"
  ) %>%
  pivot_wider(
    id_cols = c(` `, `  `, label, outcome),
    names_from = met,
    values_from = val
  ) %>%
  mutate(
    `% (CI)` = paste(`%`, CI),
    `OR (CI)` = paste(OR, CI),
    `aOR (CI)` = paste(aOR, CI),
    across(everything(), ~str_replace_all(.x, "- -", "-"))
  ) %>%
  # View()
  pivot_wider(
    id_cols = c(` `, `  `, label),
    names_from = outcome,
    values_from = c(`% (CI)`,
                    `OR (CI)`,
                    `aOR (CI)`,
                    `p-value`),
    names_glue = "{outcome}_{.value}"
  ) %>% 
  select(
    ` `,
    `  `,
    label,
    `Pregnancy in last year_% (CI)`,
    `Unplanned/ambivalent pregnancy_% (CI)`,
    `Unadjusted Odds ratio_OR (CI)`,
    `Unadjusted Odds ratio_p-value`,
    `Age-adjusted Odds ratio_aOR (CI)`,
    `Age-adjusted Odds ratio_p-value`
  ) %>% 
  gt(groupname_col = "label", rowname_col = " ") %>%
  summary_rows(
    fns = list(" "  = ~ " "),
    columns = ` `,
    groups = TRUE,
    missing_text = " "
  ) %>%
  tab_spanner_delim("_") %>%
  gtsave(., "preg_unpl_OR_plus.html")

# temp bit - calculating se for percs -------------------------------------


preg_dataset %>% 
  select(- D_LMUPScore_w2) %>% 
  pivot_longer(- c(Preg_unpl_amb, D_Preg1yr_w2, weight2), names_to = "Comparison", values_to = "Cat") %>% 
  group_by(Comparison, Cat) %>%
  filter(Comparison == "D_Age5Cat_w2") %>% 
  summarise(preg_n = sum(weight2[D_Preg1yr_w2 == "Yes"]),
            nopreg_n = sum(weight2[D_Preg1yr_w2 == "No"]),
            preg_p = preg_n/sum(weight2),
            n = sum(weight2), 
            se = sqrt((preg_p*(1-preg_p)*(1/(sum(weight2))))),
            p_t = log(preg_p/(1-preg_p)),
            ll_t = p_t-qt(0.975, 1480)*se/(preg_p*(1-preg_p)),
            ul_t = p_t+qt(0.975, 1480)*se/(preg_p*(1-preg_p)),
            ll = exp(ll_t)/(1+exp(ll_t)),
            ul = exp(ul_t)/(1+exp(ul_t)),
            )



            