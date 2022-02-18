source("R/import and convert.R")
source("R/functions.R")
library(SPHSUgraphs)
library(patchwork)
library(sandwich)
library(rlang)
library(gt)


# Variable labels ---------------------------------------------
comp_labels <- tibble(
  Comparison = c(
    "D_Age5Cat_w2",
    "qsg",
    "D_EthnicityCombined_w2",
    "Total",
    "D_Edu3Cat_w2",
    "D_SexIDL_w2",
    "EconActChg4_w2",
    "EconActChg5_w2",
    "D_drinkGrp_w2",
    "SDSdrinkchangeW2_w2",
    "Smokenow_w2",
    "D_relstatcatv7_w2",
    "D_PHQ2Cat_w2",
    "D_GAD2Cat_w2",
    "D_ConServAcc_w2",
    "CondomAcc_w2")
) %>%
  mutate(label = map_chr(Comparison, function(exp_var) {
    wave2_data %>%
      pull(exp_var) %>%
      attr("label")
  }))

# start here: new table - ORs for pregnancy, ORs and lms for unplanned ----------------

preg_dataset <- wave2_data %>% 
  select(D_Preg1yr_w2,
         D_LMUPScore_w2,
         D_LMUPCat_w2,
         Total,
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
         D_ConServAcc_w2,
         CondomAcc_w2,
         weight2) %>% 
  mutate(Preg_unpl = case_when(
    D_Preg1yr_w2 == "No" ~ NA_integer_,
    D_LMUPCat_w2 == "Unplanned" ~ 1L,
    TRUE ~ 0L
  ),
  D_Edu3Cat_w2 = fct_rev(D_Edu3Cat_w2),
  SDSdrinkchangeW2_w2 = fct_rev(SDSdrinkchangeW2_w2))


# putting all together ----------------------------------------------------

## percentages in each outcome ----------------

all_preg_perc <- preg_dataset %>% 
  select(- D_LMUPScore_w2, -D_LMUPCat_w2) %>% 
  pivot_longer(- c(Preg_unpl, D_Preg1yr_w2, weight2), names_to = "Comparison", values_to = "Cat") %>% 
  mutate(Comparison = fct_inorder(Comparison)) %>% 
  filter(!is.na(Cat)) %>%
  group_by(Comparison, Cat) %>%
  summarise(preg_p = sum(weight2[D_Preg1yr_w2 == "Yes"])/sum(weight2),
            preg_p_ll = perc_ci(preg_p, "l", sum(weight2)),
            preg_p_ul = perc_ci(preg_p, "u", sum(weight2)),
  ) %>% 
  mutate(`Pregnancy in last year_%` = sprintf("%.1f", preg_p*100),
         `Pregnancy in last year_CI` = paste0("(", sprintf("%.1f", round(preg_p_ll*100, 1)), ", ", sprintf("%.1f", round(preg_p_ul*100, 1)), ")"),
         ) %>% 
  arrange(Comparison)


# does stata use weighted n obvs in calculating se??

unpl_preg_perc <-  preg_dataset %>%
  select(-D_LMUPScore_w2,-D_LMUPCat_w2) %>%
  filter(!is.na(Preg_unpl)) %>%
  pivot_longer(-c(Preg_unpl, D_Preg1yr_w2, weight2),
               names_to = "Comparison",
               values_to = "Cat") %>%
  filter(!is.na(Cat)) %>%
  # filter(Comparison == "D_SexIDL_w2") %>%
  # group_by(Comparison) %>%
  # mutate(n = sum(weight2)) %>%
  group_by(Comparison, Cat) %>%
  summarise(
    unpl_p = sum(weight2[Preg_unpl == 1]) / sum(weight2),
    # n = n(),
    # wt = sum(weight2),
    # se = sqrt(unpl_p*(1-unpl_p)*(1/n())),
    # lt = log(unpl_p/(1-unpl_p)) - qt(0.975, n)*se/(unpl_p*(1-unpl_p)),
    # unpl_p_ll = exp(lt)/(1+exp(lt)),
    unpl_p_ll = perc_ci(unpl_p, "l", n()),
    unpl_p_ul = perc_ci(unpl_p, "u", n())
  ) %>%
  mutate(
    `Unplanned pregnancy_%` = sprintf("%.1f", unpl_p * 100),
    `Unplanned pregnancy_CI` = paste0("(",
                                      sprintf("%.1f", round(unpl_p_ll * 100, 1)),
                                      ", ",
                                      sprintf("%.1f", round(unpl_p_ul * 100, 1)),
                                      ")")
  )

# odds of all pregnancy (from all - adjusted)


unadj_ors_p <- preg_dataset %>% 
  select(- D_LMUPScore_w2, -D_LMUPCat_w2, -Total) %>% 
  pivot_longer(- c(Preg_unpl, D_Preg1yr_w2, weight2), names_to = "Comparison", values_to = "Cat") %>% 
  filter(!is.na(Cat)) %>%
  group_by(Comparison) %>% 
  nest() %>% 
  mutate(mod = map(data, ~ return_ORs(.x, D_Preg1yr_w2 ~ Cat, weight2))) %>% 
  unnest(mod) %>% 
  mutate(OR = aOR, aOR = NULL)

adj_ors_p <- preg_dataset %>% 
  select(- D_LMUPScore_w2, -D_LMUPCat_w2, -Total) %>% 
  pivot_longer(- c(Preg_unpl, D_Age5Cat_w2, D_Preg1yr_w2, weight2), names_to = "Comparison", values_to = "Cat") %>% 
  filter(!is.na(Cat)) %>%
  group_by(Comparison) %>% 
  nest() %>% 
  mutate(mod = map(data, ~ return_ORs(.x, D_Preg1yr_w2 ~ Cat + D_Age5Cat_w2, weight2))) %>% 
  unnest(mod)

## odds of unplanned ------------------

unadj_ors <- preg_dataset %>% 
  select(- D_LMUPScore_w2, -D_LMUPCat_w2, -Total) %>% 
  filter(!is.na(Preg_unpl)) %>% 
  pivot_longer(- c(Preg_unpl, D_Preg1yr_w2, weight2), names_to = "Comparison", values_to = "Cat") %>% 
  filter(!is.na(Cat)) %>%
  group_by(Comparison) %>% 
  nest() %>% 
  mutate(mod = map(data, ~ return_ORs(.x, Preg_unpl ~ Cat, weight2))) %>% 
  unnest(mod) %>% 
  mutate(OR = aOR, aOR = NULL)


adj_ors <- preg_dataset %>% 
  select(- D_LMUPScore_w2, -D_LMUPCat_w2, -Total) %>% 
  filter(!is.na(Preg_unpl)) %>% 
  pivot_longer(- c(Preg_unpl, D_Age5Cat_w2, D_Preg1yr_w2, weight2), names_to = "Comparison", values_to = "Cat") %>% 
  filter(!is.na(Cat)) %>%
  group_by(Comparison) %>% 
  nest() %>% 
  mutate(mod = map(data, ~ return_ORs(.x, Preg_unpl ~ Cat + D_Age5Cat_w2, weight2))) %>% 
  unnest(mod)


## linear regression ---------------------

unadj_lin <- preg_dataset %>% 
  filter(!is.na(Preg_unpl)) %>% 
  select(- Preg_unpl, -D_LMUPCat_w2, -D_Preg1yr_w2, -Total) %>% 
  pivot_longer(- c(D_LMUPScore_w2, weight2), names_to = "Comparison", values_to = "Cat") %>% 
  mutate(inv_score = D_LMUPScore_w2, .keep = "unused") %>% 
  group_by(Comparison) %>% 
  nest() %>% 
  mutate(mod = map(data, ~ robust_lm(.x, inv_score ~ Cat, weight2))) %>% 
  unnest(mod) %>% 
  mutate(est = sprintf("%.2f", round(est, 2)), 
         CI = paste0("(", sprintf("%.2f", round(ll, 2)), ", ", sprintf("%.2f", round(ul, 2)), ")"),
         coef = str_remove(coef, "^Cat"),
         P = case_when(
           p < 0.001 ~  "<0.001",
           TRUE ~ as.character(round(p, 3))
         ))

adj_lin <- preg_dataset %>% 
  filter(!is.na(Preg_unpl)) %>% 
  select(- Preg_unpl, -D_LMUPCat_w2, -D_Preg1yr_w2, -Total) %>% 
  pivot_longer(- c(D_LMUPScore_w2, D_Age5Cat_w2, weight2), names_to = "Comparison", values_to = "Cat") %>% 
  mutate(inv_score = D_LMUPScore_w2, .keep = "unused") %>% 
  group_by(Comparison) %>% 
  nest() %>% 
  mutate(mod = map(data, ~ robust_lm(.x, inv_score ~ Cat + D_Age5Cat_w2, weight2))) %>% 
  unnest(mod) %>% 
  mutate(adj_est = sprintf("%.2f", round(est, 2)), est = NULL,
         CI = paste0("(", sprintf("%.2f", round(ll, 2)), ", ", sprintf("%.2f", round(ul, 2)), ")"),
         coef = str_remove(coef, "^Cat"),
         P = case_when(
           p < 0.001 ~  "<0.001",
           TRUE ~ as.character(round(p, 3))
         ))



# testing score diff by age -----------------------------------------------

preg_dataset %>% 
  filter(!is.na(D_LMUPScore_w2), D_Preg1yr_w2 == "Yes") %>% 
  group_by(D_Age5Cat_w2) %>% 
  summarise(mean_lmup = weighted.mean(D_LMUPScore_w2, w = weight2))

preg_dataset %>% 
  filter(!is.na(D_LMUPScore_w2), D_Preg1yr_w2 == "Yes") %>% 
  robust_lm(D_LMUPScore_w2 ~ D_Age5Cat_w2, weight2) 


# mean scores and denominators --------------------------------------------

scores_denoms <- preg_dataset %>% 
  # filter(!is.na(Preg_unpl)) %>% 
  mutate(D_LMUPScore_w2 = if_else(is.na(Preg_unpl), na_dbl, D_LMUPScore_w2)) %>% 
  select(- Preg_unpl, -D_LMUPCat_w2, -D_Preg1yr_w2) %>% 
  pivot_longer(- c(D_LMUPScore_w2, weight2), names_to = "Comparison", values_to = "Cat") %>% 
  filter(!is.na(Cat)) %>% 
  group_by(Comparison, Cat) %>% 
  summarise(mean_sc = weighted.mean(D_LMUPScore_w2, w = weight2, na.rm = TRUE),
            # sd_sc = sd(D_LMUPScore_w2, na.rm = TRUE),
            sd_sc = sqrt(Hmisc::wtd.var(D_LMUPScore_w2, weights = weight2, na.rm = TRUE)),
            wt = sum(weight2),
            n = n(),
            .groups = "keep")  %>% 
  transmute(`Mean LMUP Score (SD)` = paste0(round(mean_sc, 1), " (", round(sd_sc, 1), ")"),
            `Denominator (weighted, unweighted)` = paste0(round(wt, 0), ", ", round(n, 0))) %>% 
  ungroup() %>% 
  left_join(comp_labels, by = "Comparison") %>% 
  select(-Comparison)



# output table - odds and linear --------------------------------------------------------




all_preg_perc %>% 
  left_join(unpl_preg_perc, by = c("Comparison", "Cat")) %>% 
  select(
  Comparison,
  `Pregnancy in last year_%`,
  `Pregnancy in last year_CI`,
  `Unplanned pregnancy_%`,
  `Unplanned pregnancy_CI`,
  Cat
) %>% 
  left_join(
    bind_rows(
    unadj_ors_p %>%
      select(
        Comparison,
        Cat,
        `Pregnancy adjusted Odds ratio_OR` =OR,
        `Pregnancy adjusted Odds ratio_CI` = CI,
        `Pregnancy adjusted Odds ratio_p-value` = P
      ) %>% 
    filter(Comparison == "D_Age5Cat_w2"),
    adj_ors_p %>%
      select(
        Comparison,
        Cat,
        `Pregnancy adjusted Odds ratio_OR` = aOR,
        `Pregnancy adjusted Odds ratio_CI` = CI,
        `Pregnancy adjusted Odds ratio_p-value` = P
      )),
    by = c("Comparison", "Cat")
  ) %>% 
  left_join(
    bind_rows(
      unadj_lin %>% 
        select(
          Comparison,
          Cat = coef,
          `Unplanned pregnancy score_Coefficient` = est,
          `Unplanned pregnancy score_CI` = CI,
          `Unplanned pregnancy score_p-value` = P
        ) %>% 
        filter(Comparison == "D_Age5Cat_w2"),
    adj_lin %>%
      select(
        Comparison,
        Cat = coef,
        `Unplanned pregnancy score_Coefficient` = adj_est,
        `Unplanned pregnancy score_CI` = CI,
        `Unplanned pregnancy score_p-value` = P
      )),
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
    `Unplanned pregnancy_%`,
    `Unplanned pregnancy_CI`,
    # `Mean LMUP Score (SD)`,
    # `Pregnancy adjusted Odds ratio_OR`,
    # `Pregnancy adjusted Odds ratio_CI`,
    # `Pregnancy adjusted Odds ratio_p-value`,
    `Unplanned pregnancy score_Coefficient`,
    `Unplanned pregnancy score_CI`,
    `Unplanned pregnancy score_p-value`,
    # `Denominator (weighted, unweighted)`,
    # `p-value`,
    label
  ) %>%
  rowwise() %>%
  # mutate(across(c(`Unplanned pregnancy score_Coefficient`,
  #                 `Unplanned pregnancy score_CI`,
  #                 `Unplanned pregnancy score_p-value`), ~ ifelse(`Unplanned pregnancy_%` == "0.0", NA, .x))) %>% 
  # mutate(across(`Unplanned pregnancy_CI`:`Age-adjusted Odds ratio_p-value`, ~ ifelse(`Unplanned pregnancy_%` == "0.0", NA, .x))) %>% 
  mutate(across(everything(), ~ ifelse(str_detect(.x, "NaN"), NA, .x))) %>%
  mutate(across(everything(), .fns = replace_na, "-")) %>%
  mutate(across(ends_with("CI"), ~str_replace_all(.x, " ", "\u00A0"))) %>% 
  mutate(across(ends_with("CI"), ~str_replace_all(.x, "-", "-\uFEFF"))) %>% 
  pivot_longer(
    -c(1:2, label),
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
    # `OR (CI)` = paste(OR, CI),
    `Coefficient (CI)` = paste(Coefficient, CI),
    across(everything(), ~str_replace_all(.x, "- -", "-")),
    across(everything(), ~str_replace_all(.x, "0.0 -", "0"))
  ) %>%
  pivot_wider(
    id_cols = c(` `, `  `, label),
    names_from = outcome,
    values_from = c(`% (CI)`,
                    # `OR (CI)`,
                    `Coefficient (CI)`,
                    `p-value`),
    names_glue = "{outcome}_{.value}"
  ) %>% 
  left_join(scores_denoms, by = c(`  ` = "Cat", "label")) %>% 
  select(
    ` `,
    `  `,
    label,
    `Pregnancy in last year_% (CI)`,
    `Of which unplanned_% (CI)` = `Unplanned pregnancy_% (CI)`,
    `Mean LMUP Score (SD)`,
    # `Pregnancy adjusted Odds ratio_OR (CI)`,
    # `Pregnancy adjusted Odds ratio_p-value`,
    `Difference in LMUP Score_Coefficient (CI)` = `Unplanned pregnancy score_Coefficient (CI)`,
    `Difference in LMUP Score_p-value` = `Unplanned pregnancy score_p-value`,
    `Denominator (weighted, unweighted)`
  ) %>% 
  gt(groupname_col = "label", rowname_col = " ") %>%
  summary_rows(
    fns = list(" "  = ~ " "),
    columns = ` `,
    groups = TRUE,
    missing_text = " "
  ) %>%
  tab_spanner_delim("_") 



# joining tables - ORs and aORs for unplanned ----------------------------------


all_preg_perc %>% 
  left_join(unpl_preg_perc, by = c("Comparison", "Cat")) %>% 
  select(
  Comparison,
  `Pregnancy in last year_%`,
  `Pregnancy in last year_CI`,
  `Unplanned pregnancy_%`,
  `Unplanned pregnancy_CI`,
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
    `Unplanned pregnancy_%`,
    `Unplanned pregnancy_CI`,
    `Unadjusted Odds ratio_OR`,
    `Unadjusted Odds ratio_CI`,
    `Unadjusted Odds ratio_p-value`,
    `Age-adjusted Odds ratio_aOR`,
    `Age-adjusted Odds ratio_CI`,
    `Age-adjusted Odds ratio_p-value`,
    # `p-value`,
    label
  ) %>%
  rowwise() %>% 
  mutate(across(`Unplanned pregnancy_CI`:`Age-adjusted Odds ratio_p-value`, ~ ifelse(`Unplanned pregnancy_%` == "0.0", NA, .x))) %>% 
  # mutate(across(everything(), ~ ifelse(str_detect(.x, "NaN"), NA, .x))) %>% 
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
    across(everything(), ~str_replace_all(.x, "- -", "-")),
    across(everything(), ~str_replace_all(.x, "0.0 -", "0"))
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
    `Unplanned pregnancy_% (CI)`,
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
  tab_spanner_delim("_") 
            


# joining tables pregnancy-unplanned ORs ----------------------------------




all_preg_perc %>% 
  left_join(unpl_preg_perc, by = c("Comparison", "Cat")) %>% 
  select(
  Comparison,
  `Pregnancy in last year_%`,
  `Pregnancy in last year_CI`,
  `Unplanned pregnancy_%`,
  `Unplanned pregnancy_CI`,
  Cat
) %>% 
  left_join(
    adj_ors_p %>%
      select(
        Comparison,
        Cat,
        `Pregnancy adjusted Odds ratio_OR` = aOR,
        `Pregnancy adjusted Odds ratio_CI` = CI,
        `Pregnancy adjusted Odds ratio_p-value` = P
      ),
    by = c("Comparison", "Cat")
  ) %>% 
  left_join(
    adj_ors %>%
      select(
        Comparison,
        Cat,
        `Unplanned pregnancy adjusted Odds ratio_aOR` = aOR,
        `Unplanned pregnancy adjusted Odds ratio_CI` = CI,
        `Unplanned pregnancy adjusted Odds ratio_p-value` = P
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
    `Unplanned pregnancy_%`,
    `Unplanned pregnancy_CI`,
    `Pregnancy adjusted Odds ratio_OR`,
    `Pregnancy adjusted Odds ratio_CI`,
    `Pregnancy adjusted Odds ratio_p-value`,
    `Unplanned pregnancy adjusted Odds ratio_aOR`,
    `Unplanned pregnancy adjusted Odds ratio_CI`,
    `Unplanned pregnancy adjusted Odds ratio_p-value`,
    # `p-value`,
    label
  ) %>%
  rowwise() %>%
  mutate(across(c(`Unplanned pregnancy adjusted Odds ratio_aOR`,
                  `Unplanned pregnancy adjusted Odds ratio_CI`,
                  `Unplanned pregnancy adjusted Odds ratio_p-value`), ~ ifelse(`Unplanned pregnancy_%` == "0.0", NA, .x))) %>% 
  # mutate(across(`Unplanned pregnancy_CI`:`Age-adjusted Odds ratio_p-value`, ~ ifelse(`Unplanned pregnancy_%` == "0.0", NA, .x))) %>% 
  mutate(across(everything(), ~ ifelse(str_detect(.x, "NaN"), NA, .x))) %>%
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
    across(everything(), ~str_replace_all(.x, "- -", "-")),
    across(everything(), ~str_replace_all(.x, "0.0 -", "0"))
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
    `Unplanned pregnancy_% (CI)`,
    `Pregnancy adjusted Odds ratio_OR (CI)`,
    `Pregnancy adjusted Odds ratio_p-value`,
    `Unplanned pregnancy adjusted Odds ratio_aOR (CI)`,
    `Unplanned pregnancy adjusted Odds ratio_p-value`
  ) %>% 
  gt(groupname_col = "label", rowname_col = " ") %>%
  summary_rows(
    fns = list(" "  = ~ " "),
    columns = ` `,
    groups = TRUE,
    missing_text = " "
  ) %>%
  tab_spanner_delim("_") 


# Graph of pregnancies by LMUP -------------------------------------
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

# ggsave(file.path(old_wd, "graphs/LMUP_scores.png"), width = 250, height = 120, units = "mm", dpi = 300)

# testing glms --------------------------------------------------------------

wave2_data %>% 
  glm(D_Planned_preg_1yr ~ D_Age5Cat_w2, data = ., family = binomial, weights = weight2) %T>%
  {broom::tidy(.) %>% print()} %>% 
  {cat("Robust SEs\n"); print(sqrt(diag(vcovHC(., type = "HC0"))))}


robust_glm(wave2_data, D_Planned_preg_1yr ~ D_Age5Cat_w2, weight2)
robust_glm(wave2_data, D_Planned_preg_1yr ~ qsg + D_Age5Cat_w2, weight2)
robust_glm(wave2_data, D_Planned_preg_1yr ~ qethnicity + D_Age5Cat_w2, weight2)
robust_glm(wave2_data, D_Planned_preg_1yr ~ D_relstatcatv7_w2 + D_Age5Cat_w2, weight2)
robust_glm(wave2_data, D_Planned_preg_1yr ~ D_Edu3Cat_w2 + D_Age5Cat_w2, weight2)

# Proportions of pregnancies with known outcomes --------------------------

# prep dataset
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
         )

# percentage who had pregnancy/unplanned+ambiv
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

# unadjusted ORs across all comparators
unadj_ors <- preg_dataset %>% 
  select(- D_LMUPScore_w2) %>% 
  pivot_longer(- c(Preg_unpl_amb, D_Preg1yr_w2, weight2), names_to = "Comparison", values_to = "Cat") %>% 
  filter(!is.na(Cat)) %>%
  group_by(Comparison) %>% 
  nest() %>% 
  mutate(mod = map(data, ~ return_ORs(.x, Preg_unpl_amb ~ Cat, weight2))) %>% 
  unnest(mod) %>% 
  mutate(OR = aOR, aOR = NULL)
  
# adjusted ORs across all comparators
adj_ors <- preg_dataset %>% 
  select(- D_LMUPScore_w2) %>% 
  pivot_longer(- c(Preg_unpl_amb, D_Age5Cat_w2, D_Preg1yr_w2, weight2), names_to = "Comparison", values_to = "Cat") %>% 
  filter(!is.na(Cat)) %>%
  group_by(Comparison) %>% 
  nest() %>% 
  mutate(mod = map(data, ~ return_ORs(.x, Preg_unpl_amb ~ Cat + D_Age5Cat_w2, weight2))) %>% 
  unnest(mod)


# Variable labels
comp_labels <- tibble(
  Comparison = c(
  "D_Age5Cat_w2",
  "qsg",
  "D_EthnicityCombined_w2",
  "D_Edu3Cat_w2",
  "D_SexIDL_w2",
  "EconActChg4_w2",
  "EconActChg5_w2",
  "D_drinkGrp_w2",
  "SDSdrinkchangeW2_w2",
  "Smokenow_w2",
  "D_relstatcatv7_w2",
  "D_PHQ2Cat_w2",
  "D_GAD2Cat_w2",
  "D_ConServAcc_w2",
  "CondomAcc_w2")
) %>%
  mutate(label = map_chr(Comparison, function(exp_var) {
    wave2_data %>%
      pull(exp_var) %>%
      attr("label")
  }))


# combining percs and ORs

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
  tab_spanner_delim("_") # %>%
  # gtsave(., "preg_unpl_OR_plus.html")



# experiement with survey package -----------------------------------------



library(survey)

svy_d <- preg_dataset %>%
svydesign(id = ~1, weights = ~weight2, data = .)

preg_dataset %>%
  filter(!is.na(Preg_unpl)) %>%
  glm(Preg_unpl ~ D_SexIDL_w2 + D_Age5Cat_w2,
      data = .,
      family = binomial("logit"),
      weights = weight2) -> mod

summary(mod)
exp(confint(mod))
exp(mod$coefficients)

preg_dataset %>%
  filter(!is.na(Preg_unpl)) %>%
  robust_glm(Preg_unpl ~ D_SexIDL_w2 + D_Age5Cat_w2, weights = weight2)




svyglm(Preg_unpl ~ D_SexIDL_w2,
    design = svy_d,
    family = binomial("logit"),
    subset = D_Preg1yr_w2 == "Yes",
    weights = weight2) -> mod_svy


summary(mod_svy)$coefficients
exp(confint(mod_svy))
exp(mod_svy$coefficients)
View(mod_svy)

# My methods were working totally fine likes!
