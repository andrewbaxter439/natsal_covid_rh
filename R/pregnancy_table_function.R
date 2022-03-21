# Replicating planned/unplanned table in function ------------------------------

pregnancy_tables <- function(all_data, lt = "\u003C") {
  ## Variable labels ---------------------------------------------
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
      # "ServTry4_w2",
      "D_ConServAcc_w2",
      "CondomAcc_w2"
    )
  ) %>%
    mutate(label = map_chr(Comparison, function(exp_var) {
      all_data %>%
        pull(exp_var) %>%
        attr("label")
    }))
  
  ## start here: new table - ORs for pregnancy, ORs and lms for unplanned ----------------
  
  preg_dataset <- all_data %>%
    select(
      D_Preg1yr_w2,
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
      # ServTry4_w2,
      CondomAcc_w2,
      weight2
    ) %>%
    mutate(
      Preg_unpl = case_when(
        D_Preg1yr_w2 == "No" ~ NA_integer_,
        D_LMUPCat_w2 == "Unplanned" ~ 1L,
        TRUE ~ 0L
      ),
      D_Edu3Cat_w2 = fct_rev(D_Edu3Cat_w2),
      SDSdrinkchangeW2_w2 = fct_rev(SDSdrinkchangeW2_w2)
    )
  
  
  ## putting all together ----------------------------------------------------
  
  ### percentages in each outcome ----------------
  
  all_preg_perc <- preg_dataset %>%
    select(-D_LMUPScore_w2,-D_LMUPCat_w2) %>%
    pivot_longer(-c(Preg_unpl, D_Preg1yr_w2, weight2),
                 names_to = "Comparison",
                 values_to = "Cat") %>%
    mutate(Comparison = fct_inorder(Comparison)) %>%
    filter(!is.na(Cat)) %>%
    group_by(Comparison, Cat) %>%
    summarise(
      preg_p = sum(weight2[D_Preg1yr_w2 == "Yes"]) / sum(weight2),
      preg_p_ll = perc_ci(preg_p, "l", sum(weight2)),
      preg_p_ul = perc_ci(preg_p, "u", sum(weight2)),
    ) %>%
    mutate(
      `Pregnancy in last year_%` = sprintf("%.1f", preg_p * 100),
      `Pregnancy in last year_CI` = paste0(
        "(",
        sprintf("%.1f", round(preg_p_ll * 100, 1)),
        ", ",
        sprintf("%.1f", round(preg_p_ul * 100, 1)),
        ")"
      ),
    ) %>%
    arrange(Comparison)
  
  
  #### does stata use weighted n obvs in calculating se??
  
  unpl_preg_perc <-  preg_dataset %>%
    select(-D_LMUPScore_w2, -D_LMUPCat_w2) %>%
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
      `Unplanned pregnancy_CI` = paste0(
        "(",
        sprintf("%.1f", round(unpl_p_ll * 100, 1)),
        ", ",
        sprintf("%.1f", round(unpl_p_ul * 100, 1)),
        ")"
      )
    )
  
  ### odds of all pregnancy (from all - adjusted)
  
  
  # unadj_ors_p <- preg_dataset %>%
  #   select(- D_LMUPScore_w2, -D_LMUPCat_w2, -Total) %>%
  #   pivot_longer(- c(Preg_unpl, D_Preg1yr_w2, weight2), names_to = "Comparison", values_to = "Cat") %>%
  #   filter(!is.na(Cat)) %>%
  #   group_by(Comparison) %>%
  #   nest() %>%
  #   mutate(mod = map(data, ~ return_ORs(.x, D_Preg1yr_w2 ~ Cat, weight2))) %>%
  #   unnest(mod) %>%
  #   mutate(OR = aOR, aOR = NULL)
  #
  # adj_ors_p <- preg_dataset %>%
  #   select(- D_LMUPScore_w2, -D_LMUPCat_w2, -Total) %>%
  #   pivot_longer(- c(Preg_unpl, D_Age5Cat_w2, D_Preg1yr_w2, weight2), names_to = "Comparison", values_to = "Cat") %>%
  #   filter(!is.na(Cat)) %>%
  #   group_by(Comparison) %>%
  #   nest() %>%
  #   mutate(mod = map(data, ~ return_ORs(.x, D_Preg1yr_w2 ~ Cat + D_Age5Cat_w2, weight2))) %>%
  #   unnest(mod)
  
  ## odds of unplanned ------------------
  
  unadj_ors <- preg_dataset %>%
    select(-D_LMUPScore_w2,-D_LMUPCat_w2,-Total) %>%
    filter(!is.na(Preg_unpl)) %>%
    pivot_longer(-c(Preg_unpl, D_Preg1yr_w2, weight2),
                 names_to = "Comparison",
                 values_to = "Cat") %>%
    filter(!is.na(Cat)) %>%
    group_by(Comparison) %>%
    nest() %>%
    mutate(mod = map(data, ~ return_ORs(.x, Preg_unpl ~ Cat, weight2))) %>%
    unnest(mod) %>%
    mutate(OR = aOR, aOR = NULL)
  
  
  adj_ors <- preg_dataset %>%
    select(-D_LMUPScore_w2,-D_LMUPCat_w2,-Total) %>%
    filter(!is.na(Preg_unpl)) %>%
    pivot_longer(
      -c(Preg_unpl, D_Age5Cat_w2, D_Preg1yr_w2, weight2),
      names_to = "Comparison",
      values_to = "Cat"
    ) %>%
    filter(!is.na(Cat)) %>%
    group_by(Comparison) %>%
    nest() %>%
    mutate(mod = map(data, ~ return_ORs(
      .x, Preg_unpl ~ Cat + D_Age5Cat_w2, weight2
    ))) %>%
    unnest(mod)
  
  
  ## linear regression ---------------------
  
  unadj_lin <- preg_dataset %>%
    filter(!is.na(Preg_unpl)) %>%
    select(-Preg_unpl,-D_LMUPCat_w2,-D_Preg1yr_w2,-Total) %>%
    pivot_longer(-c(D_LMUPScore_w2, weight2),
                 names_to = "Comparison",
                 values_to = "Cat") %>%
    mutate(inv_score = D_LMUPScore_w2, .keep = "unused") %>%
    group_by(Comparison) %>%
    nest() %>%
    mutate(mod = map(data, ~ robust_svy_lm(.x, inv_score ~ Cat, weight2))) %>%
    unnest(mod) %>%
    mutate(
      est = sprintf("%.2f", round(est, 2)),
      CI = paste0(
        "(",
        sprintf("%.2f", round(ll, 2)),
        ", ",
        sprintf("%.2f", round(ul, 2)),
        ")"
      ),
      coef = str_remove(coef, "^Cat"),
      P = case_when(
        p < 0.001 ~  paste0("p", lt, "0.001"),
        TRUE ~ as.character(sprintf("%.3f", round(p, 3)))
      )
    )
  
  adj_lin <- preg_dataset %>%
    filter(!is.na(Preg_unpl)) %>%
    select(-Preg_unpl,-D_LMUPCat_w2,-D_Preg1yr_w2,-Total) %>%
    pivot_longer(
      -c(D_LMUPScore_w2, D_Age5Cat_w2, weight2),
      names_to = "Comparison",
      values_to = "Cat"
    ) %>%
    mutate(inv_score = D_LMUPScore_w2, .keep = "unused") %>%
    group_by(Comparison) %>%
    nest() %>%
    mutate(mod = map(
      data,
      ~ robust_svy_lm(.x, inv_score ~ Cat + D_Age5Cat_w2, weight2)
    )) %>%
    unnest(mod) %>%
    mutate(
      adj_est = sprintf("%.2f", round(est, 2)),
      est = NULL,
      CI = paste0(
        "(",
        sprintf("%.2f", round(ll, 2)),
        ", ",
        sprintf("%.2f", round(ul, 2)),
        ")"
      ),
      coef = str_remove(coef, "^Cat"),
      P = case_when(
        p < 0.001 ~  paste0("p", lt, "0.001"),
        TRUE ~ as.character(sprintf("%.3f", round(p, 3)))
      )
    )
  
  
  
  # mean scores and denominators --------------------------------------------
  
  scores_denoms <- preg_dataset %>%
    # filter(!is.na(Preg_unpl)) %>%
    mutate(D_LMUPScore_w2 = if_else(is.na(Preg_unpl), na_dbl, D_LMUPScore_w2)) %>%
    select(-Preg_unpl,-D_LMUPCat_w2,-D_Preg1yr_w2) %>%
    pivot_longer(-c(D_LMUPScore_w2, weight2),
                 names_to = "Comparison",
                 values_to = "Cat") %>%
    filter(!is.na(Cat)) %>%
    group_by(Comparison, Cat) %>%
    summarise(
      mean_sc = weighted.mean(D_LMUPScore_w2, w = weight2, na.rm = TRUE),
      # sd_sc = sd(D_LMUPScore_w2, na.rm = TRUE),
      sd_sc = sqrt(
        Hmisc::wtd.var(D_LMUPScore_w2, weights = weight2, na.rm = TRUE)
      ),
      wt = sum(weight2),
      n = n(),
      .groups = "keep"
    )  %>%
    transmute(
      `Mean LMUP Score (SD)` = paste0(sprintf("%.1f", round(mean_sc, 1)), " (", sprintf("%.1f", round(sd_sc, 1)), ")"),
      `Denominator (weighted, unweighted)` = paste0(round(wt, 0), ", ", round(n, 0))
    ) %>%
    ungroup() %>%
    left_join(comp_labels, by = "Comparison") %>%
    unique() %>%
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
    # left_join(
    #   bind_rows(
    #     unadj_ors_p %>%
    #       select(
    #         Comparison,
    #         Cat,
    #         `Pregnancy adjusted Odds ratio_OR` =OR,
    #         `Pregnancy adjusted Odds ratio_CI` = CI,
    #         `Pregnancy adjusted Odds ratio_p-value` = P
    #       ) %>%
    #       filter(Comparison == "D_Age5Cat_w2"),
  #     adj_ors_p %>%
  #       select(
  #         Comparison,
  #         Cat,
  #         `Pregnancy adjusted Odds ratio_OR` = aOR,
  #         `Pregnancy adjusted Odds ratio_CI` = CI,
  #         `Pregnancy adjusted Odds ratio_p-value` = P
  #       )),
  #   by = c("Comparison", "Cat")
  # ) %>%
  bind_rows(
    comp_labels %>%
      filter(Comparison != "Total") %>%
      mutate(Cat = "glob_p") %>%
      select(-label)
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
          )
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
    mutate(across(ends_with("CI"), ~ str_replace_all(.x, " ", "\u00A0"))) %>%
    mutate(across(ends_with("CI"), ~ str_replace_all(.x, "-(?=.)", "-\uFEFF"))) %>%
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
      across(everything(), ~ str_replace_all(.x, "- -", "-")),
      across(everything(), ~ str_replace_all(.x, "0.0 -", "0"))
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
    mutate(label = fct_inorder(label)) %>%
    arrange(label) %>%
    mutate(
      `Unplanned pregnancy score_p-value` = if_else(
        `Unplanned pregnancy score_p-value` == paste0("p", lt, "0.001"),
        paste0("p", lt, "0.001"),
        paste0("p=", `Unplanned pregnancy score_p-value`)
      ),
      `Unplanned pregnancy score_Coefficient (CI)` = if_else(
        `  ` == "glob_p",
        `Unplanned pregnancy score_p-value`,
        `Unplanned pregnancy score_Coefficient (CI)`
      ),
      across(
        -c(label, `Unplanned pregnancy score_Coefficient (CI)`),
        ~ if_else(`  ` == "glob_p", " ", .x)
      )
    ) %>%
    select(
      ` `,
      `  `,
      label,
      `Pregnancy in last year\n% (CI)` = `Pregnancy in last year_% (CI)`,
      `Of which unplanned\n% (CI)` = `Unplanned pregnancy_% (CI)`,
      `Mean LMUP Score (SD)`,
      # `Pregnancy adjusted Odds ratio_OR (CI)`,
      # `Pregnancy adjusted Odds ratio_p-value`,
      # `Difference in LMUP Score_Coefficient (CI)` = `Unplanned pregnancy score_Coefficient (CI)`,
      `Age-adjusted difference in mean LMUP score (CI)` = `Unplanned pregnancy score_Coefficient (CI)`,
      # `Difference in LMUP Score_p-value` = `Unplanned pregnancy score_p-value`,
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
}
