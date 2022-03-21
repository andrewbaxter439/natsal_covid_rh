# D_ConNoCon_w2 D_SwitchTo_w2 D_ServAccComb_w2
# var_out <- quo(D_SwitchTo_w2)
# var_exp <- quo(D_relstatcatv7_w2)
# var_exp <- quo(Total)

crosstab_single_var <- function(df = wave2_data, var_exp, var_out, lt = "\u003C") {
  
  require(tidyverse)
  require(survey)
  
  op <- options(dplyr.summarise.inform=FALSE)
  on.exit(options(op))
  
  var_exp <- enquo(var_exp)
  var_out <- enquo(var_out)
  
  rlang::eval_tidy(title <- df %>%
                     pull(!!var_exp) %>%
                     attr("label"), data = df)
  
  
  df <- df %>%
    rename(exposure = !!var_exp, outcome = !!var_out) %>%
    filter(!is.na(outcome), !is.na(exposure)) %>% 
    mutate(across(c(outcome, exposure), .fns = ~fct_drop(.x)))
  
  svy_df <- svydesign(id = ~NatSal_serial_A, weights = ~weight2, data = df)
  
  tab1 <- df  %>%
    group_by(exposure, outcome) %>%
    summarise(wt = sum(weight2),
              n = n()) %>%
    mutate(
      cat = title,
      perc = wt / sum(wt),
      ll = perc_ci(perc, n = sum(wt)),
      ul = perc_ci(perc, "u", sum(wt)),
      `%` = paste0(sprintf(fmt = "%.1f", round(perc * 100, 1))),
      CI = paste0(
        "(",
        sprintf(fmt = "%.1f", round(ll * 100, 1)),
        ",\u00A0",
        sprintf(fmt = "%.1f", round(ul * 100, 1)),
        ")"
      ),
      `%_CI` = if_else(str_detect(CI, "NaN"), "-", paste(`%`, CI, sep = " "))
    ) %>%
    ungroup()
  
  if(quo_name(var_exp) == "Total") {
    
    # tab_tot <- df %>%
    #   summarise(
    #     w = round(sum(weight2), 0),
    #     n = n()
    #   ) %>%
    #   transmute(
    #     denom = paste0("\u200D(", round(w, 0), "," , n, ")")
    #   )
    
    tab2 <- tibble()
    
  } else  {
    
    
    tab2 <-
      df %>%
      summarise(
        w = round(sum(weight2), 0),
        n = n(),
        p = svychisq(~outcome + exposure, svy_df, drop.unused.levels = TRUE)$p.value,
        # p = weights::wtd.chi.sq(outcome, exposure, weight = weight2)["p.value"],
        xsq = weights::wtd.chi.sq(outcome, exposure, weight = weight2)["Chisq"]
      ) %>%
      transmute(
        cat = title,
        # Total = paste0(round(w, 0), ", " , n),
        `P-value` = if_else(p < 0.001, paste0("p", lt, "0.001"), paste0("p=", sprintf(fmt = "%.3f", round(p, 3))))
      ) %>% 
      pivot_longer(-cat, names_to = "  ", values_to = "Denominators (weighted, unweighted)") %>% 
      mutate(` ` = " ") %>% 
      ungroup()
    
  }
  
  tab1b <-   tab1 %>% 
    select(exposure, outcome, cat, `%_CI`) %>%
    mutate(` ` = " ",
           `  ` = exposure,
           .keep = "unused") %>%
    pivot_wider(
      id_cols = c(cat, `  `, " "),
      names_from = outcome,
      values_from = c(`%_CI`),
      values_fill = " "
    ) 
  
  tabout <- tab1 %>%
    group_by(exposure, cat) %>%
    summarise(n = sum(n), wt = round(sum(wt), 0)) %>% 
    ungroup() %>% 
    mutate(` ` = " ",
           `  ` = as.character(exposure),
           .keep = "unused",
           `Denominators (weighted, unweighted)` = paste0(wt, ", ", n)) %>% 
    left_join(tab1b, by = c("cat", " ", "  ")) %>% 
    bind_rows(tab2) %>%
    mutate(across(.fns = ~ replace_na(.x, " ")),
           cat = ifelse(quo_name(var_exp) == "Total", " ", cat)
           # `  ` = if_else(`  ` == "Total", " ", `  `)
    ) %>% 
    select(-`Denominators (weighted, unweighted)`, `Denominators (weighted, unweighted)`)
  
  
  tabout
}

crosstab_per_outcome <- function(data = wave2_data, outcome, ..., .gt = TRUE, lt = "\u003C") {
  require(gt)
  var_out <- rlang::enquo(outcome)
  rlang::eval_tidy(var_out, data = data)
  vars_exp <- rlang::enquos(...)
  
  df <- vars_exp %>%
    map_dfr(function(exp) {
      crosstab_single_var(df = data, !!exp, !!var_out, lt = lt)
    })
  
  if(.gt) {
    
    df %>%
      gt(rowname_col = " ", groupname_col = "cat") %>%
      summary_rows(
        fns = list(" "  = ~ " "),
        columns = ` `,
        groups = TRUE,
        missing_text = " "
      ) %>%
      tab_spanner_delim(delim = "_", split = "first")
    
  } else {
    df
  }
  
}

denom_single_var <- function(var_exp, var_out, df = wave2_data) {
  
  require(tidyverse)
  op <- options(dplyr.summarise.inform=FALSE)
  on.exit(options(op))
  
  var_exp <- enquo(var_exp)
  var_out <- enquo(var_out)
  
  title <- df %>%
    pull(!!var_exp) %>%
    attr("label")
  
  tot_name <- "All ages"
  
  if(quo_name(var_exp) == "Total") {
    
    tabout <- df %>%
      group_by(!!var_out) %>% 
      summarise(
        wt = round(sum(weight2), 0),
        n = n()
      ) %>%
      mutate(perc = wt / sum(wt)) %>% 
      janitor::adorn_totals(name = tot_name) %>% 
      as_tibble %>% 
      mutate(
        Denominators = paste0(round(wt, 0), ", " , n),
        ll = perc_ci(perc, n = sum(wt)),
        ul = perc_ci(perc, "u", sum(wt)),
        `%` = paste0(sprintf(fmt = "%.1f", round(perc * 100, 1))),
        CI = paste0(
          "(",
          sprintf(fmt = "%.1f", round(ll * 100, 1)),
          ",\u00A0",
          sprintf(fmt = "%.1f", round(ul * 100, 1)),
          ")"
        ),
        `Age distribution` = case_when(
          str_detect(!!var_out, tot_name) ~ "100.0%",
          str_detect(CI, "NaN") ~ "-", 
          TRUE ~ paste(`%`, CI, sep = " "))
      ) %>% 
      select(!!var_out, `Age distribution (% (95% CI))` = `Age distribution`, `Denominators (weighted, unweighted)` = Denominators) %>% 
      pivot_longer(-!!var_out, names_to = "  ") %>% 
      mutate(cat = "Total", ` ` = " ") %>% 
      pivot_wider(c(cat, ` `,`  `), names_from = !!var_out, values_from = value) 
    
  } else  {
    
    tab1 <-  df %>%
      rename(exposure = !!var_exp, outcome = !!var_out) %>%
      filter(!is.na(outcome), !is.na(exposure)) %>%
      group_by(outcome, exposure) %>%
      summarise(wt = sum(weight2),
                n = n(),
                .groups = "drop_last") %>%
      mutate(
        cat = title,
        perc = wt / sum(wt),
        ll = perc_ci(perc, n = sum(wt)),
        ul = perc_ci(perc, "u", sum(wt)),
        `%` = paste0(sprintf(fmt = "%.1f", round(perc * 100, 1))),
        CI = paste0(
          "(",
          sprintf(fmt = "%.1f", round(ll * 100, 1)),
          ",\u00A0",
          sprintf(fmt = "%.1f", round(ul * 100, 1)),
          ")"
        ),
        `%_CI` = if_else(str_detect(CI, "NaN"), "-", paste(`%`, CI, sep = " "))
      ) %>%
      ungroup()
    
    tab1b <- tab1 %>% 
      select(exposure, outcome, cat, `%_CI`) %>%
      mutate(` ` = " ",
             `  ` = exposure,
             .keep = "unused") %>%
      pivot_wider(
        id_cols = c(cat, `  `, " "),
        names_from = outcome,
        values_from = c(`%_CI`),
        values_fill = " "
      ) 
    
    
    tabout <- tab1 %>%
      group_by(exposure, cat) %>%
      summarise(wt = round(sum(wt), 0)) %>% 
      ungroup() %>% 
      mutate(` ` = " ",
             `  ` = as.character(exposure),
             .keep = "unused", 
             perc = wt / sum(wt),
             ll = perc_ci(perc, n = sum(wt)),
             ul = perc_ci(perc, "u", sum(wt)),
             `%` = paste0(sprintf(fmt = "%.1f", round(perc * 100, 1)), "%"),
             CI = paste0(
               "(",
               sprintf(fmt = "%.1f", round(ll * 100, 1)),
               ",\u00A0",
               sprintf(fmt = "%.1f", round(ul * 100, 1)),
               ")"
             ),
             Total = if_else(str_detect(CI, "NaN"), "-", paste(`%`, CI, sep = "\n"))) %>% 
      select(-c(perc:CI)) %>% 
      right_join(tab1b, by = c("cat", " ", "  ")) %>% 
      mutate(across(.fns = ~ replace_na(.x, " "))) %>% 
      select(-Total, !!tot_name := Total)
    
  }
  
  
  
  tabout
}

demographics_per_outcome <- function(data = wave2_data, outcome, ...) {
  
  require(gt)
  
  var_out <- rlang::enquo(outcome)
  rlang::eval_tidy(var_out, data = data)
  vars_exp <- rlang::enquos(...)
  
  vars_exp %>%
    map_dfr(function(exp) {
      denom_single_var(!!exp, !!var_out, df = data)
    }) %>%
    gt(rowname_col = " ", groupname_col = "cat") %>%
    summary_rows(
      fns = list(" "  = ~ " "),
      columns = ` `,
      groups = TRUE,
      missing_text = " "
    ) 
  
}




