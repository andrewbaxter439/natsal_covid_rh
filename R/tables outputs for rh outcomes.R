source(file.path(old_wd, "R/import and convert.R"))
source(file.path(old_wd, "R/functions.R"))
library(gt)

ungrouo <- ungroup

# D_ConNoCon_w2 D_SwitchTo_w2 D_ServAccComb_w2
# var_out <- quo(D_SwitchTo_w2)
# var_exp <- quo(D_relstatcatv7_w2)
# var_exp <- quo(Total)

crosstab_single_var <- function(var_exp, var_out, df = wave2_data) {
  
  op <- options(dplyr.summarise.inform=FALSE)
  on.exit(options(op))
  
  var_exp <- enquo(var_exp)
  var_out <- enquo(var_out)
  
  title <- df %>%
    pull(!!var_exp) %>%
    attr("label")
  

  tab1 <- df %>%
    rename(exposure = !!var_exp, outcome = !!var_out) %>%
    filter(!is.na(outcome), !is.na(exposure)) %>%
    group_by(exposure, outcome) %>%
    summarise(wt = sum(weight2),
              n = n()) %>%
    mutate(
      cat = title,
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
    
  
  tab2 <- df %>%
    rename(exposure = !!var_exp, outcome = !!var_out) %>%
    filter(!is.na(outcome),!is.na(exposure)) %>%
    summarise(
      w = round(sum(weight2), 0),
      n = n(),
      p = weights::wtd.chi.sq(outcome, exposure, weight = weight2)["p.value"],
      xsq = weights::wtd.chi.sq(outcome, exposure, weight = weight2)["Chisq"]
    ) %>%
    transmute(
      cat = title,
      Total = if_else(p < 0.001, "p<0.001", paste0("p=", sprintf(fmt = "%.3f", round(p, 3)))),
      `(Denom.)` = paste0("\u200D(", round(w, 0), "," , n, ")")
    ) %>% 
    mutate(`  ` = " ",
           ` ` = " ") %>% 
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
           `(Denom.)` = paste0("\u200D(", wt, ",", n, ")"),
           Total = "100.0%") %>% 
    left_join(tab1b, by = c("cat", " ", "  ")) %>% 
    bind_rows(tab2) %>%
    mutate(across(.fns = ~ replace_na(.x, " ")),
           `  ` = if_else(`  ` == "Total", "Distribution of outcomes", `  `)) %>% 
    select(-Total, -`(Denom.)`, `Total\n(Row)` = Total, `(Denom.)`)
    

  tabout
}

crosstab_single_var(Total, D_ConNoCon_w2)
crosstab_single_var(D_Age5Cat_w2, D_ConNoCon_w2) 
crosstab_single_var(qsg, D_ConNoCon_w2)
crosstab_single_var(D_EthnicityCombined_w2, D_ConNoCon_w2)
crosstab_single_var(D_relstatcatv7_w2, D_SwitchTo_w2, df = wave2_data %>%
                      filter(!(as.numeric(D_ConNoCon_w2) %in% c(1,4))))

crosstab_per_outcome <- function(data = wave2_data, outcome, ...) {
  
    var_out <- rlang::enquo(outcome)
    rlang::eval_tidy(var_out, data = data)
    vars_exp <- rlang::enquos(...)
    
    vars_exp %>%
      map_dfr(function(exp) {
        crosstab_single_var(!!exp, !!var_out, df = data)
      }) %>%
      gt(rowname_col = " ", groupname_col = "cat") %>%
      summary_rows(
        fns = list(" "  = ~ " "),
        columns = ` `,
        groups = TRUE,
        missing_text = " "
      ) %>%
      tab_spanner_delim(delim = "_", split = "first")
  
}


wave2_data %>%
  filter(as.numeric(D_ConNoCon_w2) != 4) %>% 
  crosstab_per_outcome(
    D_ConNoCon_w2,
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
    D_GAD2Cat_w2
  )  %>% 
  gtsave("Contraception outcomes.html")

wave2_data %>%
  filter(!(as.numeric(D_ConNoCon_w2) %in% c(1,4))) %>% 
  crosstab_per_outcome(
    D_SwitchTo_w2,
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
    D_GAD2Cat_w2
  ) %>% 
  gtsave("Contraception switching.html")

wave2_data %>%
  filter(as.numeric(D_ConServAcc_w2) != 1) %>% 
  crosstab_per_outcome(
    D_ConServAcc_w2,
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
    D_GAD2Cat_w2
  ) %>% 
  gtsave("Service access.html")

