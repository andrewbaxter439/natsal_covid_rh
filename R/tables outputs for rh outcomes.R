source(file.path(old_wd, "R/import and convert.R"))
source(file.path(old_wd, "R/functions.R"))
library(gt)

ungruo <- ungroup

# D_ConNoCon_w2 D_SwitchTo_w2 D_ServAccComb_w2
var_exp <- quo(D_Age5Cat_w2)
var_out <- quo(D_ConNoCon_w2)

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
        "%, ",
        sprintf(fmt = "%.1f", round(ul * 100, 1)),
        "%)"
      ),
      CI = if_else(str_detect(CI, "NaN"), "-", CI)
    ) %>%
    ungroup()


  tab2 <- df %>%
    rename(exposure = !!var_exp, outcome = !!var_out) %>%
    filter(!is.na(outcome), !is.na(exposure)) %>%
    summarise(
      w = round(sum(weight2), 0),
      n = n(),
      p = wtd.chi.sq(outcome, exposure, weight = weight2)["p.value"],
      xsq = wtd.chi.sq(outcome, exposure, weight = weight2)["Chisq"]
    ) %>%
    transmute(
      exposure = " ",
      cat = title,
      `%` = if_else(p < 0.001, "p<0.001", paste0("p=", round(p, 3))),
      CI = paste0("\u200D(", round(w, 0), "," , n, ")"),
      outcome = "Total"
    ) %>%
    ungroup()

  tabout <- tab1 %>%
    group_by(exposure, cat) %>%
    summarise(n = sum(n), wt = round(sum(wt), 0)) %>% 
    mutate(outcome = "Total",
           `%` = "100.0%",
           CI = paste0("\u200D(", wt, ",", n, ")")) %>%
    bind_rows(tab1, tab2, .) %>%
    select(exposure, outcome, cat, `%`, CI) %>%
    mutate(` ` = " ",
           `  ` = exposure,
           .keep = "unused") %>%
    pivot_wider(
      id_cols = c(cat, `  `, " "),
      names_from = outcome,
      values_from = c(`%`, CI),
      names_glue = "{outcome}_{.value}",
      values_fill = " "
    ) %>% 
    rename(`Total_(Denom.)` = `Total_CI`)

  tabout
}

crosstab_single_var(D_Age5Cat_w2, D_ConNoCon_w2) 
crosstab_single_var(qsg, D_ConNoCon_w2)
crosstab_single_var(D_EthnicityCombined_w2, D_ConNoCon_w2)

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
  crosstab_per_outcome(
    D_ConNoCon_w2,
    D_Age5Cat_w2,
    qsg,
    D_EthnicityCombined_w2,
    D_Edu3Cat_w2,
    EconActChg4_w2,
    EconActChg5_w2,
    D_drinkGrp_w2,
    SDSdrinkchangeW2_w2,
    Smokenow_w2,
    D_PHQ2Cat_w2,
    D_GAD2Cat_w2
  ) # %>% 
  gtsave("Contraception outcomes.html")

wave2_data %>%
  crosstab_per_outcome(
    D_SwitchTo_w2,
    D_Age5Cat_w2,
    qsg,
    D_EthnicityCombined_w2,
    D_Edu3Cat_w2,
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
  crosstab_per_outcome(
    D_ServAccComb_w2,
    D_Age5Cat_w2,
    qsg,
    D_EthnicityCombined_w2,
    D_Edu3Cat_w2,
    EconActChg4_w2,
    EconActChg5_w2,
    D_drinkGrp_w2,
    SDSdrinkchangeW2_w2,
    Smokenow_w2,
    D_PHQ2Cat_w2,
    D_GAD2Cat_w2
  ) %>% 
  gtsave("Service access.html")

