wave2_data %>%
  filter(as.numeric(D_ConNoCon_w2) != 4,
         D_ConPreUs_w2 == "More effective method") |> 
  mutate(switch = if_else(str_detect(D_SwitchTo_w2, "Switched"), TRUE, FALSE)) |> 
  mutate(stop = if_else(str_detect(D_SwitchTo_w2, "Stopped"), TRUE, FALSE)) |> 
  mutate(continue = if_else(str_detect(D_SwitchTo_w2, "Did not"), TRUE, FALSE)) |> 
  filter(!is.na(switch), !is.na(stop)) |> 
  group_by(D_Age5Cat_w2) |> 
  summarise(switch = sum(switch)/n(),
            stop = sum(stop)/n(),
            continue = sum(continue)/n())


wave2_data %>%
  filter(as.numeric(D_ConNoCon_w2) != 4,
         D_ConPreUs_w2 == "More effective method") %>%
  mutate(
    across(where(is.factor), .fns = ~ fct_drop(.x)),
    D_StopOrSwitch_w2 = fct_collapse(
      D_SwitchTo_w2,
      "Stopped using contraceptives" = "Stopped using contraceptives",
      "Did not switch or stop usual method" = "Did not switch or stop usual method",
      other_level = "Switched usual or only contraceptive method"
    ) %>%
      fct_relevel(
        "Did not switch or stop usual method",
        "Stopped using contraceptives",
        "Switched usual or only contraceptive method"
      ),
    D_SwitchTo_w2 = fct_rev(D_SwitchTo_w2),
    age_lumped = fct_collapse(D_Age5Cat_w2, yp = "18-24", other_level = "rest")
  ) |> 
  filter(!is.na(D_StopOrSwitch_w2)) |> crosstab_single_var(var_exp = age_lumped, D_StopOrSwitch_w2)


wave2_data %>%
  filter(as.numeric(D_ConNoCon_w2) != 4,
         D_ConPreUs_w2 == "More effective method") %>%
  mutate(
    across(where(is.factor), .fns = ~ fct_drop(.x)),
    D_StopOrSwitch_w2 = fct_collapse(
      D_SwitchTo_w2,
      "Stopped using contraceptives" = "Stopped using contraceptives",
      "Did not switch or stop usual method" = "Did not switch or stop usual method",
      other_level = "Switched usual or only contraceptive method"
    ) %>%
      fct_relevel(
        "Did not switch or stop usual method",
        "Stopped using contraceptives",
        "Switched usual or only contraceptive method"
      ),
    D_SwitchTo_w2 = fct_rev(D_SwitchTo_w2),
    age_lumped = fct_collapse(D_Age5Cat_w2, yp = "18-24", other_level = "rest")
  ) |> 
  filter(!is.na(D_StopOrSwitch_w2)) |> crosstab_single_var(var_exp = D_EthnicityCombined_w2, D_StopOrSwitch_w2)


wave2_data %>%
  filter(as.numeric(D_ConNoCon_w2) != 4,
         D_ConPreUs_w2 == "More effective method") %>%
  mutate(
    across(where(is.factor), .fns = ~ fct_drop(.x)),
    D_StopOrSwitch_w2 = fct_collapse(
      D_SwitchTo_w2,
      "Stopped using contraceptives" = "Stopped using contraceptives",
      "Did not switch or stop usual method" = "Did not switch or stop usual method",
      other_level = "Switched usual or only contraceptive method"
    ) %>%
      fct_relevel(
        "Did not switch or stop usual method",
        "Stopped using contraceptives",
        "Switched usual or only contraceptive method"
      ),
    D_SwitchTo_w2 = fct_rev(D_SwitchTo_w2),
    age_lumped = fct_collapse(D_Age5Cat_w2, yp = "18-24", other_level = "rest")
  ) |> 
  filter(!is.na(D_StopOrSwitch_w2)) |> crosstab_single_var(var_exp = D_PHQ2Cat_w2, D_StopOrSwitch_w2) |> 
  select(-` `, -`  `, -cat)
