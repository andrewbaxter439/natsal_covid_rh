source("R/import and convert.R")
source("R/functions.R")
source("R/tables_functions.R")
library(gt)
library(survey)

ungrouo <- ungroup



wave2_data %>% crosstab_single_var(Total, D_ConNoCon_w2)
wave2_data %>% crosstab_single_var(D_Age5Cat_w2, D_ConServFailWhy_w2)
wave2_data %>% crosstab_single_var(D_Age5Cat_w2, D_ConNoCon_w2) 
wave2_data %>% crosstab_single_var(qsg, D_ConNoCon_w2)
wave2_data %>% crosstab_single_var(D_EthnicityCombined_w2, D_ConNoCon_w2)
wave2_data %>%
  filter(!(as.numeric(D_ConNoCon_w2) %in% c(1,4))) %>% 
  crosstab_single_var(D_relstatcatv7_w2, D_SwitchTo_w2)


# outputs -----------------------------------------------------------------

## Table 2
wave2_data %>%
  filter(as.numeric(D_ConNoCon_w2) != 4) %>% 
  mutate(across(where(is.factor), .fns = ~fct_drop(.x)),
           D_Edu3Cat_w2 = fct_rev(D_Edu3Cat_w2),
           SDSdrinkchangeW2_w2 = fct_rev(SDSdrinkchangeW2_w2)
         ) %>% 
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
  )  

#%>% 
  # gtsave("graphs/Contraception outcomes.html")

## Table S3
wave2_data %>%
  filter(as.numeric(D_ConNoCon_w2) != 4,
         as.numeric(D_ConPre_w2) != 3) %>% 
  mutate(across(where(is.factor), .fns = ~fct_drop(.x)),
         D_Edu3Cat_w2 = fct_rev(D_Edu3Cat_w2),
         SDSdrinkchangeW2_w2 = fct_rev(SDSdrinkchangeW2_w2)
  ) %>% 
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
  ) #%>% 
   gtsave("graphs/Contraception switching.html")

## Table S1
wave2_data %>%
  filter(as.numeric(D_ConServAcc_w2) != 1) %>% 
  mutate(across(where(is.factor), .fns = ~fct_drop(.x)),
         D_Edu3Cat_w2 = fct_rev(D_Edu3Cat_w2),
         SDSdrinkchangeW2_w2 = fct_rev(SDSdrinkchangeW2_w2)
  ) %>% 
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
  ) # %>% 
    gtsave("graphs/Service access.html")


# stop or switch at all ---------------------------------------------------

wave2_data %>%
  filter(as.numeric(D_ConNoCon_w2) != 4,
         as.numeric(D_ConPre_w2) != 3) %>%
  mutate(
    across(where(is.factor), .fns = ~ fct_drop(.x)),
    D_Edu3Cat_w2 = fct_rev(D_Edu3Cat_w2),
    SDSdrinkchangeW2_w2 = fct_rev(SDSdrinkchangeW2_w2),
    D_StopOrSwitch_w2 = fct_collapse(
      D_SwitchTo_w2,
      "Stopped using contraceptives" = "Stopped using contraceptives",
      "Did not switch usual method" = "Did not switch usual method",
      other_level = "Switched usual or only contraceptive method"
    ) %>% 
      fct_relevel(
      "Did not switch usual method",
      "Stopped using contraceptives",
      "Switched usual or only contraceptive method"
      )
  ) %>% 
  crosstab_per_outcome(
    D_StopOrSwitch_w2,
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
  )

# Getting an N

wave2_data %>%
  filter(as.numeric(D_ConNoCon_w2) != 4,
         as.numeric(D_ConPre_w2) != 3) %>%
  mutate(
    across(where(is.factor), .fns = ~ fct_drop(.x)),
    D_Edu3Cat_w2 = fct_rev(D_Edu3Cat_w2),
    SDSdrinkchangeW2_w2 = fct_rev(SDSdrinkchangeW2_w2),
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
  ) %>% 
  filter(!is.na(D_StopOrSwitch_w2)) %>% 
  group_by(D_StopOrSwitch_w2) %>% 
  summarise(n = sum(weight2)) %>% 
  mutate(perc = n/sum(n))


# Stop or switch - only those using more effective methods ----------------

## Table 3
wave2_data %>%
  filter(as.numeric(D_ConNoCon_w2) != 4,
         D_ConPreUs_w2 == "More effective method") %>%
  mutate(
    across(where(is.factor), .fns = ~ fct_drop(.x)),
    D_Edu3Cat_w2 = fct_rev(D_Edu3Cat_w2),
    SDSdrinkchangeW2_w2 = fct_rev(SDSdrinkchangeW2_w2),
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
    D_SwitchTo_w2 = fct_rev(D_SwitchTo_w2)
  ) %>% 
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
  )
  

# switch at all - only those more effective pre-lockdown ------------------

  
wave2_data %>%
  filter(as.numeric(D_ConNoCon_w2) != 4,
         D_ConPreUs_w2 == "More effective method") %>%
  mutate(
    across(where(is.factor), .fns = ~ fct_drop(.x)),
    D_Edu3Cat_w2 = fct_rev(D_Edu3Cat_w2),
    SDSdrinkchangeW2_w2 = fct_rev(SDSdrinkchangeW2_w2),
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
    D_SwitchTo_w2 = fct_rev(D_SwitchTo_w2)
  ) %>% 
  crosstab_per_outcome(
    D_StopOrSwitch_w2,
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
  )
  
  

