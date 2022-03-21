source("R/import and convert.R")
source("R/functions.R")
source("R/tables_functions.R")
library(gt)


denom_single_var(Total, D_Age5Cat_w2)
denom_single_var(qsg, D_Age5Cat_w2) 
denom_single_var(D_EthnicityCombined_w2, D_Age5Cat_w2)


wave2_data %>%
  mutate(
    D_Edu3Cat_w2 = fct_rev(D_Edu3Cat_w2),
    SDSdrinkchangeW2_w2 = fct_rev(SDSdrinkchangeW2_w2)
  ) %>%
  demographics_per_outcome(
    D_Age5Cat_w2,
    Total,
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
  gtsave("Demographics.html")
