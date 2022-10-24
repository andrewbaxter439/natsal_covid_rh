library(tidyverse)
source("R/import and convert.R")

wave2_data |>
  select(
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
  ) |> 
  summarise(across(.fns = ~sum(is.na(.x)))) |> 
  pivot_longer(everything(), names_to = "var", values_to = "n_missing") |> 
  mutate(perc_missing = paste0(100* round(n_missing/1488, 3), "%")) |> 
  arrange(n_missing)

