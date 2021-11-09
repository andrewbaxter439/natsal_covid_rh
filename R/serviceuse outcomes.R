library(tidyverse)
library(openxlsx)


old_wd <- setwd(
  "T:/projects/National_survey_sexual_attitudes_IV_S00144/09 Natsal Covid/05 Data Analysis/Data Analysis AB/Reproductive health/dat_out"
)
conserv_labels <- c(
  "Service was closed",
  "My appointment was cancelled",
  "Couldn’t get an appointment",
  "There was no available transportation, due to Covid-19",
  "I was avoiding travel/ exposure to others, due to Covid-19",
  "Didn’t feel comfortable using online/ telephone service",
  "Other reasons",
  "Prefer not to say"
)


# by ethnicity ------------------------------------------------------------

ethn_stacked_imports <- combine_csv_stacked("D_EthnicityCombined_w2-servacc", n_responses = 3)

ethn_conserv_imports <- combine_csv_stacked(exposure = "D_EthnicityCombined_w2-servaccwhy", cat_labels = conserv_labels, n_responses = 3)


write_to_xlsx("Service access by ethnicity.xlsx", unstacked_imports = NULL, stacked_imports = ethn_stacked_imports, tidy_names = FALSE)
write_to_xlsx("Service access by ethnicity.xlsx", unstacked_imports = NULL, stacked_imports = ethn_conserv_imports, tidy_names = FALSE)



# by qsg ------------------------------------------------------------------

qsg_serv_imports <- combine_csv_stacked("qsg-servacc", n_responses = 3)
qsg_servwhy_imports <- combine_csv_stacked("qsg-servaccwhy", cat_labels = conserv_labels, n_responses = 3)

write_to_xlsx("Service access by qsg.xlsx", unstacked_imports = NULL, stacked_imports = qsg_serv_imports, tidy_names = FALSE)
write_to_xlsx("Service access by qsg.xlsx", unstacked_imports = NULL, stacked_imports = qsg_servwhy_imports, tidy_names = FALSE)

# by education ------------------------------------------------------------

D_Edu3Cat_w2_serv_imports <- combine_csv_stacked("D_Edu3Cat_w2-servacc", n_responses = 3)
D_Edu3Cat_w2_servwhy_imports <- combine_csv_stacked("D_Edu3Cat_w2-servaccwhy", cat_labels = conserv_labels, n_responses = 3)

write_to_xlsx("Service access by Education.xlsx", unstacked_imports = NULL, stacked_imports = D_Edu3Cat_w2_serv_imports, tidy_names = FALSE)
write_to_xlsx("Service access by Education.xlsx", unstacked_imports = NULL, stacked_imports = D_Edu3Cat_w2_servwhy_imports, tidy_names = FALSE)
