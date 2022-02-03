library(tidyverse)
library(openxlsx)



# Age -----------------------------------------------------------------

age_imports <- combine_csv_outputs("D_Age5Cat_w2")
age_imports_stacked <- combine_csv_stacked("D_Age5Cat_w2")


write_to_xlsx("Reproductive outcomes by age.xlsx", age_imports, age_imports_stacked)


# Social grade ------------------------------------------------------------


qsg_imports <- combine_csv_outputs("qsg")
qsg_imports_stacked <- combine_csv_stacked("qsg")


write_to_xlsx("Reproductive outcomes by qsg.xlsx", qsg_imports, qsg_imports_stacked)

# Ethnicity ----------------------------------------------------------


ethn_imports <- combine_csv_outputs("D_EthnicityCombined_w2")
ethn_imports_stacked <- combine_csv_stacked("D_EthnicityCombined_w2")

write_to_xlsx("Reproductive outcomes by ethnicity.xlsx", ethn_imports, ethn_imports_stacked)


# education ---------------------------------------------------------------

edu_imports <- combine_csv_outputs("D_Edu3Cat_w2")
edu_imports_stacked <- combine_csv_stacked("D_Edu3Cat_w2")

write_to_xlsx("Reproductive outcomes by education.xlsx", edu_imports, edu_imports_stacked)