library(tidyverse)
library(haven)

# path <-
#   "T:/projects/National_survey_sexual_attitudes_IV_S00144/09 Natsal Covid/04 Data/Quantitative dat/wave two"
# 
# import_y <- readline("Import [R]aw dataset or [T]idied dataset?\n")
# 
# if (import_y %in% c("", "R", "r")) {
#   cat("reading raw dataset...")
#   filename <-
#     grep("Working file Wave 2.*\\.sav", dir(path), value = TRUE)
#   wave2_data_import <- read_sav(file.path(path, filename))
#   cat("done!")
# } else if (import_y %in% c("T", "t")) {
  
data_dir <- "\\\\med2023.campus.gla.ac.uk/projects/projects/National_survey_sexual_attitudes_IV_S00144/09 Natsal Covid/05 Data Analysis/Data Analysis AB/Reproductive health"

  cat("reading processed dataset...")
  wave2_data_import <- read_dta(file.path(data_dir, "Working file reproductive health wv2.dta"))
  cat("done!")

  # }

wave2_data <- wave2_data_import %>%
  mutate(across(where(is.labelled), as_factor),
         D_Age5Cat_w2 = fct_drop(D_Age5Cat_w2))

levels(wave2_data$qsg) <- c("A/B - Higher/intermediate managerial, administrative and professional", "C1/C2 - Supervisory, clerical and junior managerial, administrative and professional, skilled manual workers", "D/E - Semi-skilled and unskilled manual, casual, lowest grade and unemployed")