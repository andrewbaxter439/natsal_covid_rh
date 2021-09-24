ethn_stacked_imports <- combine_csv_stacked("qethnicity-servacc")


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

write_to_xlsx("Service access by ethnicity.xlsx", unstacked_imports = NULL, stacked_imports = ethn_stacked_imports, tidy_names = FALSE)

ethn_conserv_imports <- combine_csv_stacked("qethnicity-servaccwhy", cat_labels = conserv_labels)


write_to_xlsx("Service access by ethnicity.xlsx", unstacked_imports = NULL, stacked_imports = ethn_conserv_imports, tidy_names = FALSE)



# by qsg ------------------------------------------------------------------

qsg_serv_imports <- combine_csv_stacked("qsg-servacc")
qsg_servwhy_imports <- combine_csv_stacked("qsg-servaccwhy", cat_labels = conserv_labels)

write_to_xlsx("Service access by qsg.xlsx", unstacked_imports = NULL, stacked_imports = qsg_serv_imports, tidy_names = FALSE)
write_to_xlsx("Service access by qsg.xlsx", unstacked_imports = NULL, stacked_imports = qsg_servwhy_imports, tidy_names = FALSE)
