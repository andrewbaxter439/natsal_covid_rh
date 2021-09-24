edu_imports <- combine_csv_outputs("D_Edu3Cat_w2")
edu_stacked_imports <- combine_csv_stacked("D_Edu3Cat_w2")

write_to_xlsx("Reproductive outcomes by education.xlsx",
              edu_imports, 
              edu_stacked_imports)

# Deprecated - moved to functions.R -----------------------------------
# write_to_xlsx <-
#   function(workbook,
#            unstacked_imports,
#            stacked_imports) {
#     wb <- loadWorkbook(workbook)
#     
#     unstacked_imports %>%
#       walk( ~ {
#         if (.x$variable %in% getSheetNames(workbook)) {
#           removeWorksheet(wb, .x$variable)
#         }
#         addWorksheet(wb, .x$variable)
#         order <- unique(.x$df$age_grp)
#         df_unordered <- .x$df %>%
#           mutate(`95%CI` = ifelse(`Weighted prop` == 0 | `Weighted prop` == 1, "", `95%CI`)) %>%
#           select(variable,
#                  response,
#                  age_grp,
#                  perc,
#                  `95%CI`,
#                  Denominators,
#                  p_val) %>%
#           filter(response != "No") %>%
#           mutate(
#             perc = ifelse(response != "Total", perc, ""),
#             p_val = case_when(
#               response == "Total" ~ "",
#               p_val == 0 ~ "<0.001",
#               TRUE ~ as.character(p_val)
#             )
#           ) %>%
#           pivot_wider(
#             c(response, p_val),
#             names_from = age_grp,
#             values_from = c(perc, `95%CI`, Denominators),
#             names_glue = "{age_grp};{.value}"
#           ) #%>%
#         
#         
#         order_colnames <- vector("character", ncol(df_unordered))
#         
#         order %>%
#           map( ~ {
#             str_subset(colnames(df_unordered), paste0("^", .x, ".*"))
#           }) %>%  reduce(c) %>%
#           select(df_unordered, response, ., p_val) %>%
#           writeData(wb, sheet = .x$variable, x = .)
#       })
#     
#     saveWorkbook(wb, workbook, overwrite = TRUE)
#     
#     
#     # doing it with files with multiple responses -----------------------------
#     
#     
#     
#     
#     
#     labs_replace <-
#       tibble(
#         name = c(
#           "The place where I usually get my contraception was closed/unavailable",
#           "My usual method of preventing pregnancy wasnt available",
#           "A health professional suggested I use a different method",
#           "I was avoiding visiting a GP/clinic/pharmacy because of Covid-19",
#           "I could not get to a GP/clinic/pharmacy because of Covid-19",
#           "I didnt want to bother the NHS during the pandemic",
#           "Another reason",
#           "GP/Clinic/pharmacy I usually go to was closed",
#           "My appointment was cancelled / I couldn't get an appointment",
#           "I could only access my usual GP/clinic/pharmacy by phone/video/online",
#           "I was avoiding visiting a GP/clinic/pharmacy because of Covid-19",
#           "I could not get to a GP/clinic/pharmacy because of Covid-19",
#           "didnt want to bother the NHS during the pandemic"
#         )
#       ) %>%
#       mutate(match = tolower(str_extract(name, ".{10}"))) %>%
#       unique() %>%
#       arrange(match)
#     
#     labs_out <-
#       tibble(outcome = unique(
#         c(stacked_imports[[3]]$df$outcome, stacked_imports[[1]]$df$outcome)
#       )) %>%
#       mutate(match = tolower(str_extract(outcome, ".{10}")))
#     
#     names_replace <-
#       labs_out %>% left_join(labs_replace, by = "match") %>%
#       select(outcome, name)
#     
#     imports_w_names <- stacked_imports %>%
#       modify(~ {
#         .x$df <- .x$df %>%
#           left_join(names_replace, by = "outcome", keep = FALSE) %>%
#           mutate(name = ifelse(is.na(name), outcome, name))
#         .x
#       })
#     
#     
#     # Now writing to xlsx? ----------------------------------------------------
#     
#     
#     wb <- loadWorkbook(workbook)
#     
#     
#     imports_w_names %>%
#       walk(~ {
#         if (.x$variable %in% getSheetNames(workbook)) {
#           removeWorksheet(wb, .x$variable)
#         }
#         addWorksheet(wb, .x$variable)
#         order <- unique(.x$df$age_grp)
#         df_unordered <- .x$df %>%
#           mutate(`95%CI` = ifelse(`Weighted prop` == 0 | `Weighted prop` == 1, "", `95%CI`),
#                         p_val = case_when(
#                           p_val == 0 ~ "<0.001",
#                           TRUE ~ as.character(p_val)
#                         )) %>%
#           select(variable,
#                  response = name,
#                  age_grp,
#                  perc,
#                  `95%CI`,
#                  Denominators,
#                  p_val) %>%
#           pivot_wider(
#             c(response, p_val),
#             names_from = age_grp,
#             values_from = c(perc, `95%CI`, Denominators),
#             names_glue = "{age_grp};{.value}"
#           ) #%>%
#         
#         
#         order_colnames <- vector("character", ncol(df_unordered))
#         
#         order %>%
#           map(~ {
#             str_subset(colnames(df_unordered), paste0("^", .x, ".*"))
#           }) %>%  reduce(c) %>%
#           select(df_unordered, response, ., p_val) %>%
#           writeData(wb, sheet = .x$variable, x = .)
#       })
#     
#     saveWorkbook(wb, workbook, overwrite = TRUE)
#     
#   }
