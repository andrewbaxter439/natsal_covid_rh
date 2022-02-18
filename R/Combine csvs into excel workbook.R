## defunct, use xx outcomes.R instead

library(tidyverse)
library(openxlsx)

# reset wd
# setwd(old_wd)



# old methods -------------------------------------------------------------
# 
# 
# combine_worbooks <- function(suffix, title) {
#   repr_wb <- createWorkbook(title = title)
#   addWorksheet(repr_wb, sheetName = title)
#   
#   
#   dir() %>%
#     str_subset(suffix) %>%
#     walk(~ {
#       tb <- read_csv(.x, skip = 1)
#       ws_title <- str_extract(.x, paste0(".*(?=_", suffix, ")"))
#       addWorksheet(repr_wb, sheetName = ws_title)
#       writeData(repr_wb, sheet = ws_title, tb)
#     })
#   
#   saveWorkbook(repr_wb, paste0(title, ".xlsx"))
#   
# }
# 
# combine_worbooks("byage", "Reproductive outcomes by age")
# 
# combine_worbooks("byquin", "Reproductive outcomes by IMD")
# combine_worbooks("byqsg", "Reproductive outcomes by qsg")
# combine_worbooks("byethn", "Reproductive outcomes by ethn")


# # another old way -------------------------------------------------------------
# 
# 
# imports <- dir() %>%
#   str_subset("Age5") %>%
#   head(6) %>%
#   map(~
#         {
#           dat <-  read_csv(.x, skip = 1)
#           var <-  colnames(dat[1])
#           metric <-  str_extract(.x, "(?<=-).*(?=\\.csv)")
#           p_val <- str_extract_all(dat, "(?<=Pr = )[0-9.]*") %>%
#             reduce(c) %>%
#             as.numeric()
#           list(
#             var = var,
#             metric = metric,
#             data = dat,
#             p_val = p_val
#           )
#         })
# 
# 
# p1 <- imports[[2]]$data %>%
#   filter(!is.na(Total)) %>%
#   mutate(
#     `Changed way of getting contraception` = replace_na(`Changed way of getting contraception`, "Metric")
#   ) %>%
#   pivot_longer(2:11, names_to = "age_grp", values_to = "val") %>%
#   pivot_wider(age_grp, names_from = `Changed way of getting contraception`, values_from = val) %>%
#   mutate(age_grp = str_remove(age_grp, "_1$")) %>%
#   pivot_longer(No:Total, names_to = "Changed way of getting contraception", values_to = "val") %>%
#   pivot_wider(
#     c(age_grp, `Changed way of getting contraception`),
#     names_from = "Metric",
#     values_from = val
#   )
# 
# p2 <- imports[[1]]$data %>%
#   filter(!is.na(`Changed way of getting contraception`)) %>%
#   pivot_longer(cols = 2:6,
#                names_to = "age_grp",
#                values_to = "N")
# 
# p3 <- imports[[3]]$data %>%
#   filter(!is.na(`Changed way of getting contraception`)) %>%
#   pivot_longer(cols = 2:6,
#                names_to = "age_grp",
#                values_to = "N_w")
# 
# out_tab <-
#   left_join(p1, p2, by = c("age_grp", "Changed way of getting contraception")) %>%
#   left_join(p3, by = c("age_grp", "Changed way of getting contraception"))
# 
# library(gt)
# 
# 
# out_tab %>%
#   mutate(Denominators = paste(N, N_w, sep = ","),
#          `95%CI` = replace_na(`95%CI`, "")) %>%
#   select(-N,-N_w) %>%
#   pivot_wider(
#     `Changed way of getting contraception`,
#     names_from = age_grp,
#     values_from = c(`Weighted %`, `95%CI`, Denominators),
#     names_glue = "{age_grp};{.value}"
#   ) %>%
#   gt(rowname_col = "Changed way of getting contraception") %>%
#   tab_spanner_delim(delim = ";")
# 
# 
# new function - again ----------------------------------------------------


#Deprecated - moved to functions.R
# combine_csv_outputs <- function(exposure) {
#   # exposure <- "D_Age5Cat_w2"
#   
#   imports <-
#     dir() %>%
#     str_subset(exposure) %>%
#     str_subset("_by") %>% 
#     tibble(
#       file = .,
#       outcome = str_extract(file, ".*(?=_by)"),
#       metric = str_extract(file, paste0("(?<=", exposure, "-).*(?=\\.csv)"))
#     )
#   
#   imports %>%
#     group_by(outcome) %>%
#     group_map(function(outcome, key) {
#       # outcome <- imp_sample %>% filter(outcome == "AccDisPan_w2")
#       tables <- c("prop", "n", "wn") %>%
#         set_names() %>%
#         map(function(met) {
#           outcome %>%
#             filter(metric == met) %>%
#             pull(file) %>%
#             read_csv(skip = 1)
#         })
#       
#       p_val <- str_extract_all(tables$prop, "(?<=Pr = )[0-9.]*") %>%
#         reduce(c) %>%
#         as.numeric()
#       
#       t_prop <- tables$prop %>%
#         filter(!is.na(Total)) %>%
#         mutate(across(1, replace_na, "Metric")) %>%
#         pivot_longer(-1, names_to = "age_grp", values_to = "val") %>%
#         rename(response = 1) %>%
#         pivot_wider(age_grp, names_from = response, values_from = val) %>%
#         mutate(age_grp = str_remove(age_grp, "_1$")) %>%
#         pivot_longer(c(-1,-2), names_to = "response", values_to = "val") %>%
#         pivot_wider(c(age_grp, response),
#                     names_from = "Metric",
#                     values_from = val) %>%
#         mutate(
#           `Weighted prop` = as.numeric(`Weighted prop`),
#           perc = paste0(sprintf(
#             "%#.1f", `Weighted prop` * 100
#           ), "%"),
#           lower_ci = as.numeric(str_extract(CI, "(?<=\\[)[0-9\\.]*")),
#           upper_ci = as.numeric(str_extract(CI, "(?<=,)[0-9\\.]*")),
#           across(
#             c(upper_ci, lower_ci),
#             ~ ifelse(response == "Total", "", paste0(sprintf("%#.1f", .x * 100), "%"))
#           ),
#           `95%CI` = ifelse(
#             response == "Total",
#             "",
#             paste0("(", lower_ci, ", ", upper_ci, ")")
#           )
#         )
#       
#       t_n <-  tables$n %>%
#         mutate(across(-1, as.numeric)) %>%
#         drop_na() %>%
#         rename(response = 1) %>%
#         pivot_longer(-1, names_to = "age_grp", values_to = "N")
#       
#       t_wn <- tables$wn %>%
#         mutate(across(-1, as.numeric)) %>%
#         drop_na() %>%
#         rename(response = 1) %>%
#         pivot_longer(-1, names_to = "age_grp", values_to = "N_w")
#       
#       outtab <- list(t_prop, t_n, t_wn) %>%
#         reduce(left_join, by = c("age_grp", "response")) %>%
#         mutate(Denominators = paste(N, N_w, sep = ","),
#                p_val = p_val,
#                variable = colnames(tables$prop[1]))
#       
#       list(variable = key$outcome, df = outtab)
#     })
#   
#   
# }
# combine_csv_stacked <- function(exposure) {
#   
# imports <-
#   dir() %>%
#   str_subset(exposure) %>%
#   str_subset("_stacked") %>% 
#   tibble(
#     file = .,
#     outcome = str_extract(file, ".*(?=_stacked)"),
#     metric = str_extract(file, paste0("(?<=", exposure, "-).*(?=\\.csv)"))
#   )
# 
# imports %>%
#   group_by(outcome) %>%
#   group_map(function(outcome, key) { 
#             
#     all_na <- function(x) sum(!is.na(x)) == 0
#     
#     tables <- c("prop", "n", "wn") %>%
#       set_names() %>%
#       map(function(met) {
#         outcome %>%
#           filter(metric == met) %>%
#           pull(file) %>%
#           read_csv(col_names = FALSE, skip = 0) %>%
#           janitor::remove_empty(which = "rows")
#       })
#     
#     
#     t_p <- map_dfr(1:(nrow(tables$prop)/8), ~ {
#     # map_dfr(2, ~ {
#       tables$prop[(8*.x-6): (8*.x),] %>% 
#         `colnames<-`(., paste(.[1,], .[2,], sep = "_")) %>% 
#         select(where(~!all_na(.x))) %>% 
#         mutate(filt = str_replace(.[[1]], "(Yes|No|Total)", NA_character_),
#                outcome = str_trim(str_extract(filt, "(?<=:?)[a-zA-Z /]*$"))) %>%
#         fill(outcome, .direction = "down") %>% 
#         mutate(p_val = str_extract(.[[2]], "(?<=Pr = ).*")) %>% 
#         fill(p_val, .direction = "up")  %>% 
#         filter(across(1, ~str_detect(.x, "Yes"))) %>%
#         select(outcome, everything(), -1, -filt) 
#     }) %>% select(where(~!all_na(.x)))
#     
#     
#     t_n <- map_dfr(1:(nrow(tables$n)/6), ~ {
#       tables$n[(6*.x-4): (6*.x),] %>% 
#         `colnames<-`(., paste(.[1,], .[2,], sep = "_")) %>% 
#         select(where(~!all_na(.x))) %>% 
#         mutate(filt = str_replace(.[[1]], "(Yes|No|Total)", NA_character_),
#                outcome = str_trim(str_extract(filt, "(?<=:?)[a-zA-Z /]*$"))) %>%
#         fill(outcome, .direction = "down") %>% 
#         filter(across(1, ~str_detect(.x, "Yes"))) %>%
#         select(outcome, everything(), -1, -filt) 
#     }) %>% select(where(~!all_na(.x)))
#     
#     
#     t_wn <- map_dfr(1:(nrow(tables$wn)/6), ~ {
#       tables$wn[(6*.x-4): (6*.x),] %>% 
#         `colnames<-`(., paste(.[1,], .[2,], sep = "_")) %>% 
#         select(where(~!all_na(.x))) %>% 
#         mutate(filt = str_replace(.[[1]], "(Yes|No|Total)", NA_character_),
#                outcome = str_trim(str_extract(filt, "(?<=:?)[a-zA-Z /]*$"))) %>%
#         fill(outcome, .direction = "down") %>% 
#         filter(across(1, ~str_detect(.x, "Yes"))) %>%
#         select(outcome, everything(), -1, -filt) 
#     }) %>% select(where(~!all_na(.x)))
#     
#     
#     outtab <- list(t_p, t_n, t_wn) %>% 
#       reduce(left_join, by = "outcome") %>% 
#       pivot_longer(-c(1, p_val), names_to = "var_metric", values_to = "val") %>% 
#       separate(var_metric, into = c("age_grp", "metric"), sep = "_") %>% 
#       pivot_wider(names_from = "metric", values_from = val) %>% 
#     mutate(
#       `Weighted prop` = as.numeric(`Weighted prop`),
#       perc = paste0(sprintf(
#         "%#.1f", `Weighted prop` * 100
#       ), "%"),
#       lower_ci = as.numeric(str_extract(CI, "(?<=\\[)[0-9\\.]*")),
#       upper_ci = as.numeric(str_extract(CI, "(?<=,)[0-9\\.]*")),
#       across(
#         c(upper_ci, lower_ci), ~ paste0(sprintf("%#.1f", .x * 100), "%")
#       ),
#       `95%CI` =  paste0("(", lower_ci, ", ", upper_ci, ")"),
#       Denominators = paste(N, `Weighted n`, sep = ","),
#              variable = key$outcome)# str_trim(str_extract(colnames(tables$prop[1]), "(?<=:).*(?=:?)")))
#     
#     list(variable = key$outcome, df = outtab)
#     })
# 
# }


## Doing all afresh across all sheets -------------------------
# Now combined into functions
# age_imports <- combine_csv_outputs("D_Age5Cat_w2")
# 
# 
# 
# wb <- loadWorkbook("Reproductive outcomes by age_R.xlsx")
# 
# age_imports %>%
#   walk(~ {
#     if (.x$variable %in% getSheetNames("Reproductive outcomes by age_R.xlsx")) {
#       removeWorksheet(wb, .x$variable)
#     }
#     addWorksheet(wb, .x$variable)
#     order <- unique(.x$df$age_grp)
#     df_unordered <- .x$df %>%
#       mutate(`95%CI` = ifelse(`Weighted prop` == 0, "", `95%CI`)) %>% 
#       select(variable,
#              response,
#              age_grp,
#              perc,
#              `95%CI`,
#              Denominators,
#              p_val) %>%
#       filter(response != "No") %>%
#       mutate(
#         perc = ifelse(response != "Total", perc, ""),
#         p_val = case_when(
#           response == "Total" ~ "",
#           p_val == 0 ~ "<0.001",
#           TRUE ~ as.character(p_val)
#         )
#       ) %>%
#       pivot_wider(
#         c(response, p_val),
#         names_from = age_grp,
#         values_from = c(perc, `95%CI`, Denominators),
#         names_glue = "{age_grp};{.value}"
#       ) #%>%
#     
#     
#     order_colnames <- vector("character", ncol(df_unordered))
#     
#     order %>%
#       map(~ {
#         str_subset(colnames(df_unordered), paste0("^", .x, ".*"))
#       }) %>%  reduce(c) %>%
#       select(df_unordered, response, ., p_val) %>%
#       writeData(wb, sheet = .x$variable, x = .)
#   })
# 
# saveWorkbook(wb, "Reproductive outcomes by age_R.xlsx", overwrite = TRUE)
# 
# 
# # doing it with files with multiple responses -----------------------------
# 
# 
# 
# 
# stacked_imports <- combine_csv_stacked("D_Age5Cat_w2")
# 
# labs_replace <- tibble(name = c("The place where I usually get my contraception was closed/unavailable",
# "My usual method of preventing pregnancy wasnt available",
# "A health professional suggested I use a different method",
# "I was avoiding visiting a GP/clinic/pharmacy because of Covid-19",
# "I could not get to a GP/clinic/pharmacy because of Covid-19",
# "I didnt want to bother the NHS during the pandemic",
# "Another reason",
# "GP/Clinic/pharmacy I usually go to was closed",
# "My appointment was cancelled / I couldn't get an appointment",
# "I could only access my usual GP/clinic/pharmacy by phone/video/online",
# "I was avoiding visiting a GP/clinic/pharmacy because of Covid-19",
# "I could not get to a GP/clinic/pharmacy because of Covid-19",
# "didnt want to bother the NHS during the pandemic")) %>% 
#   mutate(match = tolower(str_extract(name, ".{10}"))) %>% 
#   unique() %>% 
#   arrange(match)
# 
# labs_out <- tibble(outcome = unique(c(stacked_imports[[3]]$df$outcome, stacked_imports[[1]]$df$outcome))) %>% 
#   mutate(match = tolower(str_extract(outcome, ".{10}")))
# 
# names_replace <- labs_out %>% left_join(labs_replace, by = "match") %>% 
#   select(outcome, name)
# 
# imports_w_names <- stacked_imports %>% 
#   modify( ~ {
#     .x$df <- .x$df %>% 
#       left_join(names_replace, by = "outcome", keep = FALSE) %>% 
#       mutate(name = ifelse(is.na(name), outcome, name))
#     .x
#   })
# 
# 
# # Now writing to xlsx? ----------------------------------------------------
# 
# 
# wb <- loadWorkbook("Reproductive outcomes by age_R.xlsx")
# 
# 
# imports_w_names %>% 
#   walk( ~ {
#     if (.x$variable %in% getSheetNames("Reproductive outcomes by age_R.xlsx")) {removeWorksheet(wb, .x$variable)}
#     addWorksheet(wb, .x$variable)
#     order <- unique(.x$df$age_grp)
#     df_unordered <- .x$df %>%
#       mutate(`95%CI` = ifelse(`Weighted prop` == 0, "", `95%CI`)) %>% 
#       select(variable, response = name, age_grp, perc, `95%CI`, Denominators, p_val) %>%
#       # filter(response != "No") %>%
#       # mutate(perc = ifelse(response == "Yes", perc, ""), 
#       #        p_val = case_when(
#       #          response == "Total" ~ "",
#       #          p_val == 0 ~ "<0.001",
#       #          TRUE ~ as.character(p_val)
#       #        )) %>%
#       pivot_wider(c(response, p_val), names_from = age_grp, values_from = c(perc, `95%CI`, Denominators),
#                   names_glue = "{age_grp};{.value}") #%>%
#     
#     
#     order_colnames <- vector("character", ncol(df_unordered))
#     
#     order %>% 
#       map( ~ {
#         str_subset(colnames(df_unordered), paste0("^", .x, ".*"))
#       }) %>%  reduce(c) %>% 
#       select(df_unordered, response, ., p_val) %>% 
#       writeData(wb, sheet = .x$variable, x = .)
#   })
# 
# saveWorkbook(wb, "Reproductive outcomes by age_R.xlsx", overwrite = TRUE)
# 
# 
# # Now by ethnicity --------------------------------------------------------
# 
# 
# ethn_imports <- combine_csv_outputs("qethnicity")
# 
# wb <- loadWorkbook("Reproductive outcomes by ethnicity.xlsx")
# 
# ethn_imports %>%
#   walk(~ {
#     if (.x$variable %in% getSheetNames("Reproductive outcomes by ethnicity.xlsx")) {
#       removeWorksheet(wb, .x$variable)
#     }
#     addWorksheet(wb, .x$variable)
#     order <- unique(.x$df$age_grp)
#     df_unordered <- .x$df %>%
#       mutate(`95%CI` = ifelse(`Weighted prop` == 0, "", `95%CI`)) %>% 
#       select(variable,
#              response,
#              age_grp,
#              perc,
#              `95%CI`,
#              Denominators,
#              p_val) %>%
#       filter(response != "No") %>%
#       mutate(
#         perc = ifelse(response != "Total", perc, ""),
#         p_val = case_when(
#           response == "Total" ~ "",
#           p_val == 0 ~ "<0.001",
#           TRUE ~ as.character(p_val)
#         )
#       ) %>%
#       pivot_wider(
#         c(response, p_val),
#         names_from = age_grp,
#         values_from = c(perc, `95%CI`, Denominators),
#         names_glue = "{age_grp};{.value}"
#       ) #%>%
#     
#     
#     order_colnames <- vector("character", ncol(df_unordered))
#     
#     order %>%
#       map(~ {
#         str_subset(colnames(df_unordered), paste0("^", .x, ".*"))
#       }) %>%  reduce(c) %>%
#       select(df_unordered, response, ., p_val) %>%
#       writeData(wb, sheet = .x$variable, x = .)
#   })
# 
# saveWorkbook(wb, "Reproductive outcomes by ethnicity.xlsx", overwrite = TRUE)
# 
# 
# # doing it with files with multiple responses -----------------------------
# 
# 
# stacked_imports <- combine_csv_stacked("qethnicity")
# 
# labs_replace <- tibble(name = c("The place where I usually get my contraception was closed/unavailable",
#                                 "My usual method of preventing pregnancy wasnt available",
#                                 "A health professional suggested I use a different method",
#                                 "I was avoiding visiting a GP/clinic/pharmacy because of Covid-19",
#                                 "I could not get to a GP/clinic/pharmacy because of Covid-19",
#                                 "I didnt want to bother the NHS during the pandemic",
#                                 "Another reason",
#                                 "GP/Clinic/pharmacy I usually go to was closed",
#                                 "My appointment was cancelled / I couldn't get an appointment",
#                                 "I could only access my usual GP/clinic/pharmacy by phone/video/online",
#                                 "I was avoiding visiting a GP/clinic/pharmacy because of Covid-19",
#                                 "I could not get to a GP/clinic/pharmacy because of Covid-19",
#                                 "didnt want to bother the NHS during the pandemic")) %>% 
#   mutate(match = tolower(str_extract(name, ".{10}"))) %>% 
#   unique() %>% 
#   arrange(match)
# 
# labs_out <- tibble(outcome = unique(c(stacked_imports[[3]]$df$outcome, stacked_imports[[1]]$df$outcome))) %>% 
#   mutate(match = tolower(str_extract(outcome, ".{10}")))
# 
# names_replace <- labs_out %>% left_join(labs_replace, by = "match") %>% 
#   select(outcome, name)
# 
# imports_w_names <- stacked_imports %>% 
#   modify( ~ {
#     .x$df <- .x$df %>% 
#       left_join(names_replace, by = "outcome", keep = FALSE) %>% 
#       mutate(name = ifelse(is.na(name), outcome, name))
#     .x
#   })
# 
# 
# # Now writing to xlsx? ----------------------------------------------------
# 
# 
# wb <- loadWorkbook("Reproductive outcomes by ethnicity.xlsx")
# 
# 
# imports_w_names %>% 
#   walk( ~ {
#     if (.x$variable %in% getSheetNames("Reproductive outcomes by ethnicity.xlsx")) {removeWorksheet(wb, .x$variable)}
#     addWorksheet(wb, .x$variable)
#     order <- unique(.x$df$age_grp)
#     df_unordered <- .x$df %>%
#       mutate(`95%CI` = ifelse(`Weighted prop` == 0, "", `95%CI`)) %>% 
#       select(variable, response = name, age_grp, perc, `95%CI`, Denominators, p_val) %>%
#       pivot_wider(c(response, p_val), names_from = age_grp, values_from = c(perc, `95%CI`, Denominators),
#                   names_glue = "{age_grp};{.value}") #%>%
#     
#     
#     order_colnames <- vector("character", ncol(df_unordered))
#     
#     order %>% 
#       map( ~ {
#         str_subset(colnames(df_unordered), paste0("^", .x, ".*"))
#       }) %>%  reduce(c) %>% 
#       select(df_unordered, response, ., p_val) %>% 
#       writeData(wb, sheet = .x$variable, x = .)
#   })
# 
# saveWorkbook(wb, "Reproductive outcomes by ethnicity.xlsx", overwrite = TRUE)
# 
# 
# # Try by qsg? -------------------------------------------------------------
# 
# 
# qsg_imports <- combine_csv_outputs("qsg")
# 
# wb <- loadWorkbook("Reproductive outcomes by qsg.xlsx")
# 
# qsg_imports %>%
#   walk(~ {
#     if (.x$variable %in% getSheetNames("Reproductive outcomes by qsg.xlsx")) {
#       removeWorksheet(wb, .x$variable)
#     }
#     addWorksheet(wb, .x$variable)
#     order <- unique(.x$df$age_grp)
#     df_unordered <- .x$df %>%
#       mutate(`95%CI` = ifelse(`Weighted prop` == 0, "", `95%CI`)) %>% 
#       select(variable,
#              response,
#              age_grp,
#              perc,
#              `95%CI`,
#              Denominators,
#              p_val) %>%
#       filter(response != "No") %>%
#       mutate(
#         perc = ifelse(response != "Total", perc, ""),
#         p_val = case_when(
#           response == "Total" ~ "",
#           p_val == 0 ~ "<0.001",
#           TRUE ~ as.character(p_val)
#         )
#       ) %>%
#       pivot_wider(
#         c(response, p_val),
#         names_from = age_grp,
#         values_from = c(perc, `95%CI`, Denominators),
#         names_glue = "{age_grp};{.value}"
#       ) #%>%
#     
#     
#     order_colnames <- vector("character", ncol(df_unordered))
#     
#     order %>%
#       map(~ {
#         str_subset(colnames(df_unordered), paste0("^", .x, ".*"))
#       }) %>%  reduce(c) %>%
#       select(df_unordered, response, ., p_val) %>%
#       writeData(wb, sheet = .x$variable, x = .)
#   })
# 
# saveWorkbook(wb, "Reproductive outcomes by qsg.xlsx", overwrite = TRUE)
# 
# 
# # doing it with files with multiple responses -----------------------------
# 
# 
# stacked_imports <- combine_csv_stacked("qsg")
# 
# labs_replace <- tibble(name = c("The place where I usually get my contraception was closed/unavailable",
#                                 "My usual method of preventing pregnancy wasnt available",
#                                 "A health professional suggested I use a different method",
#                                 "I was avoiding visiting a GP/clinic/pharmacy because of Covid-19",
#                                 "I could not get to a GP/clinic/pharmacy because of Covid-19",
#                                 "I didnt want to bother the NHS during the pandemic",
#                                 "Another reason",
#                                 "GP/Clinic/pharmacy I usually go to was closed",
#                                 "My appointment was cancelled / I couldn't get an appointment",
#                                 "I could only access my usual GP/clinic/pharmacy by phone/video/online",
#                                 "I was avoiding visiting a GP/clinic/pharmacy because of Covid-19",
#                                 "I could not get to a GP/clinic/pharmacy because of Covid-19",
#                                 "didnt want to bother the NHS during the pandemic")) %>% 
#   mutate(match = tolower(str_extract(name, ".{10}"))) %>% 
#   unique() %>% 
#   arrange(match)
# 
# labs_out <- tibble(outcome = unique(c(stacked_imports[[3]]$df$outcome, stacked_imports[[1]]$df$outcome))) %>% 
#   mutate(match = tolower(str_extract(outcome, ".{10}")))
# 
# names_replace <- labs_out %>% left_join(labs_replace, by = "match") %>% 
#   select(outcome, name)
# 
# imports_w_names <- stacked_imports %>% 
#   modify( ~ {
#     .x$df <- .x$df %>% 
#       left_join(names_replace, by = "outcome", keep = FALSE) %>% 
#       mutate(name = ifelse(is.na(name), outcome, name))
#     .x
#   })
# 
# 
# # Now writing to xlsx? ----------------------------------------------------
# 
# 
# wb <- loadWorkbook("Reproductive outcomes by qsg.xlsx")
# 
# 
# imports_w_names %>% 
#   walk( ~ {
#     if (.x$variable %in% getSheetNames("Reproductive outcomes by qsg.xlsx")) {removeWorksheet(wb, .x$variable)}
#     addWorksheet(wb, .x$variable)
#     order <- unique(.x$df$age_grp)
#     df_unordered <- .x$df %>%
#       mutate(`95%CI` = ifelse(`Weighted prop` == 0, "", `95%CI`)) %>% 
#       select(variable, response = name, age_grp, perc, `95%CI`, Denominators, p_val) %>%
#       pivot_wider(c(response, p_val), names_from = age_grp, values_from = c(perc, `95%CI`, Denominators),
#                   names_glue = "{age_grp};{.value}") #%>%
#     
#     
#     order_colnames <- vector("character", ncol(df_unordered))
#     
#     order %>% 
#       map( ~ {
#         str_subset(colnames(df_unordered), paste0("^", .x, ".*"))
#       }) %>%  reduce(c) %>% 
#       select(df_unordered, response, ., p_val) %>% 
#       writeData(wb, sheet = .x$variable, x = .)
#   })
# 
# saveWorkbook(wb, "Reproductive outcomes by qsg.xlsx", overwrite = TRUE)
