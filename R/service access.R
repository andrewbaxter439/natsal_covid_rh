source("R/import and convert.R")
source("R/functions.R")
library(gt)
library(weights)
library(SPHSUgraphs)
library(janitor)

# old_wd <- setwd(
#   "T:/projects/National_survey_sexual_attitudes_IV_S00144/09 Natsal Covid/05 Data Analysis/Data Analysis AB/Reproductive health/dat_out"
# )

wave2_data %>%
  group_by(ServTry4_w2, qsg) %>%
  count(wt = weight2) %>%
  pivot_wider(names_from = ServTry4_w2, values_from = n)

attr(wave2_data$ServTry4_w2, "label")
attr(wave2_data$ServTry5_w2, "label")
attr(wave2_data$ServTry6_w2, "label")
attr(wave2_data$ServTry7_w2, "label")

rowAny <- function(x)  rowSums(x) > 0

wave2_data %>%
  select(genderidentity,
         ServTry4_w2,
         ServTry5_w2,
         ServTry6_w2,
         ServTry7_w2) %>%
  # summarise(across(.fns = ~sum(is.na(.x))))
  filter(rowAny(across(.fns = is.na)))




service_names <- wave2_data %>%
  select(ServTry4_w2,
         ServTry5_w2,
         ServTry6_w2,
         ServTry7_w2) %>%
  unclass %>%
  map( ~ attr(.x, "label") %>%
         str_extract("(?<=access: ).*"))

wave2_data %>%
  select(ServTry4_w2,
         ServTry5_w2,
         ServTry6_w2,
         ServTry7_w2) %>%
  filter(!rowAny(across(.fns = is.na))) %>%
  summarise(across(everything(), ~ sum(.x == "Yes") / n()))


wave2_data %>%
  filter(across(matches("ServTry[1-4]_w2"), .fns = ~ !is.na(.x))) %>%
  ggplot(aes(y = 1, fill = fct_rev(D_TryAccess_w2))) +
  geom_bar(position = "fill") +
  scale_fill_sphsu(name = "Tried to access sexual/\nreproductive health services", reverse = TRUE) +
  scale_x_continuous("", labels = scales::percent) +
  scale_y_continuous("") +
  theme(
    legend.position = "bottom",
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    panel.background = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 2,
                             reverse = TRUE))


ggsave(
  "graphs/try_to_access.png",
  width = 250,
  height = 60,
  units = "mm",
  dpi = 300
)


# producing table ---------------------------------------------------------

## denoms ------------------------------------------------------------------

lab1 <- wave2_data %>%
  filter(across(matches("ServTry[1-4]_w2"), .fns = ~ !is.na(.x))) %>% 
  summarise(w = sum(weight2),
            n = n()) %>% 
  mutate(`%` = paste(n, round(w,0), sep = ","),
         cat = "Tried to access a sexual health service",
         CI = "",
         ` ` = " ",
         lab = "Denominators")

lab2 <- wave2_data %>%
  # filter(D_TryAccess_w2 == "Problems accessing a RH service") %>%
  filter(D_TryAccess_w2 != "Did not need to access or did not try to access SH service") %>%
  summarise(w = sum(weight2),
            n = n()) %>% 
  mutate(`%` = paste(n, round(w,0), sep = ","),
         cat = "Problems accessing a reproductive health service",
         CI = "",
         ` ` = " ",
         lab = "Denominators")


# table bodies ------------------------------------------------------------

sh_access <- wave2_data %>%
  filter(across(matches("ServTry[1-4]_w2"), .fns = ~ !is.na(.x))) %>%
  group_by(D_TryAccess_w2) %>%
  count(wt = weight2) %>%
  ungroup() %>%
  transmute(
    lab = D_TryAccess_w2,
    perc = n / sum(n),
    ll = perc_ci(perc, n = sum(n)),
    ul = perc_ci(perc, "u", sum(n)),
    cat = "Tried to access a sexual health service"
  )


rh_service <- wave2_data %>%
  # filter(D_TryAccess_w2 == "Problems accessing a RH service") %>%
  filter(D_TryAccess_w2 != "Did not need to access or did not try to access SH service") %>%
  summarise(
    ServTry4_w2 = sum(weight2[ServTry4_w2 == "Yes"]) / sum(weight2),
    ServTry5_w2 = sum(weight2[ServTry5_w2 == "Yes"]) / sum(weight2),
    ServTry6_w2 = sum(weight2[ServTry6_w2 == "Yes"]) / sum(weight2),
    ServTry7_w2 = sum(weight2[ServTry7_w2 == "Yes"]) / sum(weight2),
    n = sum(weight2)
  ) %>% pivot_longer(-n, names_to = "var", values_to = "perc") %>%
  rowwise() %>%
  mutate(serv = service_names[[var]]) %>%
  select(lab = serv, perc, n) %>%
  mutate(ll = perc_ci(perc, n = n),
         ul = perc_ci(perc, "u", n),
         cat = "Problems accessing a reproductive health service")

try_why_labs <- wave2_data %>%
  select(starts_with("D_TryWhy")) %>%
  unclass %>%
  map( ~ attr(.x, "label"))


rh_why <- wave2_data %>% 
  # filter(D_TryAccess_w2 == "Problems accessing a RH service") %>%
  filter(D_TryAccess_w2 != "Did not need to access or did not try to access SH service") %>%
  summarise(across(starts_with("D_TryWhy"), ~ sum(weight2[.x == "Yes"], na.rm = TRUE) / sum(weight2)),
            n = sum(weight2)) %>% 
  pivot_longer(-n, names_to = "var", values_to = "perc") %>%
  rowwise() %>% 
  mutate(reason = try_why_labs[[var]]) %>% 
  filter(reason != "Prefer not to say") %>% 
  select(lab = reason, perc, n) %>% 
  mutate(ll = perc_ci(perc, n = n),
         ul = perc_ci(perc, "u", n),
         cat = "Reason for failing to access")

bind_rows(sh_access, rh_service, rh_why)  %>% 
  mutate(` ` = " ",
    `%` = paste0(sprintf(fmt = "%.1f", round(perc*100, 1)), "%"),
         CI = paste0("(", sprintf(fmt = "%.1f", round(ll*100, 1)), "%, ", sprintf(fmt = "%.1f", round(ul*100, 1)), "%)")) %>% 
  bind_rows(lab1, lab2, mutate(lab2, cat = "Reason for failing to access")) %>% 
  select(` `, "  " = lab, cat, `%`, CI) %>% 
  gt(rowname_col = " ", groupname_col = "cat") %>% 
  summary_rows(
    fns = list(" "  = ~ " "),
    columns = ` `,
    groups = TRUE,
    missing_text = " "
  ) 

# %>%
  gtsave("Serv_acc.html")



# same, but by age? -------------------------------------------------------




sh_access_dat <- wave2_data %>%
  filter(across(matches("ServTry[1-4]_w2"), .fns = ~ !is.na(.x))) 

 
  chisq.test(sh_access_dat$ServTry1_w2, sh_access_dat$D_Age5Cat_w2)
  weights::wtd.chi.sq(sh_access_dat$ServTry1_w2, sh_access_dat$D_Age5Cat_w2, weight = sh_access_dat$weight2)
  

sh_access_a <- sh_access_dat %>% 
  mutate(p = wtd.chi.sq(D_TryAccess_w2, D_Age5Cat_w2, weight = weight2)["p.value"],
         xsq = wtd.chi.sq(D_TryAccess_w2, D_Age5Cat_w2, weight = weight2)["Chisq"]) %>% 
  group_by(D_TryAccess_w2, D_Age5Cat_w2) %>%
  summarise(n = sum(weight2),
            p = max(p),
            xsq = max(xsq)) %>%
  ungroup(D_TryAccess_w2) %>%
  mutate(
    lab = D_TryAccess_w2,
    perc = n / sum(n),
    ll = perc_ci(perc, n = sum(n)),
    ul = perc_ci(perc, "u", sum(n)),
    cat = "Tried to access a sexual health service"
  ) %>% 
  select(-n)

sh_access_dat %>% 
  summarise(p = wtd.chi.sq(D_TryAccess_w2, D_Age5Cat_w2, weight = weight2)["p.value"])
  

rh_service_a <- wave2_data %>%
  # filter(D_TryAccess_w2 == "Problems accessing a RH service") %>%
  filter(D_TryAccess_w2 != "Did not need to access or did not try to access SH service") %>%
  group_by(D_Age5Cat_w2) %>% 
  summarise(
    ServTry4_w2 = sum(weight2[ServTry4_w2 == "Yes"]) / sum(weight2),
    ServTry5_w2 = sum(weight2[ServTry5_w2 == "Yes"]) / sum(weight2),
    ServTry6_w2 = sum(weight2[ServTry6_w2 == "Yes"]) / sum(weight2),
    ServTry7_w2 = sum(weight2[ServTry7_w2 == "Yes"]) / sum(weight2),
    n = sum(weight2)
  ) %>% 
  pivot_longer(-c(n, D_Age5Cat_w2), names_to = "var", values_to = "perc") %>%
  rowwise() %>%
  mutate(serv = service_names[[var]]) %>%
  select(D_Age5Cat_w2, lab = serv, perc, n) %>%
  mutate(ll = perc_ci(perc, n = n),
         ul = perc_ci(perc, "u", n),
         cat = "Problems accessing a reproductive health service") %>% 
  select(-n)

rh_why_a <- wave2_data %>% 
  # filter(D_TryAccess_w2 == "Problems accessing a RH service") %>%
  filter(D_TryAccess_w2 != "Did not need to access or did not try to access SH service") %>%
  group_by(D_Age5Cat_w2) %>% 
  summarise(across(starts_with("D_TryWhy"), ~ sum(weight2[.x == "Yes"], na.rm = TRUE) / sum(weight2)),
            n = sum(weight2)) %>% 
  pivot_longer(-c(n, D_Age5Cat_w2), names_to = "var", values_to = "perc") %>%
  rowwise() %>% 
  mutate(reason = try_why_labs[[var]]) %>% 
  filter(reason != "Prefer not to say") %>% 
  select(D_Age5Cat_w2, lab = reason, perc, n) %>% 
  mutate(ll = perc_ci(perc, n = n),
         ul = perc_ci(perc, "u", n),
         cat = "Reason for failing to access")%>% 
  select(-n) 




bind_rows(sh_access_a, rh_service_a, rh_why_a)  %>% 
bind_rows(bind_rows(sh_access, rh_service, rh_why)  %>% 
  mutate(D_Age5Cat_w2 = "Total")) %>% 
  mutate(` ` = " ",
         `%` = paste0(sprintf(fmt = "%.1f", round(perc*100, 1)), "%"),
         CI = paste0("(", sprintf(fmt = "%.1f", round(ll*100, 1)), "%, ", sprintf(fmt = "%.1f", round(ul*100, 1)), "%)"),
         CI = if_else(str_detect(CI, "NaN"), "-", CI)) %>% 
  bind_rows(bind_rows(lab1, lab2, mutate(lab2, cat = "Reason for failing to access")) %>% mutate(D_Age5Cat_w2 = "Total") ) %>% 
  select(D_Age5Cat_w2, ` `, "  " = lab, cat, `%`, CI, p) %>% 
  pivot_wider(id_cols = c(` `, `  `, cat), names_from = D_Age5Cat_w2, values_from = c(`%`, CI), names_glue = "{D_Age5Cat_w2}_{.value}", values_fill = " ") %>% 
  ## new step -- add p values in here?? ##
  gt(rowname_col = " ", groupname_col = "cat") %>% 
  summary_rows(
    fns = list(" "  = ~ " "),
    columns = ` `,
    groups = TRUE,
    missing_text = " "
  ) %>% 
  tab_spanner_delim(delim = "_", split = "first") #%>% 
  gtsave("serv_acc_byage.html")

  

# with collapsed reasons variable -----------------------------------------


serv_acc_reasons_tidy <- wave2_data %>% 
    filter(!is.na(D_ConServFailWhy_w2)) %>% 
    group_by(D_ConServFailWhy_w2, D_Age5Cat_w2) %>% 
    # group_by(exposure, outcome) %>%
    summarise(wt = sum(weight2),
              n = n(),
              .groups = "drop_last") %>%
    group_by(D_ConServFailWhy_w2) %>% 
    nest() %>% 
    mutate(data = modify(data, ~ janitor::adorn_totals(.x))) %>% 
    unnest(data) %>% 
    group_by(D_Age5Cat_w2) %>% 
    mutate(
      perc = wt / sum(wt),
      ll = perc_ci(perc, n = sum(wt)),
      ul = perc_ci(perc, "u", sum(wt)),
      `%` = paste0(sprintf(fmt = "%.1f", round(perc * 100, 1)), ""),
      CI = paste0(
        "(",
        sprintf(fmt = "%.1f", round(ll * 100, 1)),
        ",\u00A0",
        sprintf(fmt = "%.1f", round(ul * 100, 1)),
        ")"
      ),
      `%_CI` = if_else(str_detect(CI, "NaN"), "-", paste(`%`, CI, sep = " "))
      # Denominators = paste0("\u200D", round(wt, 0), "," , n)
    ) %>%
    ungroup()

serv_acc_tot_row <- serv_acc_reasons_tidy %>% 
  filter(D_Age5Cat_w2 == "Total") %>% 
  group_by(D_Age5Cat_w2) %>% 
  summarise(wt = sum(wt), n = sum(n), .groups = "drop_last") %>% 
  transmute(Total = paste0("\u200D", round(wt, 0), "," , n),
            D_ConServFailWhy_w2 = "Denominators (weighted, unweighted)")


serv_acc_reasons_tidy %>% 
  select(D_ConServFailWhy_w2,
         D_Age5Cat_w2,
         `%_CI`) %>% 
  pivot_wider(names_from = D_Age5Cat_w2, values_from = `%_CI`) %>% 
  bind_rows(serv_acc_tot_row) %>% 
  gt(rowname_col = "D_ConServFailWhy_w2") %>% 
  fmt_missing(columns = everything(), missing_text = " ") %>% 
  gtsave("Service Access barriers.html")


library(janitor)

vals <- wave2_data %>% 
  filter(!is.na(D_ConServFailWhy_w2)) %>% 
  summarise(n = n(),
            wt = sum(weight2),
            chisq = wtd.chi.sq(D_Age5Cat_w2, D_ConServFailWhy_w2, weight = weight2)[1],
            p = wtd.chi.sq(D_Age5Cat_w2, D_ConServFailWhy_w2, weight = weight2)[3]) %>% 
  unclass

label <- glue::glue("Denominators (weighted, unweighted): {round(vals$wt, 0)}, {vals$n}<br>
           X^2 = {round(vals$chisq, 2)}<br>
           p-value = {round(vals$p, 3)}")

serv_acc_gg <- serv_acc_reasons_tidy %>%
  group_by(D_ConServFailWhy_w2) %>% 
  nest() %>% 
  mutate(data = map(data, ~adorn_totals(.x))) %>% 
  unnest(data) %>% 
  ggplot(aes(D_Age5Cat_w2, wt, fill = fct_rev(D_ConServFailWhy_w2))) +
  geom_col(position = "fill", width = 0.5) +
  scale_fill_sphsu(name = str_wrap("Reasons for not being able to access contraceptive services", 50)) +
  theme_sphsu_light() +
  scale_y_continuous("Weighted prevalence", 
                     expand = expansion(0),
                     labels = scales::percent) +
  theme(panel.grid = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_x_discrete("Age group")

g <- ggplotGrob(serv_acc_gg)


# leg2 <- gtable::gtable_add_grob(g$grob[[15]], textGrob(label, gp = gpar(fontsize = 10)), 5, 3)
g$grob[[15]]$heights[[5]] <- unit(40, "mm")

g$grobs[[15]] <- gtable::gtable_add_grob(g$grob[[15]], gridtext::richtext_grob(label, gp = gpar(fontsize = 10),
                                                                               x = unit(0.1, "npc"),
                                                                               y = unit(0, "npc"),
                                                                               vjust = -1, hjust = 0), 5, 3)



  png("graphs/service access reasons.png", width = 350, height = 200, units = "mm", res = 400)
grid.draw(g)
dev.off()
  
ggsave("graphs/service access reasons.png", ., width = 350, height = 200, units = "mm", dpi = 400)

wave2_data %>%
  filter(!is.na(D_ConServFailWhy_w2)) %>%
  summarise(
    p = weights::wtd.chi.sq(D_Age5Cat_w2, D_ConServFailWhy_w2, weight = weight2)["p.value"],
    xsq = weights::wtd.chi.sq(D_Age5Cat_w2, D_ConServFailWhy_w2, weight = weight2)["Chisq"],
  )

data.frame(weights::wtd.chi.sq(wave2_data$D_Age5Cat_w2, wave2_data$D_ConServFailWhy_w2, weight = wave2_data$weight2)) %>% 
  t()


# serivice access by age --------------------------------------------------

chi_res <- weights::wtd.chi.sq(wave2_data$D_Age5Cat_w2, wave2_data$D_ConServAcc_w2, weight = wave2_data$weight2)
wave2_data %>% filter(!is.na(D_ConServAcc_w2)) %>% summarise(s = sum(weight2)) %>% pull(s)
glue::glue("{wave2_data %>% filter(!is.na(D_ConServAcc_w2)) %>% sum(.$weight2)}")


wave2_data %>% 
  select(D_ConServAcc_w2, D_Age5Cat_w2, weight2) %>% 
  group_by(D_ConServAcc_w2, D_Age5Cat_w2) %>% 
  summarise(wt = sum(weight2)) %>% 
  nest() %>% 
  mutate(data = map(data, ~adorn_totals(.x))) %>% 
  unnest(data) %>% 
  ggplot(aes(D_Age5Cat_w2, wt, fill = fct_rev(D_ConServAcc_w2))) +
  # stat_sum(geom = "col", position = "fill", width = 0.5) +
  geom_col(position = "fill", width = 0.5) +
  scale_fill_sphsu(name = str_wrap("Success or barriers accessing contraception services", 50)) +
  theme_sphsu_light() +
  scale_y_continuous("Weighted prevalence", 
                     expand = expansion(0),
                     labels = scales::percent) +
  theme(panel.grid = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_x_discrete("Age group")
  
ggsave("graphs/service access outcomes.png", last_plot(), width = 350, height = 200, units = "mm", dpi = 400)
  
# better weights ----------------------------------------------------------

library(survey)

svy_df <- svydesign(ids = ~0, weights = ~weight2, data = wave2_data %>% filter(!is.na(D_ConServFailWhy_w2)))

svychisq(~D_ConServFailWhy_w2 + D_Age5Cat_w2, svy_df, drop.unused.levels = TRUE)

summary(svytable(~D_ConServFailWhy_w2 + D_Age5Cat_w2, svy_df, drop.unused.levels = TRUE))
pf(0.6789, 8.69, 634.72, lower.tail = FALSE)
