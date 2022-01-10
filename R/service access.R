source(file.path(old_wd, "R/import and convert.R"))
source(file.path(old_wd, "R/functions.R"))
library(gt)
library(weights)

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
  file.path(old_wd, "graphs/try_to_access.png"),
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
