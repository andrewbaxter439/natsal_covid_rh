library(readxl)
library(tidyverse)
library(SPHSUgraphs)
library(broom)
conceptions2019workbook <- read_excel(file.path(old_wd, "data", "conceptions2019workbook.xlsx"),
sheet = "Table 1a", skip = 4)


names_rep <- names(conceptions2019workbook) %>% 
  str_remove("\\..*") %>% 
  str_remove("1$") %>% 
  tibble(names = .) %>% 
  mutate(fixed_names = if_else(names == "", lag(names), names),
         fixed_names = if_else(names == "", lag(fixed_names), fixed_names),
         addend = conceptions2019workbook[1,] %>% unlist(use.names = FALSE),
         full_names = paste(fixed_names, addend, sep = "_") %>% str_remove("(_NA|\\d)$")) %>% 
  pull(full_names)


conceptions_abortions_yearly <- conceptions2019workbook %>% 
  `names<-`(names_rep) %>% 
  mutate(Year = as.numeric(str_extract(`Year of conception`, "^\\d{4}")), .keep = "unused") %>%
  filter(!is.na(Year)) %>% 
  mutate(across(.fns = as.numeric)) %>% 
  pivot_longer(-Year, names_to = c("Age", "val"), names_sep = "_") %>% 
  pivot_wider(id_cols = c(Year, Age), names_from = val, values_from = value) %>% 
  janitor::clean_names() %>% 
  select(year, age, number = 3, rate = 4, abo_perc = 5) %>% 
  mutate(pop = number * 1000 / rate,
         number_corr = if_else(age == "Under 20", number - lag(number) - lag(number, 2), number),
         pop = if_else(age == "Under 20", pop - lag(pop), pop),
         abo_n = number * abo_perc / 100,
         abo_n = if_else(age == "Under 20", abo_n - lag(abo_n) - lag(abo_n, 2), abo_n),
         group = "surv"
         ) %>% 
  filter(str_detect(age, "^\\d{2}")) %>% 
  group_by(year, group) %>% 
  summarise(con_rate = weighted.mean(1000*number_corr/pop, w = pop),
            uncorr_rate = weighted.mean(rate, w = pop),
            abo_rate = weighted.mean(1000*abo_n/pop, w = pop),
            .groups = "drop") %>% 
  filter(year >= 2010) %>% 
  mutate(rel_con_rate = con_rate / con_rate[year==2010],
         rel_abo_rate = abo_rate / abo_rate[year==2010]) 

conceptions_abortions_yearly %>% 
  ggplot(aes(year, con_rate)) + 
  geom_line() + 
  geom_text(aes(label = round(con_rate, 2)), nudge_y = 0.01, angle = 45) +
  scale_x_continuous(breaks = 2010:2019) +
    ylim(0, NA)

conceptions_abortions_yearly %>% 
ggplot(aes(year, abo_rate)) + 
  geom_line() + 
  geom_text(aes(label = round(abo_rate, 2)), nudge_y = 0.01, angle = 45) +
  scale_x_continuous(breaks = 2010:2019) +
    ylim(0, NA)


# try a linear model ------------------------------------------------------

natsal_preg <- tribble(~group, ~year, ~outcome, ~perc, ~li, ~ui,
                       "natsal", 2010, "Conceptions", 0.146, 0.135, 0.158,
                       "natsal", 2020, "Conceptions", 0.106, 0.093, 0.121)

natsal_abo <- tribble(~group, ~year, ~outcome, ~perc, ~li, ~ui,
                       "natsal", 2010, "Abortions", 0.023, 0.019, 0.028,
                       "natsal", 2020, "Abortions", 0.008, 0.004, 0.014)



pregnancy_rates_joined <- conceptions_abortions_yearly %>% 
  mutate(perc = con_rate/1000) %>% 
  bind_rows(natsal_preg) %>% 
  mutate(exp = if_else(year == 2020, "cov", "pre_cov"),
    exp = factor(exp, levels = c("pre_cov", "cov")),
         time = year - 2009)


pregnancy_rates_joined %>% 
  lm(perc ~ time + group + exp, data = .) %>% 
  summary()

mod <- lm(perc ~ time + group + exp, data = pregnancy_rates_joined)


tibble(
  group = "natsal",
  exp = "pre_cov",
  year = 2011:2020,
  time = year - 2009
) %>%
  bind_rows(pregnancy_rates_joined) %>%
  add_row(
    group = "surv",
    exp = "pre_cov",
    year = 2020,
    time = 11
  ) %>%
  mutate(
    pred = predict(mod, newdata = .),
    pred_point = if_else(year == 2020 &
                           exp == "pre_cov", pred, NULL),
    line = if_else(year < 2020, pred, NULL)
  ) %>%
  ggplot(aes(year, col = group)) +
  geom_point(aes(y = perc), size = 2) +
  geom_point(aes(y = pred_point, shape = "Predicted\n(unexposed)"), size = 2) +
  geom_line(aes(y = line)) +
  scale_shape_manual("", values = c("Predicted\n(unexposed)" = 1, "Observed" = 16)) +
  scale_x_continuous(breaks = 2010:2020) +
  theme_sphsu_light() +
  theme(panel.grid.minor.x = element_blank()) +
  scale_colour_sphsu(name = "Data", palette = "cool") +
  geom_vline(aes(xintercept = 2019.5), linetype = "dashed") +
  scale_y_continuous(
    "Conceptions",
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, NA)
  )

as_tibble(confint(mod), rownames = NA) %>%
  rownames_to_column("term") %>%
  right_join(tidy(mod)) %>%
  transmute(
    Term = if_else(term == "(Intercept)", "2010 base rate", term),
    `Percentage (point change)` = estimate,
    `95%CI` = paste0("(", signif(`2.5 %`, 3), ", ", signif(`97.5 %`, 3), ")"),
    P = if_else(p.value < 0.001, "p<0.001", as.character(round(p.value, 3)))
  ) %>%
  gt::gt()



# as an odds ratio? -------------------------------------------------------

pregnancy_rates_joined %>%
  mutate(
    odds = perc / (1 - perc),
    lnodds = log(odds),
    group = factor(group, levels = c("surv", "natsal"))
  ) %>%
  lm(lnodds ~ time + group + exp, data = .) %>%
  broom::tidy() %>%
  mutate(OR = exp(estimate))


  
# graphs for pregnancy and abortion rates ---------------------------------
  


# importing Chlamydia data ------------------------------------------------



gum_clinic <-  read_excel("C:/local/OneDrive - University of Glasgow/R Studio - home folder/Natsal-Covid/data/Attendance and testing rates.xlsx", 
                          sheet = "SHC_Consultations")

hiv_testing <- read_excel("C:/local/OneDrive - University of Glasgow/R Studio - home folder/Natsal-Covid/data/Attendance and testing rates.xlsx", 
                          sheet = "HIV_test")

chl_testing <- read_excel("C:/local/OneDrive - University of Glasgow/R Studio - home folder/Natsal-Covid/data/Attendance and testing rates.xlsx", 
                          sheet = "CT_test")



sti_testing_nat <- tribble(~group, ~gender, ~year, ~outcome, ~perc, ~li, ~ui,
        "natsal", "Men", 2010, "Clinic attendance", 0.070, 0.062, 0.079,
        "natsal", "Men", 2020, "Clinic attendance", 0.087, 0.074, 0.103,
        "natsal", "Men", 2010, "Chlamydia testing", 0.151, 0.139, 0.163,
        "natsal", "Men", 2020, "Chlamydia testing", 0.039, 0.030, 0.051,
        "natsal", "Men", 2010, "HIV testing", 0.060, 0.051, 0.070,
        "natsal", "Men", 2020, "HIV testing", 0.063, 0.052, 0.076,
        "natsal", "Women", 2010, "Clinic attendance", 0.086, 0.078, 0.095,
        "natsal", "Women", 2020, "Clinic attendance", 0.080, 0.068, 0.093,
        "natsal", "Women", 2010, "Chlamydia testing", 0.251, 0.237, 0.264,
        "natsal", "Women", 2020, "Chlamydia testing", 0.069, 0.058, 0.081,
        "natsal", "Women", 2010, "HIV testing", 0.104, 0.095, 0.114,
        "natsal", "Women", 2020, "HIV testing", 0.081, 0.070, 0.095 
)

sti_testing <- hiv_testing %>% 
  mutate(hiv_test_rate = `rate per 100,000` / 1000, .keep = "unused") %>% 
  select(-number, -population) %>% 
  left_join(
    chl_testing %>% 
      mutate(chl_test_rate = `rate per 100,000` / 1000, .keep = "unused") %>% 
      select(-number, -population)
  ) %>% 
  left_join(
    gum_clinic %>% 
      mutate(clinic_attendance = `rate per 100,000` / 1000, .keep = "unused") %>% 
      select(-number, -population)
  ) %>% 
  mutate(gender = case_when(
    gender == "Male" ~ "Men",
    gender == "Female" ~ "Women",
  ),
  group = "surv")

library(patchwork)


## tidying data ------------------------------------------------------------


surv_data_tidy <- conceptions_abortions_yearly %>%
  mutate(gender = "Women",
         con_rate = con_rate / 10,
         abo_rate = abo_rate / 10) %>%
  full_join(sti_testing, by = c("year", "group", "gender")) %>%
  select(year,
         group,
         gender,
         con_rate,
         abo_rate,
         clinic_attendance,
         hiv_test_rate,
         chl_test_rate) %>%
  pivot_longer(
    c(con_rate, abo_rate, clinic_attendance, hiv_test_rate, chl_test_rate),
    names_to = "outcome",
    values_to = "rate"
  ) %>%
  filter(year < 2020) %>% 
  mutate(
    time = year - 2009,
    outcome = case_when(
      outcome == "con_rate" ~ "Conceptions",
      outcome == "abo_rate" ~ "Abortions",
      outcome == "hiv_test_rate" ~ "HIV testing",
      outcome == "chl_test_rate" ~ "Chlamydia testing",
      outcome == "clinic_attendance" ~ "Clinic attendance"
    ),
    outcome = fct_relevel(outcome, "Conceptions",
                          "Abortions",
                          "Clinic attendance",
                          "HIV testing",
                          "Chlamydia testing")
  ) %>% 
  mutate(rate = if_else(outcome == "Chlamydia testing" & year < 2012, na_dbl, rate))

natsal_data_tidy <-   bind_rows(natsal_preg, natsal_abo) %>% 
  mutate(gender = "Women") %>% 
  bind_rows(sti_testing_nat) %>% 
  mutate(across(perc:ui, ~.x*100)) %>% 
mutate(outcome = fct_relevel(outcome, "Conceptions",
                             "Abortions",
                          "Clinic attendance",
                             "HIV testing",
                             "Chlamydia testing")) 



limit_sets <- left_join(surv_data_tidy, natsal_data_tidy, by = c("year", "outcome", "gender")) %>% 
  group_by(outcome) %>% 
  summarise(max = max(rate, ui, na.rm = TRUE)*1.1)

## making graphs -----------------------------------------------------------


surv_graphs <- surv_data_tidy %>% 
  ggplot(aes(time, rate, colour = gender, shape = gender)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point(data = limit_sets, aes(x = NA_integer_, y = max), inherit.aes = FALSE, alpha = 0) +
  scale_y_continuous("Rate per 100", limits = c(0, NA), expand = expansion(mult = c(0, 0)), labels = scales::label_number(accuracy = 1)) +
  scale_x_continuous("Year", limits = c(1, 11), breaks = seq(1, 11, 2), labels = seq(2010, 2020, 2)) +
  scale_colour_manual(name = "Gender", values = c("Men" = sphsu_cols("Thistle", names = FALSE), "Women" = sphsu_cols("Turquoise", names = FALSE))) +
  scale_shape_discrete(name = "Gender") +
  labs(title = "Surveillance data") +
  theme_sphsu_light() +
  theme(legend.position = "bottom") +
  facet_wrap(~ outcome, ncol = 1, scales = "free_y")


(natsal_graphs <- natsal_data_tidy %>% 
  ggplot(aes(year, perc, colour = gender, shape = gender)) +
  geom_point(size = 2) +
  geom_linerange(aes(ymin = li, ymax = ui), size = 1, show.legend = FALSE) +
  geom_point(data = limit_sets, aes(x = NA_integer_, y = max), inherit.aes = FALSE, size = 2) +
  geom_line(linetype = "dashed") +
  scale_y_continuous("Percentage", limits = c(0, NA),
                     labels = scales::label_percent(accuracy = 1, scale = 1), expand = expansion(mult = c(0, 0))) +
  scale_x_continuous("Year", limits = c(2010, 2020), breaks = seq(2010, 2020, 2)) +
  scale_colour_manual(name = "Gender",
                      values = c(
                        "Men" = sphsu_cols("Thistle", names = FALSE),
                        "Women" = sphsu_cols("Turquoise", names = FALSE)
                      ),
                      guide = guide_legend(override.aes = list(linetype = c(NA,NA), line = c(NA, NA)))) +
  theme_sphsu_light() +
  scale_shape_discrete("Gender") +
  theme(legend.position = "none") +
  labs(title = "Natsal surveys") +
  guides(line = guide_legend(override.aes = list(line = c(1,1)))) +
  facet_wrap(~ outcome, ncol = 1, scales = "free_y"))

(surv_graphs + natsal_graphs) / guide_area()  + plot_layout(guides = "collect", heights = c(10, 1))

ggsave("suveillance_comparison.png", height = 30, width = 24, units = "cm", dpi = 400)

  # testing significance - move to rmd? -------------------------------------


surv_data_tidy %>%
  group_by(gender, outcome) %>%
  nest() %>%
  filter(!(gender == "Men" &
             outcome %in% c("Conceptions", "Abortions"))) %>%
  mutate(coefs = map(
    data,
    .f = function(data) {
      lm(rate ~ time, data = data) %>%
        broom::tidy(conf.int = TRUE)
    }
  )) %>%
  unnest(coefs) %>%
  filter(term == "time") %>%
  select(estimate, conf.low, conf.high) %>%
  ungroup() %>%
  arrange(outcome, gender) %>% 
  {
    cat(
      glue::glue(
        "{.$gender} saw a yearly change in {.$outcome} of {signif(.$estimate, 3)} per 100 {.$gender} ({signif(.$conf.low, 3)}, {signif(.$conf.high, 3)})"
      ),
      sep = "\n"
    )
  }
