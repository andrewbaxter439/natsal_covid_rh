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
  
library(patchwork)

surv_graphs <- conceptions_abortions_yearly %>% 
  mutate(time = year - 2009) %>% 
  pivot_longer(c(con_rate, abo_rate), names_to = "outcome", values_to = "rate") %>% 
  mutate(time = year - 2009,
         outcome = case_when(outcome == "con_rate" ~ "Conceptions", outcome == "abo_rate" ~ "Abortions"),
         outcome = fct_rev(outcome)) %>% 
  ggplot(aes(time, rate, colour = outcome)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous("Rate per 1,000", limits = c(0, NA)) +
  scale_x_continuous("Year", limits = c(1, 11), breaks = seq(1, 11, 2), labels = seq(2010, 2020, 2)) +
  ggpubr::stat_regline_equation(label.y.npc = 0.75) +
  scale_colour_sphsu(palette = "cool") +
  labs(tag = "Surveillance data") +
  theme_sphsu_light() +
  theme(legend.position = "none",
        plot.tag.position = "left",
        plot.tag = element_text(angle = 90, margin = margin(r = 10))) +
  facet_wrap(~ outcome, nrow = 1, scales = "free_y")
  
natsal_graphs <- bind_rows(natsal_preg, natsal_abo) %>% 
  mutate(outcome = fct_rev(outcome)) %>% 
  ggplot(aes(year, perc, colour = outcome)) +
  geom_point() +
  geom_pointrange(aes(ymin = li, ymax = ui)) +
  geom_line() +
  scale_y_continuous("Percentage", limits = c(0, NA),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous("Year", limits = c(2010, 2020), breaks = seq(2010, 2020, 2)) +
  scale_colour_sphsu(palette = "cool") +
  theme_sphsu_light() +
  labs(tag = "Natsal surveys") +
  theme(legend.position = "none",
        plot.tag.position = "left",
        plot.tag = element_text(angle = 90, margin = margin(r = 10))) +
  facet_wrap(~ outcome, nrow = 1, scales = "free_y")

surv_graphs / natsal_graphs

# importing Chlamydia data ------------------------------------------------


chlamydia_diag <- read_excel("C:/local/OneDrive - University of Glasgow/R Studio - home folder/Natsal-Covid/data/sti_workbook.xlsx",
  sheet = "England", skip = 56, n_max = 3)


sti_testing <- tribble(~group, ~year, ~outcome, ~perc, ~li, ~ui,
        "natsal", 2010, "Chlamydia test", 0.151, 0.139, 0.163,
        "natsal", 2020, "Chlamydia test", 0.041, 0.032, 0.053,
        "natsal", 2010, "HIV test", 0.060, 0.051, 0.070,
        "natsal", 2020, "HIV test", 0.065, 0.054, 0.079 
)

  