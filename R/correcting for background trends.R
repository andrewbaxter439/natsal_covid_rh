library(readxl)
library(tidyverse)
library(SPHSUgraphs)
library(broom)

conceptions2019workbook <-
  read_excel(
    file.path("data", "conceptions2019workbook.xlsx"),
    sheet = "Table 1a",
    skip = 4
  )

conceptions2020workbook <-
  read_excel(
    file.path("data", "conceptions2020workbook.xlsx"),
    sheet = "1a",
    skip = 7
  )



names_rep <- names(conceptions2019workbook) %>%
  str_remove("\\..*") %>%
  str_remove("1$") %>%
  tibble(names = .) %>%
  mutate(
    fixed_names = if_else(names == "", lag(names), names),
    fixed_names = if_else(names == "", lag(fixed_names), fixed_names),
    addend = conceptions2019workbook[1, ] %>% unlist(use.names = FALSE),
    full_names = paste(fixed_names, addend, sep = "_") %>% str_remove("(_NA|\\d)$")
  ) %>%
  pull(full_names)

names(conceptions2020workbook) <- names_rep

conceptions_abortions_yearly <-
  conceptions2020workbook %>%
  `names<-`(names_rep) %>%
  # bind_rows(conceptions2020workbook %>% filter(`Year of conception` == 2020) %>% mutate(`Year of conception` = as.character(`Year of conception`))) %>% 
  mutate(Year = as.numeric(str_extract(`Year of conception`, "^\\d{4}")), .keep = "unused") %>%
  filter(!is.na(Year)) %>%
  mutate(across(.fns = as.numeric)) %>%
  pivot_longer(-Year,
               names_to = c("Age", "val"),
               names_sep = "_") %>%
  pivot_wider(
    id_cols = c(Year, Age),
    names_from = val,
    values_from = value
  ) %>%
  janitor::clean_names() %>%
  select(year,
         age,
         number = 3,
         rate = 4,
         abo_perc = 5) %>%
  mutate(
    pop = number * 1000 / rate,
    number_corr = if_else(age == "Under 20", number - lag(number) - lag(number, 2), number),
    pop = if_else(age == "Under 20", pop - lag(pop), pop),
    abo_n = number * abo_perc / 100,
    abo_n = if_else(age == "Under 20", abo_n - lag(abo_n) - lag(abo_n, 2), abo_n),
    group = "surv"
  ) %>%
  filter(str_detect(age, "(Under 20|^\\d{2})")) %>%
  group_by(year, group) %>%
  summarise(
    con_rate = weighted.mean(1000 * number_corr / pop, w = pop),
    uncorr_rate = weighted.mean(rate, w = pop),
    abo_rate = weighted.mean(1000 * abo_n / pop, w = pop),
    .groups = "drop"
  ) %>%
  filter(year >= 2010) %>%
  mutate(rel_con_rate = con_rate / con_rate[year == 2010],
         rel_abo_rate = abo_rate / abo_rate[year == 2010]) 

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

# importing Chlamydia data ------------------------------------------------



gum_clinic <-  read_excel("data/Attendance and testing rates_updated.xlsx", 
                          sheet = "STI-related_consultations")
                          # sheet = "SHC_Consultations")

hiv_testing <- read_excel("data/Attendance and testing rates_updated.xlsx", 
                          sheet = "HIV_test")

chl_testing <- read_excel("data/Attendance and testing rates_updated.xlsx", 
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

# full abortions stats ----------------------------------------------------

library(readxl)
abortions_2010_2020 <- read_excel("data/abortions_2010_2020.xlsx")

abortions_yearly <- abortions_2010_2020 %>% 
  filter(!(Age %in% c("All ages", "Under 16", "16-17", "Under 18"))) %>% 
  pivot_longer(-c(Age, stat), names_to = "year", values_to = "val") %>% 
  pivot_wider(names_from = stat, values_from = val) %>% 
  mutate(pop = number*1000/rate) %>% 
  group_by(year) %>% 
  summarise(#number = sum(number),
            abo_rate = weighted.mean(rate, w = pop)) %>% 
            #op = sum(pop))
  mutate(group = "surv",
         year = as.numeric(year))

## tidying data ------------------------------------------------------------


surv_data_tidy <- conceptions_abortions_yearly %>%
  select(-abo_rate) %>% 
  full_join(abortions_yearly, by = c("year", "group")) %>% 
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
  # filter(year < 2020) %>% 
  mutate(
    time = year - 2009,
    outcome = case_when(
      outcome == "con_rate" ~ "Conceptions",
      outcome == "abo_rate" ~ "Abortions",
      outcome == "hiv_test_rate" ~ "HIV testing",
      outcome == "chl_test_rate" ~ "Chlamydia testing",
      outcome == "clinic_attendance" ~ "Use of STI-related services"
    ),
    outcome = fct_relevel(
      outcome,
      "Use of STI-related services",
      "Chlamydia testing",
      "HIV testing",
      "Conceptions",
      "Abortions"
    )
  ) %>% 
  mutate(rate = if_else(outcome == "Chlamydia testing" & year < 2012, NA_real_, rate),
         covid = if_else(year == 2020, 1, 0))

natsal_data_tidy <-   bind_rows(natsal_preg, natsal_abo) %>%
  mutate(gender = "Women") %>%
  bind_rows(sti_testing_nat) %>%
  mutate(across(perc:ui, ~ .x * 100)) %>%
  mutate(
    outcome = fct_relevel(
      outcome,
      "Clinic attendance",
      "Chlamydia testing",
      "HIV testing",
      "Conceptions",
      "Abortions"
    ) %>% 
      fct_recode("Use of STI-related services" = "Clinic attendance")
  ) 




limit_sets <-
  left_join(surv_data_tidy,
            natsal_data_tidy,
            by = c("year", "outcome", "gender")) %>%
  group_by(outcome) %>%
  summarise(max = max(rate, ui, na.rm = TRUE) * 1.1)


conceptions_abortions_yearly
# making graphs -----------------------------------------------------------


(
  surv_graphs <- surv_data_tidy %>%
    # filter(year < 2020) %>%
    ggplot(
      aes(
        time,
        rate,
        colour = gender,
        shape = gender,
        fill = interaction(gender, covid)
      )
    ) +
    geom_point(size = 2) +
    geom_smooth(method = "lm", se = FALSE)+
    geom_point(data = limit_sets, aes(x = NA_integer_, y = max), inherit.aes = FALSE, alpha = 0) +
    scale_y_continuous(
      "Rate per 100",
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0)),
      labels = scales::label_number(accuracy = 1)
    ) +
    scale_x_continuous(
      "Year",
      limits = c(1, 11),
      breaks = seq(1, 11, 2),
      labels = seq(2010, 2020, 2)
    ) +
    scale_colour_manual(name = "Gender",
                        values = c(
                          "Men" = sphsu_cols("Thistle", names = FALSE),
                          "Women" = sphsu_cols("Turquoise", names = FALSE)
                        )) +
    # scale_shape_discrete(name = "Gender") +
    labs(title = "Surveillance data") +
    scale_shape_manual("Gender", values = c("Men" = 21, "Women" = 24)) +
    theme_sphsu_light()+
    scale_fill_manual(values = c(
      "Men.1" = 0,
      "Women.1" = 0,
      "Men.0" = sphsu_cols("Thistle", names = FALSE),
      "Women.0" = sphsu_cols("Turquoise", names = FALSE)
    )) +
    theme(
      strip.background = element_rect(fill = "white", size = 1),
      legend.position = "none",
      panel.background = element_rect(
        fill = "white",
        size = 1,
        colour = "grey"
      ),
      strip.text = element_text(
        face = "bold",
        hjust = 0,
        margin = margin(5, 0, 5, 0)
      )
    ) +
    facet_wrap( ~ outcome, ncol = 1, scales = "free_y"))


(natsal_graphs <- 
    natsal_data_tidy %>%
    ggplot(aes(
      year,
      perc,
      colour = gender,
      fill = gender,
      shape = gender,
      alpha = "Men"
    )) +
    geom_point(size = 2) +
    geom_linerange(aes(ymin = li, ymax = ui),
                   size = 1,
                   show.legend = FALSE) +
    geom_point(
      data = limit_sets,
      aes(x = NA_integer_, y = max, alpha = "(2020 data points, not included in trend analysis)"),
      inherit.aes = FALSE,
      size = 2
    ) +
    geom_line(linetype = "dashed") +
    scale_shape_manual("Gender", guide = "none", values = c(21, 24)) +
    scale_y_continuous(
      "Percentage",
      limits = c(0, NA),
      labels = scales::label_percent(accuracy = 1, scale = 1),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_x_continuous("Year",
                       limits = c(2010, 2020),
                       breaks = seq(2010, 2020, 2)) +
    scale_colour_manual(
      name = "Gender",
      aesthetics = c("fill", "colour"),
      values = c(
        "Men" = sphsu_cols("Thistle", names = FALSE),
        "Women" = sphsu_cols("Turquoise", names = FALSE)
      ),
      guide = guide_legend(
        override.aes = list(
          linetype = c(NA, NA),
          line = c(NA, NA),
          size = c(2.5, 2.5),
          shape = c(21, 24),
          fill = sphsu_cols("Thistle", "Turquoise", names = FALSE)
        ),
        title.position = "top",
        title.hjust = 0.5,
        label.theme = element_text(size = 10),
        order = 1
      )
    ) +
    theme_sphsu_light() +
    theme(
      legend.position = "bottom",
      # legend.title.align = "top",
      legend.box = "vertical",
      legend.spacing.y = unit(0.1, "cm"),
      legend.spacing.x = unit(0.1, "cm"),
      legend.key.height = unit(0.05, "cm"),
      legend.margin = margin(l = 5, unit = "cm"),
      # legend.background = element_rect(fill = "grey"),
      strip.background = element_rect(fill = "white", size = 1),
      panel.background = element_rect(
        fill = "white",
        size = 1,
        colour = "grey"
      ),
      strip.text = element_text(
        face = "bold",
        hjust = 0,
        margin = margin(5, 0, 5, 0)
      )
    ) +
    labs(title = "Natsal surveys") +
    scale_alpha_manual(
      name = NULL,
      # name = "(2020 points omitted)",
      breaks = c("Men", "(2020 data points, not included in trend analysis)"),
      values = c(1, 1),
      guide = guide_legend(
        override.aes = list(
          shape = c(21, 24),
          title.position = "top",
          alpha = c(1, 1),
          linetype = c(NA, NA),
          line = c(NA, NA),
          size = c(2.5, 2.5),
          color = sphsu_cols("Thistle", "Turquoise", names = FALSE)
        ),
        title.position = "top",
        title.hjust = 0.5,
        title.theme = element_text(face = "italic", size = 10),
        label.theme = element_text(colour = "white", size = 10),
        order = 2
      )
    ) +
    # guides(line = guide_legend(order = 1),
    #        alpha = guide_legend(order = 2)) +
    facet_wrap(~ outcome, ncol = 1, scales = "free_y")
  )

(surv_graphs + natsal_graphs) / guide_area()  + plot_layout(guides = "collect", heights = c(10, 1))

# Forlorn attempts to rearrange within grobs
# g <- ggplotGrob(last_plot())
# 
# grid.ls(grid.force(g))
# 
# p <- editGrob(grid.force(g), "GRID.text.17364", grep = TRUE, gp = gpar(col = "black"))
# 
# grid.newpage()
# grid.draw(p)
# (surv_graphs + p) / guide_area()  + plot_layout(guides = "collect", heights = c(10, 1))

ggsave("suveillance_comparison_2.png", height = 30, width = 24, units = "cm", dpi = 400)
ggsave("suveillance_comparison_2.svg", height = 30, width = 24, units = "cm", dpi = 400)

  # testing significance - move to rmd? -------------------------------------


surv_data_tidy %>%
  mutate(covid = if_else(year == 2020, 1, 0)) %>% 
  group_by(gender, outcome) %>%
  filter(!is.na(rate)) %>% 
  nest() %>%
  filter(!(gender == "Men" &
             outcome %in% c("Conceptions", "Abortions"))) %>%
  mutate(coefs = map(
    data,
    .f = function(data) {
      lm(rate ~ time + covid, data = data) %>%
        broom::tidy(conf.int = TRUE)
    }
  )) %>%
  unnest(coefs) %>%
  mutate(minyear = map_dbl(data, ~min(.x$year))) %>% 
  filter(term !="(Intercept)") %>%
  select(estimate, term, conf.low, conf.high, minyear) %>%
  ungroup() %>%
  pivot_wider(names_from = term, values_from = c(estimate, conf.low, conf.high),
              names_glue = "{term}_{.value}") %>% 
  arrange(outcome, gender) %T>% 
  {
    cat(
      glue::glue(
        "From {.$minyear} to 2019, {.$gender} saw a yearly change in {.$outcome} of {signif(.$time_estimate, 3)} per 100 {.$gender} ({signif(.$time_conf.low, 3)}, {signif(.$time_conf.high, 3)})"
      ),
      sep = "\n"
    )
  } %>% 
  filter(!is.na(covid_estimate)) %>% 
  {
    cat(
      glue::glue(
        "In 2020, {.$gender} saw a change in {.$outcome} of {signif(.$covid_estimate, 3)} per 100 {.$gender} ({signif(.$covid_conf.low, 3)}, {signif(.$covid_conf.high, 3)})"
      ),
      sep = "\n"
    )
  }


# only public data --------------------------------------------------------



surv_data_tidy %>%
  filter(outcome %in% c("Conceptions", "Abortions")) %>%
  # mutate(outcome = fct_drop(outcome)) %>%
  # filter(year < 2020) %>%
  ggplot(aes(
    time,
    rate,
    colour = gender,
    shape = gender,
    fill = interaction(gender, covid)
  )) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point(
    data = limit_sets %>%
      filter(outcome %in% c("Conceptions", "Abortions")) ,
    aes(x = NA_integer_, y = max),
    inherit.aes = FALSE,
    alpha = 0
  ) +
  scale_y_continuous(
    "Rate per 100",
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0)),
    labels = scales::label_number(accuracy = 1)
  ) +
  scale_x_continuous(
    "Year",
    limits = c(1, 11),
    breaks = seq(1, 11, 2),
    labels = seq(2010, 2020, 2)
  ) +
  scale_colour_manual(name = "Gender",
                      values = c(
                        "Men" = sphsu_cols("Thistle", names = FALSE),
                        "Women" = sphsu_cols("Turquoise", names = FALSE)
                      )) +
  # scale_shape_discrete(name = "Gender") +
  labs(title = "Surveillance data") +
  scale_shape_manual("Gender", values = c("Men" = 21, "Women" = 24)) +
  theme_sphsu_light() +
  scale_fill_manual(values = c(
    "Men.1" = 0,
    "Women.1" = 0,
    "Men.0" = sphsu_cols("Thistle", names = FALSE),
    "Women.0" = sphsu_cols("Turquoise", names = FALSE)
  )) +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "white", size = 1),
    panel.background = element_rect(
      fill = "white",
      size = 1,
      colour = "grey"
    ),
    strip.text = element_text(
      face = "bold",
      hjust = 0,
      margin = margin(5, 0, 5, 0)
    )
  ) +
  facet_wrap( ~ outcome, ncol = 1, scales = "free_y")
