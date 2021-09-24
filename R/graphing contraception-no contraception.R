source("R/import and convert.R")
library(SPHSUgraphs)

wave2_data %>% 
  ggplot(aes(D_ConSL_w2)) +
  geom_bar()


wave2_data %>%
  select(qethnicity, D_ConSL_w2, D_MostEffective_w2) %>%
  mutate(
    D_ConSL_w2 = fct_relevel(
      D_ConSL_w2,
      c("Not applicable", "No method used", "Some method used")
    ),
    Con_noCon = factor(
      case_when(
        is.na(D_MostEffective_w2) ~ as.character(D_ConSL_w2),
        TRUE ~ as.character(D_MostEffective_w2)
      ),
      levels = lvls_union(list(D_ConSL_w2, D_MostEffective_w2))
    ),
    used = fct_other(
      Con_noCon,
      keep = c("Not applicable", "No method used"),
      other_level = "Some method used"
    )
  ) %>%
  group_by(qethnicity, used, Con_noCon) %>%
  summarise(n = n()) %>%
  mutate(n_used = sum(n)) %>%
  ungroup(used) %>%
  mutate(
    prop = n / sum(n),
    prop_used = n_used/sum(unique(n_used)),
    line = if_else(
      Con_noCon %in% c("Not applicable", "No method used"),
      cumsum(prop),
      NA_real_
    ),
    text_lab = if_else(
      Con_noCon %in% c("Not applicable", "No method used", "More effective method") & qethnicity == "35-44",
      as.character(used),
      NA_character_
    ),
    text_y = prop_used/2 + lag(line, default = 0)
  ) %>% 
  ggplot(aes(
    qethnicity,
    y = prop,
    fill = fct_rev(Con_noCon),
    xmin = qethnicity
  )) +
  geom_col(position = "fill") +
  scale_fill_sphsu(name = "Contraception used", reverse = TRUE) +
  scale_y_continuous("Proportion", labels = scales::percent) +
  xlab("Age group") +
  geom_segment(
    aes(
      y = line,
      yend = line,
      x = after_stat(xmin) - 0.45,
      xend = after_stat(xmin) + 0.45
    ),
    colour = "white",
    size = 1.5
  ) +
  geom_text(x = 4.6, aes(y = text_y, label = text_lab), hjust = 0) +
  coord_cartesian(clip = "off", xlim = c(NA, 5.5)) +
  theme(panel.background = element_blank())
  
max(factor(wave2_data$qethnicity, ordered = TRUE))


# writing function --------------------------------------------------------



contr_type <- function(strat = D_Age5Cat_w2, xlab = "Age group") {
  
  strat <- enquo(strat)
  
  dat <- wave2_data %>%
    filter(!is.na(!!strat)) %>% 
    mutate(comp_var = factor(fct_drop(!!strat), ordered = TRUE))
    
  
  right_val <- as.character(max(factor(dat %>% pull(comp_var), ordered = TRUE)))
  
  dat %>% 
    select(comp_var, D_ConSL_w2, D_MostEffective_w2) %>%
    mutate(
      D_ConSL_w2 = fct_relevel(
        D_ConSL_w2,
        c("Not applicable", "No method used", "Some method used")
      ),
      Con_noCon = factor(
        case_when(
          is.na(D_MostEffective_w2) ~ as.character(D_ConSL_w2),
          TRUE ~ as.character(D_MostEffective_w2)
        ),
        levels = lvls_union(list(D_ConSL_w2, D_MostEffective_w2))
      ),
      used = fct_other(
        Con_noCon,
        keep = c("Not applicable", "No method used"),
        other_level = "Some method used"
      )
    ) %>%
    group_by(comp_var, used, Con_noCon) %>%
    summarise(n = n()) %>%
    mutate(n_used = sum(n)) %>%
    ungroup(used) %>%
    mutate(
      prop = n / sum(n),
      prop_used = n_used/sum(unique(n_used)),
      line = if_else(
        Con_noCon %in% c("Not applicable", "No method used"),
        cumsum(prop),
        NA_real_
      ),
      text_lab = if_else(
        Con_noCon %in% c("Not applicable", "No method used", "More effective method") &
          comp_var == right_val,
        as.character(used),
        NA_character_
      ), 
      text_y = prop_used/2 + lag(line, default = 0)
    )  %>%
    ggplot(aes(
      comp_var,
      y = prop,
      fill = fct_rev(Con_noCon),
      xmin = comp_var
    )) +
    geom_col(position = "fill") +
    scale_fill_sphsu(name = "Contraception used", reverse = TRUE) +
    scale_y_continuous("Proportion", labels = scales::percent) +
    scale_x_discrete(labels = ~ str_wrap(.x, 15)) +
    xlab(xlab)  +
    geom_segment(
      aes(
        y = line,
        yend = line,
        x = after_stat(xmin) - 0.45,
        xend = after_stat(xmin) + 0.45
      ),
      colour = "white",
      size = 1.5
    ) +
    geom_text(x = length(levels(pull(dat, comp_var))) + 0.6, aes(y = text_y, label = text_lab), hjust = 0) +
    coord_cartesian(clip = "off", xlim = c(NA, length(levels(pull(dat, comp_var))) + 1.5)) +
    theme(panel.background = element_blank())

}


p_age <- contr_type(D_Age5Cat_w2, "Age groups")
p_ethn <- contr_type(D_EthnicityCombined_w2, "Ethnicity")
p_qsg <- contr_type(qsg, "Social grade")

library(patchwork)

p_age + p_ethn + p_qsg + guide_area() + patchwork::plot_layout(guides = "collect")
