source("R/import and convert.R")
source("R/functions.R")
library(SPHSUgraphs)
library(patchwork)


# testing - superceded ----------------------------------------------------
# 
# wave2_data %>%
#   select(qethnicity, D_ConSL_w2, D_MostEffective_w2) %>%
#   mutate(
#     D_ConSL_w2 = fct_relevel(
#       D_ConSL_w2,
#       c("Not applicable", "No method used", "Some method used")
#     ),
#     Con_noCon = factor(
#       case_when(
#         is.na(D_MostEffective_w2) ~ as.character(D_ConSL_w2),
#         TRUE ~ as.character(D_MostEffective_w2)
#       ),
#       levels = lvls_union(list(D_ConSL_w2, D_MostEffective_w2))
#     ),
#     used = fct_other(
#       Con_noCon,
#       keep = c("Not applicable", "No method used"),
#       other_level = "Some method used"
#     )
#   ) %>%
#   group_by(qethnicity, used, Con_noCon) %>%
#   summarise(n = n()) %>%
#   mutate(n_used = sum(n)) %>%
#   ungroup(used) %>%
#   mutate(
#     prop = n / sum(n),
#     prop_used = n_used/sum(unique(n_used)),
#     line = if_else(
#       Con_noCon %in% c("Not applicable", "No method used"),
#       cumsum(prop),
#       NA_real_
#     ),
#     text_lab = if_else(
#       Con_noCon %in% c("Not applicable", "No method used", "More effective method") & qethnicity == "35-44",
#       as.character(used),
#       NA_character_
#     ),
#     text_y = prop_used/2 + lag(line, default = 0)
#   ) %>%
#   ggplot(aes(
#     qethnicity,
#     y = prop,
#     fill = fct_rev(Con_noCon),
#     xmin = qethnicity
#   )) +
#   geom_col(position = "fill") +
#   scale_fill_sphsu(name = "Contraception used", reverse = TRUE) +
#   scale_y_continuous("Proportion", labels = scales::percent) +
#   xlab("Age group") +
#   geom_segment(
#     aes(
#       y = line,
#       yend = line,
#       x = after_stat(xmin) - 0.45,
#       xend = after_stat(xmin) + 0.45
#     ),
#     colour = "white",
#     size = 1.5
#   ) +
#   geom_text(x = 4.6, aes(y = text_y, label = text_lab), hjust = 0) +
#   coord_cartesian(clip = "off", xlim = c(NA, 5.5)) +
#   theme(panel.background = element_blank())



# writing function --------------------------------------------------------



contr_type <- function(strat = D_Age5Cat_w2, xlab = "Age group", include_na = TRUE) {

  strat <- enquo(strat)

  dat <- wave2_data %>%
    filter(!is.na(!!strat), !is.na(D_ConSL_w2)) %>%
    mutate(comp_var = factor(fct_drop(!!strat), ordered = TRUE))

  # dat <- wave2_data %>%
  #   filter(!is.na(D_EthnicityCombined_w2), !is.na(D_ConSL_w2)) %>%
  #   mutate(comp_var = factor(fct_drop(D_EthnicityCombined_w2), ordered = TRUE))
    
  
  right_val <- max(dat %>% pull(comp_var))
  
  td_data <- dat %>% 
    select(comp_var, D_ConSL_w2, D_MostEffective_w2) %>%
    mutate(
      D_ConSL_w2 = fct_relevel(
        fct_collapse(D_ConSL_w2,
                     "Not applicable" = c("Not applicable - no pregnancy risk", "Not applicable - trying to get/already pregnant")),
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
    )  
  
  td_data %>%
    ggplot(aes(
      comp_var,
      y = prop,
      fill = fct_rev(Con_noCon),
      xmin = comp_var
    )) +
    geom_col(position = "fill") +
    scale_fill_sphsu(name = "Contraception used", reverse = TRUE,
                     breaks = c("Not applicable", "No method used", "Some method used", "More effective method", 
                                "Condoms", "Less effective method", "Emergency method")) +
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
p_edu <- contr_type(D_Edu3Cat_w2, "Education")


p_age + p_ethn + p_qsg + p_edu + plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("graphs/conSL.png", height = 2000, width = 4000, units = "px", dpi = 300)
