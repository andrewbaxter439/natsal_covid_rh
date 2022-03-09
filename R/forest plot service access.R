library(tidyverse)
source("R/import and convert.R")
source("R/functions.R")
library(survey)
# library(survey)

pts <- function (size) return(size * 5/14)

comp_labels <- tibble(
  Comparison = c(
    "D_Age5Cat_w2",
    "qsg",
    "D_EthnicityCombined_w2",
    "D_Edu3Cat_w2",
    "D_SexIDL_w2",
    "EconActChg4_w2",
    "EconActChg5_w2",
    "D_drinkGrp_w2",
    "SDSdrinkchangeW2_w2",
    "Smokenow_w2",
    "D_relstatcatv7_w2",
    "D_PHQ2Cat_w2",
    "D_GAD2Cat_w2",
    "D_ConServAcc_w2",
    "CondomAcc_w2")
) %>%
  mutate(label = map_chr(Comparison, function(exp_var) {
    wave2_data %>%
      pull(exp_var) %>%
      attr("label")
  }))


# New variables - ServAcc2_w2/ServTry4_w2

serv_acc_data <- wave2_data %>%
  # filter(as.numeric(D_ConServAcc_w2) != 1) %>% 
  # filter(!is.na(D_ConServAcc_w2)) %>%
  mutate(across(where(is.factor), .fns = ~fct_drop(.x))) %>% 
  select(
    D_ConServAcc_w2,
    ServAcc2_w2,
    ServTry4_w2,
    D_Age5Cat_w2,
    D_EthnicityCombined_w2,
    D_SexIDL_w2,
    qsg,
    D_Edu3Cat_w2,
    D_relstatcatv7_w2,
    EconActChg4_w2,
    EconActChg5_w2,
    D_drinkGrp_w2,
    SDSdrinkchangeW2_w2,
    Smokenow_w2,
    D_PHQ2Cat_w2,
    D_GAD2Cat_w2,
    weight2
  ) %>% 
  mutate(
    serv_acc = ServAcc2_w2,
    serv_barr = ServTry4_w2,
  D_Edu3Cat_w2 = fct_rev(D_Edu3Cat_w2),
  SDSdrinkchangeW2_w2 = fct_rev(SDSdrinkchangeW2_w2),
  .keep = "unused") %>% 
  select(-D_ConServAcc_w2) 
  



serv_acc_ors <-
  serv_acc_data %>%
  select(-serv_barr) %>%
  filter(!is.na(serv_acc)) %>% 
  pivot_longer(-c(serv_acc, weight2, D_Age5Cat_w2),
               names_to = "Comparison",
               values_to = "Cat") %>%
  nest(-Comparison) %>%
  mutate(
    mod_serv_acc = map(data, ~ return_svy_ORs(.x, serv_acc ~ Cat + D_Age5Cat_w2, weight2)),
    sums = map(
      data,
      ~ filter(.x,!is.na(Cat)) %>% group_by(Cat) %>% summarise(wt = round(sum(weight2), 0), n = n(), prop = sum(weight2[serv_acc == "Yes"], na.rm = TRUE)/wt)
    )
  ) %>%
  rowwise() %>%
  mutate(mod_serv_acc = list(left_join(mod_serv_acc, sums, by = "Cat"))) %>%
  # unnest(sums)
  unnest(mod_serv_acc) %>%
  bind_rows(
    serv_acc_data %>%
      mutate(Cat = fct_rev(D_Age5Cat_w2)) %>%
      return_svy_ORs(serv_acc ~ Cat, weight2) %>%
      left_join(
        group_by(serv_acc_data, D_Age5Cat_w2) %>% summarise(wt = round(sum(weight2), 0), n = n(), prop = sum(weight2[serv_acc == "Yes"], na.rm = TRUE)/wt) %>% mutate(Cat = D_Age5Cat_w2)
      ) %>%
      mutate(Comparison = "D_Age5Cat_w2") %>%
      arrange(Cat),
    .
  ) %>%
  mutate(
    ll = exp(ll),
    ul = exp(ul),
    OR = if_else(est == 1, 1, exp(est)),
    aOR = if_else(est == 1, "1", aOR),
    CI = if_else(est == 1, "(ref)", CI),
    denom = paste0(wt, ", ", n)
  ) %>%
  select(Comparison, Cat, OR, ll, ul, CI, aOR, P, prop, wt, n, denom)  
  # mutate(P = if_else(Cat == "glob_p" & Comparison == "D_Age5Cat_w2", "<0.001", P))



serv_acc_disp <-
  comp_labels %>%
  filter(Comparison != "CondomAcc_w2", Comparison != "D_ConServAcc_w2") %>% 
  transmute(Cat = label, Comparison) %>% 
  left_join(serv_acc_ors %>% filter(Cat == "glob_p") %>% select(Comparison, P), by = "Comparison") %>% 
  bind_rows(serv_acc_ors %>% select(-wt, -n) %>% filter(Cat != "glob_p")) %>% 
  mutate(Comparison = fct_inorder(Comparison)) %>% 
  # arrange(Cat) %>% 
  # arrange(Comparison) %>% 
  # To do: Make CI for glob_p, clear all other parts, make glob_p base in factor
  mutate(
    # OR = ifelse(Cat == "glob_p", 0, OR),
    # Cat = if_else(Cat == "glob_p", " ", Cat),
    across(where(is.character), ~if_else(is.na(OR)&is.na(.x), " ", .x)),
         show = if_else(is.na(OR), 0, 1),
         empty = if_else(aOR == "0", NA_character_, "black"),
         across(where(is.numeric), ~if_else(is.na(OR)&is.na(.x), 1, .x)),
         Cat = fct_inorder(Cat),
    P = case_when(str_detect(P, "^0") ~ paste0("p=", P),
                  str_detect(P, "^<") ~ paste0("p", P),
                  TRUE ~ P),
    aOR = if_else(aOR == " ", P, aOR)
    ) %>% 
  arrange(Comparison, Cat)

serv_barr_ors <- serv_acc_data %>% 
  select(-serv_acc) %>% 
  filter(!is.na(serv_barr)) %>% 
  pivot_longer(-c(serv_barr, weight2, D_Age5Cat_w2), names_to = "Comparison", values_to = "Cat") %>% 
  nest(-Comparison) %>% 
  mutate(
    mod_serv_barr = map(data, ~ return_svy_ORs(.x, serv_barr ~ Cat + D_Age5Cat_w2, weight2)),
    sums = map(
      data,
      ~ filter(.x,!is.na(Cat)) %>% group_by(Cat) %>% summarise(wt = round(sum(weight2), 0), n = n(), prop = sum(weight2[serv_barr == "Yes"])/sum(weight2))
    )
  ) %>%
  rowwise() %>%
  mutate(mod_serv_barr = list(left_join(mod_serv_barr, sums, by = "Cat"))) %>%
  # unnest(sums)
  unnest(mod_serv_barr) %>%
  bind_rows(
    serv_acc_data %>%
      mutate(Cat = fct_rev(D_Age5Cat_w2)) %>%
      return_svy_ORs(serv_barr ~ Cat, weight2) %>%
      left_join(
        group_by(serv_acc_data, D_Age5Cat_w2) %>% summarise(wt = round(sum(weight2), 0), n = n(), prop = sum(weight2[serv_barr == "Yes"], na.rm = TRUE)/wt) %>% mutate(Cat = D_Age5Cat_w2)
      ) %>%
      mutate(Comparison = "D_Age5Cat_w2") %>%
      arrange(Cat),
    .
  ) %>%
  mutate(
    ll = exp(ll),
    ul = exp(ul),
    OR = if_else(est == 1 | est < -10, 1, exp(est)),
    aOR = case_when(
      est == 1 ~ "1",
      aOR == "0.00" ~ "0",
      TRUE ~ aOR
    ),
    CI = case_when(
      est == 1 ~ "(ref)",
      aOR == "0" ~ "(empty set)",
      TRUE ~ CI
    ),
    denom = paste0(wt, ", ", n)
  ) %>%
  select(Comparison, Cat, OR, ll, ul, CI, aOR, P, prop, wt, n, denom) # %>% 
  # mutate(P = if_else(Cat == "glob_p" & Comparison == "D_Age5Cat_w2", "0.055", P))


  # mutate(mod_serv_barr = map(data, ~ return_svy_ORs(.x, serv_barr ~ Cat + D_Age5Cat_w2, weight2))) %>% 
  # unnest(mod_serv_barr) %>% 
  # bind_rows(
  #   serv_acc_data %>%
  #     mutate(Cat = fct_rev(D_Age5Cat_w2)) %>%
  #     return_svy_ORs(serv_barr ~ Cat, weight2) %>%
  #     mutate(Comparison = "D_Age5Cat_w2") %>% 
  #     arrange(Cat),
  #   .
  # ) %>%
  # mutate(ll = if_else(ll<(-10), 1, exp(ll)),
  #        ul = if_else(ul<(-10), 1, exp(ul)),
  #        OR = if_else(est == 1 | est < -10, 1, exp(est)),
  #        aOR = case_when(
  #          est == 1 ~ "1",
  #          aOR == "0.00" ~ "0",
  #          TRUE ~ aOR
  #        ),
  #        CI = case_when(
  #          est == 1 ~ "(ref)",
  #          aOR == "0" ~ "(empty set)",
  #          TRUE ~ CI
  #        ),
  #        # aOR = if_else(est == 1, "1", aOR),
  #        # CI = if_else(est == 1, "(ref)", CI)
  #        ) %>% 
  # select(Comparison, Cat, OR, ll, ul, CI, aOR, P) 



serv_barr_disp <- comp_labels %>%
  filter(Comparison != "CondomAcc_w2", Comparison != "D_ConServAcc_w2") %>% 
  transmute(Cat = label, Comparison) %>% 
  left_join(serv_barr_ors %>% filter(Cat == "glob_p") %>% select(Comparison, P), by = "Comparison") %>% 
  bind_rows(serv_barr_ors %>% select(-wt, -n) %>% filter(Cat != "glob_p")) %>% 
  mutate(Comparison = fct_inorder(Comparison)) %>% 
  # arrange(Comparison) %>% 
  mutate(across(where(is.character), ~if_else(is.na(OR)&is.na(.x), " ", .x)),
         empty = if_else(aOR == "0", NA_character_, "black"),
         show = if_else(is.na(OR), 0, 1),
         across(where(is.numeric), ~if_else(is.na(OR)&is.na(.x), 1, .x)),
         Cat = fct_inorder(Cat),
         P = case_when(str_detect(P, "^0") ~ paste0("p=", P),
                       str_detect(P, "^<") ~ paste0("p", P),
                       TRUE ~ P),
         aOR = if_else(aOR == " ", P, aOR)
         )

draw_forest_table() 

draw_forest_table() %>%
ggsave("graphs/forest_plot.svg", plot = ., height = 230, width = 330, units = "mm")

# this is tricky - doing by gplot -----------------------------------------
# data = serv_barr_disp

draw_forest_table <-  function() {
  
  require(patchwork)
  require(ggplot2)
  require(dplyr)
  
    forest1 <- serv_acc_disp %>%
      filter(Comparison != "Total") %>%
      ggplot(aes(OR, fct_rev(Cat), alpha = show)) +
      geom_vline(xintercept = 1,
                 colour = "darkgrey",
                 size = 1) +
      geom_point(aes(fill = empty), shape = 23, size = 2) +
      # geom_segment(aes(x = 1, xend = 1, y = 0, yend = fct_rev(Cat)), colour = "darkgrey", inherit.aes = FALSE) +
      scale_fill_identity() +
      geom_linerange(aes(xmin = ll, xmax = ul)) +
      facet_grid(Comparison ~ .,
                 scales = "free",
                 space = "free_y",
                 switch = "y") +
        scale_x_log10("Odds ratio") +
  scale_alpha_identity() +
      theme(
        strip.text = element_blank(),
        # panel.spacing.y = unit(1, "cm"),
        plot.margin = margin(l = 0, r = 0),
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        strip.background.y = element_blank(),
        plot.title = element_text(size = 10),
        strip.placement = "outside"
      ) +
      ggtitle(paste0("\n", "Successful use of Contraceptive Services"))
    

forest2 <- serv_barr_disp %>%
      filter(Comparison != "Total") %>%
      # filter(Coefficient == coefficient) %>%
      ggplot(aes(OR, fct_rev(Cat), alpha = show)) +
      geom_vline(xintercept = 1,
                 colour = "darkgrey",
                 size = 1) +
      geom_point(aes(fill = empty), shape = 23, size = 2) +
      scale_fill_identity() +
      geom_linerange(aes(xmin = ll, xmax = ul)) +
      facet_grid(Comparison ~ .,
                 scales = "free",
                 space = "free_y",
                 switch = "y") +
        scale_x_log10("Odds ratio") +
  scale_alpha_identity() +
      theme(
        strip.text = element_blank(),
        # panel.spacing.y = unit(1, "cm"),
        plot.margin = margin(l = 0, r = 0),
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        strip.background.y = element_blank(),
        plot.title = element_text(size = 10),
        strip.placement = "outside"
      ) +
      ggtitle(paste0("\n", "Barriers accessing Contraceptive Services"))
    
    base_plot <- serv_acc_disp %>%
      # filter(Coefficient == coefficient) %>%
      ggplot(aes(x = 0, y = fct_rev(Cat))) +
      ylab(NULL) +
      xlab(" ") +
      facet_grid(Comparison ~ .,
                 scales = "free",
                 space = "free_y",
                 switch = "y") +
      theme(
        strip.text = element_blank(),
        # panel.spacing.y = unit(1, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        plot.margin = margin(l = 0, r = 0),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        # axis.line.x = element_line(colour = "white"),
        axis.text.x = element_text(color = "white", size = 12),
        ## need text to be printed so it stays aligned with figure but white so it's invisible
        legend.position = "none",
        # panel.background = element_rect(colour = "lightblue", fill = "white"),
        # panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 10, hjust = 0)
      ) +
      coord_cartesian(clip = "off") +
      scale_x_continuous(expand = expansion(add = c(0, 0)), limits = c(0, 2), breaks = 0)
    
    
    lab1 <- base_plot +
      geom_text(aes(label = Cat),
                fontface = if_else(serv_acc_disp$show == 0, "bold", "plain"),
                hjust = 0,
                vjust = 0.3,
                size = pts(9)) +
      ggtitle(" \n ")
      # xlim(0, 2) +
    
    
    lab2 <- base_plot +
      geom_text(aes(label = paste(aOR, CI)),
                fontface = if_else(str_detect(serv_acc_disp$aOR, "^p"), "italic", "plain"),
                hjust = 0,
                vjust = 0.3,
                size = pts(9)) +
      ggtitle("\naOR (CI)")
    
    
    lab3 <- base_plot +
      geom_text(data = serv_barr_disp, aes(label = paste(aOR, CI)),
                fontface = if_else(str_detect(serv_barr_disp$aOR, "^p"), "italic", "plain"),
                hjust = 0,
                vjust = 0.3,
                size = pts(9)) +
      ggtitle("\naOR (CI)")
    # 
    # 
    # lab1_grob <- ggplotGrob(lab1)
    # for (i in 1:length(lab1_grob$layout$clip)) {
    #   lab1_grob$layout$clip <- "off"
    # }
    
    # grid.arrange(lab1, forest1, lab2, forest2, lab3, layout_matrix = 
    #                matrix(c(
    #                  1,1,1,1,1,1,1,
    #                  2,2,2,2,2,2,
    #                  3,3,3,
    #                  4,4,4,4,4,4,
    #                  5,5,5
    #                ), nrow = 1))
  layout <- c(
    area(1, l = 2),
    area(1, 1, r = 2),
    area(1, 3),
    area(1, 4),
    area(1, 5)
  )
  
forest1 + lab1 + lab2 + forest2 + lab3 + plot_layout(nrow = 1, widths = c(6, 7, 3, 6, 3), design = layout)

}

draw_forest_table()

ggsave("graphs/test2.svg", plot = ., height = 230, width = 330, units = "mm")
  


# output table? -----------------------------------------------------------


comp_labels <- tibble(
  Comparison = c(
    "D_Age5Cat_w2",
    "qsg",
    "Total",
    "D_EthnicityCombined_w2",
    "D_Edu3Cat_w2",
    "D_SexIDL_w2",
    "EconActChg4_w2",
    "EconActChg5_w2",
    "D_drinkGrp_w2",
    "SDSdrinkchangeW2_w2",
    "Smokenow_w2",
    "D_relstatcatv7_w2",
    "D_PHQ2Cat_w2",
    "D_GAD2Cat_w2",
    "D_ConServAcc_w2",
    "CondomAcc_w2")
) %>%
  mutate(label = map_chr(Comparison, function(exp_var) {
    wave2_data %>%
      pull(exp_var) %>%
      attr("label")
  }))

# ooops - forgot to calculate percs as % successful!
tot_prop <- wave2_data %>% 
  transmute(serv_acc = ServAcc2_w2,
    serv_barr = ServTry4_w2,
    Total = Total, 
    weight2 = weight2) %>% 
  summarise(wt = round(sum(weight2), 0), 
            n = n(),
            `Successfully accessed services` = sum(weight2[serv_acc == "Yes"], na.rm = TRUE)/sum(weight2, na.rm = TRUE),  
            `Barriers accessing services` = sum(weight2[serv_barr == "Yes"], na.rm = TRUE)/sum(weight2, na.rm = TRUE))  %>% 
  pivot_longer(-c(n, wt), names_to = "outcome", values_to = "prop") %>% 
  mutate(denom = paste0(wt, ", ", n),
         aOR = " ", 
         CI = " ",
         Cat = "Total", Comparison = "Total")

serv_acc_ors %>% 
  mutate(outcome = "Successfully accessed services") %>% 
  # select(-P) %>% 
  # filter(Cat != "glob_p") %>% 
  # left_join(serv_acc_ors %>% select(Comparison, Cat, P) %>% )
  bind_rows(mutate(serv_barr_ors, outcome = "Barriers accessing services")) %>% 
  bind_rows(tot_prop, .) %>% 
  group_by(Comparison, outcome) %>% 
  mutate(ul_p = perc_ci(prop, "u", sum(wt, na.rm = TRUE)),
         ll_p = perc_ci(prop, "l", sum(wt, na.rm = TRUE)),
         `aOR (CI)` = paste(aOR, CI),
         `% (CI)` = paste0(sprintf("%.1f", round(100*prop, 1)), " (", sprintf("%.1f", round(100*ll_p, 1)), ", ",
                           sprintf("%.1f", round(100*ul_p, 1)), ")"
                           ),
         P = ifelse(Comparison == "Total"|CI!="(ref)", "-", P[Cat == "glob_p"]),
         # glob_p = P[Cat == "glob_p"],
         Denominator = denom) %>% 
  filter(Cat != "glob_p") %>% 
  select(Comparison, `  ` = Cat, `% (CI)`, `aOR (CI)`, P, Denominator, outcome) %>% 
  pivot_wider(names_from = outcome, values_from = c(`% (CI)`, `aOR (CI)`, P, Denominator),
              names_glue = "{outcome}_{.value}") %>% 
  left_join(comp_labels, by  = "Comparison") %>% 
  ungroup() %>% 
  select(-Comparison) %>% 
  mutate(" " = " ",
         across(.fns = ~ replace_na(.x, "-"))) %>% 
  gt(groupname_col = "label", rowname_col = " ") %>% 
  tab_spanner_delim("_")
