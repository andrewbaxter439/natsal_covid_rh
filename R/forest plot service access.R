library(tidyverse)
source(file.path(old_wd, "R/import and convert.R"))
source(file.path(old_wd, "R/functions.R"))
library(survey)
library(egg)

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


serv_acc_data <- wave2_data %>%
  filter(as.numeric(D_ConServAcc_w2) != 1) %>% 
  filter(!is.na(D_ConServAcc_w2)) %>%
  mutate(across(where(is.factor), .fns = ~fct_drop(.x))) %>% 
  select(
    D_ConServAcc_w2,
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
    serv_acc = if_else(D_ConServAcc_w2 == "Accessed services successfully", 1L, 0L),
    serv_barr = if_else(D_ConServAcc_w2 == "Unable to access contraceptive services", 1, 0),
  D_Edu3Cat_w2 = fct_rev(D_Edu3Cat_w2),
  SDSdrinkchangeW2_w2 = fct_rev(SDSdrinkchangeW2_w2)) %>% 
  select(-D_ConServAcc_w2) 
  


serv_acc_ors <- serv_acc_data %>%
  select(-serv_barr) %>%
  pivot_longer(-c(serv_acc, weight2, D_Age5Cat_w2),
               names_to = "Comparison",
               values_to = "Cat") %>%
  nest(-Comparison) %>%
  mutate(mod_serv_acc = map(data, ~ return_ORs(.x, serv_acc ~ Cat + D_Age5Cat_w2, weight2))) %>% 
  unnest(mod_serv_acc) %>% 
  mutate(
    ll = exp(ll),
    ul = exp(ul),
    OR = if_else(est == 1, 1, exp(est)),
    aOR = if_else(est == 1, "1", aOR),
    CI = if_else(est == 1, "(ref)", CI)
  ) %>%
  select(Comparison, Cat, OR, ll, ul, CI, aOR, P)



serv_acc_disp <- comp_labels %>%
  filter(Comparison != "CondomAcc_w2", Comparison != "D_ConServAcc_w2") %>% 
  transmute(Cat = label, Comparison) %>% 
  bind_rows(serv_acc_ors) %>% 
  mutate(Comparison = fct_inorder(Comparison)) %>% 
  # arrange(Comparison) %>% 
  mutate(across(where(is.character), ~if_else(is.na(OR)&is.na(.x), " ", .x)),
         show = if_else(is.na(OR), 0, 1),
         empty = if_else(aOR == "0", NA_character_, "black"),
         across(where(is.numeric), ~if_else(is.na(OR)&is.na(.x), 1, .x)),
         Cat = fct_inorder(Cat))

serv_barr_ors <- serv_acc_data %>% 
  select(-serv_acc) %>% 
  pivot_longer(-c(serv_barr, weight2), names_to = "Comparison", values_to = "Cat") %>% 
  nest(-Comparison) %>% 
  mutate(mod_serv_barr = map(data, ~ return_ORs(.x, serv_barr ~ Cat, weight2))) %>% 
  unnest(mod_serv_barr) %>% 
  mutate(ll = if_else(ll<(-10), 1, exp(ll)),
         ul = if_else(ul<(-10), 1, exp(ul)),
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
         # aOR = if_else(est == 1, "1", aOR),
         # CI = if_else(est == 1, "(ref)", CI)
         ) %>% 
  select(Comparison, Cat, OR, ll, ul, CI, aOR, P) 



serv_barr_disp <- comp_labels %>%
  filter(Comparison != "CondomAcc_w2", Comparison != "D_ConServAcc_w2") %>% 
  transmute(Cat = label, Comparison) %>% 
  bind_rows(serv_barr_ors) %>% 
  mutate(Comparison = fct_inorder(Comparison)) %>% 
  # arrange(Comparison) %>% 
  mutate(across(where(is.character), ~if_else(is.na(OR)&is.na(.x), " ", .x)),
         empty = if_else(aOR == "0", NA_character_, "black"),
         show = if_else(is.na(OR), 0, 1),
         across(where(is.numeric), ~if_else(is.na(OR)&is.na(.x), 1, .x)),
         Cat = fct_inorder(Cat))


# this is tricky - doing by gplot -----------------------------------------
# data = serv_barr_disp

draw_forest_table <-  function() {
    forest1 <- serv_acc_disp %>%
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
      ggtitle(paste0("\n", "Successful use of Contraceptive Services"))
    

forest2 <- serv_barr_disp %>%
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
      scale_x_continuous(expand = expansion(add = c(0, 0)), limits = c(0, 2))
    
    
    lab1 <- base_plot +
      geom_text(aes(label = Cat),
                fontface = if_else(data$show == 0, "bold", "plain"),
                hjust = 0,
                size = pts(9)) +
      ggtitle(" \n ") 
      # xlim(0, 2) +
    
    
    lab2 <- base_plot +
      geom_text(aes(label = paste(aOR, CI)),
                hjust = 0,
                size = pts(9)) +
      ggtitle("\nOR (CI)")
    
    
    lab3 <- base_plot +
      geom_text(data = serv_barr_disp, aes(label = paste(aOR, CI)),
                hjust = 0,
                size = pts(9)) +
      ggtitle("\nOR (CI)")
    # 
    # 
    # lab1_grob <- ggplotGrob(lab1)
    # for (i in 1:length(lab1_grob$layout$clip)) {
    #   lab1_grob$layout$clip <- "off"
    # }
    
    grid.arrange(lab1, forest1, lab2, forest2, lab3, layout_matrix = 
                   matrix(c(
                     1,1,1,1,1,1,1,
                     2,2,2,2,2,2,
                     3,3,3,
                     4,4,4,4,4,4,
                     5,5,5
                   ), nrow = 1))
}

draw_forest_table() 


#%>% 
ggsave(file.path(old_wd, "graphs/test2.svg"), plot = ., height = 230, width = 330, units = "mm")
    
    
  #}

# access_gr <- draw_forest_table(serv_acc_disp)
# barr_gr <- draw_forest_table(serv_barr_disp, plot_title = "Barriers accessing Contraceptive Services")



# grid.arrange(access_gr, barr_gr, left = -10, layout_matrix = matrix(c(1,2), nrow = 1)) 

#%>% 
