library(tidyverse)

draw_forest_table <-
  function(coefficient = "Trend", data = all_tidy_df) {
    forest1 <- all_tidy_df %>%
      filter(Coefficient == coefficient) %>%
      ggplot(aes(Estimate, model)) +
      geom_vline(xintercept = 0,
                 colour = "darkgrey",
                 size = 1) +
      geom_point(size = 3, shape = 'diamond') +
      geom_linerange(aes(xmin = `Lower 95% CI`, xmax = `Upper 95% CI`)) +
      theme(
        strip.text = element_blank(),
        panel.spacing.y = unit(1, "cm"),
        plot.margin = margin(l = 0, r = 0),
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        # panel.background = element_rect(colour = "lightblue"),
        strip.background.y = element_blank(),
        plot.title = element_text(size = 10),
        strip.placement = "outside"
      ) +
      facet_grid(facet ~ .,
                 scales = "free",
                 space = "free_y",
                 switch = "y") +
      # scale_x_continuous(limits = c(-4, 6)) +
      ggtitle(" \n ")
    
    base_plot <- all_tidy_df %>%
      filter(Coefficient == coefficient) %>%
      ggplot(aes(x = 0, y = model)) +
      ylab(NULL) +
      xlab(" ") +
      facet_grid(facet ~ .,
                 scales = "free",
                 space = "free_y",
                 switch = "y") +
      theme(
        strip.text = element_blank(),
        panel.spacing.y = unit(1, "cm"),
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
      geom_text(aes(label = Comparators),
                hjust = 0,
                size = pts(9)) +
      # xlim(0, 2) +
      ggtitle("\nComparators") +
      theme(
        strip.text = element_text(colour = "black"),
        strip.background = element_blank(),
        strip.placement = "outside"
      )
    
    
    lab2 <- base_plot +
      geom_text(aes(label = `Age group`),
                hjust = 0,
                size = pts(9)) +
      ggtitle("\nAge group")
    
    lab3 <- base_plot +
      geom_text(aes(x = 0, label = `Pill scare`),
                hjust = 0.5,
                size = pts(9)) +
      ggtitle("Pill\nscare") +
      scale_x_continuous(expand = expansion(add = c(0, 0)), limits = c(-1, 1)) +
      theme(plot.title = element_text(hjust = 0.5))
    
    lab4 <- base_plot +
      geom_text(aes(x = 0, label = `Common shock`),
                hjust = 0.5,
                size = pts(9)) +
      ggtitle("2008\nshock") +
      scale_x_continuous(expand = expansion(add = c(0, 0)), limits = c(-1, 1)) +
      theme(plot.title = element_text(hjust = 0.5))
    
    lab5 <- base_plot +
      geom_text(aes(label = est, x = Inf), hjust = 1, size = pts(9)) +
      ggtitle(glue::glue("{coefficient} change\n(95%CI)")) +
      theme(plot.title = element_text(hjust = 0.5))
    
    lab1_grob <- ggplotGrob(lab1)
    for (i in grep("strip", lab1_grob$grobs)) {
      lab1_grob$grobs[[i]]$layout$clip <- "off"
    }