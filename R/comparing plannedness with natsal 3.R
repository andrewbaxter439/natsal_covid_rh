wave2_data |> 
  filter(D_Preg1yr_w2 == "Yes") |> 
  count(D_LMUPCat_w2) |> 
  mutate(perc = n/sum(n),
         planned = D_LMUPCat_w2,
         survey = "Natsal-COVID\n(2021)",
         .keep = "none") |> 
  bind_rows(tibble(
    survey = "Natsal-3\n(2010-12)",
    planned = c("Unplanned", "Ambivalent", "Planned"),
    perc = c(0.162, 0.29, 0.548)
  )) |> 
  mutate(planned = fct_rev(fct_inorder(planned))) |> 
  ggplot(aes(x = 1, y = perc, fill = planned)) +
  geom_bar(stat = "identity", position = "fill", width = 0.5) +
  facet_wrap(~survey) +
  scale_y_continuous("%", labels = scales::percent, limits = c(0,1), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0.5, 1.5)) +
  SPHSUgraphs::scale_fill_sphsu(name = NULL,
                                guide =  guide_legend(byrow = TRUE)) +
  SPHSUgraphs::theme_sphsu_light() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.spacing.y = unit(0.5, "cm"),
        legend.key.size = unit(1, "cm"),
        text = element_text(size = 18),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"))
