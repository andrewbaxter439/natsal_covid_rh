source(file.path(old_wd, "R/import and convert.R"))
library(SPHSUgraphs)
library(patchwork)

wave2_data %>% 
  select(D_MostEffectivePre_w2, D_MostEffective_w2, D_SwitchOrStopCon_w2) %>% 
  filter(D_SwitchOrStopCon_w2 == "Yes", D_MostEffectivePre_w2 != D_MostEffective_w2) %>% 
  pivot_longer(D_MostEffectivePre_w2:D_MostEffective_w2, values_to = "con_used") %>% 
  mutate(period = if_else(name == "D_MostEffectivePre_w2", "Pre-lockdown", "During lockdown")) %>% 
  ggplot(aes(con_used)) +
  geom_bar() +
  facet_wrap(~ period)
  

wave2_data %>% 
  select(D_MostEffectivePre_w2, D_MostEffective_w2, D_SwitchOrStopCon_w2) %>% 
  filter(D_SwitchOrStopCon_w2 == "Yes", D_MostEffectivePre_w2 != D_MostEffective_w2) %>% 
  mutate(change = interaction(D_MostEffectivePre_w2, D_MostEffective_w2, sep = "\nto\n", drop = FALSE)) %>% 
  ggplot(aes(x = change)) +
  geom_bar(na.rm = FALSE)


wave2_data %>% 
  select(D_ConPreUs_w2, D_ConSLUs_w2, D_SwitchOrStopCon_w2) %>% 
  filter(D_SwitchOrStopCon_w2 == "Yes", D_ConPreUs_w2 != D_ConSLUs_w2) %>% 
  mutate(change = interaction(D_ConPreUs_w2, D_ConSLUs_w2, sep = "\nto\n", drop = FALSE)) %>% 
  ggplot(aes(y = fct_infreq(change))) +
  geom_bar() +
  ylab("Change in usual contraceptives")

ggsave("graphs/change_contraceptives.png")
