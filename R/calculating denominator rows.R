wave2_data %>% 
  group_by(D_Age5Cat_w2) %>% 
  summarise(N = n(),
            `Weighted N` = sum(weight2)) %>% 
  janitor::adorn_totals() %>% 
  mutate(X = "Denominators (unweighted,weighted)",
         across(where(is.numeric), round),
         dnoms =   paste(N, `Weighted N`, sep=",")) %>% 
  pivot_wider(X, names_from = D_Age5Cat_w2, values_from = dnoms) %>% 
  format_tsv() %>%  cat()


wave2_data %>% 
  group_by(qsg) %>% 
  summarise(N = n(),
            `Weighted N` = sum(weight2)) %>% 
  janitor::adorn_totals() %>% 
  mutate(X = "Denominators (unweighted,weighted)",
         across(where(is.numeric), round),
         dnoms =   paste(N, `Weighted N`, sep=",")) %>% 
  pivot_wider(X, names_from = qsg, values_from = dnoms) %>% 
  format_tsv() %>%  cat()
