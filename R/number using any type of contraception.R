wave2_data %>% select(
  ConSinceLD5_w2,
  ConSinceLD6_w2,
  ConSinceLD7_w2,
  ConSinceLD8_w2,
  ConSinceLD9_w2,
  ConSinceLD10_w2,
  ConSinceLD11_w2,
  ConSinceLD12_w2,
  ConSinceLD13_w2,
  ConSinceLD14_w2,
  ConSinceLD15_w2,
  ConSinceLD16_w2,
  ConSinceLD17_w2,
  ConSinceLD18_w2
) %>% 
  rowwise %>% 
  mutate(across(.fns = \(x) as.numeric(x == "Yes"))) %>% 
  transmute(num = sum(c_across(everything()), na.rm = TRUE)) %>% 
  count(num) %>% 
  filter(num > 1) %>% 
  ungroup() %>% 
  summarise(sum(n))
