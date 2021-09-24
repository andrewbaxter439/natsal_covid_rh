library(tidyverse)
library(tidymodels)

source("R/import and convert.R")

w2dat_tidy <- wave2_data %>% 
  mutate(
    imd_quint = factor(
      case_when(
        CombinedIMD %in% 1:2 ~ "1 to 2 (most deprived)",
        CombinedIMD %in% 3:4 ~ "3 to 4",
        CombinedIMD %in% 5:6 ~ "5 to 6",
        CombinedIMD %in% 7:8 ~ "7 to 8",
        CombinedIMD %in% 9:10 ~ "9 to 10 (least deprived)",
        TRUE ~ "Missing"
      )
    ),
    across(where(is.factor), ~ fct_drop(fct_explicit_na(.x, "Missing")))
  )

predict_group <- w2dat_tidy %>% 
  filter(imd_quint == "Missing")

train_test_group <- w2dat_tidy %>% 
  filter(imd_quint != "Missing")

train_test_split <- initial_split(train_test_group, strata = imd_quint)

train_data <- training(train_test_split)
test_data <- testing(train_test_split)

forest_fit_mod <-
  rand_forest(trees = 1000, mode = "classification", min_n = 100) %>%
  set_engine("ranger")

forest_workflow <- workflow() %>%
  add_model(forest_fit_mod) %>%
  add_formula(
    imd_quint ~ qsg + resp_age + resp_gender + qethnicity + qregion + qrelation + BornIn + EMP01_w2 + UK02ETH_w2 + SDSdrink_w2 + Smokenow_w2 + UK02Edu_w2
  )

forest_fit <- forest_workflow %>%
  fit(data = train_data)


forest_fit_pred <-
  predict(forest_fit, train_data) %>%
  bind_cols(train_data %>% select(imd_quint))

forest_fit_pred %>%
  accuracy(truth = imd_quint, .pred_class)

forest_fit_pred %>% select(imd_quint, .pred_class) %>%
  ggplot(aes(imd_quint, .pred_class)) +
  geom_count() +
  scale_size_area()

forest_fit_test_pred <-
  predict(forest_fit, test_data) %>%
  bind_cols(test_data %>% select(imd_quint))

forest_fit_test_pred %>%
  accuracy(truth = imd_quint, .pred_class)

forest_fit_test_pred %>%
  ggplot(aes(imd_quint, .pred_class)) +
  geom_count() +
  scale_size_area()


# with cross-val ----------------------------------------------------------


folds <- vfold_cv(train_data)

forest_10fold <- forest_workflow %>%
  fit_resamples(resamples = folds, metrics = metric_set(accuracy))

collect_metrics(forest_10fold)
