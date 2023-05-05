library(tidymodels)
library(embed)

receita =
  recipe(Species ~ ., data = iris) %>%
  step_normalize(all_numeric_predictors()) %>%
  # step_pca(all_numeric_predictors(), num_comp = 2, keep_original_cols = TRUE) %>%
  # embed::step_umap(all_numeric_predictors(), num_comp = 2, keep_original_cols = TRUE) %>%
  identity()

summary(receita)

receita2 = prep(receita)

pronta =
  receita2 %>%
  bake(iris)

modelo =
  parsnip::rand_forest(mtry = 5, trees = 300, min_n = 2) %>%
  set_mode('classification')

modelo_fit =
  modelo %>%
  fit(Species ~ ., data = pronta)

modelo_fit %>%
  extract_fit_engine() %>%
  summary()

bindado =
  predict(modelo_fit, new_data = pronta %>% select(-Species)) %>%
  bind_cols(pronta)

metricas <- metric_set(accuracy, mcc, f_meas)
metricas(bindado, truth = Species, estimate = .pred_class)
