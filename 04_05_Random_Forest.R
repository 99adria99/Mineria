# Cargamos los paquetes necesarios.
list.of.packages = c("dplyr","tidyr","ggplot2","ggpubr","tidymodels","ranger","doParallel") 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) {
  install.packages(new.packages)
}
lapply(list.of.packages, require, character.only = T)
rm(list.of.packages, new.packages)

# Cargamos la base de datos.
load(paste0(pathData,"dataDEFINITIVA.RData"))

set.seed(1234)
train    <- sample(1:nrow(dd), size = nrow(dd)/5)
dd_train <- dd[train,]
dd_test  <- dd[-train,]

# Definimos el modelo 
# ==============================================================================
modelo <- rand_forest(
  mode  = "classification",
  mtry  = tune(),
  trees = tune()
) %>%
  set_engine(
    engine     = "ranger",
    max.depth  = tune(),
    importance = "none",
    seed       = 1234
  )

transformer <- recipe(
  formula = y ~ .,
  data    =  dd_train
)

# Definimos las particiones y condiciones de partición
# ==============================================================================
set.seed(1234)
cv_folds <- vfold_cv(
  data    = dd_train,
  v       = 5,
  strata  = y
)

workflow_modelado <- workflow() %>%
  add_recipe(transformer) %>%
  add_model(modelo)

# Haremos dos pruebas
hiperpar_grid <- expand_grid(
  'trees'     = c(50, 100),
  'mtry'      = c(3, 5),
  'max.depth' = c(1, 3)
)

# Buscamos los valores óptimos
cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)

grid_fit <- tune_grid(
  object    = workflow_modelado,
  resamples = cv_folds,
  metrics   = metric_set(accuracy),
  grid      = hiperpar_grid
)

stopCluster(cl)


# Nos quedamos con el mejor
mejores_hiperpar <- select_best(grid_fit, metric = "accuracy")

# Con todo el proceso hecho vemos los resultados de la precision del modelo, sus predicciones, 
# su matriz de confusion y la importáncia de las variables al momento de predecir.


# Creamos el modelo final
# ==============================================================================

modelo_final_fit <- finalize_workflow(
  x = workflow_modelado,
  parameters = mejores_hiperpar
) %>%
  fit(
    data = dd_train
  ) %>%
  extract_fit_parsnip()

### Accuracy

predicciones <- modelo_final_fit %>%
  predict(new_data = dd_test)

predicciones <- predicciones %>% 
  bind_cols(dd_test %>% dplyr::select(y))


accuracy_test  <- accuracy(
  data     = predicciones,
  truth    = y,
  estimate = .pred_class,
  na_rm    = TRUE
)
accuracy_test

### Matriz de confusión


# Matriz de confusion
mat_confusion <- predicciones %>%
  conf_mat(
    truth     = y,
    estimate  = .pred_class
  )
mat_confusion

### Predicción


# Predicción de probabilidades
# ==============================================================================
predicciones <- modelo_final_fit %>%
  predict(new_data = dd_test, type = "prob")
head(predicciones, 4)

### Importancia por pureza de nodos


# Entrenamiento modelo
modelo <- rand_forest(
  mode  = "classification"
) %>%
  set_engine(
    engine     = "ranger",
    importance = "impurity",
    seed       = 123
  )

modelo <- modelo %>% finalize_model(mejores_hiperpar)
modelo <- modelo %>% fit(y ~., data = dd_train)

# Importancia
importancia_pred <- modelo$fit$variable.importance %>%
  enframe(name = "predictor", value = "importancia")

# Gráfico
ggplot(
  data = importancia_pred,
  aes(x    = reorder(predictor, importancia),
      y    = importancia,
      fill = importancia)
) +
  labs(x = "predictor", title = "Importancia predictores (pureza de nodos)") +
  geom_col() +
  scale_fill_viridis_c() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")

### Importancia por permutacion


# Entrenamiento modelo
modelo <- rand_forest(
  mode  = "classification"
) %>%
  set_engine(
    engine     = "ranger",
    importance = "permutation",
    seed       = 123
  )

modelo <- modelo %>% finalize_model(mejores_hiperpar)
modelo <- modelo %>% fit(y ~., data = dd_train)

# Importancia
importancia_pred <- modelo$fit$variable.importance %>%
  enframe(name = "predictor", value = "importancia")

# Gráfico
ggplot(
  data = importancia_pred,
  aes(x    = reorder(predictor, importancia),
      y    = importancia,
      fill = importancia)
) +
  labs(x = "predictor", title = "Importancia predictores (permutación)") +
  geom_col() +
  scale_fill_viridis_c() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")