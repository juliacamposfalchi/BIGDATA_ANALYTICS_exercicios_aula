## ---------------------------------------------------------
## AULA 07 - EXERCÍCIOS (Adults)
## 1) Separar treino/teste
## 2) Naive Bayes
## 3) KNN (testar vários k)
## 4) LDA, QDA, RDA
## 5) Comparar desempenho no teste e métricas
## 6) Refazer com validação cruzada k-fold
## ---------------------------------------------------------

## ---------------------------------------------------------
## 0. Pacotes ----
## ---------------------------------------------------------

if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")
if (!requireNamespace("MASS", quietly = TRUE)) install.packages("MASS")
if (!requireNamespace("klaR", quietly = TRUE)) install.packages("klaR")
if (!requireNamespace("class", quietly = TRUE)) install.packages("class")

## AUC/ROC (opcional)
if (!requireNamespace("pROC", quietly = TRUE)) {
  try(install.packages("pROC"), silent = TRUE)
}

library(dplyr)
library(caret)

## ---------------------------------------------------------
## 1. Carregar e preparar dados (Adults) ----
## ---------------------------------------------------------

url_adults <- "https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"

adults <- read.csv(
  url_adults,
  header = FALSE,
  strip.white = TRUE
)

names(adults) <- c(
  "age", "workclass", "fnlwgt", "education", "education.num",
  "marital.status", "occupation", "relationship", "race", "sex",
  "capital.gain", "capital.loss", "hours.per.week", "native.country",
  "income"
)

## Tratar "?" como NA
adults[adults == "?"] <- NA

## Limpar income (às vezes vem com ponto final)
adults$income <- trimws(as.character(adults$income))
adults$income <- gsub("\\.$", "", adults$income)

## Manter só as duas classes
adults <- adults[adults$income %in% c("<=50K", ">50K"), ]

## Definir income como fator (primeiro nível = classe positiva para ROC no caret)
## Aqui colocamos ">50K" como primeira para ser o "evento" no twoClassSummary
adults$income <- factor(adults$income, levels = c(">50K", "<=50K"))

## Variáveis categóricas
cat_vars <- c(
  "workclass", "education", "marital.status", "occupation", "relationship",
  "race", "sex", "native.country"
)

for (v in cat_vars) {
  adults[[v]] <- factor(adults[[v]])
}

## Selecionar um subconjunto de variáveis (evita alta dimensionalidade e categorias raras)
vars_modelo <- c(
  "income",
  "age",
  "education.num",
  "hours.per.week",
  "capital.gain",
  "capital.loss",
  "sex",
  "workclass",
  "marital.status",
  "occupation",
  "relationship"
)

df <- adults[, vars_modelo, drop = FALSE]

df <- na.omit(df)

## ---------------------------------------------------------
## 2. Split treino/teste ----
## ---------------------------------------------------------

set.seed(2025)
idx_train <- caret::createDataPartition(df$income, p = 0.7, list = FALSE)
train_raw <- df[idx_train, , drop = FALSE]
test_raw  <- df[-idx_train, , drop = FALSE]

## ---------------------------------------------------------
## 3. Encoding + padronização (necessário para KNN e ajuda nos demais) ----
## ---------------------------------------------------------

## One-hot encoding para fatores
## (dummyVars cria dummies e mantém numéricas)
dv <- caret::dummyVars(income ~ ., data = train_raw, fullRank = TRUE)

x_train <- predict(dv, newdata = train_raw)
x_test  <- predict(dv, newdata = test_raw)

y_train <- train_raw$income
y_test  <- test_raw$income

## Padronizar (center/scale) usando apenas treino
pp <- caret::preProcess(x_train, method = c("center", "scale"))
x_train_pp <- predict(pp, x_train)
x_test_pp  <- predict(pp, x_test)

## ---------------------------------------------------------
## 4. Funções auxiliares de avaliação ----
## ---------------------------------------------------------

avaliar_holdout <- function(model_name, pred_class, prob_pos = NULL) {
  cm <- caret::confusionMatrix(pred_class, y_test, positive = ">50K")

  out <- data.frame(
    modelo = model_name,
    accuracy = unname(cm$overall["Accuracy"]),
    kappa = unname(cm$overall["Kappa"]),
    precision = unname(cm$byClass["Precision"]),
    recall = unname(cm$byClass["Recall"]),
    F1 = unname(cm$byClass["F1"]),
    AUC = NA_real_,
    stringsAsFactors = FALSE
  )

  if (!is.null(prob_pos) && requireNamespace("pROC", quietly = TRUE)) {
    roc_obj <- pROC::roc(response = y_test, predictor = prob_pos, levels = c(">50K", "<=50K"))
    out$AUC <- as.numeric(pROC::auc(roc_obj))
  }

  out
}

## ---------------------------------------------------------
## 5. Modelos (treino/teste) ----
## ---------------------------------------------------------

resultados_holdout <- data.frame()

## 5.1 Naive Bayes (klaR)
nb_fit <- klaR::NaiveBayes(x_train_pp, grouping = y_train)
nb_pred <- predict(nb_fit, x_test_pp)
nb_class <- factor(nb_pred$class, levels = levels(y_test))
nb_prob <- nb_pred$posterior[, ">50K"]

resultados_holdout <- rbind(
  resultados_holdout,
  avaliar_holdout("NaiveBayes", nb_class, nb_prob)
)

## 5.2 KNN (vários k)
ks <- c(3, 5, 7, 9, 11, 15, 21)
knn_res <- lapply(ks, function(k) {
  pred <- class::knn(train = x_train_pp, test = x_test_pp, cl = y_train, k = k, prob = TRUE)
  prob_attr <- attr(pred, "prob")
  prob_pos <- ifelse(pred == ">50K", prob_attr, 1 - prob_attr)
  list(k = k, class = pred, prob = prob_pos)
})

knn_metrics <- do.call(rbind, lapply(knn_res, function(obj) {
  avaliar_holdout(paste0("KNN_k=", obj$k), obj$class, obj$prob)
}))

resultados_holdout <- rbind(resultados_holdout, knn_metrics)

## 5.3 LDA/QDA/RDA (klaR/MASS via caret::train é mais simples, mas aqui faremos direto)

## LDA
lda_fit <- MASS::lda(x = x_train_pp, grouping = y_train)
lda_pred <- predict(lda_fit, x_test_pp)
lda_class <- factor(lda_pred$class, levels = levels(y_test))
lda_prob <- lda_pred$posterior[, ">50K"]

resultados_holdout <- rbind(
  resultados_holdout,
  avaliar_holdout("LDA", lda_class, lda_prob)
)

## QDA
qda_fit <- MASS::qda(x = x_train_pp, grouping = y_train)
qda_pred <- predict(qda_fit, x_test_pp)
qda_class <- factor(qda_pred$class, levels = levels(y_test))
qda_prob <- qda_pred$posterior[, ">50K"]

resultados_holdout <- rbind(
  resultados_holdout,
  avaliar_holdout("QDA", qda_class, qda_prob)
)

## RDA (klaR)
rda_fit <- klaR::rda(x_train_pp, grouping = y_train)
rda_pred <- predict(rda_fit, x_test_pp)
rda_class <- factor(rda_pred$class, levels = levels(y_test))
rda_prob <- rda_pred$posterior[, ">50K"]

resultados_holdout <- rbind(
  resultados_holdout,
  avaliar_holdout("RDA", rda_class, rda_prob)
)

## Ordenar por accuracy (teste)
resultados_holdout <- resultados_holdout %>%
  arrange(desc(accuracy))

resultados_holdout

## Melhor no teste (holdout)
resultados_holdout[1, ]

## ---------------------------------------------------------
## 6. Validação cruzada k-fold (caret) ----
## ---------------------------------------------------------

## Vamos treinar com k-fold no conjunto de treino e comparar ROC/Accuracy.
## (do jeito que o caret espera: x = matriz numérica, y = fator)

ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

set.seed(2025)

## Modelos com caret
## - nb  : Naive Bayes (klaR)
## - knn : KNN (class)
## - lda : LDA (MASS)
## - qda : QDA (MASS)
## - rda : RDA (klaR)

modelos_cv <- list()

modelos_cv$NaiveBayes <- train(
  x = x_train_pp,
  y = y_train,
  method = "nb",
  metric = "ROC",
  trControl = ctrl
)

modelos_cv$KNN <- train(
  x = x_train_pp,
  y = y_train,
  method = "knn",
  metric = "ROC",
  tuneGrid = data.frame(k = ks),
  trControl = ctrl
)

modelos_cv$LDA <- train(
  x = x_train_pp,
  y = y_train,
  method = "lda",
  metric = "ROC",
  trControl = ctrl
)

modelos_cv$QDA <- train(
  x = x_train_pp,
  y = y_train,
  method = "qda",
  metric = "ROC",
  trControl = ctrl
)

modelos_cv$RDA <- train(
  x = x_train_pp,
  y = y_train,
  method = "rda",
  metric = "ROC",
  trControl = ctrl
)

## Resumo dos resultados da validação cruzada
res_cv <- data.frame(
  modelo = names(modelos_cv),
  ROC = sapply(modelos_cv, function(m) max(m$results$ROC, na.rm = TRUE)),
  Accuracy = sapply(modelos_cv, function(m) max(m$results$Accuracy, na.rm = TRUE)),
  stringsAsFactors = FALSE
) %>%
  arrange(desc(ROC))

res_cv

## Melhor em CV (k-fold)
res_cv[1, ]

## ---------------------------------------------------------
## 7. Métricas consideradas (resposta do item 5) ----
## ---------------------------------------------------------

## No holdout (teste): Accuracy, Kappa, Precision, Recall, F1 e (opcional) AUC.
## Na validação cruzada: ROC (AUC) e Accuracy (via caret::twoClassSummary).

## ---------------------------------------------------------
## FIM DO SCRIPT
## ---------------------------------------------------------
