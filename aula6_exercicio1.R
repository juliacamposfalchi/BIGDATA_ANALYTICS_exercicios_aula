## ---------------------------------------------------------
## AULA 06 - EXERCÍCIO 1
## Dataset Adult: ajustar um modelo de regressão logística
## para predizer renda (<=50K vs >50K)
## ---------------------------------------------------------

## ---------------------------------------------------------
## 0. Pacotes ----
## ---------------------------------------------------------

if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")

library(dplyr)
library(ggplot2)

## AUC/ROC (opcional)
if (!requireNamespace("pROC", quietly = TRUE)) {
  try(install.packages("pROC"), silent = TRUE)
}

## ---------------------------------------------------------
## 1. Carregar e preparar dados ----
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

## Alvo binário: 1 => >50K
adults$income_bin <- ifelse(adults$income == ">50K", 1, 0)

## Definir tipos categóricos
cat_vars <- c(
  "workclass", "education", "marital.status", "occupation", "relationship",
  "race", "sex", "native.country"
)

for (v in cat_vars) {
  adults[[v]] <- factor(adults[[v]])
}

## Selecionar um conjunto de variáveis para o modelo
vars_modelo <- c(
  "income_bin",
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

## Remover NAs (linhas incompletas)
df <- na.omit(df)

dim(df)
str(df)

## ---------------------------------------------------------
## 2. Separar treino e teste ----
## ---------------------------------------------------------

set.seed(2025)
n <- nrow(df)
idx_train <- sample(seq_len(n), size = floor(0.7 * n))

train <- df[idx_train, , drop = FALSE]
test  <- df[-idx_train, , drop = FALSE]

## ---------------------------------------------------------
## 3. Ajustar regressão logística (glm) ----
## ---------------------------------------------------------

modelo_log <- glm(
  income_bin ~ .,
  data = train,
  family = binomial()
)

summary(modelo_log)

## Odds ratios (exp(beta))
odds_ratios <- exp(coef(modelo_log))
odds_ratios

## ---------------------------------------------------------
## 4. Predição e avaliação ----
## ---------------------------------------------------------

## Probabilidade predita
test$prob <- predict(modelo_log, newdata = test, type = "response")

## Classe predita (threshold 0.5)
cutoff <- 0.5
test$pred <- ifelse(test$prob >= cutoff, 1, 0)

## Matriz de confusão
(tab <- table(
  Real = test$income_bin,
  Pred = test$pred
))

## Acurácia
acc <- mean(test$pred == test$income_bin)
acc

## Precisão/Recall (para classe 1 => >50K)
TP <- tab["1", "1"]
TN <- tab["0", "0"]
FP <- tab["0", "1"]
FN <- tab["1", "0"]

precision <- TP / (TP + FP)
recall <- TP / (TP + FN)

precision
recall

## ROC/AUC (se pROC estiver disponível)
if (requireNamespace("pROC", quietly = TRUE)) {
  roc_obj <- pROC::roc(response = test$income_bin, predictor = test$prob)
  auc_val <- pROC::auc(roc_obj)
  auc_val

  plot(roc_obj, main = paste("ROC - AUC =", round(as.numeric(auc_val), 3)))
}

## ---------------------------------------------------------
## 5. Visualizações úteis ----
## ---------------------------------------------------------

## Distribuição das probabilidades por classe real
ggplot(test, aes(x = prob, fill = factor(income_bin))) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  theme_minimal() +
  labs(
    title = "Probabilidade predita por classe real",
    x = "P(>50K)",
    fill = "Classe real (income_bin)"
  )

## Importância simples: coeficientes por magnitude (exceto intercept)
coefs <- coef(modelo_log)
coefs <- coefs[names(coefs) != "(Intercept)"]

imp <- data.frame(
  termo = names(coefs),
  coef = as.numeric(coefs),
  abs_coef = abs(as.numeric(coefs))
) %>%
  arrange(desc(abs_coef))

head(imp, 15)

## ---------------------------------------------------------
## FIM DO SCRIPT
## ---------------------------------------------------------
