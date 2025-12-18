## ---------------------------------------------------------
## AULA 03 - EXERCÍCIO 3 (Adults)
## 1) Fazer uma análise bivariada do conjunto de dados Adults
## 2) Indicar quais variáveis estão mais relacionadas com renda
##    acima/abaixo de 50k (com gráficos e estatísticas)
## ---------------------------------------------------------

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

## Tratar valores desconhecidos "?" como NA (comum nesse dataset)
adults[adults == "?"] <- NA

## Definir tipos

## Variáveis categóricas
cat_vars <- c(
  "workclass", "education", "marital.status", "occupation", "relationship",
  "race", "sex", "native.country", "income"
)

for (v in cat_vars) {
  adults[[v]] <- factor(adults[[v]])
}

## Criar alvo binário: <=50K vs >50K
## (em alguns arquivos pode vir com ponto no final, ex: ">50K.")
adults$income <- factor(trimws(as.character(adults$income)))
adults$income <- factor(gsub("\\.$", "", adults$income))

## Remover linhas com income ausente (se houver)
adults <- adults[!is.na(adults$income), ]

## Visualização inicial

dim(adults)
str(adults)
summary(adults)

## ---------------------------------------------------------
## 2. Análise bivariada: income vs variáveis ----
## ---------------------------------------------------------

## Separar numéricas e categóricas
vars_numericas   <- sapply(adults, is.numeric)
vars_categoricas <- !vars_numericas

adults_num <- adults[, vars_numericas, drop = FALSE]
adults_cat <- adults[, vars_categoricas, drop = FALSE]

## ---------------------------------------------------------
## 2.1 Categórica x income: Qui-quadrado + Cramer's V ----
## ---------------------------------------------------------

## Função para Cramer's V (força de associação)
cramers_v <- function(tab) {
  chi <- suppressWarnings(chisq.test(tab))
  chi2 <- as.numeric(chi$statistic)
  n <- sum(tab)
  r <- nrow(tab)
  k <- ncol(tab)
  sqrt(chi2 / (n * (min(r - 1, k - 1))))
}

cat_names <- setdiff(names(adults_cat), "income")

assoc_cat <- data.frame(
  variavel = character(0),
  p_value = numeric(0),
  cramers_v = numeric(0),
  stringsAsFactors = FALSE
)

for (v in cat_names) {
  tab <- table(adults[[v]], adults$income, useNA = "no")

  if (nrow(tab) >= 2 && ncol(tab) == 2) {
    chi <- suppressWarnings(chisq.test(tab))
    assoc_cat <- rbind(
      assoc_cat,
      data.frame(
        variavel = v,
        p_value = chi$p.value,
        cramers_v = cramers_v(tab),
        stringsAsFactors = FALSE
      )
    )
  }
}

assoc_cat <- assoc_cat[order(-assoc_cat$cramers_v), ]
assoc_cat

## Gráficos (proporção de >50K por categoria)

## Converter income para indicador
income_high <- adults$income

par(mfrow = c(2, 2))

for (v in head(assoc_cat$variavel, 4)) {
  tab <- table(adults[[v]], income_high)
  prop_high <- prop.table(tab, 1)[, ">50K"]
  barplot(
    prop_high,
    las = 2,
    main = paste("Proporção >50K por", v),
    ylab = "Proporção",
    col = "lightblue"
  )
}

par(mfrow = c(1, 1))

## ---------------------------------------------------------
## 2.2 Numérica x income: boxplots + t-test ----
## ---------------------------------------------------------

num_names <- setdiff(names(adults_num), character(0))

assoc_num <- data.frame(
  variavel = character(0),
  p_value = numeric(0),
  diff_media = numeric(0),
  stringsAsFactors = FALSE
)

for (v in num_names) {
  x <- adults[[v]]
  g <- adults$income

  ok <- !is.na(x) & !is.na(g)
  x <- x[ok]
  g <- g[ok]

  if (length(unique(g)) == 2) {
    tt <- t.test(x ~ g)
    m <- tapply(x, g, mean)

    assoc_num <- rbind(
      assoc_num,
      data.frame(
        variavel = v,
        p_value = tt$p.value,
        diff_media = unname(m[">50K"] - m["<=50K"]),
        stringsAsFactors = FALSE
      )
    )
  }
}

assoc_num <- assoc_num[order(assoc_num$p_value), ]
assoc_num

## Boxplots das numéricas mais relevantes (menores p-values)

par(mfrow = c(2, 2))

for (v in head(assoc_num$variavel, 4)) {
  boxplot(
    adults[[v]] ~ adults$income,
    main = paste(v, "por income"),
    xlab = "income",
    ylab = v,
    col = "lightgreen"
  )
}

par(mfrow = c(1, 1))

## ---------------------------------------------------------
## 3. Reforço do argumento: modelo preditivo (Logístico) ----
## ---------------------------------------------------------

## Criar alvo binário (0/1)
adults$income_bin <- ifelse(adults$income == ">50K", 1, 0)

## Selecionar um subconjunto de variáveis comuns para evitar modelo enorme
## (também reduz problema de muitas categorias raras)
vars_modelo <- c(
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

modelo_df <- adults[, c("income_bin", vars_modelo), drop = FALSE]
modelo_df <- na.omit(modelo_df)

m_log <- glm(
  income_bin ~ ., 
  data = modelo_df,
  family = binomial()
)

summary(m_log)

## Odds ratios (exp(beta)) para facilitar interpretação
odds_ratios <- exp(coef(m_log))
odds_ratios

## ---------------------------------------------------------
## 4. Conclusão (orientação para o seu texto) ----
## ---------------------------------------------------------

## Use como evidência:
## - assoc_cat (Cramer's V e p-value) para variáveis categóricas
## - assoc_num (p-value e diferença de médias) para numéricas
## - summary(m_log) + odds_ratios para reforçar as variáveis mais associadas
##   à chance de >50K.

## ---------------------------------------------------------
## FIM DO SCRIPT
## ---------------------------------------------------------
