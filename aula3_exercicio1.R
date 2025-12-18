## ---------------------------------------------------------
## AULA 03 - EXERCÍCIO 1
## Gerar o conjunto de dados e:
## 1) Fazer análise descritiva
## 2) Verificar associação entre as variáveis
## ---------------------------------------------------------

## ---------------------------------------------------------
## 1. Gerar o conjunto de dados (simulado) ----
## ---------------------------------------------------------

set.seed(2025)
N <- 200

Cat1 <- sample(c("A", "B"), N, replace = TRUE, prob = c(0.6, 0.4))

Cat2 <- ifelse(
  Cat1 == "A",
  sample(c("X", "Y"), N, replace = TRUE, prob = c(0.8, 0.2)),
  sample(c("X", "Y"), N, replace = TRUE, prob = c(0.3, 0.7))
)

Cat3 <- sample(c("L", "M", "N"), N, replace = TRUE)

X1 <- ifelse(
  Cat1 == "A",
  runif(N, min = 60, max = 80),
  runif(N, min = 40, max = 60)
)

X2 <- X1 + runif(N, min = -2, max = 2)
X3 <- runif(N, min = 80, max = 120)

## Dataset final

dados <- data.frame(
  Cat1 = factor(Cat1),
  Cat2 = factor(Cat2),
  Cat3 = factor(Cat3),
  X1 = X1,
  X2 = X2,
  X3 = X3
)

## Visualização inicial

dim(dados)
names(dados)
str(dados)
summary(dados)

## ---------------------------------------------------------
## 2. Análise descritiva ----
## ---------------------------------------------------------

## Separar numéricas e categóricas
vars_numericas   <- sapply(dados, is.numeric)
vars_categoricas <- !vars_numericas

dados_num <- dados[, vars_numericas, drop = FALSE]
dados_cat <- dados[, vars_categoricas, drop = FALSE]

## --- 2.1 Numéricas: estatísticas e gráficos ----

summary(dados_num)

medidas_num <- function(x) {
  c(
    media   = mean(x),
    mediana = median(x),
    desvio  = sd(x),
    min     = min(x),
    q1      = quantile(x, 0.25),
    q3      = quantile(x, 0.75),
    max     = max(x)
  )
}

aplicar_medidas <- sapply(dados_num, medidas_num)
print(t(aplicar_medidas))

par(mfrow = c(2, 2))
for (nome in names(dados_num)) {
  x <- dados_num[[nome]]
  hist(
    x,
    main = paste("Histograma de", nome),
    xlab = nome,
    col = "lightblue",
    border = "white"
  )
  boxplot(
    x,
    main = paste("Boxplot de", nome),
    horizontal = TRUE,
    col = "lightgreen"
  )
}
par(mfrow = c(1, 1))

## --- 2.2 Categóricas: frequências/proporções e gráficos ----

tabelas_cat <- function(x) {
  tab  <- table(x, useNA = "ifany")
  prop <- prop.table(tab)
  list(
    frequencias = tab,
    proporcoes  = round(prop, 3)
  )
}

lista_tabelas <- lapply(dados_cat, tabelas_cat)
names(lista_tabelas) <- names(dados_cat)

print(lista_tabelas$Cat1)
print(lista_tabelas$Cat2)
print(lista_tabelas$Cat3)

par(mfrow = c(2, 2))
for (nome in names(dados_cat)) {
  tab <- table(dados_cat[[nome]], useNA = "ifany")
  barplot(
    tab,
    main = paste("Gráfico de barras -", nome),
    ylab = "Frequência",
    col  = "lightblue",
    las  = 2
  )
}
par(mfrow = c(1, 1))

## ---------------------------------------------------------
## 3. Associação entre variáveis ----
## ---------------------------------------------------------

## --- 3.1 Categórica x Categórica (Qui-quadrado) ----

## Cat1 x Cat2
(tab_Cat1_Cat2 <- table(dados$Cat1, dados$Cat2))
chisq.test(tab_Cat1_Cat2)

## Cat1 x Cat3
(tab_Cat1_Cat3 <- table(dados$Cat1, dados$Cat3))
chisq.test(tab_Cat1_Cat3)

## Cat2 x Cat3
(tab_Cat2_Cat3 <- table(dados$Cat2, dados$Cat3))
chisq.test(tab_Cat2_Cat3)

## --- 3.2 Numérica x Numérica (Correlação) ----

cor(dados_num)

cor.test(dados$X1, dados$X2)
cor.test(dados$X1, dados$X3)
cor.test(dados$X2, dados$X3)

pairs(dados_num, main = "Dispersões (Numéricas)")

## --- 3.3 Numérica x Categórica (t-test/ANOVA) ----

## Como Cat1 tem 2 níveis, usamos t.test
by(dados$X1, dados$Cat1, summary)
t.test(X1 ~ Cat1, data = dados)

t.test(X2 ~ Cat1, data = dados)
t.test(X3 ~ Cat1, data = dados)

## Para Cat3 (3 níveis), usamos ANOVA
by(dados$X1, dados$Cat3, summary)
summary(aov(X1 ~ Cat3, data = dados))

summary(aov(X2 ~ Cat3, data = dados))
summary(aov(X3 ~ Cat3, data = dados))

boxplot(X1 ~ Cat3, data = dados, col = "lightgreen", main = "X1 por Cat3")
boxplot(X2 ~ Cat3, data = dados, col = "lightgreen", main = "X2 por Cat3")
boxplot(X3 ~ Cat3, data = dados, col = "lightgreen", main = "X3 por Cat3")

## ---------------------------------------------------------
## FIM DO SCRIPT
## ---------------------------------------------------------
