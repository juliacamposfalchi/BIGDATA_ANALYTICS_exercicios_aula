## ---------------------------------------------------------
## EXERCÍCIO 2 - Conjunto de dados "mtcars" (nativo do R)
## ---------------------------------------------------------

## 1. Carregar o conjunto de dados "mtcars" ----

data(mtcars)

## Copiar para um objeto e trazer o nome dos carros como coluna
mt <- mtcars
mt$modelo <- rownames(mtcars)
rownames(mt) <- NULL

## Visualização inicial
dim(mt)      # número de linhas e colunas
names(mt)    # nomes das variáveis
str(mt)      # estrutura das variáveis
summary(mt)  # resumo geral

## ---------------------------------------------------------
## 3. Classificar cada variável de acordo com o tipo
##    (numérica contínua / discreta / categórica)
## ---------------------------------------------------------

## Transformar variáveis tipicamente categóricas em factor
mt$cyl  <- factor(mt$cyl)
mt$vs   <- factor(mt$vs)
mt$am   <- factor(mt$am)
mt$gear <- factor(mt$gear)
mt$carb <- factor(mt$carb)

## Classe básica de cada coluna
classes <- sapply(mt, class)
classes

## Exemplo de tabela de tipos
## (Ajuste manualmente se o professor pedir uma classificação mais específica)
tipos <- data.frame(
  variavel = names(mt),
  classe_R = as.character(classes)
)

tipos

## ---------------------------------------------------------
## 4. Análise descritiva univariada
## ---------------------------------------------------------

## Separar variáveis numéricas e categóricas
vars_numericas   <- sapply(mt, is.numeric)
vars_categoricas <- !vars_numericas

mt_num <- mt[, vars_numericas, drop = FALSE]
mt_cat <- mt[, vars_categoricas, drop = FALSE]

## --- 4.1. Variáveis numéricas: estatísticas e gráficos ----

## Estatísticas descritivas básicas
summary(mt_num)

## Função auxiliar para medidas adicionais
medidas_num <- function(x) {
  c(
    media   = mean(x, na.rm = TRUE),
    mediana = median(x, na.rm = TRUE),
    desvio  = sd(x, na.rm = TRUE),
    min     = min(x, na.rm = TRUE),
    q1      = quantile(x, 0.25, na.rm = TRUE),
    q3      = quantile(x, 0.75, na.rm = TRUE),
    max     = max(x, na.rm = TRUE)
  )
}

aplicar_medidas <- sapply(mt_num, medidas_num)

## Tabela com linhas = variáveis
print(t(aplicar_medidas))

## Histograma e boxplot para cada variável numérica
par(mfrow = c(2, 2))

for (nome in names(mt_num)) {
  x <- mt_num[[nome]]
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

## --- 4.2. Variáveis categóricas: tabelas e gráficos ----

## Função para imprimir tabela e proporções
tabelas_cat <- function(x) {
  tab  <- table(x, useNA = "ifany")
  prop <- prop.table(tab)
  list(
    frequencias = tab,
    proporcoes  = round(prop, 3)
  )
}

## Exibir tabelas para cada variável categórica
lista_tabelas <- lapply(mt_cat, tabelas_cat)
names(lista_tabelas) <- names(mt_cat)

## Exemplo: ver tabela da variável cyl
print(lista_tabelas$cyl)

## Gráfico de barras para cada variável categórica
par(mfrow = c(2, 2))

for (nome in names(mt_cat)) {
  x <- mt_cat[[nome]]
  tab <- table(x, useNA = "ifany")
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
## FIM DO SCRIPT
## - "tipos" -> ajuda na classificação das variáveis
## - "summary(mt_num)" e "aplicar_medidas" -> estatísticas
## - gráficos gerados -> parte de gráficos do enunciado
## - "lista_tabelas" -> tabelas de frequências/proporções
## ---------------------------------------------------------
