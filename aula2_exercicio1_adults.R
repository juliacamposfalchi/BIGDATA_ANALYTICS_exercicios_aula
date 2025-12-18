## ---------------------------------------------------------
## EXERCÍCIO 1 - Conjunto de dados "adults" (Adult Income)
## ---------------------------------------------------------

## 1. Carregar o conjunto de dados "adults" a partir de um link ----

## Usaremos o arquivo disponível publicamente (Adult Income / Census Income)
## Caso o link fique fora do ar, o professor pode ter 
#disponibilizado o CSV na plataforma.

url_adults <- "https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"

## Leitura do arquivo CSV. Ajuste os nomes das colunas se o seu arquivo vier diferente.
adults <- read.csv(
  url_adults,
  header = FALSE,
  strip.white = TRUE
)

## Atribuir nomes de colunas típicos do dataset Adult
names(adults) <- c(
  "age", "workclass", "fnlwgt", "education", "education.num",
  "marital.status", "occupation", "relationship", "race", "sex",
  "capital.gain", "capital.loss", "hours.per.week", "native.country",
  "income"
)

## Visualização inicial
dim(adults)      # número de linhas e colunas
names(adults)    # nomes das variáveis
str(adults)      # estrutura das variáveis
summary(adults)  # resumo geral

## ---------------------------------------------------------
## 3. Classificar cada variável de acordo com o tipo
##    (numérica contínua / discreta / categórica)
## ---------------------------------------------------------

## Classe básica de cada coluna
classes <- sapply(adults, class)
classes

## Exemplo de tabela de tipos (ajuste conforme o dataset que veio pra você)
tipos <- data.frame(
  variavel = names(adults),
  classe_R = as.character(classes)
)

tipos

## ---------------------------------------------------------
## 4. Análise descritiva univariada
## ---------------------------------------------------------

## Separar variáveis numéricas e categóricas
vars_numericas   <- sapply(adults, is.numeric)
vars_categoricas <- !vars_numericas

adults_num  <- adults[, vars_numericas, drop = FALSE]
adults_cat  <- adults[, vars_categoricas, drop = FALSE]

## --- 4.1. Variáveis numéricas: estatísticas e gráficos ----

## Estatísticas descritivas básicas
summary(adults_num)

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

aplicar_medidas <- sapply(adults_num, medidas_num)

## Tabela com linhas = variáveis
print(t(aplicar_medidas))

## Histograma e boxplot para cada variável numérica
par(mfrow = c(2, 2))   # até 4 gráficos por janela; ajuste se quiser

for (nome in names(adults_num)) {
  x <- adults_num[[nome]]
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

par(mfrow = c(1, 1))   # voltar layout padrão

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
lista_tabelas <- lapply(adults_cat, tabelas_cat)
names(lista_tabelas) <- names(adults_cat)

## Exemplo: ver tabela da variável income
print(lista_tabelas$income)

## Gráfico de barras para cada variável categórica
par(mfrow = c(2, 2))

for (nome in names(adults_cat)) {
  x <- adults_cat[[nome]]
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
## - "summary(adults_num)" e "aplicar_medidas" -> estatísticas
## - gráficos gerados -> parte de gráficos do enunciado
## - "lista_tabelas" -> tabelas de frequências/proporções
## ---------------------------------------------------------
