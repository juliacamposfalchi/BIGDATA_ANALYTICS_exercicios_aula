## ---------------------------------------------------------
## AULA 04 - EXERCÍCIO 6
## 1) Baixe os dados da população dos estados brasileiros do Wikipédia:
##    https://pt.wikipedia.org/wiki/Lista_de_unidades_federativas_do_Brasil_por_popula%C3%A7%C3%A3o
## ---------------------------------------------------------

## ---------------------------------------------------------
## 0. Pacotes ----
## ---------------------------------------------------------

if (!requireNamespace("rvest", quietly = TRUE)) install.packages("rvest")
if (!requireNamespace("xml2", quietly = TRUE)) install.packages("xml2")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")

library(rvest)
library(xml2)
library(dplyr)
library(stringr)
library(readr)

## ---------------------------------------------------------
## 1. Ler HTML ----
## ---------------------------------------------------------

url <- "https://pt.wikipedia.org/wiki/Lista_de_unidades_federativas_do_Brasil_por_popula%C3%A7%C3%A3o"

pagina <- read_html(url)

tabelas <- pagina %>% html_table(fill = TRUE)

length(tabelas)

## ---------------------------------------------------------
## 2. Selecionar a tabela correta ----
## ---------------------------------------------------------

## Heurística: procurar a primeira tabela que tenha algo como
## "Unidade federativa" e "População" no cabeçalho.
idx <- which(sapply(tabelas, function(tb) {
  nms <- tolower(names(tb))
  any(str_detect(nms, "unidade")) && any(str_detect(nms, "popula"))
}))

idx

if (length(idx) == 0) {
  stop("Não encontrei automaticamente a tabela com 'Unidade federativa' e 'População'.")
}

uf_raw <- tabelas[[idx[1]]]

## Padronizar nomes
names(uf_raw) <- str_squish(names(uf_raw))

uf_raw %>% head(10)

## ---------------------------------------------------------
## 3. Limpar/organizar dados ----
## ---------------------------------------------------------

## Tentar identificar colunas principais
col_uf <- names(uf_raw)[str_detect(tolower(names(uf_raw)), "unidade")][1]
col_pop <- names(uf_raw)[str_detect(tolower(names(uf_raw)), "popula")][1]

uf <- uf_raw %>%
  transmute(
    unidade_federativa = .data[[col_uf]],
    populacao_raw = .data[[col_pop]]
  ) %>%
  mutate(
    unidade_federativa = str_squish(as.character(unidade_federativa)),
    populacao_raw = str_squish(as.character(populacao_raw)),
    populacao_raw = str_remove_all(populacao_raw, "\\[.*?\\]"),
    populacao = readr::parse_number(populacao_raw, locale = locale(grouping_mark = ".", decimal_mark = ","))
  ) %>%
  filter(!is.na(populacao))

## Conferências

dim(uf)
str(uf)
head(uf, 10)

## Top 10 por população
uf %>%
  arrange(desc(populacao)) %>%
  head(10)

## (Opcional) salvar
## write.csv(uf, "populacao_ufs_wikipedia.csv", row.names = FALSE)

## ---------------------------------------------------------
## FIM DO SCRIPT
## ---------------------------------------------------------
