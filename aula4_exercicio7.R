## ---------------------------------------------------------
## AULA 04 - EXERCÍCIO 7
## 1) Organize e limpe os dados dos estados coletados por webscraping.
## 2) Tente ler e organizar os dados do arquivo RemuneracaoServidores201901.pdf.
## ---------------------------------------------------------

## ---------------------------------------------------------
## 0. Pacotes ----
## ---------------------------------------------------------

if (!requireNamespace("rvest", quietly = TRUE)) install.packages("rvest")
if (!requireNamespace("xml2", quietly = TRUE)) install.packages("xml2")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")

library(rvest)
library(xml2)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

## ---------------------------------------------------------
## 1) Limpeza dos dados das UFs (webscraping do Exercício 6) ----
## ---------------------------------------------------------

url <- "https://pt.wikipedia.org/wiki/Lista_de_unidades_federativas_do_Brasil_por_popula%C3%A7%C3%A3o"

pagina <- read_html(url)
tabelas <- pagina %>% html_table(fill = TRUE)

## Selecionar a tabela que contém 'Unidade federativa' e 'População'
idx <- which(sapply(tabelas, function(tb) {
  nms <- tolower(names(tb))
  any(str_detect(nms, "unidade")) && any(str_detect(nms, "popula"))
}))

if (length(idx) == 0) {
  stop("Não encontrei automaticamente a tabela com 'Unidade federativa' e 'População'.")
}

uf_raw <- tabelas[[idx[1]]]

names(uf_raw) <- str_squish(names(uf_raw))

## Identificar colunas principais
col_uf  <- names(uf_raw)[str_detect(tolower(names(uf_raw)), "unidade")][1]
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

## Exemplo de limpeza adicional: remover notas entre parênteses no nome (se existir)
uf <- uf %>%
  mutate(
    unidade_federativa = str_remove(unidade_federativa, "\\s*\\(.*\\)\\s*$")
  )

## Conferências

dim(uf)
str(uf)
head(uf, 10)

## Top 10 por população
uf %>%
  arrange(desc(populacao)) %>%
  head(10)

## (Opcional) salvar
## write.csv(uf, "ufs_populacao_limpo.csv", row.names = FALSE)

## ---------------------------------------------------------
## 2) Tentar ler e organizar o PDF RemuneracaoServidores201901.pdf ----
## ---------------------------------------------------------

## Arquivo na pasta complementar

data_dir <- file.path(getwd(), "Arquivos complementares-20251217")

if (!dir.exists(data_dir)) {
  stop(
    paste(
      "Pasta não encontrada:", data_dir,
      "\nDica: rode setwd('c:/Users/Julia/Downloads/trabalho de entrega para R') antes."
    )
  )
}

pdf_path <- file.path(data_dir, "RemuneracaoServidores201901.pdf")

if (!file.exists(pdf_path)) {
  stop(paste("Arquivo não encontrado:", pdf_path))
}

remun_df <- NULL

## 2.1) Tentar extrair tabelas com tabulapdf
ok_tabulapdf <- requireNamespace("tabulapdf", quietly = TRUE)

if (!ok_tabulapdf) {
  message("Pacote 'tabulapdf' não está instalado. Tentando instalar...")
  message("Observação: o tabulapdf pode exigir Java configurado no computador.")
  try(install.packages("tabulapdf"), silent = TRUE)
  ok_tabulapdf <- requireNamespace("tabulapdf", quietly = TRUE)
}

if (ok_tabulapdf) {
  tabs_pdf <- try(
    tabulapdf::extract_tables(pdf_path, pages = 1),
    silent = TRUE
  )

  if (!inherits(tabs_pdf, "try-error") && length(tabs_pdf) > 0) {
    sizes <- sapply(tabs_pdf, function(x) nrow(x) * ncol(x))
    tab <- tabs_pdf[[which.max(sizes)]]
    remun_df <- as.data.frame(tab, stringsAsFactors = FALSE)
  }
}

## 2.2) Fallback: extrair texto com pdftools (para inspeção)
if (is.null(remun_df)) {
  message("Não consegui extrair tabelas automaticamente. Usando fallback com 'pdftools' para extrair texto.")

  if (!requireNamespace("pdftools", quietly = TRUE)) {
    install.packages("pdftools")
  }

  txt <- pdftools::pdf_text(pdf_path)

  cat("\n--- Primeira página (início) ---\n")
  cat(substr(txt[[1]], 1, 2000))
  cat("\n--- Fim do trecho ---\n")

  ## Se quiser, você pode tentar identificar padrões e depois transformar em tabela.
} else {
  ## Conferências se tabela foi extraída
  dim(remun_df)
  str(remun_df)
  head(remun_df, 10)
}

## ---------------------------------------------------------
## FIM DO SCRIPT
## ---------------------------------------------------------
