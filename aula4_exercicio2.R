## ---------------------------------------------------------
## AULA 04 - EXERCÍCIO 2
## 1) Ler a planilha penguins.xlsx e verificar a quantidade de planilhas
## 2) Ler o mesmo arquivo via Google Sheets e verificar a quantidade de planilhas
## 3) Em cada caso, ler a linha 10 a 20 da segunda planilha
## ---------------------------------------------------------

## ---------------------------------------------------------
## 0. Pacotes ----
## ---------------------------------------------------------

if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}

library(readxl)

## ---------------------------------------------------------
## 1) Ler penguins.xlsx (arquivo local) ----
## ---------------------------------------------------------

data_dir <- file.path(getwd(), "Arquivos complementares-20251217")

if (!dir.exists(data_dir)) {
  stop(
    paste(
      "Pasta não encontrada:", data_dir,
      "\nDica: rode setwd('c:/Users/Julia/Downloads/trabalho de entrega para R') antes."
    )
  )
}

path_xlsx <- file.path(data_dir, "penguins.xlsx")

if (!file.exists(path_xlsx)) {
  stop(paste("Arquivo não encontrado:", path_xlsx))
}

sheets_local <- excel_sheets(path_xlsx)
sheets_local
length(sheets_local)

## Linhas 10 a 20 da 2ª planilha
## (skip=9 pula as 9 primeiras linhas; n_max=11 lê 11 linhas => 10..20)
dados_local_l10_20 <- read_excel(
  path_xlsx,
  sheet = 2,
  skip = 9,
  n_max = 11
)

dados_local_l10_20

## ---------------------------------------------------------
## 2) Ler o mesmo arquivo via Google Sheets ----
## ---------------------------------------------------------

## Link informado no enunciado
url_gsheet <- "https://docs.google.com/spreadsheets/d/1aFu8lnD_g0yjF5O-K6SFgSEWiHPpgvFCF0NY9D6LXnY/edit?gid=0#gid=0"

## Extrair o ID do Google Sheets
id <- sub(".*?/d/([^/]+).*", "\\1", url_gsheet)

## Montar URL de download em XLSX
url_xlsx <- paste0("https://docs.google.com/spreadsheets/d/", id, "/export?format=xlsx")

## Baixar para arquivo temporário
xlsx_tmp <- tempfile(fileext = ".xlsx")

ok_download <- try(
  {
    download.file(url_xlsx, destfile = xlsx_tmp, mode = "wb", quiet = TRUE)
    TRUE
  },
  silent = TRUE
)

if (inherits(ok_download, "try-error") || !file.exists(xlsx_tmp)) {
  stop("Não foi possível baixar o arquivo do Google Sheets. Verifique se o link está acessível.")
}

sheets_web <- excel_sheets(xlsx_tmp)
sheets_web
length(sheets_web)

## Linhas 10 a 20 da 2ª planilha (arquivo baixado)
dados_web_l10_20 <- read_excel(
  xlsx_tmp,
  sheet = 2,
  skip = 9,
  n_max = 11
)

dados_web_l10_20

## ---------------------------------------------------------
## FIM DO SCRIPT
## ---------------------------------------------------------
