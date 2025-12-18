## ---------------------------------------------------------
## AULA 04 - EXERCÍCIO 3
## 1) Leia o conjunto de dados penguins em PDF.
## (Arquivo na pasta: Arquivos complementares-20251217/penguins.pdf)
## ---------------------------------------------------------

## ---------------------------------------------------------
## 0. Definir pasta de dados ----
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

pdf_path <- file.path(data_dir, "penguins.pdf")

if (!file.exists(pdf_path)) {
  stop(paste("Arquivo não encontrado:", pdf_path))
}

## ---------------------------------------------------------
## 1. Tentar extrair tabela do PDF (tabulapdf / tabulizer) ----
## ---------------------------------------------------------

penguins_pdf <- NULL

## 1.1) tabulapdf (do slide)
ok_tabulapdf <- requireNamespace("tabulapdf", quietly = TRUE)

if (!ok_tabulapdf) {
  message("Pacote 'tabulapdf' não está instalado. Tentando instalar...")
  message("Observação: o tabulapdf pode exigir Java configurado no computador.")
  try(install.packages("tabulapdf"), silent = TRUE)
  ok_tabulapdf <- requireNamespace("tabulapdf", quietly = TRUE)
}

if (ok_tabulapdf) {
  tabs <- try(
    tabulapdf::extract_tables(pdf_path, pages = NULL),
    silent = TRUE
  )

  if (!inherits(tabs, "try-error") && length(tabs) > 0) {
    sizes <- sapply(tabs, function(x) nrow(x) * ncol(x))
    tab <- tabs[[which.max(sizes)]]
    penguins_pdf <- as.data.frame(tab, stringsAsFactors = FALSE)
  }
}

## 1.2) tabulizer (opcional; apenas se já estiver instalado)
if (is.null(penguins_pdf)) {
  ok_tabulizer <- requireNamespace("tabulizer", quietly = TRUE)

  if (!ok_tabulizer) {
    message("Pacote 'tabulizer' não está instalado (e pode não estar disponível no CRAN para sua versão do R). Vou seguir para o fallback.")
  }

  if (ok_tabulizer) {
    tabs <- try(
      tabulizer::extract_tables(pdf_path, pages = NULL, guess = TRUE),
      silent = TRUE
    )

    if (!inherits(tabs, "try-error") && length(tabs) > 0) {
      sizes <- sapply(tabs, function(x) nrow(x) * ncol(x))
      tab <- tabs[[which.max(sizes)]]
      penguins_pdf <- as.data.frame(tab, stringsAsFactors = FALSE)
    }
  }
}

## ---------------------------------------------------------
## 2. Fallback: extrair texto do PDF (pdftools) ----
## ---------------------------------------------------------

if (is.null(penguins_pdf)) {
  message("Não consegui extrair tabela automaticamente. Usando fallback com 'pdftools' para extrair texto.")

  if (!requireNamespace("pdftools", quietly = TRUE)) {
    install.packages("pdftools")
  }

  txt <- pdftools::pdf_text(pdf_path)

  ## Mostrar primeiras linhas da primeira página para inspeção
  cat("\n--- Primeira página (início) ---\n")
  cat(substr(txt[[1]], 1, 1500))
  cat("\n--- Fim do trecho ---\n")

  ## Guardar o texto como objeto, caso você queira manipular depois
  penguins_pdf <- txt
}

## ---------------------------------------------------------
## 3. Conferências ----
## ---------------------------------------------------------

if (is.data.frame(penguins_pdf)) {
  dim(penguins_pdf)
  names(penguins_pdf)
  str(penguins_pdf)
  head(penguins_pdf)
} else {
  length(penguins_pdf)
  str(penguins_pdf)
}

## ---------------------------------------------------------
## FIM DO SCRIPT
## ---------------------------------------------------------
