## ---------------------------------------------------------
## AULA 04 - EXERCÍCIO 1
## 1) Leia o arquivo adults.csv
## 2) Leia o arquivo wine.data
## (Arquivos na pasta: Arquivos complementares-20251217)
## ---------------------------------------------------------

## ---------------------------------------------------------
## 0. Definir pasta de dados ----
## ---------------------------------------------------------

## Espera que seu Working Directory esteja em:
## c:/Users/Julia/Downloads/trabalho de entrega para R
## Se não estiver, ajuste com setwd() ou use o caminho completo.

data_dir <- file.path(getwd(), "Arquivos complementares-20251217")

if (!dir.exists(data_dir)) {
  stop(
    paste(
      "Pasta não encontrada:", data_dir,
      "\nDica: rode setwd('c:/Users/Julia/Downloads/trabalho de entrega para R') antes."
    )
  )
}

## ---------------------------------------------------------
## 1) Ler adults.csv ----
## ---------------------------------------------------------

path_adults <- file.path(data_dir, "adult.csv")

adults <- read.csv(
  path_adults,
  header = TRUE,
  stringsAsFactors = FALSE
)

## Conferências

dim(adults)
names(adults)
str(adults)
summary(adults)

## ---------------------------------------------------------
## 2) Ler wine.data ----
## ---------------------------------------------------------

path_wine <- file.path(data_dir, "wine.data")

wine <- read.csv(
  path_wine,
  header = FALSE
)

## Nomes clássicos do dataset Wine (UCI)
## Coluna 1 = classe (1, 2, 3)

names(wine) <- c(
  "Class",
  "Alcohol",
  "Malic.acid",
  "Ash",
  "Alcalinity.of.ash",
  "Magnesium",
  "Total.phenols",
  "Flavanoids",
  "Nonflavanoid.phenols",
  "Proanthocyanins",
  "Color.intensity",
  "Hue",
  "OD280.OD315.of.diluted.wines",
  "Proline"
)

## Ajustar tipo da classe
wine$Class <- factor(wine$Class)

## Conferências

dim(wine)
names(wine)
str(wine)
summary(wine)

## ---------------------------------------------------------
## FIM DO SCRIPT
## ---------------------------------------------------------
