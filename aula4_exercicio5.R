## ---------------------------------------------------------
## AULA 04 - EXERCÍCIO 5
## 1) Baixar as informações de todos os países usando a API:
##    https://restcountries.com/
## ---------------------------------------------------------

## ---------------------------------------------------------
## 0. Pacotes ----
## ---------------------------------------------------------

if (!requireNamespace("httr2", quietly = TRUE)) install.packages("httr2")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("tibble", quietly = TRUE)) install.packages("tibble")

library(httr2)
library(jsonlite)
library(dplyr)
library(tibble)

## ---------------------------------------------------------
## 1. Requisição na API (GET) ----
## ---------------------------------------------------------

## Endpoint v3.1
url <- "https://restcountries.com/v3.1/all"

req <- request(url) %>%
  req_user_agent("aula4_exercicio5/1.0") %>%
  req_options(timeout = 30)

resp <- req %>% req_perform()

## Conferir status HTTP
status <- resp_status(resp)
status

if (status != 200) {
  stop(paste("Erro HTTP:", status))
}

## Converter resposta JSON para objeto R
countries <- resp %>% resp_body_json()

## ---------------------------------------------------------
## 2. Organizar em data.frame (selecionar colunas principais) ----
## ---------------------------------------------------------

get_name_common <- function(x) {
  if (!is.null(x$name) && !is.null(x$name$common)) return(as.character(x$name$common))
  NA_character_
}

get_name_official <- function(x) {
  if (!is.null(x$name) && !is.null(x$name$official)) return(as.character(x$name$official))
  NA_character_
}

get_first_capital <- function(x) {
  if (!is.null(x$capital) && length(x$capital) > 0) return(as.character(x$capital[[1]]))
  NA_character_
}

get_languages <- function(x) {
  if (is.null(x$languages)) return(NA_character_)
  langs <- unname(unlist(x$languages))
  if (length(langs) == 0) return(NA_character_)
  paste(as.character(langs), collapse = ", ")
}

get_currencies <- function(x) {
  if (is.null(x$currencies)) return(NA_character_)
  cur <- x$currencies
  nms <- names(cur)
  if (length(nms) == 0) return(NA_character_)
  vals <- sapply(nms, function(k) {
    nm <- cur[[k]]$name
    if (is.null(nm)) k else paste0(k, " (", nm, ")")
  })
  paste(vals, collapse = "; ")
}

paises <- tibble(
  name_common = vapply(countries, get_name_common, character(1)),
  name_official = vapply(countries, get_name_official, character(1)),
  cca2 = vapply(countries, function(x) ifelse(is.null(x$cca2), NA_character_, as.character(x$cca2)), character(1)),
  cca3 = vapply(countries, function(x) ifelse(is.null(x$cca3), NA_character_, as.character(x$cca3)), character(1)),
  region = vapply(countries, function(x) ifelse(is.null(x$region), NA_character_, as.character(x$region)), character(1)),
  subregion = vapply(countries, function(x) ifelse(is.null(x$subregion), NA_character_, as.character(x$subregion)), character(1)),
  capital = vapply(countries, get_first_capital, character(1)),
  population = vapply(countries, function(x) ifelse(is.null(x$population), NA_real_, as.numeric(x$population)), numeric(1)),
  area = vapply(countries, function(x) ifelse(is.null(x$area), NA_real_, as.numeric(x$area)), numeric(1)),
  languages = vapply(countries, get_languages, character(1)),
  currencies = vapply(countries, get_currencies, character(1))
)

## Limpezas simples
paises <- paises %>%
  arrange(name_common)

## Conferências

dim(paises)
str(paises)
head(paises, 10)
summary(paises)

## ---------------------------------------------------------
## 3. Exemplo de estatísticas rápidas ----
## ---------------------------------------------------------

## Top 10 países mais populosos
paises %>%
  arrange(desc(population)) %>%
  select(name_common, region, population) %>%
  head(10)

## Top 10 maiores áreas
paises %>%
  arrange(desc(area)) %>%
  select(name_common, region, area) %>%
  head(10)

## ---------------------------------------------------------
## 4. (Opcional) Salvar em CSV ----
## ---------------------------------------------------------

## write.csv(paises, "paises_restcountries.csv", row.names = FALSE)

## ---------------------------------------------------------
## FIM DO SCRIPT
## ---------------------------------------------------------
