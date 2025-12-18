## ---------------------------------------------------------
## AULA 04 - EXERCÍCIO 4
## 1) Instalar SQL Server Express, criar usuário, rodar o scriptBD.sql
## 2) Carregar os dados do banco no R
## ---------------------------------------------------------

## ---------------------------------------------------------
## 0. Pacotes ----
## ---------------------------------------------------------

if (!requireNamespace("DBI", quietly = TRUE)) install.packages("DBI")
if (!requireNamespace("odbc", quietly = TRUE)) install.packages("odbc")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")

library(DBI)
library(odbc)
library(dplyr)

## ---------------------------------------------------------
## 1. Parâmetros de conexão (AJUSTE AQUI) ----
## ---------------------------------------------------------

## Exemplos comuns de instância no SQL Server Express:
## - "localhost\\SQLEXPRESS"
## - "DESKTOP-XXXX\\SQLEXPRESS"
server   <- "localhost\\SQLEXPRESS"

## Banco criado pelo scriptBD.sql
## CREATE DATABASE loja
database <- "loja"

## Driver ODBC (Windows): geralmente "ODBC Driver 17 for SQL Server" ou "ODBC Driver 18 for SQL Server"
## Para ver os drivers instalados, rode: odbc::odbcListDrivers()
driver   <- "ODBC Driver 17 for SQL Server"

## Se você criou um usuário SQL (SQL Authentication), coloque aqui:
uid <- "seu_usuario"
pwd <- "sua_senha"

## Se você estiver usando autenticação do Windows (Trusted Connection), você pode deixar uid/pwd vazios
## e usar trusted_connection = "yes" (ver abaixo).
use_windows_auth <- FALSE

## ---------------------------------------------------------
## 2. Conectar ----
## ---------------------------------------------------------

con <- NULL

if (use_windows_auth) {
  con <- dbConnect(
    odbc::odbc(),
    Driver = driver,
    Server = server,
    Database = database,
    Trusted_Connection = "Yes"
  )
} else {
  con <- dbConnect(
    odbc::odbc(),
    Driver = driver,
    Server = server,
    Database = database,
    UID = uid,
    PWD = pwd,
    TrustServerCertificate = "Yes"
  )
}

## Se conectou, vai mostrar algo como "OdbcConnection"
con

## ---------------------------------------------------------
## 3. Verificar tabelas e carregar dados ----
## ---------------------------------------------------------

## Listar tabelas do banco
(tabelas <- dbListTables(con))

## Esperado pelo scriptBD.sql:
## - Clientes
## - Pedidos

## Carregar tabelas para o R
Clientes <- dbReadTable(con, "Clientes")
Pedidos  <- dbReadTable(con, "Pedidos")

## Conferências

dim(Clientes)
str(Clientes)

head(Clientes)
summary(Clientes)



dim(Pedidos)
str(Pedidos)

head(Pedidos)
summary(Pedidos)

## ---------------------------------------------------------
## 4. Exemplo de consulta usando dplyr/dbplyr ----
## ---------------------------------------------------------

## Ler como tabelas remotas (lazy) e fazer join/agrupamentos
Clientes_tbl <- tbl(con, "Clientes")
Pedidos_tbl  <- tbl(con, "Pedidos")

## Total gasto por cliente (top 10)
resumo <- Pedidos_tbl %>%
  inner_join(Clientes_tbl, by = c("cliente_id" = "cliente_id")) %>%
  group_by(cliente_id, nome, cidade) %>%
  summarise(
    total_gasto = sum(valor_total, na.rm = TRUE),
    qtd_pedidos = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(total_gasto)) %>%
  head(10) %>%
  collect()

resumo

## ---------------------------------------------------------
## 5. Fechar conexão ----
## ---------------------------------------------------------

dbDisconnect(con)

## ---------------------------------------------------------
## FIM DO SCRIPT
## ---------------------------------------------------------
