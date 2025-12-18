## Exercicio 1: leitura de .csv e .data -----
install.packages(tidyverse)
library(tidyverse)

adults = read_csv("Aula04/dados/adult.csv")
wine = read_delim("Aula04/dados/wine.data", col_names = F)


## Exercicio 2: leitura excel e googlesheet ----

library(googlesheets4)
gs4_deauth()
gs4_auth()

pinguins = googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1aFu8lnD_g0yjF5O-K6SFgSEWiHPpgvFCF0NY9D6LXnY/edit",
  col_types = "??ddddci",
  na = "NA"
)
nomes = sheet_names(
  "https://docs.google.com/spreadsheets/d/1aFu8lnD_g0yjF5O-K6SFgSEWiHPpgvFCF0NY9D6LXnY/edit"
)
length(nomes)


## Exercício 4: Banco de dados SQL -----

install.packages("DBI")
install.packages("odbc")

library(DBI)
library(odbc)

con <- dbConnect(
  odbc::odbc(),
  Driver = "ODBC Driver 17 for SQL Server",
  Server = "LAFTOS\\SQLEXPRESS", # ou nome do servidor
  #  Database = "testeBigData",      # banco padrão
  UID = "laftos", # ou usar Trusted_Connection = "Yes"
  PWD = "12345"
)

dbExecute(con, "CREATE DATABASE loja")

dbDisconnect(con)

con <- dbConnect(
  odbc::odbc(),
  Driver = "ODBC Driver 17 for SQL Server",
  Server = "LAFTOS\\SQLEXPRESS", # ou nome do servidor
  Database = "loja", # banco padrão
  UID = "laftos", # ou usar Trusted_Connection = "Yes"
  PWD = "12345"
)


dbExecute(
  con,
  "USE loja;
  CREATE TABLE Clientes (
    cliente_id INT IDENTITY(1, 1) PRIMARY KEY, -- auto incremento
    nome VARCHAR(100) NOT NULL,
    email VARCHAR(100) UNIQUE NOT NULL,
    telefone VARCHAR(20),
    idade INT,
    cidade VARCHAR(50),
    data_cadastro DATETIME DEFAULT GETDATE()
  );
  CREATE TABLE Pedidos (
    pedido_id INT IDENTITY(1,1) PRIMARY KEY,  -- Utilizando IDENTITY para auto incremento
    cliente_id INT,
    data_pedido DATETIME DEFAULT GETDATE(),  -- Usando GETDATE() para definir o timestamp atual
    valor_total DECIMAL(10, 2),
    FOREIGN KEY (cliente_id) REFERENCES Clientes(cliente_id)
  );
"
)


dbExecute(
  con,
  "INSERT INTO Clientes (nome, email, telefone, idade, cidade)
    VALUES 
    ('João Silva', 'joao.silva@email.com', '11987654321', 32, 'São Paulo'),
    ('Maria Oliveira', 'maria.oliveira@email.com', '11912345678', 28, 'Rio de Janeiro'),
    ('Carlos Souza', 'carlos.souza@email.com', '11965498732', 45, 'São Paulo'),
    ('Ana Costa', 'ana.costa@email.com', '11976543210', 37, 'Belo Horizonte'),
    ('Paulo Mendes', 'paulo.mendes@email.com', '11987654322', 41, 'Rio de Janeiro'),
    ('Lucas Pereira', 'lucas.pereira@email.com', '11976543211', 30, 'São Paulo'),
    ('Fernanda Lima', 'fernanda.lima@email.com', '11965498733', 26, 'Curitiba'),
    ('Pedro Rocha', 'pedro.rocha@email.com', '11987654323', 39, 'São Paulo'),
    ('Roberta Alves', 'roberta.alves@email.com', '11912345679', 33, 'Belo Horizonte'),
    ('Ricardo Lima', 'ricardo.lima@email.com', '11976543212', 47, 'Rio de Janeiro'),
    ('Juliana Martins', 'juliana.martins@email.com', '11965498734', 29, 'São Paulo'),
    ('Gabriel Souza', 'gabriel.souza@email.com', '11987654324', 35, 'Curitiba'),
    ('Larissa Oliveira', 'larissa.oliveira@email.com', '11912345680', 27, 'Belo Horizonte'),
    ('Mateus Silva', 'mateus.silva@email.com', '11976543213', 40, 'São Paulo'),
    ('Mariana Santos', 'mariana.santos@email.com', '11965498735', 31, 'Rio de Janeiro');

    INSERT INTO Pedidos (cliente_id, valor_total) VALUES
    (1, 150.75),(2, 220.50),(3, 80.25),(4, 190.60),(5, 125.40),
    (6, 250.10),(7, 300.00),(8, 45.90),(9, 500.30),(10, 120.80),
    (11, 160.45),(12, 175.30),(13, 280.60),(14, 199.99),(15, 315.20),
    (1, 99.99),(2, 350.10),(3, 210.75),(4, 415.50),(5, 89.60),
    (6, 260.40),(7, 180.20),(8, 399.99),(9, 150.10),(10, 470.30),
    (11, 230.90),(12, 520.00),(13, 305.60),(14, 410.25),(15, 85.40),
    (1, 275.90),(2, 390.10),(3, 125.30),(4, 510.00),(5, 205.70),
    (6, 325.60),(7, 420.80),(8, 310.25),(9, 140.75),(10, 295.40),
    (11, 370.90),(12, 160.60),(13, 280.20),(14, 200.40),(15, 500.90),
    (1, 610.00),(3, 450.30),(7, 720.50),(12, 315.80),(14, 130.40);
")



campos_cli = dbListFields(con, "Clientes")
clientes = dbReadTable(con, "Clientes")
clientes2 = dbGetQuery(con, "SELECT * FROM Clientes")

campos_ped = dbListFields(con, "Pedidos")
pedidos = dbReadTable(con, "Pedidos")
pedidos2 = dbGetQuery(con, "SELECT * FROM Pedidos")


consulta <- tbl(con, "Pedidos") |>
  filter(valor_total < 100) |>
  left_join(tbl(con, "Clientes")) |>
  select(nome, email, idade, cidade, valor_total) |> 
  group_by(cidade) |> 
  summarise(TOTAL_CIDADE = sum(valor_total))

print(consulta)

dbDisconnect(con)

# dbExecute(con, "ALTER DATABASE loja SET SINGLE_USER WITH ROLLBACK IMMEDIATE;
#                 DROP DATABASE loja;")


## Exercicio 3: leitura de pdf -----

# install.packages("pdftools")
# install.packages("tesseract")
# install.packages("tabulapdf")


library(pdftools)
library(tabulapdf)
# library(tesseract)

ping = pdftools::pdf_text("Aula04/dados/penguins.pdf")
ping2 = tabulapdf::extract_tables("Aula04/dados/penguins2.pdf", col_names = F)
length(ping)


## Exercício 5: API -----
library(httr2)

req <- request("https://restcountries.com/v3.1/independent?fields=name,capital") |> 
  req_perform()

dados <- resp_body_json(req)
length(dados)

dados[[1]]

length(dados)

# Nome oficial e população

dados[[1]]$name$official
dados[[1]]$population

## Exercício 6: Webscraping ----

library(rvest)
library(tidyverse)

url = "https://pt.wikipedia.org/wiki/Lista_de_unidades_federativas_do_Brasil_por_população"

page = read_html(url)

tabelas = page |> html_elements("table")

estados_tbl = tabelas[[2]] |>   
  html_table(header = T)

## Exercício 7: Manipulação de dados

tjsp = pdftools::pdf_text("Aula04/pdf/RemuneracaoServidores201901.pdf")


estados_tbl
estados_tbl |> tail()

estados_tbl[c(1,2), 1]

estados = estados_tbl[-c(1, nrow(estados_tbl)), ]

names(estados) = c("UF", "POP_2025", "POP_2022", "VAR_REL", "VAR_ABS", "PROP_TOTAL", "COMP_PAIS")

estados$POP_2025 = str_replace_all(estados$POP_2025, " ", "") |> as.integer()
estados$POP_2022 = str_replace_all(estados$POP_2022, " ", "") |> as.numeric()

estados |> arrange(POP_2025)



## Sorteio
set.seed(22092025)
nomes = c("Denis", "Diego",  "Felipe", "Julia", "Kamilla", "Talita")
sorteio = sample(1:6) 

dat = data.frame(nomes, sorteio)

# Do maior para o menor 
estados_sort = estados |>
  select(UF, POP_2025) |> 
  arrange(desc(POP_2025))

estados_sort[1:24, ] |> 
  mutate(sorteio = rep(1:6, 4)) |> 
  left_join(dat) |> 
  write.csv2("TJs_sorteados.csv", fileEncoding = "latin2")

