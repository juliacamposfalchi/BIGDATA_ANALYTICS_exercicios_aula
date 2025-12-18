
CREATE DATABASE loja
GO

USE loja;
GO

-- Criando tabela de clientes
CREATE TABLE Clientes (
  cliente_id INT IDENTITY(1, 1) PRIMARY KEY, -- auto incremento
  nome VARCHAR(100) NOT NULL,
  email VARCHAR(100) UNIQUE NOT NULL,
  telefone VARCHAR(20),
  idade INT,
  cidade VARCHAR(50),
  data_cadastro DATETIME DEFAULT GETDATE()
);

-- Criando tabela de pedidos
CREATE TABLE Pedidos (
  pedido_id INT IDENTITY(1, 1) PRIMARY KEY,
  cliente_id INT,
  data_pedido DATETIME DEFAULT GETDATE(),
  valor_total DECIMAL(10, 2),
  FOREIGN KEY (cliente_id) REFERENCES Clientes(cliente_id)
);

-- Inserindo 15 clientes (com idade e cidade, cidades repetidas)
INSERT INTO Clientes (nome, email, telefone, idade, cidade)
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

-- Inserindo 50 pedidos (clientes repetem compras)
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
