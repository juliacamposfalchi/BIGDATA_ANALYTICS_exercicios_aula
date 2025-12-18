## ---------------------------------------------------------
## AULA 03 - EXERCÍCIO 2
## 1) Fazer uma análise bivariada do conjunto de dados mtcars
## 2) Indicar quais variáveis afetam mais a eficiência (mpg)
## ---------------------------------------------------------

## ---------------------------------------------------------
## 1. Carregar dados ----
## ---------------------------------------------------------

data(mtcars)

mt <- mtcars
mt$modelo <- rownames(mtcars)
rownames(mt) <- NULL

## Variáveis categóricas (no mtcars elas vêm como numéricas)
mt$cyl  <- factor(mt$cyl)
mt$vs   <- factor(mt$vs)
mt$am   <- factor(mt$am)
mt$gear <- factor(mt$gear)
mt$carb <- factor(mt$carb)

## ---------------------------------------------------------
## 2. Análise bivariada ----
## ---------------------------------------------------------

## Separar numéricas e categóricas
vars_numericas   <- sapply(mt, is.numeric)
vars_categoricas <- !vars_numericas

mt_num <- mt[, vars_numericas, drop = FALSE]
mt_cat <- mt[, vars_categoricas, drop = FALSE]

## --- 2.1 Numérica x Numérica: correlações + gráficos ----

## Matriz de correlação
cor_mat <- cor(mt_num)
cor_mat

## Correlação de mpg com outras variáveis numéricas
cor_mpg <- sort(cor_mat["mpg", ], decreasing = TRUE)
cor_mpg

## Dispersões
pairs(mt_num, main = "mtcars: dispersões (variáveis numéricas)")

## Dispersão mpg vs variáveis mais comuns
par(mfrow = c(2, 2))
plot(mt$wt, mt$mpg, main = "mpg vs wt", xlab = "wt", ylab = "mpg")
abline(lm(mpg ~ wt, data = mt), col = "red", lwd = 2)

plot(mt$hp, mt$mpg, main = "mpg vs hp", xlab = "hp", ylab = "mpg")
abline(lm(mpg ~ hp, data = mt), col = "red", lwd = 2)

plot(mt$disp, mt$mpg, main = "mpg vs disp", xlab = "disp", ylab = "mpg")
abline(lm(mpg ~ disp, data = mt), col = "red", lwd = 2)

plot(mt$drat, mt$mpg, main = "mpg vs drat", xlab = "drat", ylab = "mpg")
abline(lm(mpg ~ drat, data = mt), col = "red", lwd = 2)

par(mfrow = c(1, 1))

## --- 2.2 Categórica x Numérica: boxplots e testes ----
## Eficiência (mpg) por grupos

par(mfrow = c(2, 3))
boxplot(mpg ~ cyl, data = mt, main = "mpg por cyl", col = "lightgreen")
boxplot(mpg ~ am,  data = mt, main = "mpg por am",  col = "lightgreen")
boxplot(mpg ~ vs,  data = mt, main = "mpg por vs",  col = "lightgreen")
boxplot(mpg ~ gear,data = mt, main = "mpg por gear",col = "lightgreen")
boxplot(mpg ~ carb,data = mt, main = "mpg por carb",col = "lightgreen")
par(mfrow = c(1, 1))

## Testes para mpg ~ variável categórica
## (am e vs têm 2 níveis => t.test; cyl/gear/carb => ANOVA)

## am
by(mt$mpg, mt$am, summary)
t.test(mpg ~ am, data = mt)

## vs
by(mt$mpg, mt$vs, summary)
t.test(mpg ~ vs, data = mt)

## cyl
by(mt$mpg, mt$cyl, summary)
summary(aov(mpg ~ cyl, data = mt))

## gear
by(mt$mpg, mt$gear, summary)
summary(aov(mpg ~ gear, data = mt))

## carb
by(mt$mpg, mt$carb, summary)
summary(aov(mpg ~ carb, data = mt))

## ---------------------------------------------------------
## 3. Quais variáveis afetam mais a eficiência (mpg)? ----
## ---------------------------------------------------------

## --- 3.1 Ranking simples por |correlação| (somente numéricas) ----

cor_mpg_abs <- sort(abs(cor_mat["mpg", ]), decreasing = TRUE)
cor_mpg_abs

## Remover mpg do ranking
cor_mpg_abs <- cor_mpg_abs[names(cor_mpg_abs) != "mpg"]
cor_mpg_abs

## --- 3.2 Testes de correlação para as 3 mais fortes (numéricas) ----

top3 <- names(cor_mpg_abs)[1:3]
top3

for (v in top3) {
  print(v)
  print(cor.test(mt$mpg, mt[[v]]))
}

## --- 3.3 Regressões lineares (evidência estatística) ----

## Modelos simples
m_wt  <- lm(mpg ~ wt, data = mt)
m_hp  <- lm(mpg ~ hp, data = mt)
m_disp<- lm(mpg ~ disp, data = mt)

summary(m_wt)
summary(m_hp)
summary(m_disp)

## Modelo múltiplo clássico do mtcars
## (nota: mt$am e mt$cyl são fator; o lm trata como dummies)
m_multi <- lm(mpg ~ wt + hp + disp + cyl + am, data = mt)
summary(m_multi)

## Diagnóstico básico do modelo múltiplo
par(mfrow = c(2, 2))
plot(m_multi)
par(mfrow = c(1, 1))

## ---------------------------------------------------------
## 4. Conclusão (orientação para o seu texto) ----
## ---------------------------------------------------------

## Use os outputs acima para argumentar.
## Em geral, no mtcars, a eficiência (mpg) tende a estar mais associada a:
## - peso (wt): correlação negativa forte
## - potência (hp): correlação negativa
## - cilindrada (disp) e número de cilindros (cyl): geralmente negativos
## E transmissão (am) costuma mostrar diferenças de mpg entre grupos.

## ---------------------------------------------------------
## FIM DO SCRIPT
## ---------------------------------------------------------
