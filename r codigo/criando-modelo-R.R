# -------------------- UNIDADE 3 -----------------------------

# Importando a base de dados iris
head(iris)

# Gerando gráfico de dispersão
library("ggplot2")
x11()  
scatter <- ggplot(data=iris, aes(x = Petal.Length, y=Petal.Width))
scatter + geom_point(aes(color=Species, shape=Species), size = 3) +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1)), text=element_text(size=20)) +
  labs(title = 'Largura x Comprimento de Pétalas',
       x = 'Comprimento das Pétalas', y='Largura das Pétalas')

# criando o modelo para variável dependente largura e independente o comprimento
# lm(variável dependente ~ variável independente, data=conjunto de dados)
model <- lm(Petal.Width ~ Petal.Length, data=iris)
model
summary(model)
bptest(model)

# gerando o gráfico do modelo
library("ggplot2")

x11()
scatter <- ggplot(data=iris, aes(x = Petal.Length, y = Petal.Width))
scatter + geom_point(aes(color=Species, shape=Species), size=3) +
  geom_smooth(method = "lm", se=FALSE, colour="red") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1)), text=element_text(size=20)) +
  labs(title = 'Largura x Comprimento de Pétalas',
       x='Comprimento das Pétalas', y='Largura das Pétalas')

# criando um gráfico interativo para comparar as variáveis independente com a 
# a variável dependente da base de dados marketing

install.packages("pairsD3")
library("pairsD3")  
shinypairs(marketing)  

# pagina 101 do livro modelagem de dados, para entender como implementar
# esses valores em uma matriz, mas nunca iremos usar matriz.
# se o Pr for maior que o t_valor, a variável não tem muito valor
# no resultado do modelo, então tirar ela, não faz diferença.
# tirar variáveis é muito bom para aliviar força computacional.
modelo2 = lm(sales ~ youtube + facebook + newspaper, data=marketing)
summary(modelo2)
# --- removendo a variável newspaper
modelo2 = lm(sales ~ youtube + facebook, data=marketing)
summary(modelo2)
# bom, comparamos as variáveis para ver se eram lineares
# agr vamos verificar os residuos. 
# os residuos do modelo simples é menor " 0.6119"
modelo.a = lm(sales ~ youtube, data=marketing)
summary(modelo.a)
# agora os residuos do modelo multiplo é "0.8972"
# quanto maior o residuo, melhor para o modelo
modelo2 = lm(sales ~ youtube + facebook, data=marketing)
summary(modelo2)

# mas só comparar os residuos não é suficiente, vamos usar uma função
# de comparação de modelos para saber qual é melhor
modelo.multiplo = lm(sales ~ youtube + facebook, data = marketing)
modelo.simples = lm(sales ~ youtube, data = marketing) 
# para saber qual modelo usar, basta olhar a coluna Pr(>F)
# o valor aparece na linha 2
anova(modelo.simples, modelo.multiplo)  

# ---------- modelos não lineares -----------------------
install.packages("AER")
library(AER)
data("CASchools") 
head(CASchools)  
# criando uma coluna com o nome de pontuação, somando as variáveis read e math
CASchools$pontuacao = (CASchools$read + CASchools$math)
# visualizando com um gráfico de dispersão
x11()
plot(CASchools$income, CASchools$pontuacao,
     col = 'steelblue',
     pch = 20, lwd = 1,
     xlab = "Renda do Distrito (Milhares de Dólares",
     ylab = "Pontuação",
     cex.lab = 1.5, cex.main = 2,
     main = "Pontuação X Renda do Distrito")

# Como a dispersão parece ser logarítma ou quadrática, vamos usar um modelo
# de regressão quadrático
Modelo.Quad = lm(pontuacao ~ income + I(income^2), 
                 data = CASchools)
summary(Modelo.Quad)

x11()
plot(CASchools$income, CASchools$pontuacao,
     col = "steelblue",
     pch = 20, lwd=5, 
     xlab = "Renda do Distrito (Milhares de Dólares)", 
     ylab = "Pontuação",
     cex.lab = 1.5,cex.main=2,
     main = "Pontuação X Renda do Distrito")
order_id <- order(CASchools$income)
# Acrescentando o Modelo Quadrático
lines(x = CASchools$income[order_id], 
      y = fitted(Modelo.Quad)[order_id],
      col = "red", lty=2,
      lwd = 2) 
# Acrescentando o Modelo Logaritmico
lines(CASchools$income[order_id], 
      fitted(Modelo.Log)[order_id], 
      col = "green", 
      lwd = 2)
#Acrescentando a Legenda.
legend("bottomright",
       legend = c("Modelo Quadrático", "Modelo Linear-Log"),
       lwd = 2,
       col = c("red", "green"), lty=c(2,1))

