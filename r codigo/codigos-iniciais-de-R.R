# Base de dados buit-in do R
mtcars

# imprimindo o tipo da base de dados
class(mtcars)

# dimensionamento da base, (coluna, linhas)
dim(mtcars)

# nomes das variáveis
names(mtcars)

# mais contexto para as variáveis
help(mtcars)

# selecionando o tamanho da base de dados, no R ? incluido o ultimo valor
head(mtcars)
mtcars[1:6,]

# vetores no R começam o seu index no 1
carros = mtcars[,c(1, 2, 4, 6, 9, 10)]
carros

# buscando uma variável dentro do conjunto de dados, temos 2 opções
carros$mpg
# ou
attach(carros)
mpg

# verificando o tipo de todas as variáveis do data frame
str(carros)

# Alterando o tipo da variável.
# A variável "am" ? qualitativa nominal, as variáveis "cyl e gear", são
# quantitativas discretas, mas o banco de dados colocou elas como quan-
# titativas continuas, então, só corrigimos o tipo
carros$am = as.factor(carros$am)
carros$cyl = as.factor(carros$cyl)
carros$gear = as.factor(carros$gear)
str(carros)

# resumo das informações relativas á medidas de posição de uma variável, como média, mediana, min, max etc.
summary(mpg)

# desvio padrão
sd(mpg)

# variancia
var(mpg)


# Crindo uma função para nos informar as estatísticas descritivas, para variáveis quantitativas.
descritivas = function(x)list(
  Média = mean(x),
  Mediana = median(x),
  Max.Min = range(x),
  Amplitude = max(mpg) - min(mpg),
  Variancia = var(x),
  DesvioPadrao = sd(x),
  CoeficienteVariacao = sd(x)/ mean(x) * 100,
  Quantis = quantile(x)
)
descritivas(mpg)

# gerando gráfico vazio
x11()

# grafico de dispersão
plot(mpg)


# gerando a base do gráfico
plot(mpg, type='p', main="Gráfico de Dispersão de Milhas por Galão por Modelo", 
     xlab="", ylab="Milhas por Galão", pch=25, bg=4, col="red", bty="l",
     las=1, cex=1.5, xli=c(0,32), ylim=c(10,35), xaxt="n")

# adicionando o texto de valor acima do ponto
text(mpg, labels=mpg, pos=3, col=1, cex=1.2)

# adicionando as informações do eixo x
axis(1, at=1:32, labels=FALSE)
text(seq(1,32, by=1),
     # mover os nomes para baixo
     par("usr")[3] - 0.2,
     # identificar o texto do eixo x
     labels = row.names(carros),
     # rotacionar o texto em 45 graus
     srt=45,
     # ajustar o quão próximo do eixo está o texto
     adj=1.1,
     # ajustar a região de texto
     xpd = NA,
     # ajustar o tamanho do texto
     cex=0.8)

# adicionar grade ao gráfico
abline(h=10:40, v=1:32, col="gray", lty=3)

# Criando um gráfico de histograma
hist(mpg)
hist(mpg,
     # selecionando intervalos de 2 e 2 unidades
     breaks=seq(10, 34, 2),
     # alterando escala do exio x para os valores entre 10 e 34
     xlim=c(10, 34),
     # nome para o titulo, eixo y e eixo x
     main="Histograma de Milhas por Gal?o",
     ylab="Frequencia", xlab="Milhas por Galão",
     # inserindo cor
     col="lightblue",
     # alterando o tamanho do texto
     xaxt='n', cex.lab=1.5, cex.axis=1.5,
     #alterando a orientação do eixo y
     las=2)
# Definindo o eixo x
axis(1, at=seq(10, 34, by=2), cex.axis=1.5)

# Criando um gráfico de boxplot
# O boxplot é a principal representaçãoo dos intervalos interquartílicos
boxplot(mpg)

# Agrupando gráficos
par(mfrow = c(1,3))
boxplot(mpg, xlab="milhas por Galão")
boxplot(hp, xlab="Potência em Cavalos")
boxplot(wt, xlab="Peso (em 1000 Libras)")

# importando pacotes
# link para aprender instalar pacotes: https://www.youtube.com/watch?v=j4Xs6sr_GWE&ab_channel=MirGlobalAcademy
install.packages("ggplot2")
library(ggplot2)
qplot(hp, mpg, data=carros,
      geom="point")
qplot(hp, mpg, data=carros,
      geom=c("point", "smooth"))

# criando um gráfico com 3 variáveis, a terceira variável é a cilindrada do carro,
# alterando o formado dos pontos, e transformando eles em uma legenda
qplot(hp, mpg, data = carros, colour = cyl, shape = cyl)


# instalando mais pacotes
install.packages("ggpubr")
# a instalação é necessária somente na primeira vez, depois, basta carregar utilizando o comando library()
library("ggpubr")
# Vamos criar uma nova variável, chamada modelo, que irá conter os nomes dos carros analisados.
carros$modelo <- rownames(carros)
# ------------------------GRÁFICO DE BARRAS-------------------------------
# Criando o gráfico de barras (que na verdade será de colunas), Vamos chama-los de Barras.
Barras <- ggbarplot(carros, x = "modelo", y = "mpg",
                    fill = "cyl", # Vamos mudar o preenchimento das barras com as cores dependendo da variável cyl (cilindros)
                    color = "white",
                    # Ajustar as bordas das barras para branco
                    palette = "jco",
                    # Colocar os valores em ordem crescente
                    sort.val = "asc",
                    # Selecionar as informações por grupo
                    sort.by.groups = TRUE,
                    # Rotacionar o texto do eixo x.
                    x.text.angle = 90)
# Para visualizarmos o gráfico, basta utilizar como comando, seu nome.
# Aumentar (ou diminuir) o texto.
Barras + font("x.text", size = 8)
# --------------------------- GRáFICO DE DISPERS?O--------------------
# Criando o Gráfico de Dispersão. Vamos chama-lo de Dispersão.
Dispersao <- ggscatter(carros, x = "wt", y = "mpg",
                       # Adicionar a linha de regressão (tendência)
                       add = "reg.line",
                       # Adicionar intervalo de confiança
                       conf.int = TRUE,
                       # Colori de acordo com a variável cyl (cilindros)
                       color = "cyl", palette = "jco",
                       # Alterar a forma do ponto de acordo com a variável cyl
                       shape = "cyl")
# Acrescentar texto com o Coeficiente de Correlação
stat_cor(aes(color = cyl), label.x = 3)
# Para visualizar o gráfico
Dispersao

# Juntando os dois gráficos
# comando para visualizar o gráfico em uma janela externa
x11()
ggarrange(Barras, Dispersao,
          labels = c("Gráfico de Colunas", "Gráfico de Dispersão"),
          ncol = 2, nrow =1)