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
  class(mpg)

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

# dicionando as informações do eixo x
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
    
    
# ------------------------- UNIDADE 2-----------------------------------
    
# Instalando e carregando pacotes necessários
  install.packages("datarium")
  install.packages("tidyverse")    
  install.packages("broom")    
  install.packages("dplyr")    
  library("dplyr")
  library("datarium")    
  library("tidyverse")
  library("broom")    
  
# Carregando o banco de dados
  data("marketing", package = "datarium")

# Visualizando as primeiras 25 linhas do banco de dados.
  marketing[1:25,]
  dim(marketing)
  str(marketing)    
  summary(marketing)  
  x11()
  par(mfrow = c(1,4))
  boxplot(marketing$youtube, xlab="Gastos no YT")
  boxplot(marketing$facebook, xlab="Gastos no Face")
  boxplot(marketing$newspaper, xlab="Gastos no Jornal")
  boxplot(marketing$sales, xlab="Vendas")
  
# criação de informações de diagnóstico do modelo
  model <- lm(sales ~ youtube, data = marketing)
  model.diag.metrics <- augment(model)
  head(model.diag.metrics)
  
# Criação do gráfico de dispersão com a inserção da reta que representa o 
# modelo e os resíduos marcados em vermelho
  x11()
  ggplot(model.diag.metrics,aes(youtube,sales)) +
    geom_point()+
    stat_smooth(method = "lm", se = FALSE) +
    geom_segment(aes(xend = youtube, yend=.fitted), color="red", size=1) +
    theme(axis.text.x=element_text(size=rel(1)), text=element_text(size = 20))+
    labs(title = "Gráfico de Dispersão", x="Youtube", y = "Sales")

# ------- VERIFICANDO A NORMALIDADE DOS RESÍDUOS ------------------ 
# Criando um histograma de resíduos 
  x11()
  ggplot(data = marketing, aes(x=model$residuals)) +
    geom_histogram(fill = 'steelblue', color='black') +
    theme_bw() +
    theme(axis.text.x=element_text(size = rel(2))) +
    labs(title = 'Histograma dos Resíduos', x='Resíduos', y='Frequência')

# Criando um outro gráfico para resíduo
# Instalando o pacote necessário para outros tipos de visualizações - nima
  install.packages("nima")
  library("nima")  

# o termo model$residuals nos dá os resíduos encontrados no modelo utilizado.
  x11()
  qq_plot(model$residuals) + theme_bw() +
    theme(axis.text.x = element_text(size=rel(1)), text=element_text(size = 20)) +
    labs(title = 'QQ-Plot.Resíduos', x = 'Quantis Teóricos', y = 'Quantis dos Resíduos do Modelo')
  
# Utilizando o teste nos dados de resíduos do nosso modelo.
  shapiro.test(model$residuals)
# -------------- VERIFICANDO SE A VARIÂNCIA É CONSTANTE (HOMOCEDASTICIDADE) --------

# Gerando gráficos para verificar a homocedasticidades dos resíduos
  x11()
  par(mfrow=c(2,2))
  plot(model)  
  
# Instalando o teste Breusch Pagan
  install.packages("lmtest")
  library("lmtest")  
  bptest(model)
  
# visualizando as primeiras 25 linhas do banco de dados
  marketing[1:25,]

# criando um modelo linear que relaciona a variável "sales
# com a variável "newspaper".  
  modelo <- lm(sales ~ newspaper, data = marketing)
  modelo


# verificando característicase informações sobre o modelo
  summary(modelo)
  
# criação de informações de diagnóstico do modelo (chamado de modelo)
  Diag.modelo = augment(modelo)
  head(Diag.modelo)  

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

  
  
  
  
  
  
  
  
  
    