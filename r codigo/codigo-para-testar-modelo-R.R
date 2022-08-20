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
