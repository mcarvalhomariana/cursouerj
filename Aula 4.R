##___________________________________##
##      AULA 3: LIMPEZA DOS DADOS    ##
##___________________________________##

# comando para limpar memória
rm(list = ls())

#escolher o diretório: ctrl(cmd) + shift + H

## OU

setwd("C:/Users/Mariana/Documents/R!/CursoR") # selecionar diretório

library(tidyverse)
library(SnowballC)
library(wordcloud)
library(janitor)
library(tidytext)

########## GRÁFICOS #############
#-------------------------------#

#Desablitar notação cientifica
options(scipen = 999)

################Gráfico de barras#################

#Visualizações em cada canal

glimpse(yt_receitas)

yt_receitas %>%
  group_by(Titulo_Canal)%>%
  summarise(Visualizacoes = sum(Visualizacoes,  na.rm = T)) %>% #contar visualizações
  ggplot(aes(Titulo_Canal, Visualizacoes)) + #gráfico
  geom_bar(stat="identity") #barras

yt_receitas %>%
  group_by(Titulo_Canal)%>%
  summarise(Visualizacoes = sum(Visualizacoes,  na.rm = T)) %>%
  ggplot(aes(x=Titulo_Canal, y=Visualizacoes, fill=Titulo_Canal)) +
  geom_bar(stat="identity")+
  labs(x = "Canal", y = "Visualizações") + #alterar nome das etiquetas
  labs(fill = " ")+ #não preencher legenda
  scale_fill_brewer(palette="Dark2")+ #definir cor
  theme_minimal() #definir tema

yt_receitas %>%
    group_by(Titulo_Canal)%>%
    summarise(Visualizacoes = sum(Visualizacoes,  na.rm = T)) %>%
    ggplot(aes(x=Titulo_Canal, y=Visualizacoes, fill=Titulo_Canal)) +
    geom_bar(stat="identity")+
    labs(x = "Canal", y = "Visualizações") +
    labs(fill = " ")+
    scale_fill_brewer(palette="Dark2")+
    theme_minimal()+
    theme(legend.position="none")

##https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/

#Likes e dislikes

library(reshape2)

glimpse(yt_receitas)

#Presunto Vegetariano
yt_receitas %>%
  filter(Titulo_Canal == "Presunto Vegetariano") %>% #selecionar canal
  select(Titulo_Video, Dislikes, Likes)%>%  #selecionar dados que quero
  group_by(Titulo_Video) %>%  #agrupar
  summarise(Dislikes = sum(Dislikes,  na.rm = T),
            Likes = sum(Likes,  na.rm = T)) %>% #contar as interações
  top_n(10)%>% #top10
  melt(id.vars = c("Titulo_Video"), measure.vars = c("Dislikes", "Likes")) %>% #criar novas variáveis
  ggplot(aes(x=Titulo_Video, y=value, fill=variable)) +
  geom_bar(stat="identity")+
  labs(x = " ", y = " ") +
  labs(fill=" ")+
  coord_flip()

#Vai dar erro, vamos tentar outra vez, especificando a função "melt"
yt_receitas %>%
  filter(Titulo_Canal == "Presunto Vegetariano") %>% #selecionar canal
  select(Titulo_Video, Dislikes, Likes)%>%  #selecionar dados que quero
  group_by(Titulo_Video) %>%  #agrupar
  summarise(Dislikes = sum(Dislikes,  na.rm = T),
            Likes = sum(Likes,  na.rm = T)) %>% #contar as interações
  top_n(10)%>% #top10
  reshape2::melt(id.vars = c("Titulo_Video"), measure.vars = c("Dislikes", "Likes")) %>% #criar novas variáveis
  ggplot(aes(x=Titulo_Video, y=value, fill=variable)) +
  geom_bar(stat="identity")+
  labs(x = " ", y = " ") +
  labs(fill=" ")+
  coord_flip()

#Panelinha
yt_receitas %>%
  filter(Titulo_Canal == "Panelinha") %>%
  select(Titulo_Video, Dislikes, Likes)%>%
  group_by(Titulo_Video)  %>%
  summarise(Dislikes = sum(Dislikes,  na.rm = T),
            Likes = sum(Likes,  na.rm = T)) %>%
  top_n(10)%>%
  reshape2::melt(id.vars = c("Titulo_Video"), measure.vars = c("Dislikes", "Likes")) %>%
  ggplot(aes(x=Titulo_Video, y=value, fill=variable)) +
  geom_bar(stat="identity")+
  labs(x = " ", y = " ") +
  labs(fill=" ")+
  coord_flip()

#Ana Maria Brogui
yt_receitas %>%
  filter(Titulo_Canal == "Ana Maria Brogui") %>%
  select(Titulo_Video, Dislikes, Likes)%>%
  group_by(Titulo_Video)  %>%
  summarise(Dislikes = sum(Dislikes,  na.rm = T),
            Likes = sum(Likes,  na.rm = T)) %>%
  top_n(10)%>%
  reshape2::melt(id.vars = c("Titulo_Video"), measure.vars = c("Dislikes", "Likes")) %>%
  ggplot(aes(x=Titulo_Video, y=value, fill=variable)) +
  geom_bar(stat="identity")+
  labs(x = " ", y = " ") +
  labs(fill=" ")+
  coord_flip()

##################Gráfico de Linhas#####################

glimpse(yt_receitas)

## Criar coluna ano

yt_receitas$Ano <- strftime(yt_receitas$publishedAtSQL, format="%y")

glimpse(yt_receitas)

yt_receitas%>%
  group_by(Ano, Titulo_Canal) %>% #agrupar por mês
  summarise(Visualizacoes = sum(Visualizacoes,  na.rm = T)) %>% #contagem
  ggplot(aes(x=Ano, y=Visualizacoes, group=Titulo_Canal)) + #gráfico de linha sem grupo
  geom_line(aes(color=Titulo_Canal))+ theme_classic() +
  scale_color_manual(values=c('orange','royalblue3', 'red3'))+
  labs(x = "", y = "", title = " ")

################Gráfico de pontinhos####################

#Presunto Vegetariano
yt_receitas %>%
  filter(Titulo_Canal == "Presunto Vegetariano") %>%
  select(Titulo_Video, Data, Dislikes, Likes)%>%
  reshape2::melt(id.vars = c("Data"), measure.vars = c("Dislikes", "Likes")) %>%
  ggplot() +
  geom_point(mapping = aes(x = Data, y = value, color = variable))+
  labs(x = "Data", y = " ", color = "Interação")+
  theme(axis.text.x=element_blank(), #remover a data
        axis.ticks.x=element_blank()) #remover os tracinhos

#Panelinha
yt_receitas %>%
  filter(Titulo_Canal == "Panelinha") %>%
  select(Titulo_Video, Data, Dislikes, Likes)%>%
  reshape2::melt(id.vars = c("Data"), measure.vars = c("Dislikes", "Likes")) %>%
  ggplot() +
  geom_point(mapping = aes(x = Data, y = value, color = variable))+
  labs(x = "Data", y = " ", color = "Interação")+
  theme(axis.text.x=element_blank(), #remover a data
        axis.ticks.x=element_blank()) #remover os tracinhos


#Ana Maria Brogui
yt_receitas %>%
  filter(Titulo_Canal == "Ana Maria Brogui") %>%
  select(Titulo_Video, Data, Dislikes, Likes)%>%  #tabela com o nome dos candidatos e reações
  reshape2::melt(id.vars = c("Data"), measure.vars = c("Dislikes", "Likes")) %>%
  ggplot() +
  geom_point(mapping = aes(x = Data, y = value, color = variable)) +
  labs(x = "Data", y = " ", color = "Interação")+
  theme(axis.text.x=element_blank(), #remover a data
        axis.ticks.x=element_blank()) #remover os tracinhos

#-------------------------------#
#######NUVEM DE PALAVRAS#########

install.packages("wordcloud2")

library(tidytext)
library(wordcloud)

??wordcloud

#ver processo anterior na aula 3

wordcloud(con_text_impbolso$word,con_text_impbolso$n, scale=c(8,1),min.freq=50,
          max.words=200, random.order=FALSE)

#retirar outras palavras
palavrasteimosas <- data.frame(word = c("t.co", "so","mas", "aw5v9ouk6a" ))

palavrasteimosas<- palavrasteimosas %>%
  mutate(word = as.character(word))

text_impbolso <- text_impbolso%>%
  anti_join(palavrasteimosas, by = "word")

con_text_impbolso <- text_impbolso %>%
  count(word)

pal <- brewer.pal(6,"Dark2")

wordcloud(con_text_impbolso$word,con_text_impbolso$n, scale=c(6,1),min.freq=50,
          max.words=100, random.order=FALSE, rot.per=.3, colors = pal)

#ajustar a escala para caber a #
wordcloud(con_text_impbolso$word,con_text_impbolso$n, scale=c(4,1),min.freq=50,
          max.words=100, random.order=FALSE, rot.per=.3, colors = pal)

#e para salvar na minha pasta?

png("nuvem.png", width=1280,height=800)

wordcloud(con_text_impbolso$word,con_text_impbolso$n, scale=c(4,1),min.freq=50,
          max.words=100, random.order=FALSE, rot.per=.3, colors = pal)

dev.off()

