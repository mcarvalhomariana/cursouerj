

##___________________________________##
##      AULA 2: COLETA DE DADOS      ##
##___________________________________##


# comando para limpar memória
rm(list = ls())

getwd() # saber o diretório de trabalho

#escolher o diretório: ctrl(cmd) + shift + H

## OU

setwd("C:/Users/Mariana/Documents/R!/CursoR") # selecionar diretório

install.packages("rtweet", dependencies = T) #coletar dados do Twitter
#https://cran.r-project.org/web/packages/twitteR/twitteR.pdf

install.packages("janitor", dependencies = T) #limpar dados
#https://cran.r-project.org/web/packages/janitor/index.html

install.packages("tidyverse", dependencies = T)

library(rtweet)
library(janitor)
library(tidyverse)

# Token de Acesso

# Criar app em https://developer.twitter.com/en/apps

# Website: https://twitter.com/username
# Callback URL: http://127.0.0.1:1410

# Autenticar

token <- create_token(
  app = "nome do usuario",
  consumer_key = "app key",
  consumer_secret = "app key secret")

help("search_tweets")

#Busca por palavras
maratona <- search_tweets("maratona", n =100, include_rts = F, token = token)
#há um limite de 18 mil tweets a cada 15 minutos

#Buscar trends no Rio de Janeiro
trends <- trends_available(token = NULL, parse = TRUE)
#woeid Rio de Janeiro:455825
trendsrj <- get_trends(woeid = 455825)

#Seguidores
seguecrivella <- get_followers("MCrivella", retryonratelimit = T) #não rodar, o retorno será muito grande
crivellasegue <- get_friends("MCrivella", retryonratelimit = T)

#Buscar com data e idioma
chile <- search_tweets("chile since:2019-10-25 until:2019-10-27 lang:pt", n=1000, include_rts = T, token=token)


#Usuários mais engajados na postagem de tweets

#atalho para pipe: ctrl+shift+M

#tabela
usuarios <- data.frame(table(chile$screen_name)) %>%
  rename(Usuário = Var1,
         N = Freq) %>%
  arrange(-N) %>%
  head(10)


#gráfico
ggplot(usuarios, aes(reorder(Usuário, N), N)) +
  geom_bar(stat = "identity", fill= "deeppink2") +
  coord_flip()


#adicionar data

chile$data <- strftime(chile$created_at, format="%d/%m")


#quais tweets foram mais compartilhados?

textchile <- chile %>%
  group_by(text) %>% #agrupar por
  summarise(n = n()) %>% #organizar por n?mero, quantidade
  na.omit()  %>% # não quero os casos sem url
  arrange(-n) %>% #reordenar de maior para menor
  head(10)

toptextchile <- chile %>%
  select(text, screen_name, created_at)

toptextchile <- toptextchile %>%
  filter(text == "América Latina inteira desestabilizada. Até o Chile, que é o país mais desenvolvido da região. Eleição do PR Bolsonaro é a real barreira contra o domínio esquerdista coordenado por Cuba e Venezuela. Parece bíblico, mas a esperança vem do Brasil. Cabe a nós barrarmos essa ameaça.")

#quais perfis twitaram mais?

chile%>%
  group_by(screen_name) %>% #agrupar por screen_name
  summarise(n = n()) %>% #fazendo uma operaçao matemática de contagem do n
  arrange(-n) %>% #organizar do maior para o menor
  head(10) #só os dez primeiros

chile%>%
  group_by(screen_name) %>%
  summarise(n = n()) %>%
  arrange(-n) %>%
  head(10) %>%
  ggplot(aes(reorder(screen_name, n), n)) + #gráfico de barras
  geom_bar(stat = "identity", fill="blueviolet") +
  coord_flip()

#que dia teve mais tweets?
chile%>%
  group_by(data) %>% #agrupar por screen_name
  summarise(n = n()) %>% #fazendo uma operaçao matemática de contagem do n
  arrange(-n) #organizar do maior para o menor

#vai dar apenas um dia porque a coleta é pequena, fazendo uma coleta maior, teremos mais dados

#quais as hashtags mais tuitadas?

chilehashtags <- chile %>%
  unnest(hashtags) %>% # unnest transforma colunas-lista em novas linhas
  group_by(hashtags) %>%
  summarise(n = n()) %>% #contar o número de hashtags
  na.omit()  %>% # eliminar os espaços vazios
  arrange(-n) %>%
  head(10)

chilehashtags %>%
  ggplot(aes(reorder(hashtags, n), n)) +
  geom_bar(stat = "identity", fill="coral1") +
  coord_flip()


#cores no R: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf


#############OUTRAS FORMAS DE COLETA##############

#Dados do YouTube: https://tools.digitalmethods.net/netvizz/youtube/

#Outros: https://netlytic.org/home/


#________________________________________________#
################EXERCÍCIOS########################
#________________________________________________#

#Fazer uma busca com n=100 de um termo ou hashtag ligado a sua pesquisa (coletar os retweets)

#Responder as seguintes questões:

#Quais os 15 perfis que mais tuitaram?

#Quais as 5 hashtags mais utilizadas?

#Quais os posts mais retweetados?

#Fazer um gráfico de barras com os retweets mais frequentes.





