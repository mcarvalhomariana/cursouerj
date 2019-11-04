##___________________________________##
##      AULA 3: LIMPEZA DOS DADOS    ##
##___________________________________##

# comando para limpar memória
rm(list = ls())

#escolher o diretório: ctrl(cmd) + shift + H

## OU

setwd("C:/Users/Mariana/Documents/R!/CursoR") # selecionar diretório

install.packages("RSQLite", dependencies = T)
install.packages("SnowballC", dependencies = T)

library(RSQLite)
library(tidyverse)
library(SnowballC)
library(janitor)
library(rtweet)


#______________________________________________________________#
###################Tratamento de Dados##########################
#______________________________________________________________#

##Limpar dados coletados do YouTube

#Importar arquivo

arquivos_tab <- list.files(pattern = "*.tab", full.names = TRUE)

lista <- sapply(arquivos_tab, read.delim)

yt_receitas <- lapply(arquivos_tab, read.delim, encoding = "UTF-8") %>% bind_rows()

## Criar coluna dia

glimpse(yt_receitas)

yt_receitas$Data <- strftime(yt_receitas $publishedAtSQL, format="%d/%m/%y")

#Renomear variáveis
yt_receitas  <- yt_receitas  %>%
  rename(Id_Canal = channelId,
         Titulo_Canal = channelTitle,
         Id_Video = videoId,
         Titulo_Video = videoTitle,
         Descrição_Video = videoDescription,
         Duração_segundos = durationSec,
         Visualizacoes = viewCount,
         Likes = likeCount,
         Dislikes = dislikeCount,
         Favoritos = favoriteCount,
         Comentarios = commentCount)

glimpse(yt_receitas)

#Separar canais

panelinha <- yt_receitas %>%
  filter(Titulo_Canal == "Panelinha") %>%
  select(Titulo_Canal,
         Titulo_Video,
         Descrição_Video,
         Duração_segundos,
         Visualizacoes,
         Likes,
         Dislikes,
         Favoritos,
         Comentarios,
         Data)

presuntoveg <- yt_receitas %>%
  filter(Titulo_Canal == "Presunto Vegetariano") %>%
  select(Titulo_Canal,
         Titulo_Video,
         Descrição_Video,
         Duração_segundos,
         Visualizacoes,
         Likes,
         Dislikes,
         Favoritos,
         Comentarios,
         Data)


ambrogui <- yt_receitas %>%
  filter(Titulo_Canal == "Ana Maria Brogui") %>%
  select(Titulo_Canal,
         Titulo_Video,
         Descrição_Video,
         Duração_segundos,
         Visualizacoes,
         Likes,
         Dislikes,
         Favoritos,
         Comentarios,
         Data)

#______________________________________________________________#
###################Tratamento de Texto##########################
#______________________________________________________________#

#Coletar dados do Twitter

#token <- create_token(
  #app = " ",
  #consumer_key = " ",
  #consumer_secret = " ")

#tweets_impbolso <- search_tweets(q= "#ImpeachmantBolsonaroUrgente", n=30000, include_rts = T, retryonratelimit = T, token = token)

#Limpar dados do Twitter
## Criar coluna dia

glimpse(tweets_impbolso)

tweets_impbolso$Data <- strftime(tweets_impbolso$created_at, format="%d/%m/%y")

#Renomear variáveis
impbolso<- tweets_impbolso%>%
  rename(Id_Usuario = user_id,
         Id_Status = status_id,
         Usuario = screen_name,
         Tweet = text,
         Retweet = retweet_count,
         Hashtags = hashtags,
         Favoritos = favorite_count,
         Menções = quote_count,
         Respostas = reply_count)

glimpse(impbolso)

#Separar canais

impbolso <- impbolso %>%
  select(Id_Usuario,
         Id_Status,
         Usuario,
         Tweet,
         Retweet,
         Hashtags,
         Favoritos,
         Menções,
         Respostas)

# Por ser case sensitive, converter strings para minúsculas pode ser útil
impbolso <- impbolso %>%
  mutate(Tweet = str_to_lower(Tweet))

#Limpar o texto

impbolso <- impbolso %>%
  mutate(Tweet = str_replace_all(Tweet, "[aàáâäãåæ]", "a"))

impbolso <- impbolso %>%
  mutate(Tweet = str_replace_all(Tweet,"[eèéêë]", "e"))

impbolso <- impbolso %>%
  mutate(Tweet = str_replace_all(Tweet,"[iìíîï]", "i"))

impbolso <- impbolso %>%
  mutate(Tweet = str_replace_all(Tweet,"[oòóôöõø]", "o"))

impbolso <- impbolso %>%
  mutate(Tweet = str_replace_all(Tweet, "[uùúûü]", "u"))

impbolso <- impbolso %>%
  mutate(Tweet = str_replace_all(Tweet,"ç", "c"))

#tokenizar texto
text_impbolso <- impbolso %>%
  unnest_tokens(word, Tweet) %>%
  select(word)

#Adicionar dicionário de stopwords
stoppt <- read.csv2("stop_mariana.csv")

str(stoppt)
str(text_impbolso)

#transformar stopwords em character
stoppt<- stoppt %>%
  mutate(word = as.character(word))

#retirar stopwords
text_impbolso <- text_impbolso %>%
  anti_join(y = stoppt, by = c("word" = "word"))

#retirar outras palavras
palavrasteimosas <- data.frame(word = c("", "","", ""))

palavrasteimosas<- palavrasteimosas %>%
  mutate(word = as.character(word))

text_impbolso <- text_impbolso%>%
  anti_join(palavrasteimosas, by = "word")

con_text_impbolso <- text_impbolso %>%
  count(word)


