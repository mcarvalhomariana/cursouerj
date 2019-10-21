############FERRAMENTAS RÁPIDAS DE COLETA DE DADOS###########

#Extensão Print => Full Page Screen Capture

#Usar o “importHTML” no Google Sheets => https://docs.google.com/spreadsheets
#=importHTML(“http://www.adorocinema.com/filmes/bilheterias/”, “table”, 1)

#Inspecionar elementos em uma página => botão esquerdo -> inspecionar

#Extensão seleção => SelectorGadget

#Busca por dados Google => https://toolbox.google.com/datasetsearch
#Buscar por “uso de internet brasil”

#Busca avançada Twitter => https://twitter.com/search-advanced

#Netlytic => https://netlytic.org/

##_________________________________##

#########COMANDOS BÁSICOS############

#criar objeto(setinha): alt -

#rodar comando: ctrl(cmd) + enter

#comentário: ctrl(cmd) + shift + C

#salvar: ctrl(cmd) + S

# comando para limpar memória
rm(list = ls())

#pedir ajudar:
help.search("mean")
#ou
??mean

help(mean)
#ou
?mean

#escolher o diretório: ctrl(cmd) + shift + H

getwd() # saber o diretório de trabalho

dir() # para saber o que tem no diretório

setwd("C:/Users/Mariana/Documents/R!/CursoR") # selecionar diretório


##### Instalar pacotes #####

# instalar: install.packages("nome_do_pacote")

# abrir: library(nome_do_pacote)

# Exemplo

install.packages("tidyverse", dependencies = T)

library(tidyverse)

update.packages("") # atualizar pacotes quando necessário

# Atualizar versão do R
library(installr)
updateR


##___________________________________##
##   AULA 1: INTRODUÇÃO AO RSTUDIO   ##
##___________________________________##



#  Calculadora #

3 + 3

6 - 3

2 * 3

6 / 3


#  Criar bases #

x <- c(1, 2, 3, 4)
y <- c("João", "Maria", "Pedro", "Luisa")

tabela <- data.frame(x, y)



# qual o nome das variáveis?

names(tabela)

# qual a categoria das variáveis? (numéricas ou categóricas)

str(tabela)

# sumarizar os dados (melhor para contínuas)

summary(tabela)

# Outra forma:

mean(tabela$x) #média
median(tabela$x) #mediana
sd(tabela$x) #desvio padrão

# Pergunta: alguém sabe o que é desvio-padrão?


### Tabular os dados

x <- c(1, 2, 3, 4)
y <- c("João", "Maria", "Pedro", "Pedro")

tabela2 <- data.frame(x, y)


# números absolutos
table(tabela2$y)

?"$"

# porcentagens
prop.table(table(tabela2$y))

######TIPOS DE OBJETOS#####

#Existem cinco classes básicas de objetos no R:

#character: texto
#numeric: número
#integer
#complex
#logical

class(x)
class(y)


######## IMPORTAR DADOS ########
#______________________________#

library(tidyverse)

#exportar arquivo

topcinema <- read.csv("topcinema.csv", sep=",", encoding = "UTF-8")

View(topcinema)

#renomear variável
topcinema <- topcinema %>%
  rename(Filme = X)

#alterar para minúscula
topcinema <- topcinema %>%
  mutate(Filme = str_to_lower(Filme))

#retirar palavras
topcinema <- topcinema %>%
  mutate(Filme = str_replace_all(Filme, "ver o trailer", ""))

#remover caracteres
topcinema <- topcinema %>%
  mutate(Filme = str_remove_all(Filme, "[1234567890]"))

topcinema <- topcinema %>%
  mutate(Entradas = str_remove_all(Entradas, "[*]"))

#remover espaço
topcinema <- topcinema %>%
  mutate(Entradas = str_remove_all(Entradas, "[ ]"))
#remover espaço
topcinema <- topcinema %>%
  mutate(Acumulado = str_remove_all(Acumulado, "[ ]"))

#alterar tipo do objeto
topcinema<- topcinema %>%
  mutate(Entradas = as.integer(Entradas))

topcinema<- topcinema %>%
  mutate(Acumulado = as.integer(Acumulado))


glimpse(topcinema)

#criar gráficos
topcinema %>%
  group_by(Filme) %>%
  arrange(Acumulado) %>%
ggplot(aes(Filme, Acumulado)) +
  geom_bar(stat="identity")

#Desablitar notação cientifica
options(scipen = 999)

#colorir
topcinema %>%
  group_by(Filme) %>%
  arrange(Acumulado) %>%
  ggplot(aes(Filme, Acumulado)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()

#reordenar
topcinema %>%
  group_by(Filme) %>%
  arrange(Entradas) %>%
  ggplot(aes(x = reorder(Filme, Acumulado), y = Acumulado)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()

#girar
topcinema %>%
  group_by(Filme) %>%
  arrange(Entradas) %>%
  ggplot(aes(x = reorder(Filme, Acumulado), y = Acumulado)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()+
  coord_flip()





