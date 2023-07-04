# Carregando os pacotes
library(beepr)
library(tidyverse)
library(psych)
library(psychTools)
library(janitor)
library(sjmisc)
library(careless)
library(readxl)
library(ggthemes)

# O que é R? 

## Cumprimentando o R
print('Olá!')

## Vamos fazer o R cantar?
library(beepr)
beep(sound=8)

# R express: Objetos e classes 

# Converta o vetor escolaridade para fator
escolaridade <- c("Médio", "Superior", "Fundamental", "Fundamental", "Médio")
fator <- as.factor(escolaridade)
class(fator)

# Converta a matriz de dados numero_frutas para data.frame 
numero_frutas <- matrix(1:10,nrow=5, ncol=5)
dataframe <- as.data.frame(numero_frutas)
str(dataframe)

# Pacotes e funções essenciais
# Checando a integridade dos dados
# Objetivo: Corrigir erros estruturais do banco de dados e verificar a integridade das respostas no objeto data_raw

## Abrir o banco de dados
bfi_fake <- read_excel("data_raw/bfi_data.xlsx")

## Visualizar o banco de dados
View(bfi_fake)

# Verificar premissas do tidy data
View(bfi_fake)
glimpse(bfi_fake)

# Identificar o nome das variáveis
names(bfi_fake)

# Número de itens do BFI
bfi_fake %>% 
  select(A1:O5)

# Ponto de atenção 1: nomes de variáveis com caixa alta ou espaço entre elas
bfi_fake <- bfi_fake %>% 
  clean_names() 

names(bfi_fake)

# Ponto de atenção 2: As características pessoais estão fora de ordem no banco de dados
bfi_fake <- relocate(bfi_fake, c("genero","education","idade", "duracao_minutos"), .before = "a1")

# Ponto de atenção 3: A variável ID está com o nome de identidade
bfi_fake <- bfi_fake %>% 
  rename("id" = "identidade")

names(bfi_fake)

# Tipos de variáveis
glimpse(bfi_fake)

# Investigação sobre a variável idade
frq(bfi_fake$idade)

bfi_fake$idade <- as.numeric(bfi_fake$idade)

# Investigação sobre a variável idade
frq(bfi_fake$mh)

bfi_fake<- bfi_fake %>% 
  mutate(mh = na_if(mh, "x"))

frq(bfi_fake$mh)

bfi_fake$mh <- as.numeric(bfi_fake$mh)

# Investigação sobre a variável idade
frq(bfi_fake$wb)

bfi_fake<- bfi_fake %>% 
  mutate(wb = na_if(wb, "x"))

frq(bfi_fake$wb)

bfi_fake$wb <- as.numeric(bfi_fake$wb)

# Checagem de dados duplicados sem a variável id 
get_dupes(bfi_fake, -id)

# Checagem de dados duplicados sem a variável id
get_dupes(bfi_fake, id_estudo, idade, education, genero)

# Removendo os participantes com linhas duplicadas
bfi_fake %>% 
  filter(id_estudo == "2801" | id_estudo == "2804") %>% 
  glimpse()

bfi_fake <- bfi_fake %>% 
  filter(!id %in% c("2805", "2806"))

# Checagem de respostas descuidadas
# Separe as medidas de autorrelato para isso

bfi_fake_careless <- bfi_fake %>% 
  select(id, a1:o5)

bfi_fake_careless$careless <- longstring(bfi_fake_careless, avg = FALSE)

frq(bfi_fake_careless$careless)

bfi_fake_careless %>% 
  filter(careless > 10)

bfi_fake <- bfi_fake %>%   
  filter(!id %in% c("562", "1430", "1555", "2043", "2801", "2802", "2803", "2804"))

# Extra: tempo de resposta e respostas descuidadas

# Tempo de duração

skimr::skim(bfi_fake$duracao_minutos)

bfi_fake %>%
  ggplot(aes(x=duracao_minutos)) +
  geom_histogram(fill="steelblue", color="#e9ecef", alpha=0.6, position = 'identity') +
  theme_hc() +
  labs(fill="")

# Calcular respostas descuidadas via média e DP

# Criar médias por blocos de itens que são apresentados em sequência
bfi_fake <- bfi_fake %>% 
  row_means(a1:e2, n = 2, append = TRUE, var = "bloco1")

bfi_fake <- bfi_fake %>% 
  row_means(e3:o5, n = 2, append = TRUE, var = "bloco2")

# Separar bancos de dados com estes itens
bfi_a1e2 <- bfi_fake %>% 
  dplyr::select(a1:e2)

bfi_e3o5 <- bfi_fake %>% 
  dplyr::select(e3:o5)

# Calcular DPs por linhas
bfi_a1e2 <- transform(bfi_a1e2, sd_bloco1=apply(bfi_a1e2, 1, sd, na.rm=TRUE))
bfi_e3o5 <- transform(bfi_e3o5, sd_bloco2=apply(bfi_e3o5, 1, sd, na.rm=TRUE))

# Combinar os bancos de dados
bfi_a1e2 <- bfi_a1e2 %>% 
  dplyr::select(sd_bloco1)

bfi_e3o5 <- bfi_e3o5 %>% 
  dplyr::select(sd_bloco2)

bfi_fake <- cbind(bfi_fake, bfi_a1e2, bfi_e3o5)

# Gerar gráficos de dispersão com média e DP por blocos

ggplot(bfi_fake, aes(x=bloco1, y=sd_bloco1)) + 
  geom_point(color = "steelblue", alpha = 0.5) +
  theme_hc()

ggplot(bfi_fake, aes(x=bloco2, y=sd_bloco2)) + 
  geom_point(color = "#FC4E07", alpha = 0.5) +
  theme_hc()


# Características pessoais e socioeconômicas
frq(bfi_fake$genero)

frq(bfi_fake$education)

skimr::skim(bfi_fake$idade)

# Criterio de seleção: Manter apenas estudantes com 18 anos ou mais
bfi_fake <- bfi_fake %>%   
  filter(idade >= 18)

skimr::skim(bfi_fake$idade)

# Salvando o banco de dados em /data
saveRDS(bfi_fake, "bfi_fake.RDS")




