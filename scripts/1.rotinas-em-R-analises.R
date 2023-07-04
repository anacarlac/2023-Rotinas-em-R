# Carregando os pacotes
library(tidyverse)
library(psych)
library(psychTools)
library(janitor)
library(sjmisc)
library(careless)
library(formattable)
library(qgraph)
library(corrplot)
library(lavaan)

# Carregar o banco de dados limpo
bfi_fake <- readRDS("data/bfi_fake.RDS")

# Análises descritivas

# Funções do R
mean(bfi_fake$idade)
sd(bfi_fake$idade)

table_idade_dplyr <- bfi_fake %>% 
  summarise(
    mean = mean(idade, na.rm=TRUE),
    sd = sd(idade, na.rm=TRUE)
  )

formattable(table_idade_dplyr)

# Exemplos práticos

# Gênero
table_gender <- frq(bfi_fake$genero)
table_gender <- table_gender %>% 
  as.data.frame() %>% 
  dplyr::select(-variable, -label) %>% 
  dplyr::filter(val %in% c("1","2"))

formattable(table_gender)

# Estatística descritiva
table_education <- frq(bfi_fake$education)
table_education <- table_education %>% 
  as.data.frame() %>% 
  dplyr::select(-variable, -label) %>% 
  dplyr::filter(val %in% c("1","2", "3", "4", "5"))

formattable(table_education)

# Idade
hist(bfi_fake$idade)
boxplot(bfi_fake$idade)

skimr::skim(bfi_fake$age)

# Respostas de autorrelato

# Personalidade
bfi_only <- bfi_fake %>% 
  select(a1:o5)

skimr::skim(bfi_only)

# Evidências de validade e confiabilidade do instrumento

# Checagem da matriz de correlação: Pearson x Policórica
v_bfi <- bfi_fake %>% 
  dplyr::select(a1:o5) %>% 
  colnames()

pearson_cor <- cor(bfi_fake[,v_bfi], 
                   use="pairwise.complete.obs")

corrplot(pearson_cor, method="color", 
         number.cex = 0.4, 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45,
         diag=TRUE)

poly_cor <- cor_auto(bfi_fake, select=v_bfi)

corrplot(poly_cor, method="color", 
         number.cex = 0.4, 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45,
         diag=TRUE)

# Scree plot
scree(poly_cor, factors = TRUE, main = "Scree plot")

paral_bfi <- fa.parallel(poly_cor, n.obs=2545, fm = "minres", fa = "fa",
            nfactors = 5, main = "Análise paralela")

paral_bfi 

# EFA 
efa_bfi <- efa(
  bfi_fake, 
  nfactors = 4:6, 
  rotation = "oblimin", 
  ov.names= v_bfi, 
  ordered=TRUE
)

# Verificar resultados de cargas fatoriais
summary(efa_bfi, 
        standardized = TRUE, 
        fit.measures = TRUE, 
        cutoff = 0.1)

# Verificar resultados de model fit
fitMeasures(efa_bfi)

# Checar os residuos 

# 4 fatores
efa_bfi_res4 <- fa(poly_cor, 
                   nfactors= 4,
                   n.obs=2545,
                   rotate="oblimin",
                   fm = "wls")

res4 <- efa_bfi_res4$residual

corrplot(res4, method="color", 
         number.cex = 0.35, 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45,
         diag=TRUE)

# 5 fatores
efa_bfi_res5 <- fa(poly_cor, 
                  nfactors= 5,
                  n.obs=2545,
                  rotate="oblimin",
                  fm = "wls")

res5 <- efa_bfi_res5$residual

corrplot(res5, method="color", 
         number.cex = 0.35, 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45,
         diag=TRUE)

# 6 fatores
efa_bfi_res6 <- fa(poly_cor, 
                   nfactors= 6,
                   n.obs=2545,
                   rotate="oblimin",
                   fm = "wls")

res6 <- efa_bfi_res6$residual

corrplot(res6, method="color", 
         number.cex = 0.35, 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45,
         diag=TRUE)

# Target rotation

# Carregar os keys
targ <- read_excel("data/bfi_dic.xlsx", sheet = "target")

targ <- as.matrix(targ)

# Rodar a análise
efa_bfi_target <- fa(poly_cor,nfactors = 5, n.obs=2545,
                     rotate="TargetQ",Target=list(targ))  #targeted oblique rotation

efa_bfi_target

# Confiabilidade

# A
jmv::reliability(data = bfi_fake, 
                 vars = vars(a1, a2, a3, a4, a5), 
                 revItems = c(a1),  
                 omegaScale = TRUE,  
                 alphaItems = TRUE,  
                 omegaItems = TRUE)
# C
jmv::reliability(data = bfi_fake, 
                 vars = vars(c1, c2, c3, c4, c5),  	
                 revItems = c(c4,c5),  
                 omegaScale = TRUE,  
                 alphaItems = TRUE,  
                 omegaItems = TRUE)


# E 
jmv::reliability(data = bfi_fake, 
                 vars = vars(e1, e2, e3, e4, e5),  	
                 revItems = c(e1,e2),  
                 omegaScale = TRUE,  
                 alphaItems = TRUE,  
                 omegaItems = TRUE)
# N
jmv::reliability(data = bfi_fake, 
                 vars = vars(n1, n2, n3, n4, n5),  	
                 #revItems = c(c4,c5),  
                 omegaScale = TRUE,  
                 alphaItems = TRUE,  
                 omegaItems = TRUE)

# O
jmv::reliability(data = bfi_fake, 
                 vars = vars(o1, o2, o3, o4, o5),  	
                 revItems = c(o2,o5),  
                 omegaScale = TRUE,  
                 alphaItems = TRUE,  
                 omegaItems = TRUE)

# Computar escores para identificar possíveis diferenças entre grupos

key_list <- list(A = c("-a1", "a2", "a3", "a4", "a5"),
                 C = c("c1", "c2", "c3", "-c4", "-c5"),
                 E = c("-e1", "-e2", "e3", "e4", "e5"),
                 N = c("n1", "n2", "n3", "n4", "n5"),
                 O = c("o1", "-o2", "o3", "o4", "-o5")
)

bfi_fake <- bfi_fake %>% 
  select(v_bfi) %>% 
  drop_na()

bfi_fake <- bfi_fake %>%
  # select(it_hol) %>% 
  drop_na(v_bfi)

bfi_scores <- psych::scoreItems(key_list, 
                                 bfi_fake, 
                                 totals = TRUE,
                                 #missing=FALSE, 
                                 min = 1, 
                                 max = 6, 
                                 digits = 2)

bfi_fake_final <- cbind(bfi_fake, bfi_scores$scores)

# Correlações convergentes e discriminantes

v_conv_disc <- c("mh", "wb", "logic_rac", "diferenciacao_interesses")
v_personality <- c("O", "C", "E", "A", "N")
                        
corr.test(bfi_fake_final[,v_conv_disc], y = bfi_fake_final[,v_personality], use = "pairwise")



