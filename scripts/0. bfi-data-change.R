# Carregando os pacotes
library(tidyverse)
library(psychTools)
library(writexl)

# Alteração de dados para fins didáticos apenas, não reutilizar esse banco de dados
# O banco de dados original e sem alterações pode ser recuperado via data(bfi)[pacote psychTools]
help(bfi)

# Carregar o banco de dados
data(bfi)
data("bfi.dictionary")

# Exportar o banco de dados
write_xlsx(bfi, "bfi_data.xlsx")

# Exportar o banco de dados
write_xlsx(bfi.dictionary, "bfi_dic.xlsx")
