# setwd("~/Downloads/Tese Priscilla/r-codes") # versão inicial em 30/04/2022.
# versão atual em projeto intitulado "espaco-virtual-e-geracoes" no GitHub
setwd("/Users/jpalbino/Library/Mobile Documents/com~apple~CloudDocs/GitHub/espaco-virtual-e-geracoes")

library(readxl)
dbf <- read_excel("dados/dados brutos formatados.xlsx")

string_idade <- c(14, 17,20,30,40,50,60,70,80)
names(string_idade) <- c("de 11 a 14 anos.", "de 14 a 17 anos.", "de 17 a 20 anos.",
                         "de 20 a 30 anos.", "de 30 a 40 anos.", "de 40 a 50 anos.",
                         "de 50 a 60 anos.", "de 60 a 70 anos.", "acima de 70 anos.")

for (i in 1:nrow(dbf)) {
  if (!is.na(as.numeric(dbf$idade[i]))) {
    for (j in 1:length(string_idade)) {
      if (as.numeric(dbf$idade[i]) < string_idade[[j]]) {
        dbf$idade[i] <- names(string_idade[j])
        break
      }
    }
  }
}
  
library(writexl)
write_xlsx(
  dbf,
  path ="dados/dados_brutos_transformados.xlsx",
  col_names = TRUE,
  format_headers = TRUE,
  use_zip64 = FALSE
)


# ------
# para a segunda etapa
library(readxl)
dbf02 <- read_excel("dados/dados_brutos_transformados.xlsx", 
                      +     sheet = "trabalho", col_names = FALSE)
# limpando
dbf02$faixa = dbf02$...1
dbf02$...1 = NULL

#define function to calculate mode
#source: https://www.statology.org/mode-in-r/#:~:text=The%20statistical%20software%20R%20does%20not%20have%20a,show%20how%20to%20use%20this%20function%20in%20practice.
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

#find mode
find_mode(dbf02$faixa)

#Mode in R: How to Find Mode of Vectors
#source: https://r-lang.com/mode-in-r/
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

result <- getmode(dbf02$faixa)
print(result) 

# ----------
# Para definir estilos de uso da planilha "Alejandro"

library(readxl)
library(writexl)

df_alex <- read_excel("dados/dados_brutos_transformados_Original.xlsx", 
                      sheet = "alejandro")
View(df_alex)

# Estabelecendo as colunas com as ocorrências dos tipos A, B, C, D
tipo_A <- c(6, 11, 16, 19, 25, 28, 37, 40, 44, 45)
tipo_B <- c(7, 10, 15, 20, 24, 29, 36, 38, 39, 41)
tipo_C <- c(8, 12, 14, 21, 23, 30, 32, 33, 35, 42)
tipo_D <- c(9, 13, 17, 18, 22, 26, 27, 31, 34, 43)

# Variáveis de controle a serem acrescentadas
conta_A = conta_B = conta_C = conta_D = 0
estilo_uso = "" 

# Incluindo variáveis no dataframe para receber as somatórias dos Estilos de Uso
df_alex <- cbind(df_alex, conta_A, conta_B, conta_C, conta_D, estilo_uso)

# Calculando as ocorrências dos tipos A, B, C e D 
for (i in 1:nrow(df_alex)) {
  for (j in 1:length(tipo_A)) {
    if (!is.na(df_alex[i, tipo_A[j]])) conta_A = conta_A + 1
    if (!is.na(df_alex[i, tipo_B[j]])) conta_B = conta_B + 1
    if (!is.na(df_alex[i, tipo_C[j]])) conta_C = conta_C + 1
    if (!is.na(df_alex[i, tipo_D[j]])) conta_D = conta_D + 1
  }
# Atualizando o conteúdo das variáveis no arquivo
  df_alex$conta_A[i] <- conta_A
  df_alex$conta_B[i] <- conta_B
  df_alex$conta_C[i] <- conta_C 
  df_alex$conta_D[i] <- conta_D

# Definido qual Tipo de Uso de maior ocorrência:
  if((df_alex$conta_A[i] == 0) && (df_alex$conta_B[i] == 0) && (df_alex$conta_C[i] == 0) && (df_alex$conta_D[i] == 0))
    qual_maior_contagem = 0
  else
    qual_maior_contagem <- which.max(c(df_alex$conta_A[i], df_alex$conta_B[i], df_alex$conta_C[i], df_alex$conta_D[i]))
  
  if(qual_maior_contagem == 0) 
    df_alex$estilo_uso[i] = "Não definido !" 
  else 
    df_alex$estilo_uso[i] = switch(qual_maior_contagem, 
                                   "Estilo de Uso A - Uso Participativo no Espaço Virtual", 
                                   "Estilo de Uso B - Busca e Pesquisa no Espaço Virtual", 
                                   "Estilo de Uso C - Estruturação e Planejamento no Espaço Virtual", 
                                   "Estilo de Uso D - Ação Concreta e Produção no Espaço Virtual")
 
  conta_A = conta_B = conta_C = conta_D = 0
  
  # Mais alguma outra ocorrência igual à maior ?
  
  if ((qual_maior_contagem > 0) && (qual_maior_contagem < 4)) {
    # Acertando os indices
    outra = c(46, 47, 48, 49)
    # procurando a segunda ocorrência    
    for (k in (qual_maior_contagem + 1):4){
      if (df_alex[i, outra[k]] == df_alex[i, outra[qual_maior_contagem]]) 
        df_alex[i, 50] = switch(k, "", paste(df_alex[i, 50], "e Estilo de Uso B - Busca e Pesquisa no Espaço Virtual"),
                                       paste(df_alex[i, 50], "e Estilo de Uso C - Estruturação e Planejamento no Espaço Virtual"),
                                       paste(df_alex[i, 50], "e Estilo de Uso D - Ação Concreta e Produção no Espaço Virtual"))
    }
  }
  
  qual_maior_contagem = 0
}

# Gravando nova planilha com os resultados
write_xlsx(
  list("alejandro" = df_alex),
  path ="dados/dados_transformados_alejandro.xlsx",
  col_names = TRUE,
  format_headers = TRUE,
  use_zip64 = FALSE
)

# ------------
# Para definir estilos de uso da planilha "espanha" de Maria Carmen López Berlanga

library(readxl)
library(writexl)

dfesp <- read_excel("dados/dados_brutos_transformados_Original.xlsx", sheet = "espanha")
View(dfesp)

# Estabelecendo as colunas com as ocorrências dos tipos A, B, C, D
tipo_A <- c(6, 11, 16, 19, 25, 28, 37, 40, 44, 45)
tipo_B <- c(7, 10, 15, 20, 24, 29, 36, 38, 39, 41)
tipo_C <- c(8, 12, 14, 21, 23, 30, 32, 33, 35, 42)
tipo_D <- c(9, 13, 17, 18, 22, 26, 27, 31, 34, 43)

# Variáveis de controle a serem acrescentadas
conta_A = conta_B = conta_C = conta_D = 0
estilo_uso = "" 

# Incluindo variáveis no dataframe para receber as somatórias dos Estilos de Uso
dfesp <- cbind(dfesp, conta_A, conta_B, conta_C, conta_D, estilo_uso)

# Calculando as ocorrências dos tipos A, B, C e D 
for (i in 1:nrow(dfesp)) {
  for (j in 1:length(tipo_A)) {
    if (dfesp[i, tipo_A[j]] == 2) conta_A = conta_A + 1
    if (dfesp[i, tipo_B[j]] == 2) conta_B = conta_B + 1
    if (dfesp[i, tipo_C[j]] == 2) conta_C = conta_C + 1
    if (dfesp[i, tipo_A[j]] == 2) conta_D = conta_D + 1
  }
# Atualizando o conteúdo das variáveis no arquivo
  dfesp$conta_A[i] <- conta_A
  dfesp$conta_B[i] <- conta_B
  dfesp$conta_C[i] <- conta_C 
  dfesp$conta_D[i] <- conta_D
  
# Definido qual Tipo de Uso de maior ocorrência:
  
  if((dfesp$conta_A[i] == 0) && (dfesp$conta_B[i] == 0) && (dfesp$conta_C[i] == 0) && (df_alex$conta_D[i] == 0))
    qual_maior_contagem = 0
  else
    qual_maior_contagem <- which.max(c(dfesp$conta_A[i], dfesp$conta_B[i], dfesp$conta_C[i], dfesp$conta_D[i]))
  
  if(qual_maior_contagem == 0) 
    dfesp$estilo_uso[i] = "Não definido !" 
  else 
    dfesp$estilo_uso[i] = switch(qual_maior_contagem, 
                                   "Estilo de Uso A - Uso Participativo no Espaço Virtual", 
                                   "Estilo de Uso B - Busca e Pesquisa no Espaço Virtual", 
                                   "Estilo de Uso C - Estruturação e Planejamento no Espaço Virtual", 
                                   "Estilo de Uso D - Ação Concreta e Produção no Espaço Virtual")

  conta_A = conta_B = conta_C = conta_D = 0
  
# Mais alguma outra ocorrência igual à maior ?

  if ((qual_maior_contagem > 0) && (qual_maior_contagem < 4)) {
    # Acertando os indices
    outra = c(46, 47, 48, 49)
    # procurando a segunda ocorrência    
    for (k in (qual_maior_contagem + 1):4){
      if (dfesp[i, outra[k]] == dfesp[i, outra[qual_maior_contagem]]) 
        dfesp[i, 50] = switch(k, "", paste(dfesp[i, 50], "e Estilo de Uso B - Busca e Pesquisa no Espaço Virtual"),
                                paste(dfesp[i, 50], "e Estilo de Uso C - Estruturação e Planejamento no Espaço Virtual"),
                                paste(dfesp[i, 50], "e Estilo de Uso D - Ação Concreta e Produção no Espaço Virtual"))
    }
  }
  
  qual_maior_contagem = 0
}

# Gravando nova planilha com os resultados
write_xlsx(
  list("espanha" = dfesp),
  path = "dados/dados_transformados_espanha.xlsx",
  col_names = TRUE,
  format_headers = TRUE,
  use_zip64 = FALSE
)

# acertando faixas etárias diferenciadas!
library(readxl)
df_idade <- read_excel("dados/dados_brutos_transformados_Original.xlsx", sheet = "trabalho")
View(df_idade)   

summary(df_idade$idade)
#Length     Class      Mode 
#   179 character character

table(df_idade$idade)
# 18 a 24 anos     25 a 34 anos     35 a 44 anos     45 a 54 anos     55 a 64 anos de 60 a 70 anos. 
#           22               61               55               33                7                1

plot(table(df_idade$idade))

barplot(table(df_idade$idade))

# criando nova variariavel
idade_nova = ""
# Incluindo variáveis no dataframe para receber as somatórias dos Estilos de Uso
df_idade <- cbind(df_idade, idade_nova)

# transformando (por aproximação/média)
for (i in 1:nrow(df_idade)) {
  if (df_idade$idade[i] == "18 a 24 anos") df_idade$idade_nova[i] = "de 17 a 20 anos."
  if (df_idade$idade[i] == "25 a 34 anos") df_idade$idade_nova[i] = "de 20 a 30 anos."
  if (df_idade$idade[i] == "35 a 44 anos") df_idade$idade_nova[i] = "de 30 a 40 anos."
  if (df_idade$idade[i] == "45 a 54 anos") df_idade$idade_nova[i] = "de 40 a 50 anos."
  if (df_idade$idade[i] == "55 a 64 anos") df_idade$idade_nova[i] = "de 50 a 60 anos."
}

