# Dados obtidos em "engenharia reversa" do artigo Estudio de Identificación de Los Estilos Del Uso Del Virtual de Los Seniors: perspectivas iniciales
# URL: https://repositorioaberto.uab.pt/bitstream/10400.2/11649/1/1099-2355-1-SM.pdf 

#Gerando números Aleatórios
#
if(!require(readxl)){
  install.packages("readxl")
  library(readxl)
}
library(readxl)
df_seniors <- read_excel("dados/seniors_2020.xlsx", sheet = "uned")
View(df_seniors) 

# Fonte: Set seed in R, https://r-coder.com/set-seed-r/
# Por que definir "seed" em R?
# Ao usar funções que amostram números pseudoaleatórios, cada vez que se as executa, obtém-se um resultado diferente.
# Isso implica que o código não é reproduzível, pois não se conhece a semente que R usou para gerar essa sequência.
# É possível que alguém queira que o seu código nâo seja reproduzível, mas existem vários casos em que a reprodutibilidade é desejada.
# Potanto, um seed em R é usado para:
#  1.Reproduzir a mesma saída em estudos de simulação.
#  2.Ajudar a depurar o código ao lidar com números pseudoaleatórios.
# Como definir semente em R?
# O objetivo da função set.seed do R é permitir que você defina um seed e um gerador (com o argumento kind) em R. 
# Vale a pena mencionar que:
# 1. O estado do gerador de números aleatórios é armazenado em .Random.seed (no ambiente global). É um vetor de inteiros cujo comprimento depende do gerador.
# 2. Se a semente não for especificada, R usa o relógio do sistema para estabelecer uma.

# Atribuindo o gênero no data frame, com a seguinte proporção: 59% (39) masculino, 33% (22) feminino, e 8% (5) ND/NA
# Etapa 1: gerando randomicamente os gêneros usando a função "sample" (https://r-lang.com/sample-function-in-r-with-example/)

set.seed(1234)

genero = sample(1:nrow(df_seniors), nrow(df_seniors), prob= c(rep(0.59, 39), rep(0.33, 22), rep(0.08, 5)), replace=F)
genero
# [1] 21 34  4 35 48 36 13 11 38 12 20 32  2 43 17 49 16 24  3 15 31  5  1 33 19 47  8 42 46
#[30] 27 22 14  6 37 28 51 18 26 64 45 60 56 25 54 23 61 52 39  9 44 10  7 59 53 29 50 55 40
#[59] 30 62 63 58 57 41 66 65
# loop "masculino"
for (i in 1:39) {
  df_seniors[genero[i], 4] = "masculino"
}

# loop "feminino"
for (i in 40:61) {
  df_seniors[genero[i], 4] = "feminino"
}

# Verificando: as linhas/casos 41 57 58 65 e 66 ficarão como gênero NA

string_idade <- c(14, 17,20,30,40,50,60,70,80)
names(string_idade) <- c("de 11 a 14 anos.", "de 14 a 17 anos.", "de 17 a 20 anos.",
                         "de 20 a 30 anos.", "de 30 a 40 anos.", "de 40 a 50 anos.",
                         "de 50 a 60 anos.", "de 60 a 70 anos.", "acima de 70 anos.")
for (i in 1:nrow(df_seniors)) {
  if (as.numeric(df_seniors$idade[i] == 0)) df_seniors$idade[i] <- NA # situação especial para este dataset !
  if (!is.na(as.numeric(df_seniors$idade[i])) && as.numeric(df_seniors$idade[i] != 0 )) {
    for (j in 1:length(string_idade)) {
      if (as.numeric(df_seniors$idade[i]) < string_idade[[j]]) {
        df_seniors$idade[i] <- names(string_idade[j])
        break
      }
    }
  }
}

# acertando as respostas, de P1 a P40

library(readxl)
tabla_6 <- read_excel("dados/Estudio Del Uso Del Virtual de Los Seniors - extract p 151-152.xlsx", sheet = "tabla6")
View(tabla_6)   

tabla_6
# A tibble: 40 × 2
I#TEMS RESPUESTAS
#   <chr>      <dbl>
# 1 P1            58
# 2 P2            21
# 3 P3            19
# 4 P4            24
# 5 P5            58
# 6 P6            19
# 7 P7            19
# 8 P8            15
# 9 P9             5
#10 P10           33
# … with 30 more rows

# Calculando a porcentagem das respostas
ind_prob <- round(tabla_6$RESPUESTAS/66,2)
ind_resp <- tabla_6$RESPUESTAS
tabla_6$porc <- ind_prob

# Atribuindo escolhas de tipo de uso no data frame, com as proporções calculadas pelas rotinas abaixo
# Etapa 1: gerando randomicamente as respostas usando a função "sample" (https://r-lang.com/sample-function-in-r-with-example/)

set.seed(1234)
p = matrix(nrow = nrow(df_seniors), ncol = nrow(df_seniors))
for (pindex in 1:40) {
  p_string <- sample(1:nrow(df_seniors), nrow(df_seniors), 
                      prob = c(rep(ind_prob[pindex], ind_resp[pindex]),
                               rep((1 - ind_prob[pindex]), (66 - ind_resp[pindex]))),
                      replace=F)
  
  for(k in 1:ind_resp[pindex]) {
    p[pindex, k] <- p_string[k]
  }

}

# Inserindo nos casos estabelecidos aleatoriamente as respostas 

# loop de preenchimento dos itens
coluna = c(6:45)            # # colunas de 6 a 45 correspondem aos itens de respostas P1 a P40
for (indice in 1:40) {      # 40 indices de resposta
  for (posicao in 1: 66) {  # posição dos casos
    if(!is.na(p[indice, posicao])) {
      df_seniors[p[indice, posicao], coluna[indice]] = 1  #  preenchendo com número 1 os casos com respostas positivas
    }
  }
} 

# checando os resultados
coluna = 6
while (coluna < 46) {
  ind_check[I(coluna - 5)] <- sum(df_seniors[, coluna], na.rm = T)
  print (paste0("coluna ", coluna, " P",I(coluna - 5), " Total: ", ind_check[I(coluna - 5)]))
  coluna = coluna + 1
}
tabla_6$check <- ind_check

# Etapa 2: Definir estilos de uso

# Estabelecendo as colunas com as ocorrências dos tipos A, B, C, D
tipo_A <- c(6, 11, 16, 19, 25, 28, 37, 40, 44, 45)
tipo_B <- c(7, 10, 15, 20, 24, 29, 36, 38, 39, 41)
tipo_C <- c(8, 12, 14, 21, 23, 30, 32, 33, 35, 42)
tipo_D <- c(9, 13, 17, 18, 22, 26, 27, 31, 34, 43)

# Variáveis de controle a serem acrescentadas
conta_A = conta_B = conta_C = conta_D = 0
estilo_uso = "" 

# Incluindo variáveis no dataframe para receber as somatórias dos Estilos de Uso
df_seniors <- cbind(df_seniors, conta_A, conta_B, conta_C, conta_D, estilo_uso)

# Calculando as ocorrências dos tipos A, B, C e D 
for (i in 1:nrow(df_seniors)) {
  for (j in 1:length(tipo_A)) {
    if (!is.na(df_seniors[i, tipo_A[j]])) conta_A = conta_A + 1
    if (!is.na(df_seniors[i, tipo_B[j]])) conta_B = conta_B + 1
    if (!is.na(df_seniors[i, tipo_C[j]])) conta_C = conta_C + 1
    if (!is.na(df_seniors[i, tipo_D[j]])) conta_D = conta_D + 1
  }
  # Atualizando o conteúdo das variáveis no arquivo
  df_seniors$conta_A[i] <- conta_A
  df_seniors$conta_B[i] <- conta_B
  df_seniors$conta_C[i] <- conta_C 
  df_seniors$conta_D[i] <- conta_D
  
  # Definido qual Tipo de Uso de maior ocorrência:
  if((df_seniors$conta_A[i] == 0) && (df_seniors$conta_B[i] == 0) && (df_seniors$conta_C[i] == 0) && (df_seniors$conta_D[i] == 0))
    qual_maior_contagem = 0
  else
    qual_maior_contagem <- which.max(c(df_seniors$conta_A[i], df_seniors$conta_B[i], df_seniors$conta_C[i], df_seniors$conta_D[i]))
  
  if(qual_maior_contagem == 0) 
    df_seniors$estilo_uso[i] = "Não definido !" 
  else 
    df_seniors$estilo_uso[i] = switch(qual_maior_contagem, 
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
      if (df_seniors[i, outra[k]] == df_seniors[i, outra[qual_maior_contagem]]) 
        df_seniors[i, 50] = switch(k, "", paste(df_seniors[i, 50], "e Estilo de Uso B - Busca e Pesquisa no Espaço Virtual"),
                                paste(df_seniors[i, 50], "e Estilo de Uso C - Estruturação e Planejamento no Espaço Virtual"),
                                paste(df_seniors[i, 50], "e Estilo de Uso D - Ação Concreta e Produção no Espaço Virtual"))
    }
  }
  
  qual_maior_contagem = 0
}

library(writexl)
# Gravando nova planilha com os resultados
write_xlsx(
  list("seniors" = df_seniors),
  path ="dados/dados_transformados_seniors.xlsx",
  col_names = TRUE,
  format_headers = TRUE,
  use_zip64 = FALSE
)
