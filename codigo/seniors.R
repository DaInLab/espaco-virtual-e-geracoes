#Gerando números Aleatórios
#
library(readxl)
df_seniors <- read_excel("dados/seniors_2020.xlsx", 
                         +     sheet = "uned")
View(df_seniors) 

## We could re-generate the dataset by the following R code
seed <- as.double(1)
RANDU <- function() {
  seed <<- ((2^16 + 3) * seed) %% (2^31)
  seed/(2^31)
}
for(i in 1:400) {
  U <- c(RANDU(), RANDU(), RANDU(), RANDU(), RANDU())
  print(round(U[1:3], 6))
}

#Distribuição uniforme
# Source: https://www.datamentor.io/r-programming/examples/random-number/
# Descrição: Essas funções fornecem informações sobre a distribuição uniforme 
# no intervalo de min a max. runif gera desvios aleatórios
round(runif(66, min = 1, max = 66), 0) # define o intervalo entre 1 e 66

# A distribuição normal
# Descrição : Geração aleatória para a distribuição normal com média igual 
# à média e desvio padrão igual ao desvio padrão (sd).
round(rnorm(66, mean=mean(df_seniors$idade), sd=sd(df_seniors$idade)), 0) # fornecendo nossa própria média e desvio padrão

# Amostras aleatórias e permutações
# Descrição : "sample" pega uma amostra do tamanho especificado dos elementos de x usando com ou sem reposição.
g = sample(1:66)
g1 = sample(1:66, 66, replace=F)
sample(1:nrow(df_seniors), nrow(df_seniors), prob= c(rep(0.59, 39), rep(0.33, 22), rep(0.08, 5)), replace=F)
# [1] 25  5 56 59 23 50 52  7 22 10 35 31 39 12 48 42 30 32 37 38  3 14 29 36 62 55 33 21 61 54 41 26  2 34 18 15  8 11  9 53 46 20
# [43] 13 43 49  1  6 66 60 57 47 19 28 40  4 17 24 45 65 16 51 27 64 44 58 63

# Atribuindo o gênero no data frame, com a seguinte proporção: 59% (39) masculino, 33% (22) feminino, e 8% (5) ND/NA
# Etapa 1: gerando randomicamente os gêneros usando a função "sample" (https://r-lang.com/sample-function-in-r-with-example/)
genero = sample(1:nrow(df_seniors), nrow(df_seniors), prob= c(rep(0.59, 39), rep(0.33, 22), rep(0.08, 5)), replace=F)
# loop "masculino"
for (i in 1:39) {
  df_seniors[genero[i], 4] = "masculino"
}

# loop "feminino"
for (i in 40:61) {
  df_seniors[genero[i], 4] = "feminino"
}

# Verificando: as linhas/casos 27 64 44 58 63 ficarão com NA

# acertando as rspostas, de P1 a P40
library(readxl)
tabla_6 <- read_excel("dados/Estudio Del Uso Del Virtual de Los Seniors - extract p 151-152.xlsx", sheet = "tabla6")
View(tabla_6)   
