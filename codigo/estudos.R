soma_pais_grafico <- table(euevgera$pais)
soma_pais_grafico[5] <- sum(soma_pais_grafico)
names(soma_pais_grafico) <- c("Brasil", "Espanha", "Mexico", "Portugal", "Total Geral")
soma_pais_grafico
#     Brasil     Espanha      Mexico    Portugal Total Geral 
#        982         268         138          23        1411 

pct_pais <- paste(round(unname(soma_pais_grafico) / (unname(soma_pais_grafico[5])) * 100), "%")
pct_pais
#[1] "70 %" "19 %" "10 %" "2 %"

ggplot(data = euevgera, aes(pais)) +
  geom_bar(fill = "orange") +
  labs(x = "Origem dos dados", y = "Quantidade") +
  annotate(geom = "text", x = c(1:5), y = c(1010, 295, 175, 55, 1410), label = as.vector(soma_pais_grafico)) 

graph.pais <- barplot(as.vector(soma_pais_grafico), 
                     xlab = "Origem dos dados", 
                     ylab = "CEUEV Questionários Aplicados",
                     col = "orange",
                     ylim = c(0,max(soma_pais_grafico) + 200),
                     names.arg = names(soma_pais_grafico))
text(x = graph.pais, y = soma_pais_grafico, label = unname(soma_pais_grafico), cex=1,pos=3)
axis(1, at=graph.pais, labels=pct_pais, tick=F, las=1, line=-2.0, cex=0.5)

# Gráfico tipo "pizza"
pie(casos_genero,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("red", "orange", "yellow"),
    labels = paste(names(casos_genero), "-", pct_genero))

# Gráfico do tipo barra
graph.genero <- barplot(casos_genero, 
                        xlab = "Gênero dos respondentes", 
                        ylab = "Quantidade",
                        col = "orange",
                        ylim = c(0,max(casos_genero) + 100))
text(x = graph.genero, y = casos_genero, label = unname(casos_genero), cex=1, pos=3)
axis(1, at=graph.genero, labels=pct_genero, tick=F, las=1, line=-6.0, cex= 3.0)

# Gráfico do tipo pizza no ggplot2
#Gráfico de pizza sem rótulos
data_genero = data.frame(genero=euevgera$genero)

pie = ggplot(data_genero, aes(x="", fill=genero)) + 
  geom_bar(width=1) +
  coord_polar(theta="y",start=0) +
  geom_text(aes(y="",label="")) +
  xlab("") + ylab("") +
  theme_bw() + 
  theme(legend.position = "none",
        panel.grid.major = element_line(color="grey60"),
        panel.border=element_blank())
pie

#Porcentagem de cada categoria
feminino = round((sum(data_genero$genero=="feminino")/length(data_genero$genero))*100)
feminino = paste("(",feminino,"%)",sep="")
masculino = round((sum(data_genero$genero=="masculino")/length(data_genero$genero))*100)
masculino = paste("(",masculino,"%)",sep="")
nao_repondeu = round((sum(data_genero$genero=="")/length(data_genero$genero))*100)
nao_repondeu =paste("(",nao_repondeu,"%)",sep="")  
 
#
if (!require(grid)) {
  install.packages(("grid"))
  library(grid)
}

#Adicionar rótulos com % no gráfico de pizza
grid.text(paste("Masculino",masculino,sep=" "), x=unit(0.62, "npc"), y=unit(0.66, "npc"), gp=gpar(fontsize=12, col="black"), rot = 00)
grid.text(paste("Feminino",feminino,sep=" "), x=unit(0.40, "npc"), y=unit(0.66, "npc"), gp=gpar(fontsize=12, col="black"), rot = 00)
grid.text(paste("NR/NS" ,nao_repondeu,sep=" "), x=unit(0.52, "npc"), y=unit(0.32, "npc"), gp=gpar(fontsize=12, col="black"), rot = 00)

casos_sexo <-
  for (i in 1:nrow(euevgera)) {
    euevgera$grupo[i] <- if(euevgera$genero[i] == "feminino") "Feminino" else "Masculino"
  }

ggplot(data = euevgera, aes(x = "", y=genero, fill = grupo)) +  
  geom_col() +
  #  geom_col(color = "black") +
  coord_polar(theta = "y")

library(stringr)
estilos_de_uso_geral <- euevgera$estilo # selecionando todos os casos da variável estilo de uso
qtde_estilo_uso <- subset(euevgera, estilo != "", select = estilo) # selecionando apenas os casos com estilo 
nrow(qtde_estilo_uso)

library(dplyr)
qtde_estilo_uso %>%
  select(estilo) %>%
  mutate(estilos_de_uso = substr(estilo, 1, 15))

# Gráfico do tipo pizza no ggplot2 dos estilos de uso
# Preparando os dados para o gráfico

qtde_estilo_uso = data.frame(estilo=euevgera$estilo)
qtde_estilo_uso <- subset(euevgera, estilo != "", select = estilo) # selecionando apenas os casos com a variável estilo diferente de branco/NA/NULL
nrow(qtde_estilo_uso)
#[1] 726
# Agregando os valores dos estilos de uso individualmente!
qtde_estilo_uso_tratado <- data.frame(estilo_uso_A = rep(0, length(estilos_de_uso_geral)), estilo_uso_B = rep(0, length(estilos_de_uso_geral)),
                                      estilo_uso_C = rep(0, length(estilos_de_uso_geral)), estilo_uso_D = rep(0, length(estilos_de_uso_geral)),
                                      estilo_uso_Nao_Identificado = rep(0, length(estilos_de_uso_geral)))
library(stringr)

for (i in 1:length(estilos_de_uso_geral)) {
  if(!is.na(str_match(estilos_de_uso_geral[i], "Estilo de Uso A - Uso Participativo no Espaço Virtual"))){
    qtde_estilo_uso_tratado$estilo_uso_A[i] <- 1
    #  qtde_estilo_uso_tratado$estilo_uso_B[i] = qtde_estilo_uso_tratado$estilo_uso_C[i] = qtde_estilo_uso_tratado$estilo_uso_D[i] = qtde_estilo_uso_tratado$estilo_uso_Nao_Identificado[i] = 0
  }
  if(!is.na(str_match(estilos_de_uso_geral[i], "Estilo de Uso B - Busca e Pesquisa no Espaço Virtual"))){
    qtde_estilo_uso_tratado$estilo_uso_B[i] <- 1
    #  qtde_estilo_uso_tratado$estilo_uso_A[i] = qtde_estilo_uso_tratado$estilo_uso_C[i] = qtde_estilo_uso_tratado$estilo_uso_D[i] = qtde_estilo_uso_tratado$estilo_uso_Nao_Identificado[i] = 0
  }
  if(!is.na(str_match(estilos_de_uso_geral[i], "Estilo de Uso C - Estruturação e Planejamento no Espaço Virtual"))){
    qtde_estilo_uso_tratado$estilo_uso_C[i] <- 1
    #  qtde_estilo_uso_tratado$estilo_uso_A[i] = qtde_estilo_uso_tratado$estilo_uso_B[i] = qtde_estilo_uso_tratado$estilo_uso_D[i] = qtde_estilo_uso_tratado$estilo_uso_Nao_Identificado[i] = 0
  }
  if(!is.na(str_match(estilos_de_uso_geral[i], "Estilo de Uso D - Ação Concreta e Produção no Espaço Virtual"))){
    qtde_estilo_uso_tratado$estilo_uso_D[i] <- 1
    #  qtde_estilo_uso_tratado$estilo_uso_A[i] = qtde_estilo_uso_tratado$estilo_uso_B[i] = qtde_estilo_uso_tratado$estilo_uso_C[i] = qtde_estilo_uso_tratado$estilo_uso_Nao_Identificado[i] = 0
  }
  if(!is.na(str_match(estilos_de_uso_geral[i], "Não Identificado"))){
    qtde_estilo_uso_tratado$estilo_uso_Nao_Identificado <- 1
    #  qtde_estilo_uso_tratado$estilo_uso_A[i] = qtde_estilo_uso_tratado$estilo_uso_B[i] = qtde_estilo_uso_tratado$estilo_uso_C[i] = qtde_estilo_uso_tratado$estilo_uso_D[i] = 0
  }
}

for (j in 1:1411) {
if(!is.na(str_match(estilos_de_uso_geral[j], "Não Identificado"))){
  
  print(paste(estilos_de_uso_geral[j], "j=", j))
  qtde_estilo_uso_tratado$estilo_uso_Nao_Identificado = 1
  
}
}
# Verificando os valores armazenados
sum(qtde_estilo_uso_tratado$estilo_uso_Nao_Identificado)
# [1] 1
sum(qtde_estilo_uso_tratado$estilo_uso_D)
# [1] 24
sum(qtde_estilo_uso_tratado$estilo_uso_C)
# [1] 168
sum(qtde_estilo_uso_tratado$estilo_uso_B)
# [1] 272
sum(qtde_estilo_uso_tratado$estilo_uso_A)
# [1] 428
  
# Gráfico 4, novo !
piramide = structure(list(`Faixa etária` = c("60 a 69", "70 a 79", "80 a 89","90 ou mais", "60 a 69", "70 a 79", "80 a 89", "90 ou mais"),
               sexo = c("Feminino", "Feminino", "Feminino", "Feminino", "Masculino", "Masculino", "Masculino", "Masculino"), 
               pop = c(401425,242451, 118671, 36191, 288951, 150313, 57404, 11262)), 
          row.names = c(NA, -8L),
          class = c("tbl_df", "tbl", "data.frame"))

ggplot(data = piramide, 
       mapping = aes(x = `Faixa etária`, y = ifelse(test = sexo == "Feminino",  yes = pop, no = -pop), fill = sexo)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = (max(piramide$pop))* c(-1,1)) +
  labs(y = "População", x = "Faixa etária (em anos)") +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  coord_flip()

# Versão 2
library(ggplot2)
library(dplyr)

abs_virgula <- function (x) {
  format(abs(x), big.mark = ".", decimal.mark = ",", scientific = FALSE)
}

piramide %>%
  mutate(sexo = factor(sexo, levels = c("Masculino", "Feminino"))) %>%
  ggplot(mapping = aes(x = `Faixa etária`,
                       y = ifelse(sexo == "Feminino",  yes = pop, no = -pop), fill = sexo)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs_virgula, limits = (max(piramide$pop))* c(-1,1)) +
  labs(y = "População", x = "Faixa etária (em anos)") +
  scale_fill_brewer(palette = "Set1", direction = 1) +
  theme(legend.position = "bottom") +
  coord_flip()

# Só age groups - faixa etária
library(AMR)

ages <- c(3, 8, 16, 54, 31, 76, 101, 43, 21)

#age_groups(x, split_at = c(12, 25, 55, 75), na.rm = FALSE)

# split into 0-49 and 50+
age_groups(ages, 50)
#[1] 0-49 0-49 0-49 50+  0-49 50+  50+  0-49 0-49
#Levels: 0-49 < 50+

# split into 0-19, 20-49 and 50+
age_groups(ages, c(20, 50))
#[1] 0-9   0-9   10-19 50-59 30-39 70-79 100+  40-49 20-29
#Levels: 0-9 < 10-19 < 20-29 < 30-39 < 40-49 < 50-59 < 60-69 < 70-79 < 80-89 < 90-99 < 100+

# split into groups of ten years
age_groups(ages, 1:10 * 10)
#[1] 0-9   0-9   10-19 50-59 30-39 70-79 100+  40-49 20-29
#Levels: 0-9 < 10-19 < 20-29 < 30-39 < 40-49 < 50-59 < 60-69 < 70-79 < 80-89 < 90-99 < 100+
age_groups(ages, split_at = "tens")
#[1] 0-9   0-9   10-19 50-59 30-39 70-79 100+  40-49 20-29
#Levels: 0-9 < 10-19 < 20-29 < 30-39 < 40-49 < 50-59 < 60-69 < 70-79 < 80-89 < 90-99 < 100+
  
# split into groups of five years
age_groups(ages, 1:20 * 5)
#[1] 0-4   5-9   15-19 50-54 30-34 75-79 100+  40-44 20-24
#21 Levels: 0-4 < 5-9 < 10-14 < 15-19 < 20-24 < 25-29 < 30-34 < 35-39 < 40-44 < 45-49 < ... < 100+
age_groups(ages, split_at = "fives")
#1] 0-4   5-9   15-19 50-54 30-34 75-79 100+  40-44 20-24
#21 Levels: 0-4 < 5-9 < 10-14 < 15-19 < 20-24 < 25-29 < 30-34 < 35-39 < 40-44 < 45-49 < ... < 100+
  
# split specifically for children
age_groups(ages, c(1, 2, 4, 6, 13, 17))
#[1] 2-3   6-12  13-16 17+   17+   17+   17+   17+   17+  
#Levels: 0 < 1 < 2-3 < 4-5 < 6-12 < 13-16 < 17+
age_groups(ages, "children")
#[1] 2-3   6-12  13-17 18+   18+   18+   18+   18+   18+  
#Levels: 0 < 1 < 2-3 < 4-5 < 6-12 < 13-17 < 18+
  
# \donttest{
# resistance of ciprofloxacin per age group
if (require("dplyr")) {
  example_isolates %>%
    filter_first_isolate() %>%
    filter(mo == as.mo("E. coli")) %>%
    group_by(age_group = age_groups(age)) %>%
    select(age_group, CIP) %>%
    ggplot_rsi(x = "age_group", minimum = 0)
}
# }

casos_idade <- table(euevgera$idade)
casos_idade
#                 acima de 70 anos  de 11 a 14 anos  de 14 a 17 anos  de 17 a 20 anos  de 20 a 30 anos 
#              76               14               83              120              130              252 
#de 30 a 40 anos  de 40 a 50 anos  de 50 a 60 anos  de 60 a 70 anos 
#            270              269              135               62

names(casos_idade) <- c("SR/ND", "+70 anos","11-14 anos","14-17 anos","17-20 anos","20-30 anos",
                 "30-40 anos","40-50 anos","50-60 anos","60 a 70 anos")
casos_idade
#       SR/ND     +70 anos   11-14 anos   14-17 anos   17-20 anos   20-30 anos   30-40 anos   40-50 anos 
#          76           14           83          120          130          252          270          269 
#  50-60 anos 60 a 70 anos 
#         135           62 

#Cálculo da porcentagem sem os dados faltantes, considerando apenas as respostas de estilos de uso individualmente!
pct_idade <- paste(round(unname(casos_idade) / sum(unname(casos_idade)) * 100,0), "%")
pct_idade
#[1] "5 %"  "1 %"  "6 %"  "9 %"  "9 %"  "18 %" "19 %" "19 %" "10 %" "4 %"

# Gráfico tipo "pizza" dos estilos de uso
pie(casos_idade,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("red", "orange", "yellow", "green", "black"),
    labels = paste(names(casos_idade), "-", pct_idade))

# Gráfico do tipo barra dos estilos de uso
graph.idade <- barplot(casos_idade, 
                             xlab = "Faixa Etária", 
                             ylab = "Quantidade",
                             col = "orange",
                             ylim = c(0,max(casos_idade) + 30))
text(x = graph.idade, y = casos_idade, label = unname(casos_idade), cex=1, pos=3)
axis(1, at=graph.idade, labels=paste("(", pct_idade, ")"), tick=F, las=1, line=-1.0, cex.axis= 1.1)

