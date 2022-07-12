# Análise Exploratória dos Dados
# Author: João Pedro Albino
# 
# Importando os dados para o ambiente R
# Criando o dataframe de trabalho denominado "euevgera"
# ----
# Problema na importação do formato da variável "data.ano" em arquivos ".csv" por meio de read.table()!
# 05/11/2018 => 005-11-20??
# Solução: escrever uma função que receba uma string e a converte em uma data usando o formato desejado utilizando 
# “setAs” para defini-la como um método depois utilizá-la como função como parte das colClasses em read.table(). 
# Fonte: https://stackoverflow.com/questions/13022299/specify-custom-date-format-for-colclasses-argument-in-read-table-read-csv
setClass('myDate')
setAs("character","myDate", function(from) as.Date(from, format="%d/%m/%Y") )

euevgera <- read.table("./dados/dados_minerados.csv", 
                       header = TRUE, sep = ";", as.is = T,
                       colClasses = c("character", "myDate", "character", "factor", "factor", "character"))
# Totais de casos/observações
total_casos <- nrow(euevgera)
total_casos
#[1] 1411

# Variáveis importadas
nome_variaveis <- names(euevgera)
nome_variaveis
#[1] "pesquisador" "data.ano"    "pais"        "genero"      "idade"       "estilo"

# Os dados nesta análise foram compilados dos trabalhos desenvolvidos por pesquisadores 
# que utilizaram o questionário CEUEV (ANEXO 4) desenvolvidos em Barros (2008) e em Alonso e Gallego (2008),
# referentes a quatro países, Brasil, Espanha, México e Portugal.
# Os casos selecionados e analisados totalizam 1411 casos ou observções e foram consideradas as seguintes vaviáveis:
# "pesquisador", "data.ano", "pais", "genero", "idade" e "estilo".
# Fontes dos dados utilizadas:
# Pesquisadores                       | País      | Trabalho de origem
# María Carmen López Berlanga         | Espanha   | El estilo de uso del espacio virtual con estudiantes de Educación Secundaria - Berlanga, M. C. L., et. al, artigo, 2019.
# Alejandro Villegas Barrera          | México    | Herramienta para la creación de un Entorno Personal de Aprendizaje - Barrera, A. V., reporte, 2017.  
# Marcos Andrei Ota                   | Brasil    | Adaptatividade em Ambientes Virtuais: uma proposta para personalizar a aprendizagem em cursos híbridos de ensino superior - Ota, M. A., tese, 2018. 
# Adriana Aparecida de Lima Terçariol | Brasil    | Os Estilos de Uso dos Espaços Virtuais e as Redes Sociais na Pedagogia: Um Estudo Exploratório - Terçariol, A.A.L., e Barros, D, M., 2017.
# Daniela Melaré Vieira Barros        | Brasil    | ???
# Daniela Melaré Vieira Barros        | Portugal  | ???
# Cristina Sánchez Romero             | Espanha   | Estudio de Identificación de Los Estilos Del Uso Del Virtual de Los Seniors: perspectivas iniciales - Romero, C. S., et al., 2020.

# ------------------------------------------------
# Gráfico 1: Quantidade de casos por país
# concatenando os dados
casos_pais <- table(euevgera$pais)
casos_pais
#Brasil  Espanha   Mexico Portugal 
#   982      268      138       23 
# Acertando os campos para o Gráfico 1: adicionando coluna "Total Geral"
# Utilizando o barplot
soma_pais_grafico <- table(euevgera$pais)
soma_pais_grafico[5] <- sum(soma_pais_grafico)
names(soma_pais_grafico) <- c("Brasil", "Espanha", "Mexico", "Portugal", "Total Geral")
soma_pais_grafico
#     Brasil     Espanha      Mexico    Portugal Total Geral 
#        982         268         138          23        1411 
pct_pais <- paste(round(unname(soma_pais_grafico) / (unname(soma_pais_grafico[5])) * 100), "%")
pct_pais
#[1] "70 %"  "19 %"  "10 %"  "2 %"   "100 %"

# Utilizando barplot
graph.pais <- barplot(as.vector(soma_pais_grafico), 
                      xlab = "Origem dos dados", 
                      ylab = "CEUEV Questionários Aplicados",
                      col = "orange",
                      ylim = c(0,max(soma_pais_grafico) + 139),
                      names.arg = names(soma_pais_grafico))
text(x = graph.pais, y = soma_pais_grafico, label = unname(soma_pais_grafico), cex=1,pos=3)
axis(1, at=graph.pais, labels=paste("(",pct_pais,")"), tick=F, las=1, line=-1, cex=0.5)

# utilizando o ggplot2
library(ggplot2)
ggplot(data = euevgera, aes(pais)) +
  geom_bar(fill = "orange") +
  labs(x = "Origem dos dados", y = "Quantidade") +
  annotate(geom = "text", x = c(1:4), y = c(1010, 295, 175, 55), label = unname(casos_pais)) +
  facet_wrap(vars(sum(casos_pais)))

# ------------------------------------------------
#Gráfico 2: Quantidade de respondentes por sexo
casos_genero <- table(euevgera$genero)
casos_genero
#     feminino masculino 
#511       553       347

# Acertando os casos "em branco" ou "não respondeu" como "NS/NR"
names(casos_genero) <- c("NS/NR", "Feminino", "Masculino")
casos_genero
#    NS/NR  Feminino Masculino 
#      511       553       347
pct_genero <- paste(round(unname(casos_genero) / sum(unname(casos_genero)) * 100), "%")
pct_genero
#[1] "36 %" "39 %" "25 %"

# Gráfico tipo "pizza"
pie(casos_genero,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("red", "orange", "yellow"),
    labels = paste(names(casos_genero), "-", pct_genero))

# Gráfico do tipo pizza no ggplot2
#Gráfico de pizza sem rótulos
data_genero = data.frame(genero=euevgera$genero)

pie = ggplot(data_genero, aes(x="", fill=genero)) + 
  geom_bar(width=1) +
  coord_polar(theta="y",start=0) +
  geom_text(aes(y="",label="")) +
  xlab("") + ylab("Respondentes por gênero") +
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

#Adicione rótulos com % no gráfico de pizza
if (!require(grid)) {
  install.packages(("grid"))
  library(grid)
}

grid.text(paste("Masculino",masculino,sep=" "), x=unit(0.62, "npc"), y=unit(0.66, "npc"), gp=gpar(fontsize=12, col="black"), rot = 00)
grid.text(paste("Feminino",feminino,sep=" "), x=unit(0.40, "npc"), y=unit(0.66, "npc"), gp=gpar(fontsize=12, col="black"), rot = 00)
grid.text(paste("NR/NS" ,nao_repondeu,sep=" "), x=unit(0.52, "npc"), y=unit(0.32, "npc"), gp=gpar(fontsize=12, col="black"), rot = 00)

# Gráfico do tipo barra
graph.genero <- barplot(casos_genero, 
                      xlab = "Gênero dos respondentes", 
                      ylab = "Quantidade",
                      col = "orange",
                      ylim = c(0,max(casos_genero) + 100))
text(x = graph.genero, y = casos_genero, label = unname(casos_genero), cex=1, pos=3)
axis(1, at=graph.genero, labels=paste("(",pct_genero,")"), tick=F, las=1, line=-1.0, cex.axis= 1.1)

# ------------------------------------------------
# Gráfico 3: Quantidade de respondentes por Estilo de Uso do espaço virtual

library(stringr)
estilos_de_uso_geral <- euevgera$estilo # selecionando todos os casos da variável estilo de uso

estilo_uso_A <- length(na.omit(str_match(estilos_de_uso_geral, "Estilo de Uso A - Uso Participativo no Espaço Virtual")))
estilo_uso_B <- length(na.omit(str_match(estilos_de_uso_geral, "Estilo de Uso B - Busca e Pesquisa no Espaço Virtual")))
estilo_uso_C <- length(na.omit(str_match(estilos_de_uso_geral, "Estilo de Uso C - Estruturação e Planejamento no Espaço Virtual")))
estilo_uso_D <- length(na.omit(str_match(estilos_de_uso_geral, "Estilo de Uso D - Ação Concreta e Produção no Espaço Virtual")))
estilo_uso_Nao_Identificado <- length(na.omit(str_match(estilos_de_uso_geral, "Não Identificado")))
#estilo_uso_NR <- length(na.omit(str_match(estilos_de_uso_geral, " ")))

library(genTS)
estilo_uso_NR <- 0
for (i in 1:length(estilos_de_uso_geral)) {
  if(is_empty(estilos_de_uso_geral[i])) estilo_uso_NR <- estilo_uso_NR + 1
}

casos_estilo_absoluto <- c(estilo_uso_A, estilo_uso_B, estilo_uso_C, estilo_uso_D, estilo_uso_Nao_Identificado, estilo_uso_NR)
names(casos_estilo_absoluto) <- c("Estilo de Uso A",
                                  "Estilo de Uso B",
                                  "Estilo de Uso C",
                                  "Estilo de Uso D",
                                  "Não Identificado",
                                  "Sem resposta/Não Disponível")
casos_estilo_absoluto
#            Estilo de Uso A             Estilo de Uso B             Estilo de Uso C 
#                        428                         272                         168 
#            Estilo de Uso D            Não Identificado Sem resposta/Não Disponível 
#                         24                           2                         685 

# Em números absolutos, a quantidade de respondentes por estilo de uso do espaço virtual, notando maior número de respondentes
# para o Estilo de Uso A, ou seja, uso participativo no virtual com 428 respostas computadas, seguido pelo estilo de uso B,
# busca e pesquisa no espaço virtual com 272 respostas.
# O grande número de "Sem resposta" ou "Não Disponível" se deve ao envio de dados incompletos (faltantes) pelos pesquisadores.  

#Cálculo da porcentagem sem os dados faltantes, considerando apenas as respostas de estilos de uso individualmente!
pct_estilo <- paste(round(unname(casos_estilo_absoluto[1:5]) / sum(unname(casos_estilo_absoluto[1:5])) * 100,1), "%")
pct_estilo
#[1] "47.9 %" "30.4 %" "18.8 %" "2.7 %"  "0.2 %"

# Isolando os casos Sem Resposta
casos_estilo_absoluto[6]
#Sem resposta/Não Disponível 
#                        685
pct_estilo_sem_respostas <- paste(round(unname(casos_estilo_absoluto) / sum(unname(casos_estilo_absoluto)) * 100,1), "%")
pct_estilo_sem_respostas[6]
# [1] "43.4 %"

# Gráfico tipo "pizza" dos estilos de uso
pie(casos_estilo_absoluto[1:5],
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("red", "orange", "yellow", "green", "black"),
    labels = paste(names(casos_estilo_absoluto[1:5]), "-", pct_estilo[1:5]))

# Gráfico do tipo barra dos estilos de uso
graph.estilos_uso <- barplot(casos_estilo_absoluto[1:5], 
                        xlab = "Estilos de uso", 
                        ylab = "Quantidade",
                        col = "orange",
                        ylim = c(0,max(casos_estilo_absoluto[1:5]) + 100))
text(x = graph.estilos_uso, y = casos_estilo_absoluto[1:5], label = unname(casos_estilo_absoluto[1:5]), cex=1, pos=3)
axis(1, at=graph.estilos_uso, labels=paste("(", pct_estilo, ")"), tick=F, las=1, line=-1.0, cex.axis= 1.1)

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

for (i in 1:length(estilos_de_uso_geral)){
  
  if(!is.na(str_match(estilos_de_uso_geral[i], "Estilo de Uso A - Uso Participativo no Espaço Virtual"))){
  qtde_estilo_uso_tratado$estilo_uso_A[i] <- 1
  }
  if(!is.na(str_match(estilos_de_uso_geral[i], "Estilo de Uso B - Busca e Pesquisa no Espaço Virtual"))){
  qtde_estilo_uso_tratado$estilo_uso_B[i] <- 1
  }
  if(!is.na(str_match(estilos_de_uso_geral[i], "Estilo de Uso C - Estruturação e Planejamento no Espaço Virtual"))){
  qtde_estilo_uso_tratado$estilo_uso_C[i] <- 1
  }
  if(!is.na(str_match(estilos_de_uso_geral[i], "Estilo de Uso D - Ação Concreta e Produção no Espaço Virtual"))){
  qtde_estilo_uso_tratado$estilo_uso_D[i] <- 1
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

#Gráfico de pizza sem rótulos
library(ggplot2)
pie = ggplot(qtde_estilo_uso_tratado, aes(x="", fill = casos_estilo_absoluto)) + 
  geom_bar(width=1) +
  coord_polar(theta="y",start=0) +
  geom_text(aes(y="",label="")) +
  xlab("") + ylab("Respondentes por estilo de uso") +
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

#Adicione rótulos com % no gráfico de pizza
if (!require(grid)) {
  install.packages(("grid"))
  library(grid)
}

grid.text(paste("Masculino",masculino,sep=" "), x=unit(0.62, "npc"), y=unit(0.66, "npc"), gp=gpar(fontsize=12, col="black"), rot = 00)
grid.text(paste("Feminino",feminino,sep=" "), x=unit(0.40, "npc"), y=unit(0.66, "npc"), gp=gpar(fontsize=12, col="black"), rot = 00)
grid.text(paste("NR/NS" ,nao_repondeu,sep=" "), x=unit(0.52, "npc"), y=unit(0.32, "npc"), gp=gpar(fontsize=12, col="black"), rot = 00)

# Gráfico 4 Novo ! Faixa Etária dos respondentes
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

#Cálculo da porcentagem das faixas etárias
pct_idade <- paste(round(unname(casos_idade) / sum(unname(casos_idade)) * 100,0), "%")
pct_idade
#[1] "5 %"  "1 %"  "6 %"  "9 %"  "9 %"  "18 %" "19 %" "19 %" "10 %" "4 %"

# Gráfico tipo "pizza" das faixas etárias
pie(casos_idade,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("red", "orange", "yellow", "green", "black"),
    labels = paste(names(casos_idade), "-", pct_idade))

# Gráfico do tipo barra das faixas etárias
graph.idade <- barplot(casos_idade, 
                       xlab = "Faixa Etária", 
                       ylab = "Quantidade",
                       col = "orange",
                       ylim = c(0,max(casos_idade) + 30))
text(x = graph.idade, y = casos_idade, label = unname(casos_idade), cex=1, pos=3)
axis(1, at=graph.idade, labels=paste("(", pct_idade, ")"), tick=F, las=1, line=-1.0, cex.axis= 1.1)

# Versão 2, agregando as idades menores, de 11 à 20 anos!
idade_concat <- data.frame(idade=as.character(euevgera$idade))
for (k in 1:nrow(idade_concat)) {
  if((idade_concat$idade[k] == "de 11 a 14 anos") || (idade_concat$idade[k] == "de 14 a 17 anos")
     || (idade_concat$idade[k] == "de 17 a 20 anos")) idade_concat$idade[k] <- "de 11 a 20 anos"
}

casos_idade_concat <- table(idade_concat$idade)
casos_idade_concat
#                acima de 70 anos  de 11 a 20 anos  de 20 a 30 anos  de 30 a 40 anos  de 40 a 50 anos 
#             76               14              333              252              270              269 
#de 50 a 60 anos  de 60 a 70 anos 
#            135               62 
names(casos_idade_concat) <- c("SR/ND", "+70 anos","11-20 anos","20-30 anos",
                        "30-40 anos","40-50 anos","50-60 anos","60 a 70 anos")
casos_idade_concat
#       SR/ND     +70 anos   11-20 anos   20-30 anos   30-40 anos   40-50 anos   50-60 anos 60 a 70 anos 
#          76           14          333          252          270          269          135           62 

#Cálculo da porcentagem das faixas etárias
pct_idade <- paste(round(unname(casos_idade_concat) / sum(unname(casos_idade_concat)) * 100,0), "%")
pct_idade
#[1] "5 %"  "1 %"  "24 %" "18 %" "19 %" "19 %" "10 %" "4 %" 

# Gráfico tipo "pizza" das faixas etárias
pie(casos_idade_concat,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("red", "orange", "yellow", "grey"),
    labels = paste(names(casos_idade_concat), "-", pct_idade))

# Gráfico do tipo barra das faixas etárias
graph.idade_concat <- barplot(casos_idade_concat, 
                       xlab = "Faixa Etária", 
                       ylab = "Quantidade",
                       col = "orange",
                       ylim = c(0,max(casos_idade_concat) + 30))
text(x = graph.idade_concat, y = casos_idade_concat, label = unname(casos_idade_concat), cex=1, pos=3)
axis(1, at=graph.idade_concat, labels=paste("(", pct_idade, ")"), tick=F, las=1, line=-1.0, cex.axis= 1.1)



