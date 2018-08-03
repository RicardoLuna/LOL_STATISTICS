# Universidade Federal Rural de Pernambuco     #
# Disciplina: ComputaÃ§Ã£o para AnÃ¡lise de Dados #
# Professor: Ermerson Andrade                  #
# Alunos: Iverson Pereira                      #
#         Ricardo Luna                         #
################################################

library(tidyr)
library(dplyr)
library(stringr)

team<-c('Graves', 'Ornn', 'Annie', 'Gragas','Alistar')
k<-5

work_station<-'C:/Users/ricardo/Documents/LOL_STATISTICS'
setwd(work_station)

champ_names <- read.table("champions.txt", sep=",")
champ_names <- gather(champ_names, "champ")

dados<-read.csv('patch811.csv', stringsAsFactors = F)

# Concatena os valores dos nomes do champ como um Ãºnico como uma Ãºnica lista
dados<-dados%>%
  unite(col = "team_1",
        1,2,3,4,5,
        sep = "_")

dados<-dados%>%
  unite(col = "team_2",
        2,3,4,5,6,
        sep = "_")
dados$result[dados$result=='Victory']<-T
dados$result[dados$result=='Defeat']<-F
dados$result<-as.logical(dados$result)

tamanho_base <-length(dados$team_1)

df_knn<-data.frame(dist_team=rep(0,tamanho_base*2), classe = rep(0, tamanho_base*2))
for (x in 1:tamanho_base){
  df_knn$dist_team[x]<-sum(str_count(dados$team_1[x], team))
  df_knn$classe[x]<-dados$result[x]
  df_knn$dist_team[x+tamanho_base]<-sum(str_count(dados$team_2[x], team))
  df_knn$classe[x+tamanho_base]<-!dados$result[x]
}
df_knn<-df_knn[order(df_knn$dist_team, decreasing=TRUE),]

df_final<-df_knn[1:k,]

if(length(df_final$classe[df_final$classe == 1]) > length(df_final$classe[df_final$classe == 0])){
  print("O time tem mais chances de vencer!")
}else if (length(df_final$classe[df_final$classe == 1]) < length(df_final$classe[df_final$classe == 0])){
  print("O time tem mais chances de perder!")
}else{
  print("O time tem as mesmas chances de vitória e derrota!")
}

