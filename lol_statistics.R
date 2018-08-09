################################################
# Universidade Federal Rural de Pernambuco     #
# Disciplina: Computação para Análise de Dados #
# Professor: Ermerson Andrade                  #
# Alunos: Iverson Pereira                      #
#         Ricardo Luna                         #
################################################


# use this if you need to install the packages
install.packages('tidyr')
install.packages('dplyr')

# put here the libraries
library(tidyr)
library(dplyr)

# set here your work station
work_station<-'/home/iverson.luis/Downloads/LOL_STATISTICS/'
setwd(work_station)

# read the file with champ names
champ_names <- read.table("champions.txt", sep=",")
champ_names <- gather(champ_names, "champ")
champ_names <- champ_names$value

dados<-read.csv('patch811.csv', stringsAsFactors = F)

# Concatena os valores dos nomes do champ como um único como uma única lista
dados<-dados%>%
  unite(col = "team_1",
        1,2,3,4,5,
        sep = "_")

dados<-dados%>%
  unite(col = "team_2",
        2,3,4,5,6,
        sep = "_")

# Converter os valores para 1 ou 0
dados$result[dados$result=='Victory']<-T
dados$result[dados$result=='Defeat']<-F

# Converte o tipo
dados$result<-as.logical(dados$result)
# Win rate para toda a base
tamanho = length(champ_names)
win_rate_final<-matrix(1:2*length(champ_names),length(champ_names),2)
for (x in 1:tamanho){
  champ_posicao<-which(grepl(champ_names[x],dados$team_1))
  champ_posicao2<-which(grepl(champ_names[x],dados$team_2))
  win_rate<-dados$result[champ_posicao]
  win_temp<-!dados$result[champ_posicao2]
  win_rate<-c(win_rate,win_temp)
  valor<-mean(win_rate)
  print(paste('Champion ', champ_names[x], ' possuiu uma win rate de ', valor, '!'))
  win_rate_final[x,1]<-valor
  win_rate_final[x,2]<-champ_names[x]
}

colors <- c("green", "red", "blue", "yellow", "pink", "orange")

data_champion<-data.frame(names=win_rate_final[,2],
                          rate=as.numeric(win_rate_final[,1]),stringsAsFactors = F)
data_champion<-data_champion[order(data_champion$rate, data_champion$names),]

# win rate global
par(mfrow=c(1,2))
barplot(tail(data_champion$rate), 
        names.arg=tail(data_champion$names), 
        main="Higher win rate - Global",
        ylim=c(0,max(tail(data_champion$rate))),
        xlab="Champion",
        ylab="Win rate",
        col=colors)

legend("bottomright", legend = paste(round(tail(data_champion$rate), digits = 4)*100, '%'),
       col =colors, pch = 15)

barplot(head(data_champion$rate), 
        names.arg=head(data_champion$names), 
        main="Lower win rate - Global",
        ylim=c(0,max(head(data_champion$rate))),
        xlab="Champion",
        ylab="Win rate",
        col=colors)
legend("bottomright", legend = paste(round(head(data_champion$rate), digits = 4)*100, '%'),
       col =colors, pch = 15)

# champiom's win rate by region
rate_by_region <- c()
regions <- unique(dados$server)
tamanho = length(champ_names)
for (x in 1:tamanho){
  champ_pos_team1 <- which(grepl(champ_names[x],dados$team_1))
  champ_pos_team2 <- which(grepl(champ_names[x],dados$team_2))
  champ_data_team1 <- dados[champ_pos_team1, ]
  champ_data_team2 <- dados[champ_pos_team2, ]
  champ_data_team2$result <- !champ_data_team2$result
  
  champ_data <- rbind(champ_data_team1, champ_data_team2)

  champ_popularity <- c()
  
  for(y in 1:length(regions)){
    champ_popularity[y] <- sum(champ_data$server == regions[y])
  }
  print(champ_names[x])
  champ_popularity <- data.frame(server = regions, value = champ_popularity)
  
  print(champ_popularity)
  
  rate_by_region <- rbind(rate_by_region, data.frame(champ=champ_names[x], champ_rate_by_region, popularity = champ_popularity))
}

# most popular champions in the world
most_popular <- rate_by_region %>%
  group_by(champ, popularity.server) %>%
  summarise(count = sum(popularity.value)) %>%
  arrange(desc(count))

# most popular champions in the world
most_popular_by_region <- rate_by_region %>%
  filter(popularity.server != "www")  %>%
  group_by(champ, popularity.server) %>%
  summarise(count = sum(popularity.value)) %>%
  arrange(desc(count))

barplot(most_popular_by_region$count[1:10], 
        names.arg=most_popular_by_region$champ[1:10], 
        main="Most popular champions per region",
        xlab="Champion",
        ylab="Matches",
        col=colors)

legend("bottomright", legend = most_popular_by_region$popularity.server[1:10],
       col =colors, pch = 15)

barplot(head(most_popular$count), 
        names.arg=head(most_popular$champ), 
        main="Most popular champions - Global",
        xlab="Champion",
        ylab="Matches",
        col=colors)
legend("bottomright", legend = head(most_popular_by_region$popularity.server),
       col =colors, pch = 15)

barplot(tail(most_popular$count), 
        names.arg=tail(most_popular$champ), 
        main="Most popular champions - Global",
        xlab="Champion",
        ylab="Matches",
        col=colors)
legend("bottomright", legend = tail(most_popular$champ),
       col =colors, pch = 15)

# get the global rate for each champion
global_rate <- aggregate(formula = rate ~ champ,
                        FUN = mean,
                        data=rate_by_region)

